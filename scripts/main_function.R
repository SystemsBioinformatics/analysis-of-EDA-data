# Main function
# Installs necessary packages if they are not installed
# Sources all necessary scripts
# Checks and parses input parameters
# Generates report

# Install necessary packages if they are not installed already
packages <- c("dplyr", "tidyr", "tibble", "ggplot2", "ggpubr", "platetools", "viridis", "Cairo", "purrr",
              "lubridate", "broom", "readr", "stringi", "readxl", "mgcv", "yaml", "rmarkdown",
              "knitr", "kableExtra")
install.packages(setdiff(packages, rownames(installed.packages()))) 

# Source all necessary scripts
files.sources <- list.files("scripts") # List of source files
files.sources <- files.sources[!grepl("Rmd", files.sources) & !grepl("main", files.sources)] # Keep .R files only
files.sources <- file.path("scripts", files.sources)

for (file in files.sources){
  source(file)
} 

# Checks and parses input parameters
eda_bioassay_analysis <- function(pb_fm_file, pb_p_file, directory_name, ylab, report_title = NULL, design_file = NULL, sample_fm_file = NULL, 
                                  sample_p_file = NULL, sample_plate_id = NULL, sd_threshold = 3, 
                                  plate_threshold = 1, normalization = FALSE, neg_control = NULL, 
                                  pos_control = NULL, iteration_time = 60, iteration1 = 1, iteration2 = 4){
  
  # Check if arguments are missing for the main parameters
  if (is.null(pb_fm_file)){
    stop('argument "pb_fm_file" is missing, with no default')
  }
  
  if (is.null(pb_p_file)){
    stop('argument "pb_p_file" is missing, with no default')
  }
  
  if (is.null(directory_name)){
    stop('argument "directory_name" is missing, with no default')
  } else if (dir.exists(file.path(getwd(), directory_name))){ # check if directory exists
    stop("directory already exists.")
  } else {
    # if directory does not exist, create directory
    dir.create(file.path(getwd(), directory_name)) 
    # Set directory for results
    result_directory <- file.path(getwd(), directory_name)
  }
  
  if (is.null(ylab)){
    stop('argument "ylab" is missing, with no default')
  }
  
  # List of parameters that will be fed to the r R Markdown file
  parameters <- list()
  
  # Parse fractiomate data and store it in the parameters list
  pb_fm_data <- parse_fractiomate(pb_fm_file)
  parameters <- c(parameters, pb_fm_data = list(pb_fm_data))
  
  # Check extension of platereader file and determine which parser to use
  # .txt files are parsed using the parse_platereader function
  # excel files (.xls or .xlsx) are parsed using the parse_platereader_excel function
  # A design file must be supplied with an excel file
  if (tools::file_ext(pb_p_file) == "txt"){
    pb_p_data <- parse_platereader(pb_p_file)
  } else if ((tools::file_ext(pb_p_file) == "xlsx" | tools::file_ext(pb_p_file) == "xls") &
             !is.null(design_file)){
    pb_p_data <- parse_platereader_excel(pb_p_file, design_file)
  } else if ((tools::file_ext(pb_p_file) == "xlsx" | tools::file_ext(pb_p_file) == "xls") &
             is.null(design_file)){
    stop('argument "design_file" is missing')
  }
  
  # Store platereader data in the parameters list
  parameters <- c(parameters, pb_p_data = list(pb_p_data))
  
  
  # Check if sample data is supplied in addition to procedure blank data and parse it
  
  # Sample fractiomate data must be supplied if sample platereader data is supplied
  # And vice versa sample platereader data must be supplied if sample fractiomate data is supplied,
  # except if sample data is present in procedure blank platereader data (pb_p_data)
  if (is.null(sample_fm_file) & !is.null(sample_p_file)){
    stop('argument "sample_fm_file" is missing')
  } else if (!is.null(sample_fm_file) & is.null(sample_p_file) && (nlevels(as.factor(pb_p_data$m_plate)) <= 1)){
    stop('argument "sample_p_file" is missing')
  }
  
  # Parse sample fractiomate files and if necessary sample platereader files
  if (!is.null(sample_fm_file)){
    
    # Sample plate IDs must be provided and/or present in the excel platereader file defined with pb_p_file
    if (is.null(sample_plate_id)){
      stop('argument "sample_plate_id" is missing')
    } else if (all(c("m_plate", "m_order", "m_levels") %in% colnames(pb_p_data)) && !all(sample_plate_id %in% pb_p_data$m_plate)){ 
      stop(sprintf("sample_plate_id %s not found in design_file\n", 
                   as.character(sample_plate_id[which(!sample_plate_id %in% pb_p_data$m_plate)])))
    }
        
    # Number of plate IDs must match number of sample_fm_file
    if (length(sample_fm_file) > length(sample_plate_id)){
      stop('too few sample plate IDs (sample_plate_id) provided')
    } else if (length(sample_fm_file) < length(sample_plate_id)){
      stop('too many sample plate IDs (sample_plate_id) provided')
    }
    
    # A maximum of 6 samples/replicates are allowed to preserve quality of figures
    if (length(sample_fm_file) > 6){
      stop('analysis of maximum of 6 samples/replicates allowed to preserve quality of figures')
    }
    
    # Check SD threshold input
    if(!is.null(sd_threshold) && !is.numeric(sd_threshold)){
      message('numeric value expected for argument "sd_threshold". using default sd_threshold = 3.')
      sd_threshold <- 3
    }
    
    # Check plate threshold input 
    if(!is.null(plate_threshold) && plate_threshold > length(sample_fm_file)){
      message('argument "plate_threshold" too large. using default plate_threshold = 2.')
      plate_threshold <- 2
    } else if (!is.null(plate_threshold) && !is.numeric(plate_threshold)){
      message('numeric value expected for argument "plate_threshold". using default plate_threshold = 2.')
      plate_threshold <- 2
    } else if (!is.null(plate_threshold) && plate_threshold < 1){
      message('argument "plate_threshold" too small. using default plate_threshold = 2.')
      plate_threshold <- 2
    }
      
    # Parse sample fractiomate and platereader files if platereader data is not present in the procedure blank data file (pb_p_file)
    if (!is.null(sample_p_file) && !all(c("m_plate", "m_order", "m_levels") %in% colnames(pb_p_data))){
      
      # Check if a sample_p_file is provided for every sample_fm_file
      if (length(sample_fm_file) > length(sample_p_file)){
        stop('more sample fractiomate files (sample_fm_file) provided than sample platereader file (sample_p_file)')
        } else if (length(sample_fm_file) < length(sample_p_file)){
        stop('less sample fractiomate files (sample_fm_file) provided than sample platereader file (sample_p_file)')
        }
      
      # Parse sample fractiomate file(s) and platereader file(s)
      sample_fm_data <- list() # List where sample fractiomate data is stored
      sample_p_data <- list() # List where sample platereader data is stored
      for (idx in 1:length(sample_fm_file)){
        sample_fm_data <- c(sample_fm_data, list(parse_fractiomate(sample_fm_file[idx])))
        sample_p_data <- c(sample_p_data, list(parse_platereader(sample_p_file[idx])))
      }
      
      # Add parsed data to parameters list
      parameters <- c(parameters, sample_fm_data = list(sample_fm_data), sample_p_data = list(sample_p_data), 
                      sample_plate_id = list(as.character(sample_plate_id)))
      
    } # Parse sample fractiomate files if platereader data is present in the procedure blank data file (pb_p_file)
    else if (is.null(sample_p_file) && all(c("m_plate", "m_order", "m_levels") %in% colnames(pb_p_data)) |
               !is.null(sample_p_file) && all(c("m_plate", "m_order", "m_levels") %in% colnames(pb_p_data))){
      
      # Check if there are not too many sample_fm_file arguments provided
      if (length(sample_fm_file) > nlevels(as.factor(pb_p_data$m_plate))){
        stop('too many sample fractiomate files (sample_fm_file) provided than plates are present\n
               in the sample platereader file (sample_p_file)')
      }
      
      # Parse sample fractiomate file(s)
      sample_fm_data <- list() # List where sample fractiomate data is stored
      for (idx in 1:length(sample_fm_file)){
        sample_fm_data <- c(sample_fm_data, list(parse_fractiomate(sample_fm_file[idx])))
      }
      
      message('sample platereader data already present in "pb_p_file". using this data for further analysis')
      parameters <- c(parameters, sample_fm_data = list(sample_fm_data), 
                      sample_plate_id = list(as.character(sample_plate_id)))
    }
        
  }
  
  
  # Check parameters iteration_time, iteration1 and iteration2. These parameters will
  # be used for calculating slopes for plates that have been measured more than once
  if ("m_iter" %in% colnames(pb_p_data) && max(as.numeric(pb_p_data$m_iter)) > 1){
    if (is.null(iteration_time)){
      message('using default iteration_time = 60')
    } else if (!is.null(iteration_time) && !is.numeric(iteration_time)){
      message('argument "iteration_time" not valid. using default iteration_time = 60')
    } else {
      parameters <- c(parameters, iteration_time = iteration_time)
    }
    
    if (is.null(iteration1)){
      message('using default iteration1 = 1')
    } else if (!is.null(iteration1) && !is.numeric(iteration1)){
      message('argument "iteration1" not valid. using default iteration1 = 1')
    } else {
      parameters <- c(parameters, iteration1 = iteration1)
    }
    
    if (is.null(iteration2)){
      message('using default iteration2 = 4')
    } else if (!is.null(iteration2) && !is.numeric(iteration2)){
      message('argument "iteration2" not valid. using default iteration2 = 1')
    } else {
      parameters <- c(parameters, iteration2 = iteration2)
    }
  }
  
  
  # Check parameters for normalization, including normalization, neg_control and pos_control
  if (is.null(normalization) | !is.logical(normalization)){
    message('using default normalization = FALSE')
  } else if (normalization){
    if (is.null(neg_control) && is.null(pos_control)){
      stop('argument "neg_control" and "pos_control" is missing')
    } else if (!is.null(neg_control) && is.null(pos_control)){
      parameters <- c(parameters, normalization = normalization, neg_control = list(neg_control))
    } else if (!is.null(neg_control) && is.null(pos_control)){
      parameters <- c(parameters, normalization = normalization, pos_control = list(pos_control))
    } else if (!is.null(neg_control) && !is.null(pos_control)){
      parameters <- c(parameters, normalization = normalization, neg_control = list(neg_control), 
                      pos_control = list(pos_control))
    }
  }
  
  parameters <- c(parameters, ylab = ylab, sd_threshold = sd_threshold, plate_threshold = plate_threshold)
  
  # Produce HTML report
  rmarkdown::render(input = file.path("scripts", "EDA_html_report.Rmd"), output_dir = result_directory,
                    output_file = "EDA_report.html", params = parameters)
  
}


