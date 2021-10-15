# Script to run to perform a Bioassay Analysis

# Set working directory to this file's location
        
# Source main function
source("scripts/main_function.R") 

# Import and parse parameters
config = yaml::yaml.load_file("config_PFAS3.yml")

# Run analysis
eda_bioassay_analysis(pb_fm_file = config$pb_fm_file, 
                      pb_p_file = config$pb_p_file, 
                      directory_name = config$directory_name, 
                      report_title = config$report_title, 
                      ylab = config$ylab, 
                      design_file = config$design_file, 
                      sample_fm_file = config$sample_fm_file, 
                      sample_p_file = config$sample_p_file, 
                      sample_plate_id = config$sample_plate_id, 
                      sd_threshold = config$sd_threshold,
                      plate_threshold = config$plate_threshold,
                      normalization = config$normalization, 
                      neg_control = config$neg_control, 
                      pos_control = config$pos_control, 
                      iteration_time = config$iteration_time, 
                      iteration1 = config$iteration1, 
                      iteration2 = config$iteration2)
