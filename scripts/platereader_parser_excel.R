# Parser for platereader files in excel format

require(readxl)
require(tidyr)
require(dplyr)

parse_platereader_excel <- function(file, design_file){

  # Read in design file
  design <- read_excel(design_file)
  design$m_order <- as.character(design$m_order)
  design <- design %>%
    select(m_order, m_plate, m_levels)

  # Read in platereader file
  p_data <- read_excel(file, col_names = FALSE)
  empty_rows <- rowSums(is.na(p_data)) == ncol(p_data) # Detect empty rows containing only NAs
  p_data <- p_data[!empty_rows,] # Remove the empty rows from the data

  # For each 96-well plate measurement,
  # the first row consists of the column indices and
  # follows the following pattern:
  # NA, <column indices of the 96-well plate>, NA
  pattern <- c(NA, 1:(ncol(p_data)-2), NA)

  # Detect rows that match the pattern
  pattern_rows <- apply(p_data, 1, function(x) identical(as.integer(x), pattern))

  # Split the data based on the pattern
  # A list containing tibbles of each measurement separately is created
  p_data <- split(subset(p_data, !pattern_rows), cumsum(pattern_rows)[!pattern_rows])

  # Check if platereader data contains as many measurements as
  # is given in the design file
  if (length(p_data) != nrow(design)){
    stop("Design file does not match the platereader data.")
  }

  # Name each measurement according to the design file
  names(p_data) <- design$m_order

  # Unnest the platereader data list into a single tibble
  p_data <- tibble(m_order = names(p_data), p_data) %>%
    unnest(cols=c(p_data))

  # Apply column names
  colnames(p_data) <- c("m_order", "m_row", 1:(ncol(p_data)-3), "m_wavel")

  # Gather data into long form
  p_data <- p_data %>%
    gather(key = "m_col", value = "m_value", -c(1, 2, ncol(.))) %>%
    unite("m_well", c(m_row, m_col), sep = "", remove = FALSE) %>% # Add well identifiers
    mutate(m_row = as.numeric(as.factor(m_row)), # Convert row index to numbers to match fractiomate data format
           m_col = as.numeric(m_col))

  # Add information from design file
  p_data <- p_data %>%
    inner_join(design)

  return(p_data)
}
