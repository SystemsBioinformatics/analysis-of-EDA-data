# Functions to preprocess raw data
# preprocess_single preprocesses data
# of which plates have been measured
# once (iteration = 1). 
# preprocess_multiple preprocesses data
# of which plates have been measured
# multiple times (iteration > 1).

require(dplyr)
require(tidyr)

preprocess_single <- function(fm_data, p_data, plate_id = NULL){
  
  # If a plate is measured just once,
  # fractiomate data and platereader data of the
  # procedure blank are combined into a single data frame and
  # fraction number and well identifiers are added
  fraction_data <- fm_data$timing %>%
    rename(m_row = well_row, m_col = well_column) %>%
    arrange(ready_time) %>%
    mutate(m_fraction = 1:n(), m_well = paste0(LETTERS[m_row], m_col)) %>%
    inner_join(p_data) %>%
    rename(raw_value = m_value) # Add fractionnumber and well ID
  
  control_data <- p_data %>%
    filter(!m_row %in% fraction_data$m_row | !m_col %in% fraction_data$m_col) %>%
    rename(raw_value = m_value) %>%
    mutate(m_well = paste0(LETTERS[m_row], m_col)) # Add fractionnumber and well ID
  
  # If an excel platereader file was provided for the procedure blank platereader
  # file argument (p_file) and a background level was present, it is preprocessed 
  # by correcting the target signal for background fluorescence
  if (all(c("m_plate", "m_order", "m_levels") %in% colnames(p_data))){
    fraction_data <- fraction_data %>%
      filter(m_plate == plate_id) %>%
      select(-m_order) %>%
      spread(m_levels, raw_value) %>%
      rowwise() %>%
      mutate(raw_value = ifelse("background" %in% colnames(.), target - background, target)) %>% # Correct for background fluorescence
      ungroup() %>%
      select(-c(any_of(c("buffer", "background", "target"))))
    
    control_data <- control_data %>%
      filter(m_plate == plate_id) %>%
      select(-m_order) %>%
      spread(m_levels, raw_value) %>%
      rowwise() %>%
      mutate(raw_value = ifelse("background" %in% colnames(.), target - background, target)) %>% # Correct for background fluorescence
      ungroup() %>%
      select(-c(any_of(c("buffer", "background", "target"))))
  } else {
      fraction_data <- fraction_data %>%
        mutate(m_plate = plate_id)
      
      control_data <- control_data %>%
        mutate(m_plate = plate_id)
  }
  return(list(fraction_data = fraction_data, control_data = control_data))
}

preprocess_multiple <- function(fm_data, p_data, iteration_time, iter1, iter2, plate_id = NULL){
  # If a plate is measured more than once,
  # the slope of the fluorescence increase for each well is caluclated.
  # The calculate_slopes function also combines the fractiomate data and
  # platereader data into a single data frame
  data <- calculate_slopes(fm_data, p_data, iteration_time = iteration_time, 
                           iter1 = iter1, iter2 = iter2, 
                           plate_id = plate_id)
  
  # Calculate the slope of the fractionated wells
  fraction_data <- data$sample_data %>%
    arrange(ready_time) %>% 
    mutate(m_fraction = 1:n(), m_well = paste0(LETTERS[m_row], m_col)) # Add fractionnumber and well ID to fractionated data
  
  # Calculate the slope of the control wells
  control_data <- data$control_data  %>% 
    mutate(m_well = paste0(LETTERS[m_row], m_col)) # Add fractionnumber and well ID to control data
  
  return(list(fraction_data = fraction_data, control_data = control_data))
}
