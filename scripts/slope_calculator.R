# Function to calculate slopes of measurements that 
# were performed over multiple iterations

require(dplyr)
require(tidyr)
require(lubridate)
require(broom)
require(readr)

# Function that calculates the slopes
slopes <- function(data, iter1, iter2, iteration_time, fractiomate_data=NULL){
  # Fit a linear model for each fractionated well using iterations 1 - 4
  if ("ready_time" %in% colnames(data) && !is.null(fractiomate_data)){
    rates <- data %>%
      # Change the time type to numeric in order to be able to use it in a model
      mutate(ready_time = as.numeric(as.duration(ready_time))) %>%
      # Each iteration is performed after the given iteration_time in seconds
      mutate(ready_time = ready_time + (iteration_time*(as.numeric(m_iter)-1))) %>%
      # select iterations within interval
      filter(m_iter >= iter1 & m_iter <= iter2) %>%
      group_by(m_row, m_col) %>%
      do(fit = tidy(lm(m_value ~ ready_time, data = .))) %>%
      unnest(fit) %>%
      ungroup %>%
      # Remove the intercept values to keep the slopes only
      filter(term != "(Intercept)") %>%
      mutate(raw_value = estimate) %>%
      select(m_row, m_col, raw_value) %>%
      # Add fractionation and optical density data to the calculated rates
      inner_join(fractiomate_data$timing %>%
                   rename(m_row = well_row, m_col = well_column)) %>%
      arrange(ready_time)

  } else {
    rates <- data %>%
      # Each iteration is performed after the given iteration_time in seconds
      mutate(ready_time = ready_time + (iteration_time*(as.numeric(m_iter)-1))) %>%
      # select iterations within interval
      filter(m_iter >= iter1 & m_iter <= iter2) %>%
      group_by(m_row, m_col) %>%
      do(fit = tidy(lm(m_value ~ ready_time, data = .))) %>%
      unnest(fit) %>%
      ungroup %>%
      # Remove the intercept values to keep the slopes only
      filter(term != "(Intercept)") %>%
      mutate(raw_value = estimate) %>%
      select(m_row, m_col, raw_value)
  }

  return(rates)
}


# Join Fractiomate data and resazurin platereader data to calculate the rate of increase
# in fluorescence in RFU/s by using a linear model for each fractionated well
# over iteration interval [iter1, iter2]
calculate_slopes <- function(fractiomate_data, platereader_data, iter1, iter2, iteration_time,
                            plate_id=NULL){

  # Prepare sample data
  sample_data <- fractiomate_data$timing %>%
    # change column names for row and column on the well-plate to match the column names for row and column in density and resazurin data
    rename(m_row = well_row, m_col = well_column) %>%
    inner_join(platereader_data, by = c("m_row", "m_col"))

  # Prepare control data
  control_data <- platereader_data %>%
    filter(!(m_col %in% sample_data$m_col)) %>%
    mutate(ready_time = 0)


  if (iter2 > max(as.numeric(sample_data$m_iter))){
    message(paste0("maximum number of iterations is ", max(as.numeric(sample_data$m_iter)), ".\niteration2 = ", 
                   max(as.numeric(sample_data$m_iter))))
    iter2 <- max(as.numeric(sample_data$m_iter))
    
    if (iter1 > iter2){
      stop(paste0('argument "iter1" is larger than the maximum number of iterations:', max(as.numeric(platereader_data$m_iter)), '.\niteration1 = 1'))
    } 
    
  } else if (iter1 < 1 || iter2 < 1){
    stop("Minimum number of iterations is 1.")
  } 
  
  # Calculate the slopes over the given iterations
  sample_data <- slopes(sample_data, iter1, iter2, iteration_time, fractiomate_data)
  control_data <- slopes(control_data, iter1, iter2, iteration_time)

  # Add plate ID to data 
  if (!is.null(plate_id)){
    sample_data <- sample_data %>%
      mutate(m_plate = plate_id)

    control_data <- control_data %>%
      mutate(m_plate = plate_id)
  }

  return((list(sample_data=sample_data, control_data=control_data)))
}

