# Control-based normalization

require(dplyr)
require(stringi)

# Normalize data to controls
normalize <- function(fraction_data, columns, control_data, neg_control, pos_control){

  # Check if columns that should be normalized are present in the provided data
  if (!all(columns %in% colnames(fraction_data))){
    stop(sprintf("column %s not found in supplied fraction_data", columns[which(!columns %in% fraction_data)]))
  }
  
  if (is.null(neg_control) & !is.null(pos_control)){ # Normalize to negative control
    # Check if control positions are present in the data
    pos_control <- control_data %>%
      filter(m_well %in% pos_control) %>%
      group_by(m_plate) %>%
      summarise(pos_control = mean(raw_value))
    
    fraction_data <- fraction_data %>%
      inner_join(pos_control)
      
    for (column in columns){
      fraction_data[[paste0("norm_", column)]] <-  (fraction_data[[column]]/fraction_data$pos_control) * 100
    }
    
  } else if (!is.null(neg_control) & is.null(pos_control)){ # Normalize to positive control
    # Check if control positions are present in the data
    neg_control <- control_data %>%
      filter(m_well %in% neg_control) %>%
      group_by(m_plate) %>%
      summarise(neg_control = mean(raw_value))
    
    fraction_data <- fraction_data %>%
      inner_join(neg_control)
    
    # Normalize the indicated columns. Normalized columns get the prefix "norm_"
    for (column in columns){
      fraction_data[[paste0("norm_", column)]] <-  (fraction_data[[column]]/fraction_data$neg_control) * 100
    }
      
  } else if (!is.null(neg_control) & !is.null(pos_control)){ # Standardize using both negative and positive control
    # Check if control positions are present in the data
    neg_control <- control_data %>%
      filter(m_well %in% neg_control) %>%
      group_by(m_plate) %>%
      summarise(neg_control = mean(raw_value))
    
    pos_control <- control_data %>%
      filter(m_well %in% pos_control) %>%
      group_by(m_plate) %>%
      summarise(pos_control = mean(raw_value))
    
    fraction_data <- fraction_data %>%
      inner_join(neg_control) %>%
      inner_join(pos_control)
    
    for (column in columns){
      fraction_data[[paste0("norm_", column)]] <-  ((fraction_data[[column]] - fraction_data$pos_control)/
        (fraction_data$neg_control - fraction_data$pos_control)) * 100
    }
  }
  
  return(fraction_data = tibble(fraction_data) %>% select(-any_of(c("pos_control", "neg_control"))))
}
