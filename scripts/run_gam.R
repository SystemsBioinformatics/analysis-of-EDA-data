# Function to run a GAM analysis

require(mgcv)
require(dplyr)

# Fits a GAM model and returns the results
run_GAM <- function(data){
  
  # Set conditions for the use of this function
  # GAM is not applied if model has more
  # basis dimensions than data
  if (nrow(data) < (13 + 9)-1){
    stop("model has more coefficients (k) than data")
  }

  # Set seed
  set.seed(455)
  
  # Fit GAM model
  fit <- gam(raw_value ~ s(m_row, m_col, k = 13) + s(m_fraction, k = 9), data = data, method = "REML")

  # Create data frame in which predictions are stored
  nd <- expand.grid(list(m_row = seq(from = as.numeric(min(data$m_row)), to = as.numeric(max(data$m_row)), by = 1),
                         m_col = seq(from = as.numeric(min(data$m_col)), to = as.numeric(max(data$m_col)), by = 1)),
                    KEEP.OUT.ATTRS = FALSE)

  # Add fraction/time variable
  nd <- nd %>%
    inner_join(data %>%
                 select(m_row, m_col, m_fraction)) %>%
    arrange(m_fraction)

  # Make predictions and store them in the nd data frame
  nd <- cbind(nd, fitted_value=predict(fit, newdata = nd, se = TRUE)$fit)

  # Store all results in a ist
  GAMres <- list(fit = fit, fitted_data = nd)

  return(GAMres)
}

