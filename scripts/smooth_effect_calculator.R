# Function that extracts the effect
# of each smooth
require(dplyr)
require(tidyr)
require(mgcv)

# Function that removes values of continuous row and column indices and 
# retains only effect values for discrete row and column numbers
# This funcrion also stores the effect values in a data frame
extract_plate_effect <- function(plate_effect_data){
  # Extract values of the first smooth term (plate effect) and store them in a data frame
  plate_effect <- matrix(plate_effect_data$fit, nrow = length(plate_effect_data$x))
  colnames(plate_effect) <- plate_effect_data$y # add column grid points
  plate_effect <- as_tibble(plate_effect) # convert matrix to a tibble/data frame
  plate_effect$m_row <- plate_effect_data$x # add row grid points
  plate_effect <- plate_effect %>% # gather the data into long form for easier plotting
    gather("m_col", "value", -m_row) %>%
    mutate(m_col = as.numeric(m_col), value = as.numeric(value)) %>% # convert m_col and m_value columns to type numeric
    filter(m_row %% 1 == 0, m_col %% 1 == 0) %>% # keep only whole column and row indices
    mutate(m_well = paste0(LETTERS[m_row], m_col)) # add well labels

  return(plate_effect)
}

# Store fraction number effect values in a data frame
extract_fn_effect <- function(fn_effect_data){
  # Extract values of the second smooth term (fraction number effect) and store them in a data frame
  fn_effect <- tibble(m_fraction = fn_effect_data$x, value = fn_effect_data$fit, upper = time_effect_data$fit + time_effect_data$se,
                      lower = time_effect_data$fit - time_effect_data$se)

  return(fn_effect)
}


calculate_effect <- function(fit){
  
  # Specify the number of estimated data points
  n <- length(fit$fitted.values)
  
  # The individual smooth effects can be extracted from the plot.gam function
  # To estimate effect values for 2D functions, a square root of number of points 
  # should be supplied (n2). However, this could result in continuous row and column
  # indices instead of discrete, which is needed to properly display the effect in 
  # a plate heatmap format. To obtain values for discrete row and column numbers, a
  # square root number of points should be chosen that produces values for ALL discrete
  # row and column indices. The values of continues indices can then be removed
  # to keep only the effect for discrete indices.
  n_row <- nlevels(as.factor(fit$model$m_row)) # specify number of rows
  n_col <- nlevels(as.factor(fit$model$m_col)) # specify number of columns
  
  # generate sequences with possible square root of number of points using n_row - 1 steps 
  # and n_col - 1 steps
  nrow_range <- seq(n_row, 2000, n_row - 1)
  ncol_range <- seq(n_col, 2000, n_col - 1)

  # select the first number that is identical between both sequences
  # this suqare root of number will be able to
  # estimate values for both discrete row AND column numbers
  n2 <- nrow_range[nrow_range %in% ncol_range][1] 

  # extract all effects
  effect <- plot.gam(fit, n = n, n2 = n2, select = 0)

  # store plate and fraction number effect in separate data frames
  plate_effect <- extract_plate_effect(effect[[1]])
  fn_effect <- extract_fn_effect(effect[[2]])

  return(list(plate_effect = plate_effect, fn_effect = fn_effect))
}



