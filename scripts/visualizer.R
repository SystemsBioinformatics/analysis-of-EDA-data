# Functions for the visualization of fluorescence
# intensities and for diagnostic visualizations 
# of residuals

require(ggplot2)
require(platetools)
require(ggpubr)
require(viridis)
require(Cairo)

# Function to save plots in .png format
save_plot <- function(plot, filename, width, height){
  
  # Heatmap of procedure blank
  ggsave(
    filename = paste0(filename, ".png"),
    plot = plot,
    scale = 1,
    dpi = 300,
    type="cairo",
    width=width,
    height=height
  )
}

# Function that produces heatmaps of the plate
# data: data frame containing all data
# value: column with numerical values to be plotted
# well: column with well identifiers ("A1")
# title: title for the plot
# subtitle: subtitle for the plot
# ylab: y-axis label
# multiple: logical, if true plots mutliple heatmaps
# plate_id: column with plate identifiers. must be supplied if multiple = TRUE
heatmap <- function(data, value, well, title = NULL, subtitle = NULL, ylab = NULL, multiple = FALSE, plate_id = NULL){

  if (multiple){
    p <- raw_grid(data[[value]], data[[well]], data[[plate_id]], plate = 96, ncols = 2, size=7.5) +
      labs(title = title,
           subtitle = subtitle,
           fill = {if (!is.null(ylab)) stringr::str_wrap(ylab, floor(0.9 * stri_width(ylab))) else ylab}) +
      theme_dark() +
      theme(text = element_text(size = 12.5), plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm"),
            plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12.5),
            legend.title = element_text(size = 10, vjust = 0.75), legend.position = "bottom",
            legend.text = element_text(size = 10, angle = 45, vjust = 1, hjust=1)) +
      scale_fill_viridis(breaks = scales::pretty_breaks(n = 4))
  } else{
    p <- raw_map(data[[value]], data[[well]], plate = 96) +
      labs(title = title,
           subtitle = subtitle,
           fill = {if (!is.null(ylab)) stringr::str_wrap(ylab, floor(0.9 * stri_width(ylab))) else ylab}) +
      theme_dark() +
      theme(text = element_text(size = 12.5), plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm"),
            plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12.5),
            legend.title = element_text(size = 10, vjust = 0.75), legend.position = "bottom",
            legend.text = element_text(size = 10, angle = 45, vjust = 1, hjust=1)) +
      scale_fill_viridis(breaks = scales::pretty_breaks(n = 4))
  }
  
  return(p)
}

# Function to produce a scatterplot
# data: data frame containing all data
# x: column that should be plotted on the x-axis
# y: column that should be plotted on the y-axis
# col: column with groups to assign specific colors
# title: title for the plot
# subtitle: subtitle for the plot
# xlab: x-axis label
# ylab: y-axis label
# clab: color legend label
# line: logical, if true draws line through scatterplot
scatterplot <- function(data, x, y, col = NULL, title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, clab = NULL, line = TRUE){

  p <- data %>%
    ggplot(aes(x = get(x), y = get(y), col = if(is.null(col)) NULL else {get(col)})) +
    geom_point() +
    labs(title = title,
         subtitle = subtitle,
         x = xlab,
         y = ylab,
         col = clab) +
    theme_bw() +
    theme(text = element_text(size = 12.5), plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm"),
          plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12.5),
          legend.title = element_text(hjust = 0.5, size = 10), legend.position = "bottom", legend.text = element_text(size = 10)) +
    scale_colour_viridis_d()

  if (line){
    p <- p +
      geom_line()
  }

  return(p)
}

# Function that produces diagnostic plots of the residuals
residuals_diagnostic_plot <- function(data){

  # Observed values plotted against the fitted values
  p1 <- scatterplot(data, x = "fitted_value", y = "raw_value", title = "Observed vs. fitted values",
                 xlab = "Fitted", ylab = "Observed", line = FALSE)

  # Residuals plotted against the fitted values
  p2 <- scatterplot(data, x = "fitted_value", y = "residuals", title = "Residuals vs. fitted values",
                 xlab = "Fitted", ylab = "Residuals", line = FALSE) +
    geom_hline(yintercept = 0, linetype="dashed", color = "red")

  # Q-Q plot of the residuals
  p3 <- ggplot(data, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(title=" Q-Q Plot",
         x="Theoretical Quantiles",
         y="Sample Quantiles") +
    theme_bw() +
    theme(text = element_text(size = 12.5), plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm"),
          plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12.5),
          legend.title = element_text(hjust = 0.5, size = 10), legend.position = "bottom", legend.text = element_text(size = 10))

  # Frequency and density histogram of the residuals
  ybreaks <- seq(0, 16, 2) # set breaks for second y-axis
  bin_width <- round(max(data$residuals)/10, digits = 2) # define bin width
  n_obs <- sum(!is.na(data$residuals)) # number of observations
  p4 <- ggplot(data, aes(residuals)) +
    geom_histogram(aes(y = ..density..), binwidth = bin_width) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(data$residuals), sd = sd(data$residuals)),
                  color = "red", size=0.75) +
    labs(title="Residuals distribution",
         x=paste0("Residuals ", "(SD=", format(sd(data$residuals), digits = 3), ")")) +
    theme_bw() +
    theme(text = element_text(size = 12.5), plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm"),
          plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12.5),
          axis.title.x = element_text(size=12.5)) +
    scale_y_continuous("Density", sec.axis = sec_axis(
      trans = ~ . * bin_width * n_obs, name = "Frequency", breaks = ybreaks)) # add second y-axis

  # Combine all diagnostic plots into one single plot
  p <- annotate_figure(ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), align = "hv"),
                       top = text_grob("Diagnostic plots", size = 17.5, hjust=0.5, face="bold"))

  return(p)
}



