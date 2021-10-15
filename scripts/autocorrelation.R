# Function to calculate autocorrelations
# and partial autocorrelations and 
# plot them

require(dplyr)
require(ggplot2)
require(ggpubr)

# Function to plot (partial) autocorrelations
acf_plot <- function(data){

  p <- ggplot(data, aes(x = Lag, y = value)) +
    geom_bar(stat="identity", width=.05) +
    geom_hline(yintercept = 0) +
    geom_hline(aes(yintercept = -CI), color="blue", linetype="dashed") +
    geom_hline(aes(yintercept = CI), color="blue", linetype="dashed") +
    labs(title = levels(as.factor(data$Function)),
         x = "Lag",
         y = "") +
    ylim(round(min(data$value), digits = 1) - 0.1, 1) +
    theme_bw() +
    theme(text = element_text(size = 15), plot.margin = unit(c(2.5,2.5,2.5,2.5), "mm"),
          plot.title = element_text(hjust = 0.5, size = 17.5), plot.subtitle = element_text(hjust = 0.5, size = 15),
          legend.title = element_text(size = 12.5), legend.position = "bottom", legend.text = element_text(size = 10))

  return(p)
}

# Function to calculate (partial) autocorrelations
autocorr <- function(data){
  autocor <- acf(x = data, lag.max = 20, plot = FALSE) # calculate autocorrelations
  pautocor <- pacf(x = data, lag.max = 20, plot = FALSE) # calculate partial autocorrelation

  # combine the autocorrelations and partial autocorrelations in a single data frame
  results <- rbind(tibble(Lag = autocor$lag, value = autocor$acf, Function = "ACF"),
                tibble(Lag = pautocor$lag, value = pautocor$acf, Function = "Partial ACF")) %>%
    group_by(Function) %>%
    mutate(CI = qnorm((1 + 0.95)/2)/sqrt(length(data))) %>% # calculate 95% confidence interval
    ungroup
  
  # plot the autocorrelations and partial autocorrelations
  p1 <- acf_plot(results[results$Function == "ACF",])
  p2 <- acf_plot(results[results$Function == "Partial ACF",])

  p <- ggarrange(p1, p2, ncol=2, labels = c("A","B"))
  
  return(list(results = results, fig = p))
}
