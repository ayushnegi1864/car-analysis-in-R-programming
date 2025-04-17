# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Sample data: Car models and prices across three categories
cars <- data.frame(
  Model = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  Sedan = c(25000, 27000, 26000, 24000, 25500),
  SUV = c(32000, 34000, 31000, 33000, 33500),
  Truck = c(40000, 41000, 39500, 42000, 40500)
)

# Summary statistics
summary_stats <- function(category_prices) {
  list(
    Mean = mean(category_prices),
    Median = median(category_prices),
    Std_Dev = sd(category_prices)
  )
}

# Category list
categories <- c("Sedan", "SUV", "Truck")
for (category in categories) {
  cat("\nSummary for", category, ":\n")
  print(summary_stats(cars[[category]]))
}

# Highest & Lowest Prices
for (category in categories) {
  max_price <- max(cars[[category]])
  min_price <- min(cars[[category]])
  highest <- cars$Model[which.max(cars[[category]])]
  lowest <- cars$Model[which.min(cars[[category]])]
  
  cat("\nIn", category, ":\n")
  cat("Highest priced model:", highest, "with", max_price, "\n")
  cat("Lowest priced model:", lowest, "with", min_price, "\n")
}

# Average price bar plot
avg_prices <- colMeans(cars[2:4])
avg_df <- data.frame(Category = names(avg_prices), Average = avg_prices)
bar_plot <- ggplot(avg_df, aes(x = Category, y = Average, fill = Category)) +
  geom_bar(stat = "identity") + theme_minimal() +
  ggtitle("Average Car Prices by Category")
print(bar_plot)

# Histograms for each category
for (category in categories) {
  hist_plot <- ggplot(cars, aes(x = .data[[category]])) +
    geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
    theme_minimal() + ggtitle(paste("Price Distribution in", category)) +
    xlab("Price") + ylab("Frequency")
  print(hist_plot)
}

# Box plot
cars_long <- pivot_longer(cars, cols = Sedan:Truck,
                          names_to = "Category", values_to = "Price")
box_plot <- ggplot(cars_long, aes(x = Category, y = Price, fill = Category)) +
  geom_boxplot() + theme_minimal() +
  ggtitle("Boxplot of Car Prices by Category")
print(box_plot)

