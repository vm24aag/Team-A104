library(tidyverse)

# Load the dataset
dataset <- read.csv("carbon-segment.csv")

# Save plots to a PDF
pdf("visualization.pdf")

# Scatterplots for each greenhouse gas against temperature with trendlines
# CO2 vs Temperature
ggplot(dataset, aes(x = CO2, y = Temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Correlation Between CO2 Concentration and Temperature", 
       x = "CO2 Concentration (ppm)", 
       y = "Annual Temperature Variation (Celsius)") +
  theme_minimal() %>% print()

# CH4 vs Temperature
ggplot(dataset, aes(x = CH4, y = Temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Correlation Between CH4 Concentration and Temperature", 
       x = "CH4 Concentration (ppb)", 
       y = "Annual Temperature Variation (Celsius)") +
  theme_minimal() %>% print()

# N2O vs Temperature
ggplot(dataset, aes(x = N2O, y = Temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "green") +
  labs(title = "Correlation Between N2O Concentration and Temperature", 
       x = "N2O Concentration (ppb)", 
       y = "Annual Temperature Variation (Celsius)") +
  theme_minimal() %>% print()

# CFC-11 vs Temperature
ggplot(dataset, aes(x = CFC.11, y = Temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "purple") +
  labs(title = "Correlation Between CFC-11 Concentration and Temperature", 
       x = "CFC-11 Concentration (ppt)", 
       y = "Annual Temperature Variation (Celsius)") +
  theme_minimal() %>% print()

# Histogram of Temperature with normal curve overlay
ggplot(dataset, aes(x = Temp)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$Temp, na.rm = TRUE), 
                                         sd = sd(dataset$Temp, na.rm = TRUE)), color = "black") +
  labs(title = "Distribution of Annual Temperature Variations", 
       x = "Annual Temperature Variation (Celsius)", 
       y = "Density") +
  theme_minimal() %>% print()

# Close PDF
dev.off()
