# Load required libraries
library(ggplot2)   
library(moments)     # For descriptive stats like skewness and kurtosis

ev_data <- read.csv(".\\DataSet\\EVChargingStationUsage.csv")

str(ev_data)

# Select relevant columns
ev_data <- ev_data[, c("End.Date", "Station.Name", "Charging.Time..hh.mm.ss.", "GHG.Savings..kg.", "Energy..kWh.", "Total.Duration..hh.mm.ss.")]

# Convert "End.Date" column to Date type
ev_data$`End.Date` <- as.Date(ev_data$`End.Date`)

# Convert "Charging.Time..hh.mm.ss." to minutes
duration_parts <- strsplit(as.character(ev_data$`Charging.Time..hh.mm.ss.`), ":")
duration_in_minutes <- sapply(duration_parts, function(x) {
  as.numeric(x[1]) * 60 + as.numeric(x[2]) + as.numeric(x[3]) / 60
})

total_duration_parts <- strsplit(as.character(ev_data$`Total.Duration..hh.mm.ss.`), ":")
total_duration_in_minutes <- sapply(total_duration_parts, function(x) {
  as.numeric(x[1]) * 60 + as.numeric(x[2]) + as.numeric(x[3]) / 60
})
# Add the calculated duration back to the dataframe
ev_data$Duration_Minutes <- duration_in_minutes
ev_data$Total_Duration_Minutes <- total_duration_in_minutes

# ----- Statistical Analysis of Data -----
# Central tendency (mean, median)
mean_duration <- mean(ev_data$Duration_Minutes, na.rm = TRUE)
median_duration <- median(ev_data$Duration_Minutes, na.rm = TRUE)

# Variation (variance and standard deviation)
variance_duration <- var(ev_data$Duration_Minutes, na.rm = TRUE)
sd_duration <- sd(ev_data$Duration_Minutes, na.rm = TRUE)

# Skewness and Kurtosis
skew_duration <- skewness(ev_data$Duration_Minutes)
kurtosis_duration <- kurtosis(ev_data$Duration_Minutes)

# ----- Data Visualization -----
# Plot duration over time
ggplot(ev_data, aes(x = `End.Date`, y = GHG.Savings..kg.)) +
  geom_line(color = "blue") +
  labs(title = "EV Charging Station GHG Saved Over Time", x = "Date", y = "Green House Gases Saved (kg)")

# Plot duration distribution
ggplot(ev_data, aes(x = Duration_Minutes)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of EV Charging Duration", x = "Duration (minutes)", y = "Count")

# ----- Correlation -----
# Correlation between duration and GHG Savings
cor_duration_ghg <- cor(ev_data$Duration_Minutes, ev_data$`GHG.Savings..kg.`, use = "complete.obs")

# Correlation between duration and Energy
cor_duration_energy <- cor(ev_data$Duration_Minutes, ev_data$`Energy..kWh.`, use = "complete.obs")

cor_duration_total <- cor(ev_data$Duration_Minutes, ev_data$Total_Duration_Minutes, use = "complete.obs")


# Scatter plot for correlation with GHG Savings
ggplot(ev_data, aes(x = `GHG.Savings..kg.`, y = Duration_Minutes)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot: Duration vs GHG Savings", x = "GHG.Savings..kg.", y = "Duration (minutes)")

# Scatter plot for correlation with Energy
ggplot(ev_data, aes(x = `Energy..kWh.`, y = Duration_Minutes)) +
  geom_point(color = "darkorange") +
  labs(title = "Scatter Plot: Duration vs Energy", x = "Energy..kWh.", y = "Duration (minutes)")

# ----- Regression Analysis -----
# Simple regression (Duration ~ GHG Savings)
simple_regression_ghg <- lm(Duration_Minutes ~ `GHG.Savings..kg.`, data = ev_data)
summary(simple_regression_ghg)



# Simple regression (Duration ~ Energy)
simple_regression_duration <- lm(Duration_Minutes ~ Total_Duration_Minutes, data = ev_data)
summary(simple_regression_duration)


# Multiple regression (Duration ~ GHG Savings + Energy)
multiple_regression <- lm(Duration_Minutes ~ `GHG.Savings..kg.` + `Energy..kWh.`, data = ev_data)
summary(multiple_regression)

# Regression diagnostics (Check residuals, assumptions)
ggplot(ev_data, aes(x = Duration_Minutes, y = Total_Duration_Minutes)) +
  geom_point(color = "blue", alpha = 0.5) +  
  labs(title = "Regression of Charging Duration vs Total Duration of Stay", 
       x = "Charging Duration (minutes)", y = "Total Duration Stayed (minutes)") +
  geom_smooth(method = "lm", se = FALSE, color = "green")

# ----- Classical Tests -----

# T-test to compare duration between two stations (e.g., Station 1 vs Station 2)
station1_duration <- ev_data[ev_data$`Station.Name` == "PALO ALTO CA / BRYANT #1", "Duration_Minutes"]
station2_duration <- ev_data[ev_data$`Station.Name` == "PALO ALTO CA / HAMILTON #1", "Duration_Minutes"]
t_test_result <- t.test(station1_duration, station2_duration)
t_test_result

# ANOVA to compare duration across multiple stations
anova_result <- aov(Duration_Minutes ~ `Station.Name`, data = ev_data)
summary(anova_result)

# ----- Regression Analysis with Prediction -----                                       

# Make predictions for GHG savings using the new model
ghg_regression <- lm(`GHG.Savings..kg.` ~ Duration_Minutes + `Energy..kWh.`, data = ev_data)

# Define new data for prediction (hypothetical values for Duration and Energy)
new_data_for_ghg <- data.frame(
  Duration_Minutes = c(120, 90, 60),  # Hypothetical charging times (in minutes)
  `Energy..kWh.` = c(6, 4, 5)        # Hypothetical energy consumption values
)

# Make predictions for GHG savings using the new model
predicted_ghg_savings <- predict(ghg_regression, newdata = new_data_for_ghg)


# Plot the predicted GHG savings vs the actual data
ggplot(ev_data, aes(x = Duration_Minutes, y = `GHG.Savings..kg.`)) +
  geom_point(color = "blue", alpha = 0.5) +  # Actual data points
  geom_point(data = new_data_for_ghg, aes(x = Duration_Minutes, y = predicted_ghg_savings), 
             color = "red", size = 3) +  # Predicted points
  labs(title = "Predicted vs Actual GHG Savings", 
       x = "Charging Duration (minutes)", y = "GHG Savings (kg)") +
  geom_smooth(method = "lm", se = FALSE, color = "green")


# ----- Conclusion -----
# Print summary of key statistics
cat("Mean Duration (minutes):", mean_duration, "\n")
cat("Median Duration (minutes):", median_duration, "\n")
cat("Variance of Duration:", variance_duration, "\n")
cat("Standard Deviation of Duration:", sd_duration, "\n")
cat("Skewness of Duration:", skew_duration, "\n")
cat("Kurtosis of Duration:", kurtosis_duration, "\n")
cat("Correlation between Duration and GHG Savings:", cor_duration_ghg, "\n")
cat("Correlation between Duration and Energy:", cor_duration_energy, "\n")
cat("Correlation between Charge Duration and Total Duration Stayed:", cor_duration_total, "\n")
cat("T-test result: p-value:", t_test_result$p.value, "\n")
cat("ANOVA result: p-value:", summary(anova_result)[[1]][["Pr(>F)"]][1], "\n")
cat("The predicted GHG Value is: ", predicted_ghg_savings)