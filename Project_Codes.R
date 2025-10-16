#Importing Datasets, Summarising Data
library(readxl)
data1 <- read_excel("E:\\Colleeggeeeee\\Stat_project\\car_reliability_repairpal_metrics_final.xlsx")
View(data1)
summary(data1)

library(readxl)
data2 <- read_excel("E:\\Colleeggeeeee\\Stat_project\\car_reliability_user_metrics_corrected_final.xlsx")
View(data2)
summary(data2)


# Study of Data Types for Dataset 1 (RepairPal)
str(data1)
sapply(data1, class)
sapply(data1, typeof)
sapply(data1, is.numeric)
sapply(data1, is.factor)

#checking for missing values
no_of_missing_value=sum(is.na(data1))
cat("The number of missing values in dataset 1 is ",no_of_missing_value)

# Study of Data Types for Dataset 2 (Broader Attributes)
str(data2)
sapply(data2, class)
sapply(data2, typeof)
sapply(data2, is.numeric)
sapply(data2, is.factor)

#checking for missing values
no_of_missing_value=sum(is.na(data1))
cat("The number of missing values in dataset 1 is",no_of_missing_value)

#checking for missing values
no_of_missing_value=sum(is.na(data2))
cat("The number of missing values in dataset 2 is",no_of_missing_value)

# Create tertile-based reliability categories
cuts <- quantile(repairpal$Reliability_Rating, probs=c(0,1/3,2/3,1))
repairpal$RelLevel <- cut(repairpal$Reliability_Rating,
                          breaks=cuts, labels=c("Low","Medium","High"), include.lowest=TRUE)
user$RelLevel     <- cut(user$Reliability_Rating, breaks=cuts,
                         labels=c("Low","Medium","High"), include.lowest=TRUE)

table(repairpal$RelLevel)  # e.g., Low  Medium High counts

#Code for discretizing Repair Frequency
freq_cuts <- quantile(repairpal$Repair_Frequency, probs=c(0,1/3,2/3,1))
repairpal$FreqLevel <- cut(repairpal$Repair_Frequency, breaks=freq_cuts,
                           labels=c("LowFreq","MedFreq","HighFreq"), include.lowest=TRUE)
user$FreqLevel      <- cut(user$Repair_Frequency, breaks=freq_cuts,
                           labels=c("LowFreq","MedFreq","HighFreq"), include.lowest=TRUE)
table(repairpal$FreqLevel)


# Histogram of Annual Repair Cost
hist(repairpal$Annual_Repair_Cost, main="Annual Repair Cost", xlab="Cost (USD)", col="lightblue")
# Boxplot of Annual Repair Cost by Reliability Level
boxplot(Annual_Repair_Cost ~ RelLevel, data=repairpal, main="Repair Cost by Reliability", ylab="Cost (USD)")
# Bar chart of Reliability category counts
barplot(table(repairpal$RelLevel), col="lightblue", main="Reliability Category Counts")

library(ggplot2)
 
# RepairPal numeric variables
 numeric_vars_rp <- c("Reliability_Rating", "Annual_Repair_Cost", "Repair_Frequency", "Severe_Repair_Probability")

# DISPLAY MULTIPLE HISTOGRAMS IN A MULTI-PANEL GRID
# Load patchwork for grid layout
library(patchwork)

# Create a list of plots
plot_list <- lapply(numeric_vars_rp, function(var) {
  ggplot(repairpal, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
})

#Combine using patchwork: arrange 2 plots per row
multi_panel_plot1 <- wrap_plots(plotlist = plot_list, ncol = 2)
# Display all at once
print(multi_panel_plot1)


# User numeric variables
numeric_vars_user <- c("Purchase_Cost", "Fuel_Economy")

# # Create a list of plots
plot_list <- lapply(numeric_vars_user, function(var) {
  ggplot(user, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
})

# Combine using patchwork: arrange 2 plots per row
multi_panel_plot2 <- wrap_plots(plotlist = plot_list, ncol = 2)

# Display all at once
print(multi_panel_plot2)

library(ggplot2)
library(gridExtra)
# Continuous predictors in RepairPal
boxplot_vars_rp <- c("Annual_Repair_Cost", "Repair_Frequency", "Severe_Repair_Probability")
plot_list <- list()

# Generate plots and store in a list
for (var in boxplot_vars_rp) {
  p <- ggplot(repairpal, aes_string(x = "RelLevel", y = var, fill = "RelLevel")) +
    geom_boxplot() +
    labs(title = paste(var, "by Reliability Level"), x = "Reliability Level", y = var) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  plot_list[[var]] <- p
}

# Arrange all plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

# Continuous predictors in User metrics
boxplot_vars_user <- c("Purchase_Cost", "Fuel_Economy")

for (var in boxplot_vars_user) {
  p <- ggplot(user, aes_string(x = "RelLevel", y = var, fill = "RelLevel")) +
    geom_boxplot() +
    labs(title = paste(var, "by Reliability Level"), x = "Reliability Level", y = var) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  plot_list[[var]] <- p
}

# Arrange all plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

# Categorize Repair Frequency
freq_cuts <- quantile(repairpal$Repair_Frequency, probs = c(0, 1/3, 2/3, 1))
repairpal$FreqLevel <- cut(repairpal$Repair_Frequency, breaks = freq_cuts, labels = c("LowFreq", "MedFreq", "HighFreq"), include.lowest = TRUE)

# Bar plot: Repair Frequency Level
ggplot(repairpal, aes(x = FreqLevel, fill = RelLevel)) +
  geom_bar(position = "dodge") +
  labs(title = "Repair Frequency Category by Reliability Level", x = "Repair Frequency Level", y = "Count") +
  theme_minimal()

library(tidyverse)

# Summary function for repairpal dataset
repairpal %>%
  group_by(RelLevel) %>%
  summarise(
    mean_RepairCost = mean(Annual_Repair_Cost),
    median_RepairCost = median(Annual_Repair_Cost),
    mean_SevereProb = mean(Severe_Repair_Probability),
    median_SevereProb = median(Severe_Repair_Probability),
    mean_Freq = mean(Repair_Frequency),
    median_Freq = median(Repair_Frequency)
  )

# Summary for user dataset
user %>%
  group_by(RelLevel) %>%
  summarise(
    mean_Purchase = mean(Purchase_Cost),
    median_Purchase = median(Purchase_Cost),
    mean_Fuel = mean(Fuel_Economy),
    median_Fuel = median(Fuel_Economy)
  )


#Chisq
freq_table <- table(repairpal$RelLevel, repairpal$FreqLevel)
chisq.test(freq_table)

#fligner killeenn
fligner.test(Annual_Repair_Cost ~ RelLevel, data = repairpal)

# Likewise, for other variables:
fligner.test(Severe_Repair_Probability ~ RelLevel, data = repairpal)

# Analyzing user dataset:
fligner.test(Purchase_Cost ~ RelLevel, data = user)
fligner.test(Fuel_Economy ~ RelLevel, data = user)

#Anova tests
anova_cost <- aov(Annual_Repair_Cost ~ RelLevel, data=repairpal)
summary(anova_cost)

anova_sev <- aov(Severe_Repair_Probability ~ RelLevel, data=repairpal)
summary(anova_sev)

#tukey 
# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_sev)

# View Tukey's test result for pairwise comparisons
print(tukey_result)


anova_pur <- aov(Purchase_Cost ~ RelLevel, data=user)
summary(anova_pur)

anova_fuel <- aov(Fuel_Economy ~ RelLevel, data=user)
summary(anova_fuel)

#Kruskal Wallis
kruskal.test(Annual_Repair_Cost ~ RelLevel, data=repairpal)
kruskal.test(Severe_Repair_Probability ~ RelLevel, data=repairpal)
kruskal.test(Purchase_Cost ~ RelLevel, data=user)
kruskal.test(Fuel_Economy ~ RelLevel, data=user)

#all Goodman Kruskal G

library(DescTools)

# Calculate Goodman-Kruskal's Gamma for ordinal variables
# Ensure both variables are factors with ordered levels

# RelLevel vs. FreqLevel (categorical repair frequency)
repairpal$RelLevel <- factor(repairpal$RelLevel, levels = c("Low", "Medium", "High"), ordered = TRUE)
repairpal$FreqLevel <- factor(repairpal$FreqLevel, levels = c("LowFreq", "MedFreq", "HighFreq"), ordered = TRUE)
GoodmanKruskalGamma(repairpal$RelLevel, repairpal$FreqLevel)


# Make sure RelLevel is treated as ordered
repairpal$RelLevel <- factor(repairpal$RelLevel, levels = c("Low", "Medium", "High"), ordered = TRUE)
user$RelLevel     <- factor(user$RelLevel, levels = c("Low", "Medium", "High"), ordered = TRUE)

# Load DescTools for GoodmanKruskalGamma
library(DescTools)

# Ensure ordered factors are declared correctly
repairpal$RelLevel <- factor(repairpal$RelLevel, levels = c("Low", "Medium", "High"), ordered = TRUE)
user$RelLevel     <- factor(user$RelLevel, levels = c("Low", "Medium", "High"), ordered = TRUE)

# Utility function for quantile-based ordinal binning
ordinal_bin <- function(x, labels) {
  breaks <- quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  cut(x, breaks = breaks, include.lowest = TRUE, labels = labels, ordered_result = TRUE)
}

# --- RelLevel vs. Fuel Economy (ordinalized) ---
user$FuelEcoLevel <- ordinal_bin(user$Fuel_Economy, c("LowMPG", "MedMPG", "HighMPG"))
GoodmanKruskalGamma(user$RelLevel, user$FuelEcoLevel)

# --- RelLevel vs. Severe Repair Probability (ordinalized) ---
repairpal$SevProbLevel <- ordinal_bin(repairpal$Severe_Repair_Probability, c("LowSev", "MedSev", "HighSev"))
GoodmanKruskalGamma(repairpal$RelLevel, repairpal$SevProbLevel)

# --- RelLevel vs. Purchase Cost (ordinalized) ---
user$PurchaseLevel <- ordinal_bin(user$Purchase_Cost, c("LowPrice", "MedPrice", "HighPrice"))
GoodmanKruskalGamma(user$RelLevel, user$PurchaseLevel)

# --- RelLevel vs. Repair Frequency (ordinalized) ---
repairpal$RfreqLevel <- ordinal_bin(repairpal$Repair_Frequency, c("LowFreq", "MedFreq", "HighFreq"))
GoodmanKruskalGamma(repairpal$RelLevel, repairpal$RfreqLevel)

# --- RelLevel vs. Annual Repair Cost (ordinalized) ---
repairpal$CostLevel <- ordinal_bin(repairpal$Annual_Repair_Cost, c("LowCost", "MedCost", "HighCost"))
GoodmanKruskalGamma(repairpal$RelLevel, repairpal$CostLevel)



