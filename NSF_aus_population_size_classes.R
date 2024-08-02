# Coral Size population structure data analysis
# NSF IRES Australia Summer '24
# R script by Hannah-Marie Lamle and Katrina Giambertone
# 6/7/2024


library(tidyverse)
install.packages("moments") #package needed to calculate skewness and kertosis
library(moments)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/hanna/OneDrive - Florida International University/R")

data_raw <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/Coral_size_structure_mermaid.csv")

### Acropora hyacinthus 
# 2024
Acropora_hyacinthus_2024 <- data_raw %>%
  filter(Year == "2024", 
         Species == "Acropora cf. hyacinthus") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Acropora_hyacinthus_2024)
  
Ah_skew24 <- skewness(Acropora_hyacinthus_2024$log_diameter) # 0-.61 meaning that this dataset is negatively skewed 
Ah_kurt24 <- kurtosis(Acropora_hyacinthus_2024$log_diameter) # 3.45 meaning dataset is relatively mesokurtic (pretty normally distributed, true)

Ah_24 <- ggplot(data = Acropora_hyacinthus_2024, aes(x = log_diameter))+
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2024", x = "", y = "")+
  scale_x_continuous(breaks = seq(1.4, 3.2, by = 0.2), limits = c(1.4, 3.2)) +  # Manually set x-axis breaks
  geom_text(x = 2.2, y = 17, label = paste("Skewness:", round(Ah_skew24, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 2.2, y = 15, label = paste("Kurtosis:", round(Ah_kurt24, 2)), vjust = 1.5, hjust = 1) 

# 2022
Acropora_hyacinthus_2022 <- data_raw %>%
  filter(Year == "2022", 
         Species == "Acropora cf. hyacinthus") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Acropora_hyacinthus_2022)
Ah_skew22 <- skewness(Acropora_hyacinthus_2022$log_diameter) #  meaning that this dataset is not very skewed. slightly positively skewed (tail extends out towards higher numbers)
Ah_kurt22 <- kurtosis(Acropora_hyacinthus_2022$log_diameter) #  meaning this data is slightly platykurtic (doesn't have a lot of peak in the middle of graph)

Ah_22 <- ggplot(data = Acropora_hyacinthus_2022, aes(x = log_diameter)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2022", x = "log-transformed colony diameter (mm)", y = "") +
  scale_x_continuous(breaks = seq(1.4, 3.2, by = 0.2), limits = c(1.4, 3.2)) +  # Manually set x-axis breaks
  geom_text(x = 2.2, y = 17, label = paste("Skewness:", round(Ah_skew22, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 2.2, y = 15, label = paste("Kurtosis:", round(Ah_kurt22, 2)), vjust = 1.5, hjust = 1)

# 2020
Acropora_hyacinthus_2020 <- data_raw %>%
  filter(Year == "2020", 
         Species == "Acropora cf. hyacinthus") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Acropora_hyacinthus_2020)
Ah_skew20 <- skewness(Acropora_hyacinthus_2020$log_diameter) #  meaning that this dataset is not very skewed. slightly positively skewed (tail extends out towards higher numbers)
Ah_kurt20 <- kurtosis(Acropora_hyacinthus_2020$log_diameter) #  meaning this data is slightly platykurtic (doesn't have a lot of peak in the middle of graph)

Ah_20 <-ggplot(data = Acropora_hyacinthus_2020, aes(x = log_diameter)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2020",x = "",  y = "Frequency") +
  scale_x_continuous(breaks = seq(1.4, 3.2, by = 0.2), limits = c(1.4, 3.2)) +  # Manually set x-axis breaks
  geom_text(x = 3.2, y = 20, label = paste("Skewness:", round(Ah_skew20, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 3.2, y = 18, label = paste("Kurtosis:", round(Ah_kurt20, 2)), vjust = 1.5, hjust = 1)
Ah_20

grid.arrange(Ah_20, Ah_22, Ah_24, ncol=3,
             widths = c(2,2,2),
             top = "Acropora cf. hyacinthus")

### Acropora Nasuta 
# 2024
Acropora_nasuta_2024 <- data_raw %>%
  filter(Year == "2024", 
         Species == "Acropora cf. nasuta") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Acropora_nasuta_2024)

An_skew24 <- skewness(Acropora_nasuta_2024$log_diameter) # 0-.61 meaning that this dataset is negatively skewed 
An_kurt24 <- kurtosis(Acropora_nasuta_2024$log_diameter) # 3.45 meaning dataset is relatively mesokurtic (pretty normally distributed, true)

An_24 <- ggplot(data = Acropora_nasuta_2024, aes(x = log_diameter))+
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2024", x = "", y = "")+
  scale_x_continuous(breaks = seq(1.7, 2.9, by = 0.2), limits = c(1.7, 2.9)) +  # Manually set x-axis breaks
  geom_text(x = 2.2, y = 22.5, label = paste("Skewness:", round(An_skew24, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 2.2, y = 21, label = paste("Kurtosis:", round(An_kurt24, 2)), vjust = 1.5, hjust = 1) 
An_24

# 2022
Acropora_nasuta_2022 <- data_raw %>%
  filter(Year == "2022", 
         Species == "Acropora cf. nasuta") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Acropora_nasuta_2022)
An_skew22 <- skewness(Acropora_nasuta_2022$log_diameter) #  meaning that this dataset is not very skewed. slightly positively skewed (tail extends out towards higher numbers)
An_kurt22 <- kurtosis(Acropora_nasuta_2022$log_diameter) #  meaning this data is slightly platykurtic (doesn't have a lot of peak in the middle of graph)

An_22 <- ggplot(data = Acropora_nasuta_2022, aes(x = log_diameter)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2022", x = "log-transformed colony diameter (mm)", y = "") +
  scale_x_continuous(breaks = seq(1.7, 2.9, by = 0.2), limits = c(1.7, 2.9)) +  # Manually set x-axis breaks
  geom_text(x = 2.2, y = 15, label = paste("Skewness:", round(An_skew22, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 2.2, y = 14, label = paste("Kurtosis:", round(An_kurt22, 2)), vjust = 1.5, hjust = 1)
An_22

# 2020
Acropora_nasuta_2020 <- data_raw %>%
  filter(Year == "2020", 
         Species == "Acropora cf. nasuta") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Acropora_nasuta_2020)
An_skew20 <- skewness(Acropora_nasuta_2020$log_diameter) #  meaning that this dataset is not very skewed. slightly positively skewed (tail extends out towards higher numbers)
An_kurt20 <- kurtosis(Acropora_nasuta_2020$log_diameter) #  meaning this data is slightly platykurtic (doesn't have a lot of peak in the middle of graph)

An_20 <-ggplot(data = Acropora_nasuta_2020, aes(x = log_diameter)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2020", x = "", y = "Frequency") +
  scale_x_continuous(breaks = seq(1.7, 2.9, by = 0.2), limits = c(1.7, 2.9)) +  # Manually set x-axis breaks
  geom_text(x = 2.95, y = 13, label = paste("Skewness:", round(An_skew20, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 2.95, y = 12, label = paste("Kurtosis:", round(An_kurt20, 2)), vjust = 1.5, hjust = 1)
An_20

grid.arrange(An_20, An_22, An_24, ncol=3,
             top = "Acropora cf. nasuta")

### Pocillopora meandrina
# 2024
Pocillopora_meandrina_2024 <- data_raw %>%
  filter(Year == "2024", 
         Species == "Pocillopora cf. meandrina") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Pocillopora_meandrina_2024)
Pm_skew24 <- skewness(Pocillopora_meandrina_2024$log_diameter) # 0-.61 meaning that this dataset is negatively skewed 
Pm_kurt24 <- kurtosis(Pocillopora_meandrina_2024$log_diameter) # 3.45 meaning dataset is relatively mesokurtic (pretty normally distributed, true)

Pm_24 <- ggplot(data = Pocillopora_meandrina_2024, aes(x = log_diameter))+
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2024", x = "", y = "")+
  scale_x_continuous(breaks = seq(1.2, 2.9, by = 0.2), limits = c(1.2, 2.9)) +  # Manually set x-axis breaks
  geom_text(x = 1.95, y = 11, label = paste("Skewness:", round(Pm_skew24, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 1.95, y = 10, label = paste("Kurtosis:", round(Pm_kurt24, 2)), vjust = 1.5, hjust = 1) 
Pm_24

# 2022
Pocillopora_meandrina_2022 <- data_raw %>%
  filter(Year == "2022", 
         Species == "Pocillopora cf. meandrina") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Pocillopora_meandrina_2022)
Pm_skew22 <- skewness(Pocillopora_meandrina_2022$log_diameter) #  meaning that this dataset is not very skewed. slightly positively skewed (tail extends out towards higher numbers)
Pm_kurt22 <- kurtosis(Pocillopora_meandrina_2022$log_diameter) #  meaning this data is slightly platykurtic (doesn't have a lot of peak in the middle of graph)

Pm_22 <- ggplot(data = Pocillopora_meandrina_2022, aes(x = log_diameter)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2022", x = "log-transformed colony diameter (mm)", y = "") +
  scale_x_continuous(breaks = seq(1.2, 2.9, by = 0.2), limits = c(1.2, 2.9)) +  # Manually set x-axis breaks
  geom_text(x = 1.95, y = 11, label = paste("Skewness:", round(Pm_skew22, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 1.95, y = 10, label = paste("Kurtosis:", round(Pm_kurt22, 2)), vjust = 1.5, hjust = 1)
Pm_22

# 2020
Pocillopora_meandrina_2020 <- data_raw %>%
  filter(Year == "2020", 
         Species == "Pocillopora cf. meandrina") %>%
  mutate(log_diameter = log10(Diameter..mm.))
summary(Pocillopora_meandrina_2020)
Pm_skew20 <- skewness(Pocillopora_meandrina_2020$log_diameter) #  meaning that this dataset is not very skewed. slightly positively skewed (tail extends out towards higher numbers)
Pm_kurt20 <- kurtosis(Pocillopora_meandrina_2020$log_diameter) #  meaning this data is slightly platykurtic (doesn't have a lot of peak in the middle of graph)

Pm_20 <-ggplot(data = Pocillopora_meandrina_2020, aes(x = log_diameter)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +  
  labs(title = "2020", x = "", y = "Frequency") +
  scale_x_continuous(breaks = seq(1.2, 2.9, by = 0.2), limits = c(1.2, 2.9)) +  # Manually set x-axis breaks
  geom_text(x = 1.95, y = 10.5, label = paste("Skewness:", round(Pm_skew20, 2)), vjust = 1.5, hjust = 1) +  
  geom_text(x = 1.95, y = 9.5, label = paste("Kurtosis:", round(Pm_kurt20, 2)), vjust = 1.5, hjust = 1)
Pm_20

grid.arrange(Pm_20, Pm_22, Pm_24, ncol=3,
             top = "Pocillopora cf. meandrina")

