install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
library(readxl)
lower_limp_geonometry <- read_excel("lower_limp_geonometry.xlsx")
View(lower_limp_geonometry)

demog_amp_anthro_measurements <- read_excel("demog&amp;anthro_measurements.xlsx")
View(demog_amp_anthro_measurements)

spatiotemporal_gait_parameter <- read_excel("spatiotemporal_gait_parameter.xlsx")
View(spatiotemporal_gait_parameter)
head(lower_limp_geonometry)
sum(is.na(lower_limp_geonometry))

head(demog_amp_anthro_measurements)
sum(is.na(demog_amp_anthro_measurements))

head(spatiotemporal_gait_parameter)
sum(is.na(spatiotemporal_gait_parameter))
## joining all datasets using subjet_id as primary key
toddlers <- lower_limp_geonometry %>%
  right_join(demog_amp_anthro_measurements, by=c('subject_id')) %>%
  left_join(spatiotemporal_gait_parameter, by=c('subject_id'))
View(toddlers)
colnames(toddlers)
toddlers <- toddlers %>% rename(step_length_cm = 'step length_cm')

# verifying the data types using class() function
data_types_class <- sapply(toddlers, class)

clean_toddlers <- toddlers
##converting gender to categorical variable
clean_toddlers$gender <- factor(clean_toddlers$gender, levels = c("M", "F"), labels = c("Male", "Female"))
View(clean_toddlers)
#3 convert age to a whole number
clean_toddlers$age <- as.integer(round(clean_toddlers$age))
clean_toddlers$age

## Variability of anthropometric characteristics among the population
## 1.What is the mean height, mean weight,  and mean waist cm among the population.  The variable of the means among the genders and age ranges

anthropometric_characteristics <- clean_toddlers %>%
  group_by(gender, age) %>%
  summarise(
    mean_height = round(mean(height_cm), 2),
    mean_weight = round(mean(weight_kg), 2),
    mean_waist = round(mean(waist_cm), 2)
  )

## Variability of spatial parameters among the population 
## Mean step length, mean stride length and means step width of the the population and the variation among the genders and age groups
spatial_parameter_variability <- clean_toddlers %>%
  group_by(gender, age) %>%
  summarise(
    mean_step_length = round(mean(step_length_cm), 2),
    mean_stride_length = round(mean(stride_length_cm),2),
    mean_step_width = round(mean(step_width_cm), 2)
  )

## Variations of temporal parameter among the population
## Mean cadence, mean gait speed among the population and variability among genders and age groups
temporal_paramer_variations <- clean_toddlers %>%
  group_by(gender, age) %>%
  summarise(
    mean_cadence = round(mean(cadence),2),
    mean_gait_speed = round(mean(gait_speed), 2)
  )
## Correlation between the anthropometric characteristics (that is height weight and waist  measurement ) and their spatial parameters ( step length, stride length, step width)
## first, we will create a table for all mean values called correlated
library(corrplot)
correlated <- temporal_paramer_variations %>%
  right_join(spatial_parameter_variability, by=c('gender', 'age')) %>%
  left_join(anthropometric_characteristics, by=c('gender', 'age'))


# Calculating the correlation matrix between anthropometric characteristics and spatial parameters
correlation_matrix <- round(cor(correlated[, c("mean_height", "mean_weight", "mean_waist", "mean_step_length", "mean_stride_length", "mean_step_width")]),2)
width <- 800
height <- 800
png("correlation_plot.png", width = width, height = height)
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 1.5)
dev.off()  # Close the PNG deviceprint(correlation_matrix)

## correlation of temporal parameter (cadence and gait speed)  to the anthropometric measure (height , weight and weight)
correlation_mat <- round(cor(correlated[, c("mean_cadence", "mean_gait_speed", "mean_height", "mean_weight", "mean_waist")]),2)
corrplot(correlation_mat, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

## WHAT ARE THE TEMPORAL PARAMETERS AMONG TYPICALLY DEVELOPING TODDLER GAIT
## We will calculate the stride time in seconds
clean_toddlers$stride_time <- clean_toddlers$step_length_cm / clean_toddlers$stride_length_cm
clean_toddlers$stride_time

# we will calculate Gait Speed (in meters per second)
clean_toddlers$gait_speed <- clean_toddlers$stride_length_cm / (clean_toddlers$stride_time/100)

# Calculate Cadence (in steps per minute)
clean_toddlers$cadence <- clean_toddlers$stride_length_cm / clean_toddlers$step_length_cm * (60 / clean_toddlers$step_length_cm)
# Summary statistics for Gait Speed and Cadence
gait_speed_summary <- summarise(clean_toddlers, Mean_Gait_Speed = mean(gait_speed), SD_Gait_Speed = sd(gait_speed))
cadence_summary <- summarise(clean_toddlers, Mean_Cadence = mean(cadence), SD_Cadence = sd(cadence))

# Print the results
print("Gait Speed:")
print(gait_speed_summary$Mean_Gait_Speed)
print(gait_speed_summary$SD_Gait_Speed)

print("Cadence:")
print(cadence_summary$Mean_Cadence)
print(cadence_summary$SD_Cadence)

## Now we can answer the question as follows:

 ##Gait Speed: Mean Gait Speed: [Mean_Gait_Speed value] meters per second. Standard Deviation of Gait Speed: [SD_Gait_Speed value] meters per second

 ##Cadence: Mean Cadence: [Mean_Cadence value] steps per minute. Standard Deviation of Cadence: [SD_Cadence value] steps per minute


# Summary statistics for height, weight, and waist-hip ratio
height_summary <- summarise(clean_toddlers, Mean_Height = mean(height_cm), SD_Height = sd(height_cm))
weight_summary <- summarise(clean_toddlers, Mean_Weight = mean(weight_kg), SD_Weight = sd(weight_kg))
##whr_summary <- summarise(data, Mean_WHR = mean(waist_cm / hip_cm), SD_WHR = sd(waist_cm / hip_cm))

# Print the results
print("Height:")
print(height_summary$Mean_Height)
print(height_summary$SD_Height)

print("Weight:")
print(weight_summary$Mean_Weight)
print(weight_summary$SD_Weight)

## uploading dataframe to google drive
install.packages("googledrive")
library(googledrive)
drive_auth()
csv_file_path <- "correlated.csv"

write.csv(correlated, file = csv_file_path, row.names = FALSE)
drive_upload(csv_file_path)
