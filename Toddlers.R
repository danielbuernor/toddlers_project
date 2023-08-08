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
## 1.What is the mean height, mean weight,  and mean waist among the population.  The variable of the means among the genders and age ranges

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
    mean_cadence = round(mean(`cadence_steps/ 30 secs`),2),
    mean_gait_speed = round(mean(`gait_speed_m/30 secs`), 2)
  )
## Correlation between the anthropometric characteristics (that is height weight and waist  measurement ) and their spatial parameters ( step length, stride length, step width)
## first, we will create a table for all mean values called correlated
library(corrplot)
correlated <- temporal_paramer_variations %>%
  right_join(spatial_parameter_variability, by=c('gender', 'age')) %>%
  left_join(anthropometric_characteristics, by=c('gender', 'age'))


# Calculating the correlation matrix between anthropometric characteristics and spatial parameters
correlation_matrix <- round(cor(correlated[, c("mean_height", "mean_weight", "mean_waist", "mean_step_length", "mean_stride_length", "mean_step_width")]),2)

corrplot(correlation_matrix, method = "number", type = "upper", tl.cex = 0.8)

## correlation of temporal parameter (cadence and gait speed)  to the anthropometric measure (height , weight and weight)
correlation_mat <- round(cor(correlated[, c("mean_cadence", "mean_gait_speed", "mean_height", "mean_weight", "mean_waist")]),2)
corrplot(correlation_mat, method = "number", type = "upper", tl.col = "black", tl.cex = 0.8)


## uploading dataframe to google drive
install.packages("googledrive")
library(googledrive)
drive_auth()
csv_file_path <- "correlated.csv"

write.csv(correlated, file = csv_file_path, row.names = FALSE)
drive_upload(csv_file_path)
