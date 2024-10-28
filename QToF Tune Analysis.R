## QToF Tuning Data Analysis ##

rm(list=ls())

#### This package contains functions and ggplot formatting written by my friend Jack Hutchings (now at Wash U; jackh@wustl.edu).  
### To download: first install the package "devtools".  
## Then run install_github("jackahutchings/jahrfun")

library(jahrfun) # Load the "jahrfun" package from Jack.  
jah_settings() # Set your ggplot theme settings to Jack's preferences.

input <- "Tune Log.xlsx"

## Read in Excel Worksheets
{
  raw_tune_data <- list(check_tunes = read_excel(input,sheet = 1),
                        transmission_tunes = read_excel(input,sheet = 2),
                        system_tunes = read_excel(input,sheet = 3))
}

## Data Joining
tune_data <- raw_tune_data$check_tunes %>%
  mutate(tune_type = "check_tune") %>%
  full_join(raw_tune_data$transmission_tunes %>%
              mutate(tune_type = "transmission_tune")) %>%
  full_join(raw_tune_data$system_tunes %>%
              mutate(tune_type = "system_tune")) %>%
  filter(Theoretical %in% unique(.$Theoretical[which(.$tune_type == "check_tune")]))

## Plots
{
  tune_abundance <-
    tune_data %>%
    ggplot(aes(x = Date,Abundance,color = tune_type)) +
    geom_point() +
    facet_wrap(Mode ~ Theoretical,scales = "free_y",nrow = 2) +
    ggtitle("Abundance")
  
  tune_time <-
    tune_data %>%
    ggplot(aes(x = Date,Time,color = tune_type)) +
    geom_point() +
    facet_wrap(Mode ~ Theoretical,scales = "free_y",nrow = 2) +
    ggtitle("Time")  
  
  tune_calibration_abundance <-
    tune_data %>%
    ggplot(aes(x = Date,Calibration_Abundance,color = tune_type)) +
    geom_point() +
    facet_wrap(Mode ~ Theoretical,scales = "free_y",nrow = 2) +
    ggtitle("Calibration Abundance")
  
  tune_resolution <-
    tune_data %>%
    ggplot(aes(x = Date,Resolution,color = tune_type)) +
    geom_point() +
    facet_wrap(Mode ~ Theoretical,scales = "free_y",nrow = 2) +
    ggtitle("Resolution") 
  
  tune_primary_residuals <-
    tune_data %>%
    ggplot(aes(x = Date,Primary_Residuals,color = tune_type)) +
    geom_point() +
    facet_wrap(Mode ~ Theoretical,scales = "free_y",nrow = 2) +
    ggtitle("Primary Residuals") 
  
  tune_corrected_residuals <-
    tune_data %>%
    ggplot(aes(x = Date,Corrected_Residuals,color = tune_type)) +
    geom_point() +
    facet_wrap(Mode ~ Theoretical,scales = "free_y",nrow = 2) +
    ggtitle("Corrected Residuals")  
  }