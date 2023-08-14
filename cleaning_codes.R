################### CLEANING DATA ################################

setwd("C:/R") # Set the working directory

# Loading data
download.file("https://raw.githubusercontent.com/duongkhanhk29/MERITJUST/main/ESS9e03_1.zip",
              destfile = "ESS09.zip", mode = "wb") # Download the ZIP file
unzip("ESS09.zip") # Unzip the contents of the ZIP file

library(haven)
data <- read_sav("ESS9e03_1.sav") # Read the dataset
choose_vars <- read.csv("https://raw.githubusercontent.com/duongkhanhk29/MERITJUST/main/choose_vars.csv")
data <- subset(data, select = choose_vars$Var)

# Create occupation ladder
occupation_ladder <- function(vector) {
  vector <- ifelse(vector < 1000, NA, as.integer(substr(vector, 1, 1)))
  return(vector)
} 
data$isco08 <- 10- occupation_ladder(data$isco08) # reverse the scale
data$isco08p <- 10 - occupation_ladder(data$isco08p)
data$occf14b <- 10 - data$occf14b
data$occm14b <- 10 - data$occm14b

# remove other education level
education_level <- function(input) {
  input <- ifelse(input == 55, NA, input)
  return(input)
}
data$eisced <- education_level(data$eisced)
data$eiscedp <- education_level(data$eiscedp)
data$eiscedf <- education_level(data$eiscedf)
data$eiscedm <- education_level(data$eiscedm)

# Convert to binary after removing NA
two_level_vars <- sapply(data, function(x) length(unique(na.omit(x))) == 2)
data[two_level_vars] <- lapply(data[two_level_vars], 
                               function(x) ifelse(x == unique(na.omit(x))[1], 0, 1))

library(dplyr) # offset the negative scales
data <- data %>%
  mutate_if(~ any(. < 0, na.rm = TRUE), ~ . + abs(min(., na.rm = TRUE)))

saveRDS(data, file = "clean_data.RDS") # save to disk