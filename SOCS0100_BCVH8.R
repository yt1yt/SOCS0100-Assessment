install.packages("pacman") 
library(pacman) 
pacman::p_load(tidyverse) 
install.packages("dplyr") 
library(dplyr) 
install.packages("purrr") 
library(purrr)

library(dplyr) 

data <- read.csv("Vaccine2017.csv")
typeof('Vaccine2017.csv') 
str(data) 
data.frame(data) 

df <- data.frame(data, na.omit = TRUE) 
summary(df) 

library(purrr) 
df <- data %>% map_dbl(mean, na.rm = TRUE) 
df

pacman::p_load( tidyverse, skimr ) 
setwd("~/Desktop") 
rm(list = ls()) 

#check if "pacman" package is available, if not, install.
if (!require("pacman")) { 
  install.packages("pacman") 
} 

pacman::p_load( tidyverse,
                purrr, 
                kableExtra,
                flextable, 
                skimr) 

data <- read.csv("SOCS0100-Assessment/Vaccine2017.csv", header = TRUE)

#generate summarize statistics and information of the data set
skim(data)

#select the variables
library(dplyr)

#choose specific columns from data set
df <- data %>% select(Entity, Year, 
                      BCG.immunization.coverage.among.1.year.olds..WHO.2017.,  
                      Number.of.confirmed.tetanus.cases..WHO.2017., 
                      Estimated.deaths.due.to.tuberculosis.per.100.000.population..excluding.HIV..WHO.2017.) %>% 
  #rename the complicated variables' name into concise abbreviation.
  rename(Country = Entity, 
         BCG_coverage = BCG.immunization.coverage.among.1.year.olds..WHO.2017., 
         tetanus_cases = Number.of.confirmed.tetanus.cases..WHO.2017., 
         deaths = Estimated.deaths.due.to.tuberculosis.per.100.000.population..excluding.HIV..WHO.2017.) 

#to check the structure and variables after selection and rename of variables
glimpse(df) 

#calculate the average for the three selected variables
averages <- df %>% 
  filter(!Country %in% c("World", "Western Pacific", "South-East Asia", "Europe", "Africa")) %>% #excluding continents, only count countries in the calculation
  group_by(Country) %>% 
  summarise( 
    avg_BCG_coverage = mean(BCG_coverage, na.rm = TRUE), 
    avg_tetanus_cases = mean(tetanus_cases, na.rm = TRUE), 
    avg_deaths = mean(deaths, na.rm = TRUE)
  )#compute the average values with excluding missing values

#generate tables of top 10 averages of countries
top_10_BCG_coverage <- averages %>%
  arrange(desc(avg_BCG_coverage)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average BCG coverage")
top_10_BCG_coverage %>%
  kable_styling("bordered") %>%
  kable_classic(full_width = FALSE)

top_10_tetanus_cases <- averages %>%
  arrange(desc(avg_tetanus_cases)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average tetanus cases")
top_10_tetanus_cases %>%
  kable_styling("bordered") %>%
  kable_classic(full_width = FALSE)

top_10_deaths <- averages %>%
  arrange(desc(avg_deaths)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Deaths")
top_10_deaths %>%
  kable_styling("bordered") %>%
  kable_classic(full_width = FALSE)

#create list of two data frames, with specific countries along with average data
data_list <- list(
  data.frame(Country = c("Hungary", "Nigeria", "China"),
             avg_BCG_coverage = c(99.00000, 48.20000, 87.24242),
             avg_tetanus_cases = c(21.902439, 2279.529, 1515.400),
             avg_deaths = c(2.10625, 93.50000, 5.03125)),
  data.frame(Country = c("Fiji", "India", "Cambodia"),
             avg_BCG_coverage = c(97.94444, 62.11429, 71.90625),
             avg_tetanus_cases = c(0.900000, 21199.575, 554.09375),
             avg_deaths = c(3.80625, 45.81250, 97.2500))
)

#calculate the total average BCG coverage of the selected six countries
combined_data <- do.call(rbind, data_list)
total_avg_BCG_coverage <- mean(combined_data$avg_BCG_coverage)
total_avg_tetanus_cases <- mean(combined_data$avg_tetanus_cases)
total_avg_deaths <- mean(combined_data$avg_deaths)
print(total_avg_BCG_coverage)
print(total_avg_tetanus_cases)
print(total_avg_deaths)

#filtered the data based on the thresholds
thresholds <- c("total_avg_BCG_coverage" = 77.73457, "total_avg_tetanus_cases" = 4261.9, "total_avg_deaths" = 41.25104) #the average of the six countries

avgs <- c("total_avg_BCG_coverage", "total_avg_tetanus_cases", "total_avg_deaths")

filtered_data_list <- list() #to store the filtered data frames for each variable

for (var in avgs) {
  filtered_data <- map(data_list, ~ filter(.x, !!sym(var) >= thresholds[var])) #check the values in the specified variable are greater than or equal to the threshold
  filtered_data_list <- c(filtered_data_list, list(filtered_data))
} 

#create new variables into the data with re-coding
df <- df %>% mutate(Countries_with_high_BCG_coverage = ifelse(BCG_coverage > 97, 1, 0 ))
df <- df %>% mutate(Countries_with_high_tetanus_cases = ifelse(tetanus_cases > 70, 1, 0 ))
df <- df %>% mutate(Countries_with_high_deaths = ifelse(deaths > 38, 1, 0 ))

# Reshape from long to wide using pivot_wider and ignore NAs
df_wide_int <- df %>% # #reshape the date to create a wide-format data frame for integer columns
  pivot_wider(
    names_from = Country,
    values_from = c(BCG_coverage, tetanus_cases, deaths),
    values_fill = 0
  )

df_wide_double <- df %>% #for double columns
  pivot_wider(
    names_from = Country,
    values_from = c(Countries_with_high_BCG_coverage,   
                    Countries_with_high_tetanus_cases, Countries_with_high_deaths),
    values_fill = NA 
  )

library(ggplot2)
library(readr)
library(dplyr)
library(purrr)

df <- read_csv("Vaccine2017.csv")

combined_data <- combined_data %>% filter(Country %in% c("Hungary", "China", "India"))

mean_values <- combined_data %>%
  select(-Country) %>%
  map_dbl(mean, na.rm = TRUE)

# Create a list of combinations of variables to visualize
combinations <- list(
  list(x = "avg_BCG_coverage", y = "avg_tetanus_cases", title = "BCG Coverage vs. Tetanus Cases"),
  list(x = "avg_BCG_coverage", y = "avg_deaths", title = "BCG Coverage vs. Deaths"),
  list(x = "avg_tetanus_cases", y = "avg_deaths", title = "Tetanus Cases vs. Deaths")
)

# Create an empty list to store the plots
plots_list <- list()

# Loop through the combinations and create the plots
for (comb in combinations) {
  p <- ggplot(filtered_averages, aes_string(x = comb$x, y = comb$y, color = "Country")) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "grey") +
    labs(
      title = comb$title,
      y = comb$y
    )
  plots_list[[comb$title]] <- p
}

plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2) 
