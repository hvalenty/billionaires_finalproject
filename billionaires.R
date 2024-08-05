library(tidyverse)
library(ggplot2)

setwd('C:/Users/Valenty/VSCode/billionaires_finalproject')

df <- read.csv('data/Billionaires-Statistics.csv', na.strings = '')
names(df)
View(df)
table(df$country)

table(df$gender)

table(df$selfMade)
table(df$status)

head(df)
names(df)

library(ggcorrplot)
library(dplyr)
newdf <- df %>%
  select('finalWorth', 'age', 'birthYear', 'cpi_country',
         'cpi_change_country', 'gross_tertiary_education_enrollment',
         'gross_primary_education_enrollment_country', 'life_expectancy_country',
         'tax_revenue_country_country', 'total_tax_rate_country',
         'population_country')
# , 'gdp_country'
head(newdf)
View(newdf)

table(newdf$gdp_country)
#replace_na(newdf$gdp_country, '')
newdf$gdp_country <- str_remove(newdf$gdp_country, '[,]')
newdf$gdp_country <- as.numeric(str_remove(newdf$gdp_country, '\\$'))

drop_na(newdf)
cor_mat <- round(cor(newdf),2)
cor_mat
ggcorrplot(cor_mat, lab=T, type='lower')

ggcorrplot(cor_mat, lab=T, type='lower', method='circle')

df[isna(df$gdp_country), ] 
