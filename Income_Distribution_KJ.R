#This uses data from the Philippines: 
# Source: https://psa.gov.ph/sites/default/files/attachments/ird/pressrelease/tab2.pdf 

###Data Inputs####
# Find this data for your chosen country: 
#Income thresholds from data (per annum per capita)
inc_1 <- 23523  #Lowest Income Quintile
inc_2 <- 35886  #Second Quintile
inc_3 <- 53943  #Third quintile
inc_4 <- 91136  #Fourth Quintile

ave_income <- 67622 # Average Income
gini<- 0.4439 # https://psa.gov.ph/content/average-family-income-2015-estimated-22-thousand-pesos-monthly-results-2015-family-income

###Income Distribution Function####
set.seed(300)

# Function declaration
fgamma <- function(phi){gini-(1/(phi*4^phi))*1/beta(phi,phi + 1)}

phi <- uniroot(fgamma,lower=0.000001,upper=100)$root
beta <- (1/phi)*(ave_income)

# Simulated population
population <- rgamma(10000000,phi,rate=1/beta)  #This generate a population with the given income distribution. 

###CHECKS####
population_1 <- population[population<inc_1]
population_2 <- population[population<inc_2&population>inc_1]
population_3 <- population[population<inc_3&population>inc_2]
population_4 <- population[population<inc_4&population>inc_3]
population_5 <- population[population>inc_4]

# c is a function that combines the parameters
population.vec<-c(population_1, population_2, population_3, population_4, population_5)
summary(population.vec)
length(population.vec)

summary(population)
summary(population_1)
summary(population_2)
summary(population_3)
summary(population_4)

population_1

#library('Gini')

###Your own analysis starts####
#Gini(population_1)