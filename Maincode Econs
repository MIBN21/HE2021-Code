
#This uses data from the Philippines: 
# Source: https://psa.gov.ph/sites/default/files/attachments/ird/pressrelease/tab2.pdf 

###Data Inputs####
# Find this data for your chosen country: 
#Income thresholds from data (per annum per capita in yen)
inc_1 <- 997421  #Lowest Income Quintile
inc_2 <- 1521926  #Second Quintile
inc_3 <- 2145314  #Third quintile
inc_4 <- 3172829  #Fourth Quintile
#above is per person in household

#below is per earner
# inc_1 <- 2148148  #Lowest Income Quintile
# inc_2 <- 3277778  #Second Quintile
# inc_3 <- 4620370  #Third quintile
# inc_4 <- 6833333  #Fourth Quintile
#above is wrong

ave_income <- 2849000 # https://stats.oecd.org/index.aspx?queryid=66670
gini<- 0.334 # https://stats.oecd.org/index.aspx?queryid=66670 (after taxes and transfers)

###Income Distribution Function####
set.seed(300)
fgamma <- function(phi){gini-(1/(phi*4^phi))*1/beta(phi,phi + 1)} 
phi <- uniroot(fgamma,lower=0.000001,upper=100)$root
beta <- (1/phi)*(ave_income)
population <- rgamma(10000000,phi,rate=1/beta)  #This generate a population with the given income distribution. 

###CHECKS####
income_1 <- population[population<inc_1]
income_2 <- population[population<inc_2&population>inc_1]
income_3 <- population[population<inc_3&population>inc_2]
income_4 <- population[population<inc_4&population>inc_3]
income_5 <- population[population>inc_4]

income.vec<-c(income_1, income_2, income_3, income_4, income_5)
summary(income.vec)
length(income.vec)

summary(income)
summary(income_1)
summary(income_2)
summary(income_3)
summary(income_4)
###Your own analysis starts####

new_metadata <- data.frame(income)
View(new_metadata)

hist(income,
     main="Income Distribution Chart of Japan 2018",
     xlab="Income Values",
     col="blue",
     freq=TRUE
)

install.packages(c('tibble', 'dplyr', 'readr'))
