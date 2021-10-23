###Data Inputs####
# Find this data for your chosen country: 
#Income thresholds from data (per annum per capita)
inc_1 <- 997420.46  #Lowest Income Quintile
inc_2 <- 1521926.05  #Second Quintile
inc_3 <- 2145313.84  #Third quintile
inc_4 <- 3172828.89  #Fourth Quintile
inc_5 <- 4299226.14  #Fifth quintile taking 10,000,000 yen per household

ave_income <- 2849000 # https://stats.oecd.org/index.aspx?queryid=66670
gini<- 0.334 # https://stats.oecd.org/index.aspx?queryid=66670 (after taxes and transfers)

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
population_2 <- population[population<inc_2 & population>inc_1]
population_3 <- population[population<inc_3 & population>inc_2]
population_4 <- population[population<inc_4 & population>inc_3]
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
summary(population_5)

length(population_1)
length(population_2)
length(population_3)
length(population_4)
length(population_5)

hist(population_1,
     main="Income Distribution Chart of Japan 2018",
     xlab="Income Values",
     col="blue",
     freq=TRUE
)

popn_df <- data.frame(population_2)
View(popn_df)

library('ineq')

ineq(population.vec, type="Gini")

# From Irfan's code
Income_Data <- data.frame(population.vec)
colnames(Income_Data) <- c("Income")
Income_Data <- data.frame(Income_Data[order(Income_Data$Income),])

##Average consumption expenditure of each quintile per capita (big assumptions made here)
Q1 = 137856*12/2.326
Q2 = 192473*12/2.326
Q3 = 237683*12/2.326
Q4 = 282792*12/2.326
Q5 = 381189*12/2.326

##Average APC of each quintile (big assumptions made here)
###PROB need to change because APC1 is suppose to be above 100%
APC1 = Q1/inc_1*100
APC2 = Q2/inc_2*100
APC3 = Q3/inc_3*100
APC4 = Q4/inc_4*100
APC5 = Q5/inc_5*100

##Tax rate columns
Old_Tax_Rate<-seq(from = 8.0, to = 8.0, length.out = 10000000)
Old_Tax_Rate<-data.frame(Old_Tax_Rate)
New_Tax_Rate<-seq(from = 10.0, to = 10.0, length.out = 10000000)
New_Tax_Rate<-data.frame(New_Tax_Rate)

### APC columns
ClmAPC1<-seq(from = APC1, to = APC1, length.out = length(income_1))
ClmAPC2<-seq(from = APC2, to = APC2, length.out = length(income_2))
ClmAPC3<-seq(from = APC3, to = APC3, length.out = length(income_3))
ClmAPC4<-seq(from = APC4, to = APC4, length.out = length(income_4))
ClmAPC5<-seq(from = APC5, to = APC5, length.out = length(income_5))
ClmAPC = c(ClmAPC1,ClmAPC2,ClmAPC3,ClmAPC4,ClmAPC5)
ClmAPC<-data.frame(ClmAPC) 

Income_Data<-cbind(Income_Data, ClmAPC, Old_Tax_Rate,New_Tax_Rate)
colnames(Income_Data) <- c("Income","APC_Value","Old_Tax","New_Tax")
Income_Data$Consume <- Income_Data$Income * Income_Data$APC_Value /100
Income_Data$PreTaxChange <- Income_Data$Consume * Income_Data$Old_Tax/100
Income_Data$PostTaxChange <- Income_Data$Consume * Income_Data$New_Tax/100
Income_Data$RemainingInc <- Income_Data$Income - Income_Data$PostTaxChange

colnames(Income_Data) <- c("Income","APC_Value","Old_Tax","New_Tax","Consumption_Expenditure","Tax_Amount(Pre)","Tax_Amount(Post)","Remaining_Balance")
Income_Data <- Income_Data[c("Income","APC_Value", "Consumption_Expenditure", "Old_Tax","New_Tax","Tax_Amount(Pre)","Tax_Amount(Post)", "Remaining_Balance")]
View(Income_Data)

# New inequality amount post-tax policy
ineq(Income_Data$Remaining_Balance, type="Gini")
