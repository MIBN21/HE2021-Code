#install.packages("ineq")
#This uses data from the Philippines: 
# Source: https://www.stat.go.jp/english/data/sousetai/es18.html (Table 3)

library("ineq")
library(dplyr)
library(tidyr)
library(stringr)

###Data Inputs####
# Find this data for your chosen country: 
#Income thresholds from data (per annum per capita in yen)

#Average income per household/average number of people in a household
Averagenuminhouse= (1.27+1.91+2.29+2.89+3.27)/5

#Average yearly income quintile per household/average number of people in a household
#to obtain average yearly income quintile per capita.

inc_1 <- 2320000/Averagenuminhouse #Lowest Income Quintile
inc_2 <- 3540000/Averagenuminhouse #Second Quintile
inc_3 <- 4990000/Averagenuminhouse #Third quintile
inc_4 <- 7380000/Averagenuminhouse #Fourth Quintile
#above is per person in household

ave_income <- 2849000 # https://www.ceicdata.com/datapage/charts/ipc_japan_annual-household-income-per-capita/?type=area&from=2009-12-01&to=2020-12-01&lang=en
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
summary(income_1)
summary(income_2)
summary(income_3)
summary(income_4)
summary(income_5)

###Your own analysis starts####

## Orginial Income Data
Income_Data <- data.frame(income.vec)
colnames(Income_Data) <- c("Income")
Income_Data <- data.frame(Income_Data[order(Income_Data$Income),])

Income_Data_recieved_voucher<-Income_Data[!Income_Data$Income>2560000,]+30000
AmountofVouchers<-length(Income_Data_recieved_voucher)

Income_Data_recieved_voucher<- data.frame(Income_Data_recieved_voucher)
Income_Data_DNrecieved_voucher<-Income_Data[!Income_Data$Income<=2560000,]
Income_Data_DNrecieved_voucher <- data.frame(Income_Data_DNrecieved_voucher)
colnames(Income_Data_recieved_voucher) <- c("Income")
colnames(Income_Data_DNrecieved_voucher) <- c("Income")

Income_Data_with_Redistributive<-rbind(Income_Data_recieved_voucher,Income_Data_DNrecieved_voucher)
Income_Data_with_Redistributive <- data.frame(Income_Data_with_Redistributive)
colnames(Income_Data_with_Redistributive) <- c("Income")

Income_Data_Total<-cbind(Income_Data,Income_Data_with_Redistributive)
colnames(Income_Data_Total) <- c("Income","Post-Policy-Income")

Income_Data_Red=Income_Data_Total
View(Income_Data_Red)

########################DO NOT TOUCH THE IF ELSE STATEMENT:THE INDENTATIONS ALL MATTER
##Analysis (30000yen per year per capita)
Remaining_people<-AmountofVouchers
Government_Spending_Total =Remaining_people*30000

if(Remaining_people>=length(income_1)){
  Government_Spending_Q1 = length(income_1)*30000
  Remaining_people=Remaining_people-length(income_1)
} else if (Remaining_people>0){

  Government_Spending_Q1 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q1 =0
}

if(Remaining_people>=length(income_2)){
  Government_Spending_Q2 = length(income_2)*30000
  Remaining_people=Remaining_people-length(income_2)
}else if (Remaining_people>0){
  Government_Spending_Q2 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q2 =0
}

if(Remaining_people>=length(income_3)){
  Government_Spending_Q3 = length(income_3)*30000
  Remaining_people=Remaining_people-length(income_3)
}else if (Remaining_people>0){
  Government_Spending_Q3 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q3 =0
}

if(Remaining_people>=length(income_4)){
  Government_Spending_Q4 = length(income_4)*30000
  Remaining_people=Remaining_people-length(income_4)
}else if (Remaining_people>0){
  Government_Spending_Q4 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q4 =0
}

if(Remaining_people>=length(income_5)){
  Government_Spending_Q5 = length(income_5)*30000
  Remaining_people=Remaining_people-length(income_5)
}else if (Remaining_people>0){
  Government_Spending_Q5 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q5 =0
}

#Part A 
Originalgini<-c(round(ineq(income.vec, type="Gini"),6),"-","-","-","-","-")
Newgini<-c(round(ineq(Income_Data_with_Redistributive$Income, type="Gini"),6),"-","-","-","-","-")

#Part B and C
Governmentspend<-c(Government_Spending_Total,Government_Spending_Q1,Government_Spending_Q2,
                   Government_Spending_Q3,Government_Spending_Q4,Government_Spending_Q5)

#Part D
IncomeQ1=sum(Income_Data_Total[1:length(income_1),1])
IncomeQ2=sum(Income_Data_Total[(length(income_1)+1):length(income_2),1])
IncomeQ3=sum(Income_Data_Total[(length(income_2)+1):length(income_3),1])
IncomeQ4=sum(Income_Data_Total[(length(income_3)+1):length(income_4),1])
IncomeQ5=sum(Income_Data_Total[(length(income_4)+1):length(income.vec),1])
IncomeTot=sum(Income_Data_Total[,1])

BenefitQ1 = Government_Spending_Q1/IncomeQ1*100
BenefitQ2 = Government_Spending_Q2/IncomeQ2*100
BenefitQ3 = Government_Spending_Q3/IncomeQ3*100
BenefitQ4 = Government_Spending_Q4/IncomeQ4*100
BenefitQ5 = Government_Spending_Q5/IncomeQ5*100
BenefitTotal =Government_Spending_Total/IncomeTot*100

Benefits<-c(BenefitTotal,BenefitQ1,BenefitQ2,BenefitQ3,BenefitQ4,BenefitQ5)

#Part E
##Assume poverty line is 2000000 yen, source: https://www.economist.com/asia/2015/04/04/struggling 
poverty_line_income = 2000000
AboveLinePre<-Income_Data[!Income_Data$Income <= poverty_line_income,]
AboveLinePost<-Income_Data_with_Redistributive[!Income_Data_with_Redistributive$Income <= poverty_line_income,]

Prepercent=length(AboveLinePre)/length(Income_Data$Income)
Postpercent=length(AboveLinePost)/length(Income_Data_with_Redistributive$Income)

Prepercent<-c(Prepercent,"-","-","-","-","-")
Postpercent<-c(Postpercent,"-","-","-","-","-")
IncomeQuintile<-c("Total","Income Quintile 1","Income Quintile 2","Income Quintile 3","Income Quintile 4","Income Quintile 5")

Summary_statistics_Red<-data.frame(IncomeQuintile, Originalgini, Newgini,Governmentspend,Benefits,Prepercent,Postpercent)
colnames(Summary_statistics_Red) <- c("Income_Bracket","Old_Gini","Post_Policy_Gini","Government_Expenditure",
                                      "Average_Benefit_of_Policy","Proportion_above_Poverty_Line(Pre)",
                                      "Proportion_above_Poverty_Line(Post)")
View(Summary_statistics_Red)