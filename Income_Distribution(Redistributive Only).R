#This uses data from Japan: 
# Source: https://www.stat.go.jp/english/data/sousetai/es18.html (Table 3)

#install.packages("ineq")
#install.packages("tidyverse")
#install.packages("reshape2")

library(ineq)
library(ggplot2)
library(reshape2)

###Data Inputs####
# Find this data for your chosen country: 
#Income thresholds from data (per annum per capita in yen)

#Average income per household/average number of people in a household
Averageearninhouse= (0.32 + 0.62 + 1.04 + 1.52 + 1.88)/5

#Average yearly income quintile per household/average number of people in a household
#to obtain average yearly income quintile per capita.
inc_1 <- 2320000/Averageearninhouse #Lowest Income Quintile
inc_2 <- 3540000/Averageearninhouse #Second Quintile
inc_3 <- 4990000/Averageearninhouse #Third quintile
inc_4 <- 7380000/Averageearninhouse #Fourth Quintile
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

# Filter population based on eligible income for the voucher
Income_Data_recieved_voucher<-Income_Data[!Income_Data$Income>2560000,]+30000
AmountofVouchers<-length(Income_Data_recieved_voucher)

Income_Data_recieved_voucher<- data.frame(Income_Data_recieved_voucher)
Income_Data_DNrecieved_voucher<-Income_Data[!Income_Data$Income<=2560000,]
Income_Data_DNrecieved_voucher <- data.frame(Income_Data_DNrecieved_voucher)
colnames(Income_Data_recieved_voucher) <- c("Income")
colnames(Income_Data_DNrecieved_voucher) <- c("Income")

# Generate data frame of the population income with post policy income after distributing the vouchers
Income_Data_with_Redistributive<-rbind(Income_Data_recieved_voucher,Income_Data_DNrecieved_voucher)
Income_Data_with_Redistributive <- data.frame(Income_Data_with_Redistributive)
colnames(Income_Data_with_Redistributive) <- c("Income")

Income_Data_Total<-cbind(Income_Data,Income_Data_with_Redistributive)
colnames(Income_Data_Total) <- c("Income","Post_Policy_Income")

Income_Data_Red=Income_Data_Total
View(Income_Data_Red)

########################DO NOT TOUCH THE IF ELSE STATEMENT:THE INDENTATIONS ALL MATTER
## Analysis (30000 yen per year per capita)
# calculate Government Spending before and after redistributive policy
Remaining_people<-AmountofVouchers
Government_Spending_Total =Remaining_people*30000

# Quintile 1
if(Remaining_people>=length(income_1)){
  Government_Spending_Q1 = length(income_1)*30000
  Remaining_people=Remaining_people-length(income_1)
  
} else if (Remaining_people>0){
  Government_Spending_Q1 = Remaining_people*30000
  Remaining_people=0
  
}else{
  Government_Spending_Q1 =0
}

# Quintile 2
if(Remaining_people>=length(income_2)){
  Government_Spending_Q2 = length(income_2)*30000
  Remaining_people=Remaining_people-length(income_2)
  
}else if (Remaining_people>0){
  Government_Spending_Q2 = Remaining_people*30000
  Remaining_people=0
  
}else{
  Government_Spending_Q2 =0
}

# Quintile 3
if(Remaining_people>=length(income_3)){
  Government_Spending_Q3 = length(income_3)*30000
  Remaining_people=Remaining_people-length(income_3)
  
}else if (Remaining_people>0){
  Government_Spending_Q3 = Remaining_people*30000
  Remaining_people=0
  
}else{
  Government_Spending_Q3 =0
}

# Quintile 4
if(Remaining_people>=length(income_4)){
  Government_Spending_Q4 = length(income_4)*30000
  Remaining_people=Remaining_people-length(income_4)
  
}else if (Remaining_people>0){
  Government_Spending_Q4 = Remaining_people*30000
  Remaining_people=0
  
}else{
  Government_Spending_Q4 =0
}

# Quintile 5
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
A = length(income_1)
B= length(income_1)+length(income_2)
C=length(income_1)+length(income_2)+length(income_3)
D=length(income_1)+length(income_2)+length(income_3)+length(income_4)

IncomeQ1=sum(Income_Data_Total[1:A,1])
IncomeQ2=sum(Income_Data_Total[(A+1):B,1])
IncomeQ3=sum(Income_Data_Total[(B+1):C,1])
IncomeQ4=sum(Income_Data_Total[(C+1):D,1])
IncomeQ5=sum(Income_Data_Total[(D+1):length(income.vec),1])
IncomeTot=sum(Income_Data_Total[,1])

PostIncomeQ1=sum(Income_Data_Total[1:A,2])
PostIncomeQ2=sum(Income_Data_Total[(A+1):B,2])
PostIncomeQ3=sum(Income_Data_Total[(B+1):C,2])
PostIncomeQ4=sum(Income_Data_Total[(C+1):D,2])
PostIncomeQ5=sum(Income_Data_Total[(D+1):length(income.vec),2])
PostIncomeTot=sum(Income_Data_Total[,2])

BenefitQ1 = Government_Spending_Q1/IncomeQ1*100
BenefitQ2 = Government_Spending_Q2/IncomeQ2*100
BenefitQ3 = Government_Spending_Q3/IncomeQ3*100
BenefitQ4 = Government_Spending_Q4/IncomeQ4*100
BenefitQ5 = Government_Spending_Q5/IncomeQ5*100
BenefitTotal =Government_Spending_Total/IncomeTot*100

Pre_Policy_Income<-c(IncomeTot,IncomeQ1,IncomeQ2,IncomeQ3,IncomeQ4,IncomeQ5)
Post_Policy_Income<-c(PostIncomeTot,PostIncomeQ1,PostIncomeQ2,PostIncomeQ3,PostIncomeQ4,PostIncomeQ5)
Benefits<-c(BenefitTotal,BenefitQ1,BenefitQ2,BenefitQ3,BenefitQ4,BenefitQ5)

#Part E
##Assume poverty line is 2000000 yen, source: https://www.economist.com/asia/2015/04/04/struggling 
poverty_line_income = 2000000
AboveLinePre<-Income_Data_Total[!Income_Data_Total$Income <= poverty_line_income,]
AboveLinePost<-Income_Data_Total[!Income_Data_Total$Post_Policy_Income <= poverty_line_income,]
Prespercent=(nrow(AboveLinePre)/length(Income_Data_Total$Income))*100
Postspercent=(nrow(AboveLinePost)/length(Income_Data_Total$Post_Policy_Income))*100

Prepercent<-c(Prespercent,"-","-","-","-","-")
Postpercent<-c(Postspercent,"-","-","-","-","-")

# Create column to store the results for each quintile before and after tax
IncomeQuintile<-c("Total","Income Quintile 1","Income Quintile 2","Income Quintile 3","Income Quintile 4","Income Quintile 5")

# Generate table of results for each quintile and the overall population
Summary_statistics_Red<-data.frame(IncomeQuintile, Originalgini, Newgini,Governmentspend,Pre_Policy_Income,Post_Policy_Income,Benefits,Prepercent,Postpercent)
colnames(Summary_statistics_Red) <- c("Income_Bracket","Old_Gini","Post_Policy_Gini","Government_Expenditure","Pre_Policy_Income","Post_Policy_Income",
                                      "Average_Benefit_of_Policy","Proportion_above_Poverty_Line(Pre)",
                                      "Proportion_above_Poverty_Line(Post)")
View(Summary_statistics_Red)

##Graphs
giniid<-c("Old Gini","New Gini")
ginivalue<-c(round(ineq(income.vec, type="Gini"),6),round(ineq(Income_Data_with_Redistributive$Income, type="Gini"),6))
giniplot<-data.frame(giniid,ginivalue)
giniplot<-melt(giniplot)
p<-ggplot(giniplot,aes(x=reorder(giniid, -ginivalue),y=ginivalue,fill=giniid))+
  geom_bar(stat="identity",position="dodge",width = 0.5)+ylim(0, 0.4)
p.labs <- p + labs(title = "Gini Changes", x = "Gini", y = "Gini Value") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Type")
p.labs

govrevplot<-data.frame(IncomeQuintile,Governmentspend)
govrevplot<-melt(govrevplot)
c<-ggplot(govrevplot,aes(x=IncomeQuintile, y=value ,fill=variable))+
  geom_bar(stat="identity",width = 0.5, position = position_dodge(0.7))
c.labs <- c + labs(title = "Government Expenditure", x = "Income Bracket", y = "Value in Yen") + theme(plot.title = element_text(hjust = 0.5),legend.position = "none")
c.labs

popincplot<-data.frame(IncomeQuintile,Pre_Policy_Income,Post_Policy_Income)
popincplot<-melt(popincplot)
g<-ggplot(popincplot,aes(x=IncomeQuintile, y=value ,fill=variable))+
  geom_bar(stat="identity",width = 0.5, position = position_dodge(0.7))
g.labs <- g + labs(title = "Income of Population", x = "Income Bracket", y = "Value in Yen") + theme(plot.title = element_text(hjust = 0.5)) +scale_fill_discrete(name = "Legend", labels = c("PrePolicies_Income", "PostPolicies_Income"))
g.labs

benefitplot<-data.frame(IncomeQuintile,Benefits)
benefitplot<-melt(benefitplot)
d<-ggplot(benefitplot,aes(x=IncomeQuintile, y=value ,fill=variable))+
  geom_bar(stat="identity",width = 0.5, position = position_dodge(0.7))
d.labs <- d + labs(title = "Average Benefit of Policy", x = "Income Bracket", y = "Average Percentage (%)") + theme(plot.title = element_text(hjust = 0.5),legend.position = "none")
d.labs

propid<-c("Old Proportion","New Proportion")
propvalue<-c(Prespercent,Postspercent)
propplot<-data.frame(propid,propvalue)
propplot<-melt(propplot)
e<-ggplot(propplot,aes(x=reorder(propid,propvalue),y=propvalue,fill=propid))+
  geom_bar(stat="identity",position="dodge",width = 0.5)
e.labs <- e + labs(title = "Proportion of Population above Poverty Line", x = "Proportion", y = "Percentage (%)") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Type")
e.labs