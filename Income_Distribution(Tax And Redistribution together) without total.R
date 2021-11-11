#install.packages("ineq")
#This uses data from Japan: 
# Source: https://www.stat.go.jp/english/data/sousetai/es18.html (Table 3)
#install.packages("tidyverse")
#install.packages("reshape2")
library(ineq)
library(ggplot2)
library(reshape2)
###Data Inputs####
# Find this data for your chosen country: 
#Income thresholds from data (per annum per capita in yen)

#Average income per household/average number of people in a household
Averageearninhouse= (0.32+0.62+	1.04+	1.52	+1.88)/5


 #Average yearly income quintile per household/average number of people in a household
 #to obtain average yearly income quintile per capita.

 inc_1 <- 2320000/Averageearninhouse#Lowest Income Quintile
 inc_2 <- 3540000/Averageearninhouse#Second Quintile
 inc_3 <- 4990000/Averageearninhouse#Third quintile
 inc_4 <- 7380000/Averageearninhouse#Fourth Quintile
 
 
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

##Average consumption expenditure of each quintile per capita (big assumptions made here)
#https://www.stat.go.jp/english/data/sousetai/es18.html (Table 3)
Q1 = 137856*12/Averageearninhouse
Q2 = 192473*12/Averageearninhouse
Q3 = 237683*12/Averageearninhouse
Q4 = 282792*12/Averageearninhouse
Q5 = 381189*12/Averageearninhouse

##Average APC of each quintile (big assumptions made here)
###PROB need to change because APC1 is suppose to be above 100%
APC1 = (Q1/summary(income_1)[4])*100
APC2 = (Q2/summary(income_2)[4])*100
APC3 = (Q3/summary(income_3)[4])*100
APC4 = (Q4/summary(income_4)[4])*100
APC5 = (Q5/summary(income_5)[4])*100
## average apc documented is about 69.3
#MeanAPC = (APC1+ APC2+APC3+ APC4 + APC5)/5
#print(MeanAPC) (74.29 Very good considering there are assum,ptions in our data made)

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

# Prepare and generate dataframe to analyse the income values
Income_Data_Total<-cbind(Income_Data_Total, ClmAPC, Old_Tax_Rate, New_Tax_Rate)
colnames(Income_Data_Total) <- c("Income","New_Income_with_Red","APC_Value","Old_Tax" ,"New_Tax")
Income_Data_Total$Pre_Consume_Expenditure <- Income_Data_Total$Income * Income_Data_Total$APC_Value /100
Income_Data_Total$Post_Consume_Expenditure <- Income_Data_Total$New_Income_with_Red * Income_Data_Total$APC_Value /100
Income_Data_Total$Pre_Policy_Tax_Amount <- Income_Data_Total$Pre_Consume_Expenditure * Income_Data_Total$Old_Tax/100
Income_Data_Total$Post_Policy_Tax_Amount <- Income_Data_Total$Post_Consume_Expenditure * Income_Data_Total$New_Tax/100
Income_Data_Total$RemainingInc_Pre <- Income_Data_Total$Income - Income_Data_Total$Pre_Policy_Tax_Amount
Income_Data_Total$RemainingInc_Post <- Income_Data_Total$New_Income_with_Red - Income_Data_Total$Post_Policy_Tax_Amount


# Make duplicate of the data to calculate Gini in part A later
dupli_income=Income_Data_Total
dupli_income$justTax <- Income_Data_Total$Pre_Consume_Expenditure * Income_Data_Total$New_Tax/100
dupli_income$justTaxRemain<-Income_Data_Total$Income - dupli_income$justTax

###Summary Statistics

###Part A
##Gini
Originalgini<-c(round(ineq(income.vec, type="Gini"),6),"-","-","-","-","-")
Middlegini<-c(round(ineq(dupli_income$justTaxRemain, type="Gini"),6),"-","-","-","-","-")
Newgini<-c(round(ineq(Income_Data_Total$RemainingInc_Post, type="Gini"),6),"-","-","-","-","-")

### Part B and C
#calculate Government Spending before and after redistributive policy

## Before policy
Governmentspend_Pre = c(0,0,0,0,0,0)
Governmentspend_Pre<-data.frame(Governmentspend_Pre)
#duplicate for graph
Governmentspend_Prewt = c(0,0,0,0,0)
Governmentspend_Prewt<-data.frame(Governmentspend_Prewt)
## After policy

Remaining_people<-AmountofVouchers
Government_Spending_Total = Remaining_people*30000
# Q1
if(Remaining_people>=length(income_1)){
  Government_Spending_Q1 = length(income_1)*30000
  Remaining_people=Remaining_people-length(income_1)
} else if (Remaining_people>0){
  
  Government_Spending_Q1 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q1 =0
}
# Q2
if(Remaining_people>=length(income_2)){
  Government_Spending_Q2 = length(income_2)*30000
  Remaining_people=Remaining_people-length(income_2)
}else if (Remaining_people>0){
  Government_Spending_Q2 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q2 =0
}
# Q3
if(Remaining_people>=length(income_3)){
  Government_Spending_Q3 = length(income_3)*30000
  Remaining_people=Remaining_people-length(income_3)
}else if (Remaining_people>0){
  Government_Spending_Q3 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q3 =0
}
# Q4
if(Remaining_people>=length(income_4)){
  Government_Spending_Q4 = length(income_4)*30000
  Remaining_people=Remaining_people-length(income_4)
}else if (Remaining_people>0){
  Government_Spending_Q4 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q4 =0
}
# Q5
if(Remaining_people>=length(income_5)){
  Government_Spending_Q5 = length(income_5)*30000
  Remaining_people=Remaining_people-length(income_5)
}else if (Remaining_people>0){
  Government_Spending_Q5 = Remaining_people*30000
  Remaining_people=0
}else{
  Government_Spending_Q5 =0
}

#duplicate for graph
Governmentspend_Postwt<-c(Government_Spending_Q1,Government_Spending_Q2,
                   Government_Spending_Q3,Government_Spending_Q4,Government_Spending_Q5)
Governmentspend_Post<-c(Government_Spending_Total,Government_Spending_Q1,Government_Spending_Q2,
                        Government_Spending_Q3,Government_Spending_Q4,Government_Spending_Q5)

#calculate Government Revenue from Tax

# Tax amount before policy
A = length(income_1)
B= length(income_1)+length(income_2)
C=length(income_1)+length(income_2)+length(income_3)
D=length(income_1)+length(income_2)+length(income_3)+length(income_4)

PretaxQ1=sum(Income_Data_Total[1:A,8])
PretaxQ2=sum(Income_Data_Total[(A+1):B,8])
PretaxQ3=sum(Income_Data_Total[(B+1):C,8])
PretaxQ4=sum(Income_Data_Total[(C+1):D,8])
PretaxQ5=sum(Income_Data_Total[(D+1):length(income.vec),8])
PretaxTot=sum(Income_Data_Total[,8])
AmPretax<-c(PretaxTot,PretaxQ1,PretaxQ2,PretaxQ3,PretaxQ4,PretaxQ5)
#duplicate for graph
AmPretaxwt<-c(PretaxQ1,PretaxQ2,PretaxQ3,PretaxQ4,PretaxQ5)

# Tax amount after policy
PosttaxQ1=sum(Income_Data_Total[1:A,9])
PosttaxQ2=sum(Income_Data_Total[(A+1):B,9])
PosttaxQ3=sum(Income_Data_Total[(B+1):C,9])
PosttaxQ4=sum(Income_Data_Total[(C+1):D,9])
PosttaxQ5=sum(Income_Data_Total[(D+1):length(income.vec),9])
PosttaxTot=sum(Income_Data_Total[,9])
AmPosttax<-c(PosttaxTot,PosttaxQ1,PosttaxQ2,PosttaxQ3,PosttaxQ4,PosttaxQ5)
#duplicate for graph
AmPosttaxwt<-c(PosttaxQ1,PosttaxQ2,PosttaxQ3,PosttaxQ4,PosttaxQ5)

#If value is positive Tax revenue is more than Spending
Net_Government_Revenue_Pre<-AmPretax -Governmentspend_Pre
Net_Government_Revenue_Post<-AmPosttax -Governmentspend_Post
Net_Government_Revenue_Pre<-data.frame(Net_Government_Revenue_Pre)
Net_Government_Revenue_Post<-data.frame(Net_Government_Revenue_Post)
#duplicate for graph
Net_Government_Revenue_Prewt<-AmPretaxwt -Governmentspend_Prewt
Net_Government_Revenue_Postwt<-AmPosttaxwt -Governmentspend_Postwt
Net_Government_Revenue_Prewt<-data.frame(Net_Government_Revenue_Prewt)
Net_Government_Revenue_Postwt<-data.frame(Net_Government_Revenue_Postwt)

###Part D
# Get original income per quintile
PreIncomeQ1=sum(Income_Data_Total[1:A,1])
PreIncomeQ2=sum(Income_Data_Total[(A+1):B,1])
PreIncomeQ3=sum(Income_Data_Total[(B+1):C,1])
PreIncomeQ4=sum(Income_Data_Total[(C+1):D,1])
PreIncomeQ5=sum(Income_Data_Total[(D+1):length(income.vec),1])
PreIncomeTot=sum(Income_Data_Total[,1])

# Get income per quintile after redistribution
PostIncomeQ1=sum(Income_Data_Total[1:A,2])
PostIncomeQ2=sum(Income_Data_Total[(A+1):B,2])
PostIncomeQ3=sum(Income_Data_Total[(B+1):C,2])
PostIncomeQ4=sum(Income_Data_Total[(C+1):D,2])
PostIncomeQ5=sum(Income_Data_Total[(D+1):length(income.vec),2])
PostIncomeTot=sum(Income_Data_Total[,2])

# Calculate benefit of redistributive policy
Average_Benefit_of_PolicyQ1=Government_Spending_Q1/PreIncomeQ1*100
Average_Benefit_of_PolicyQ2=Government_Spending_Q2/PreIncomeQ2*100
Average_Benefit_of_PolicyQ3=Government_Spending_Q3/PreIncomeQ3*100
Average_Benefit_of_PolicyQ4=Government_Spending_Q4/PreIncomeQ4*100
Average_Benefit_of_PolicyQ5=Government_Spending_Q5/PreIncomeQ5*100
Average_Benefit_of_PolicyTot =Government_Spending_Total/PreIncomeTot*100
Average_Benefit_of_Policy<-c(Average_Benefit_of_PolicyTot,Average_Benefit_of_PolicyQ1,Average_Benefit_of_PolicyQ2,Average_Benefit_of_PolicyQ3,Average_Benefit_of_PolicyQ4,Average_Benefit_of_PolicyQ5)
#duplicate for graph
Average_Benefit_of_Policywt<-c(Average_Benefit_of_PolicyQ1,Average_Benefit_of_PolicyQ2,Average_Benefit_of_PolicyQ3,Average_Benefit_of_PolicyQ4,Average_Benefit_of_PolicyQ5)

# Calculate burden of tax
Average_Burden_Of_PolicyQ1<-PosttaxQ1/PostIncomeQ1*100
Average_Burden_Of_PolicyQ2<-PosttaxQ2/PostIncomeQ2*100
Average_Burden_Of_PolicyQ3<-PosttaxQ3/PostIncomeQ3*100
Average_Burden_Of_PolicyQ4<-PosttaxQ4/PostIncomeQ4*100
Average_Burden_Of_PolicyQ5<-PosttaxQ5/PostIncomeQ5*100
Average_Burden_Of_PolicyTot<-PosttaxTot/PostIncomeTot*100
Average_Burden_Of_Policy_Post_Red<-c(Average_Burden_Of_PolicyTot,Average_Burden_Of_PolicyQ1,Average_Burden_Of_PolicyQ2,Average_Burden_Of_PolicyQ3,Average_Burden_Of_PolicyQ4,Average_Burden_Of_PolicyQ5)
#duplicate for graph
Average_Burden_Of_Policy_Post_Redwt<-c(Average_Burden_Of_PolicyQ1,Average_Burden_Of_PolicyQ2,Average_Burden_Of_PolicyQ3,Average_Burden_Of_PolicyQ4,Average_Burden_Of_PolicyQ5)

###Part E


##Proportion of population against poverty line
poverty_line_income = 2000000
AboveLinePre<-Income_Data_Total[!Income_Data_Total$RemainingInc_Pre <= poverty_line_income,]
AboveLinePost<-Income_Data_Total[!Income_Data_Total$RemainingInc_Post <= poverty_line_income,]
Prespercent=(nrow(AboveLinePre)/length(Income_Data_Total$RemainingInc_Pre))*100
Postspercent=(nrow(AboveLinePost)/length(Income_Data_Total$RemainingInc_Post))*100
Prepercent<-c(Prespercent,"-","-","-","-","-")
Postpercent<-c(Postspercent,"-","-","-","-","-")
IncomeQuintile<-c("Total","Income Quintile 1","Income Quintile 2","Income Quintile 3","Income Quintile 4","Income Quintile 5")
#duplicate for graph
IncomeQuintilewt<-c("Income Quintile 1","Income Quintile 2","Income Quintile 3","Income Quintile 4","Income Quintile 5")

# Generate the structured data

colnames(Income_Data_Total) <- c("Income","New_Income_with_Red","APC_Value","Old_Tax (%)" ,"New_Tax (%)",
                                 "Expenditure Before Redistribution","Expenditure After Redistribution",
                                 "Tax Before Redistribution","Tax After Redistribution",
                                 "Remaining Income Before Redistribution less tax",
                                 "Remaining Income After Redistribution less tax")
View(Income_Data_Total)

Summary_statistics_Both<-data.frame(IncomeQuintile, Originalgini, Middlegini,Newgini,Governmentspend_Pre,Governmentspend_Post,AmPretax,AmPosttax,Net_Government_Revenue_Pre,Net_Government_Revenue_Post,Average_Benefit_of_Policy,Average_Burden_Of_Policy_Post_Red,Prepercent,Postpercent)
colnames(Summary_statistics_Both) <- c("Income_Bracket","Old_Gini","Temp_Gini_Tax_Only","Post_Policy_Gini","Government_Expenditure_Pre","Government_Expenditure_Post","Government_Revenue_Pre","Government_Revenue_Post","Net_Government_Revenue_Pre","Net_Government_Revenue_Post",
                                      "Average_Benefit_of_Policy","Average_Burden_Of_Policy_Post_Red","Proportion_above_Poverty_Line(Pre)",
                                      "Proportion_above_Poverty_Line(Post)")
View(Summary_statistics_Both)

##Graphs
giniid<-c("Pre Policies Gini","Just Tax Increase Gini","Post Policies Gini")
ginivalue<-c(round(ineq(income.vec, type="Gini"),6),round(ineq(dupli_income$justTaxRemain, type="Gini"),6),round(ineq(dupli_income$RemainingInc_Post, type="Gini"),6))
giniplot<-data.frame(giniid,ginivalue)
giniplot<-melt(giniplot)
p<-ggplot(giniplot,aes(x=reorder(giniid,ginivalue),y=ginivalue,fill=giniid))+
  geom_bar(stat="identity",position="dodge",width = 0.5)
p.labs <- p + labs(title = "Gini Changes", x = "Gini", y = "Gini Value") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Type") +coord_cartesian(ylim = c(0.3, 0.4))
p.labs

govexpendplot<-data.frame(IncomeQuintilewt,Governmentspend_Prewt,AmPretaxwt,Governmentspend_Postwt,AmPosttaxwt)
govexpendplot<-melt(govexpendplot)
c<-ggplot(govexpendplot,aes(x=IncomeQuintilewt, y=value ,fill=variable))+
  geom_bar(stat="identity",width = 0.5,position = position_dodge(0.7))
c.labs <- c + labs(title = "Government Revenue & Expenditure", x = "Income Bracket", y = "Value in Yen") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Legend", labels = c("PrePolicies_Expenditure", "PrePolicies_Revenue","PostPolicies_Expenditure", "PostPolicies_Revenue"))
c.labs

govnetplot<-data.frame(IncomeQuintilewt,Net_Government_Revenue_Prewt,Net_Government_Revenue_Postwt)
govnetplot<-melt(govnetplot)
d<-ggplot(govnetplot,aes(x=IncomeQuintilewt, y=value ,fill=variable))+
  geom_bar(stat="identity",width = 0.5, position = position_dodge(0.7))
d.labs <- d + labs(title = "Government Net Revenue & Expenditure", x = "Income Bracket", y = "Value in Yen") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Legend", labels = c("PrePolicies_NetGov_Revenue", "PostPolicies_NetGov_Revenue"))
d.labs

govavgplot<-data.frame(IncomeQuintilewt,Average_Benefit_of_Policywt,Average_Burden_Of_Policy_Post_Redwt)
govavgplot<-melt(govavgplot)
f<-ggplot(govavgplot,aes(x=IncomeQuintilewt, y=value ,fill=variable))+
  geom_bar(stat="identity",width = 0.5, position = position_dodge(0.7))
f.labs <- f + labs(title = "Average Benefit/Burden of Policies", x = "Income Bracket", y = "Percentage (%)") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Legend", labels = c("Average Benefit of Policy", "Average Burden of Policy"))
f.labs

propid<-c("Old Proportion","New Proportion")
propvalue<-c(Prespercent,Postspercent)
propplot<-data.frame(propid,propvalue)
propplot<-melt(propplot)
e<-ggplot(propplot,aes(x=reorder(propid,propvalue),y=propvalue,fill=propid))+
  geom_bar(stat="identity",position="dodge",width = 0.5)
e.labs <- e + labs(title = "Proportion of Population above Poverty Line", x = "Proportion", y = "Percentage (%)") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Pre/Post Policy",labels = c("PostPolicy", "PrePolicy")) +coord_cartesian(ylim = c(55, 60))
e.labs