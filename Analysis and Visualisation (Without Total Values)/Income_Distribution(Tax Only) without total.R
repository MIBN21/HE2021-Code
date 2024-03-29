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

length(income_1)
length(income_2)
length(income_3)
length(income_4)
length(income_5)

###Your own analysis starts####

## Orginial Income Data
Income_Data <- data.frame(income.vec)
colnames(Income_Data) <- c("Income")
Income_Data <- data.frame(Income_Data[order(Income_Data$Income),])

#Sole_IncomeData=Income_Data
#colnames(Sole_IncomeData) <- c("Income")

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
#print(MeanAPC)# (74.29 Very good considering there are assumptions in our data made)

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
Income_Data<-cbind(Income_Data, ClmAPC, Old_Tax_Rate,New_Tax_Rate)
colnames(Income_Data) <- c("Income","APC_Value","Old_Tax","New_Tax")
Income_Data$Consume <- Income_Data$Income * Income_Data$APC_Value /100
Income_Data$PreTaxChange <- Income_Data$Consume * Income_Data$Old_Tax/100
Income_Data$PostTaxChange <- Income_Data$Consume * Income_Data$New_Tax/100
Income_Data$RemainingInc1 <- Income_Data$Income - Income_Data$PreTaxChange
Income_Data$RemainingInc2 <- Income_Data$Income - Income_Data$PostTaxChange

# poverty_line_income <- 2000000
# Income_Data$Income_With_Poverty_Line <- ifelse(Income_Data$RemainingInc >= poverty_line_income, "Above", "Below")

colnames(Income_Data) <- c("Income","APC_Value","Old_Tax","New_Tax","Consumption_Expenditure",
                           "Consumption_Tax_Amount(Pre)","Consumption_Tax_Amount(Post)","Remaining_Balance_Before_TaxChange",
                           "Remaining_Balance_After_TaxChange")#,"Check_with_Poverty_Line")

Income_Data_Tax=Income_Data

# proportions <- table(Income_Data_Tax$Income_With_Poverty_Line)/length(Income_Data_Tax$Income_With_Poverty_Line)
# percentages <- proportions*100

# Income_Data_Tax %>%
#   group_by(Income_With_Poverty_Line) %>%
#   summarise(n = n()) %>%
#   mutate(Freq = n/sum(n))

View(Income_Data_Tax)

# Tax amount before policy
A = length(income_1)
B= length(income_1)+length(income_2)
C=length(income_1)+length(income_2)+length(income_3)
D=length(income_1)+length(income_2)+length(income_3)+length(income_4)

PretaxQ1=sum(Income_Data[1:A,6])
PretaxQ2=sum(Income_Data[(A+1):B,6])
PretaxQ3=sum(Income_Data[(B+1):C,6])
PretaxQ4=sum(Income_Data[(C+1):D,6])
PretaxQ5=sum(Income_Data[(D+1):length(income.vec),6])
PretaxTot=sum(Income_Data[,6])

# Tax amount after policy
PosttaxQ1=sum(Income_Data[1:A,7])
PosttaxQ2=sum(Income_Data[(A+1):B,7])
PosttaxQ3=sum(Income_Data[(B+1):C,7])
PosttaxQ4=sum(Income_Data[(C+1):D,7])
PosttaxQ5=sum(Income_Data[(D+1):length(income.vec),7])
PosttaxTot=sum(Income_Data[,7])

IncomeQ1=sum(Income_Data[1:A,1])
IncomeQ2=sum(Income_Data[(A+1):B,1])
IncomeQ3=sum(Income_Data[(B+1):C,1])
IncomeQ4=sum(Income_Data[(C+1):D,1])
IncomeQ5=sum(Income_Data[(D+1):length(income.vec),1])
IncomeTot=sum(Income_Data[,1])

# Compute tax amount
AveragepretaxQ1 = PretaxQ1/IncomeQ1 *100
AveragepretaxQ2 = PretaxQ2/IncomeQ2 *100
AveragepretaxQ3 = PretaxQ3/IncomeQ3 *100
AveragepretaxQ4 = PretaxQ4/IncomeQ4 *100
AveragepretaxQ5 = PretaxQ5/IncomeQ5 *100
AveragepretaxTot = PretaxTot/IncomeTot *100

AverageposttaxQ1 = PosttaxQ1/IncomeQ1 *100
AverageposttaxQ2 = PosttaxQ2/IncomeQ2 *100
AverageposttaxQ3 = PosttaxQ3/IncomeQ3 *100
AverageposttaxQ4 = PosttaxQ4/IncomeQ4 *100
AverageposttaxQ5 = PosttaxQ5/IncomeQ5 *100
AverageposttaxTot = PosttaxTot/IncomeTot *100

Changeinrev1=(PosttaxQ1-PretaxQ1)/PretaxQ1*100
Changeinrev2=(PosttaxQ2-PretaxQ2)/PretaxQ2*100
Changeinrev3=(PosttaxQ3-PretaxQ3)/PretaxQ3*100
Changeinrev4=(PosttaxQ4-PretaxQ4)/PretaxQ4*100
Changeinrev5=(PosttaxQ5-PretaxQ5)/PretaxQ5*100
ChangeinrevTot=(AverageposttaxTot-AveragepretaxTot)/AveragepretaxTot*100

#duplicate for graph
IncomeQuintilewt<-c("Q1","Q2","Q3","Q4","Q5")
IncomeQuintile<-c("Total","Income Quintile 1","Income Quintile 2","Income Quintile 3","Income Quintile 4","Income Quintile 5")

#duplicate for graph
AmPretaxwt<-c(PretaxQ1,PretaxQ2,PretaxQ3,PretaxQ4,PretaxQ5)
AmPosttaxwt<-c(PosttaxQ1,PosttaxQ2,PosttaxQ3,PosttaxQ4,PosttaxQ5)
AveragePretaxwt<-c(AveragepretaxQ1,AveragepretaxQ2,AveragepretaxQ3,AveragepretaxQ4,AveragepretaxQ5)
AveragePosttaxwt<-c(AverageposttaxQ1, AverageposttaxQ2, AverageposttaxQ3,AverageposttaxQ4,AverageposttaxQ5)
Changeinrevwt<-c(Changeinrev1,Changeinrev2,Changeinrev3,Changeinrev4,Changeinrev5)

AmPretax<-c(PretaxTot,PretaxQ1,PretaxQ2,PretaxQ3,PretaxQ4,PretaxQ5)
AmPosttax<-c(PosttaxTot,PosttaxQ1,PosttaxQ2,PosttaxQ3,PosttaxQ4,PosttaxQ5)
AveragePretax<-c(AveragepretaxTot,AveragepretaxQ1,AveragepretaxQ2,AveragepretaxQ3,AveragepretaxQ4,AveragepretaxQ5)
AveragePosttax<-c(AverageposttaxTot,AverageposttaxQ1, AverageposttaxQ2, AverageposttaxQ3,AverageposttaxQ4,AverageposttaxQ5)
Changeinrev<-c(ChangeinrevTot,Changeinrev1,Changeinrev2,Changeinrev3,Changeinrev4,Changeinrev5)

Originalgini<-c(round(ineq(income.vec, type="Gini"),6),"-","-","-","-","-")
Newgini<-c(round(ineq(Income_Data$Remaining_Balance_After_TaxChange, type="Gini"),6),"-","-","-","-","-")

# Part E
##Assume poverty line is 2000000 yen, source: https://www.economist.com/asia/2015/04/04/struggling 
poverty_line_income = 2000000
AboveLinePre<-Income_Data_Tax[!Income_Data_Tax$Remaining_Balance_Before_TaxChange <= poverty_line_income,]
AboveLinePost<-Income_Data_Tax[!Income_Data_Tax$Remaining_Balance_After_Tax <= poverty_line_income,]
Prespercent=100-((nrow(AboveLinePre)/length(Income_Data$Remaining_Balance_Before_TaxChange))*100)
Postspercent=100-((nrow(AboveLinePost)/length(Income_Data$Remaining_Balance_After_TaxChange))*100)

Prepercent<-c(Prespercent,"-","-","-","-","-")
Postpercent<-c(Postspercent,"-","-","-","-","-")

Summary_statistics<-data.frame(IncomeQuintile, Originalgini, Newgini,AmPretax, AmPosttax, AveragePretax,AveragePosttax,Changeinrev,Prepercent,Postpercent)
colnames(Summary_statistics) <- c("Income_Bracket","Old_Gini","Post_Tax_Gini","AmountCollected_PreTaxChange","AmountCollected_PostTaxChange", 
                                  "Burden_Of_Tax(Pre)%", "Burden_Of_Tax(Post)%","PercentageChange_TaxCollected(Pre vs Post)",
                                  "Proportion_Below_Poverty_Line(Pre)","Proportion_Below_Poverty_Line(Post)")

Summary_statistics_Tax=Summary_statistics
View(Summary_statistics_Tax)

##Graphs
giniid<-c("Old Gini","New Gini")
ginivalue<-c(round(ineq(income.vec, type="Gini"),6),round(ineq(Income_Data$Remaining_Balance_After_TaxChange, type="Gini"),6))
giniplot<-data.frame(giniid,ginivalue)
giniplot<-melt(giniplot)
p<-ggplot(giniplot,aes(x=reorder(giniid, ginivalue),y=ginivalue,fill=giniid))+
  geom_bar(stat="identity",position="dodge",width = 0.5)+ylim(0, 0.4)
p.labs <- p + labs(title = "Gini Changes", x = "Gini", y = "Gini Value") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Type")+coord_cartesian(ylim = c(0.3, 0.4))
p.labs

govexpendplot<-data.frame(IncomeQuintilewt,AmPretaxwt,AmPosttaxwt)
govexpendplot<-melt(govexpendplot)
c<-ggplot(govexpendplot,aes(x=IncomeQuintilewt, y=value ,fill=variable))+
  geom_bar(stat="identity",width = 0.5, position = position_dodge(0.7))
c.labs <- c + labs(title = "Government Revenue", x = "Income Quintile", y = "Value in Yen") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Pre/Post Policy", labels = c("PreTax", "PostTax"))
c.labs

burdenplot<-data.frame(IncomeQuintilewt,AveragePretaxwt,AveragePosttaxwt)
burdenplot<-melt(burdenplot)
d<-ggplot(burdenplot,aes(x=IncomeQuintilewt, y=value ,fill=variable))+
  geom_bar(stat="identity",width = 0.5, position = position_dodge(0.7))
d.labs <- d + labs(title = "Average Tax Burden", x = "Income Quintile", y = "Average Percentage (%)") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Pre/Post Policy", labels = c("Pre-Tax-Burden", "Post-Tax-Burden"))
d.labs

propid<-c("Old Proportion","New Proportion")
propvalue<-c(Prespercent,Postspercent)
propplot<-data.frame(propid,propvalue)
propplot<-melt(propplot)
e<-ggplot(propplot,aes(x=reorder(propid,propvalue),y=propvalue,fill=propid))+
  geom_bar(stat="identity",position="dodge",width = 0.5)
e.labs <- e + labs(title = "Population Below Poverty Line", x = "Proportion", y = "Percentage (%)") + theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Pre/Post Policy",labels = c("PostTax", "PreTax")) +coord_cartesian(ylim = c(40, 45))
e.labs