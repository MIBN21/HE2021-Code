
#This uses data from the Philippines: 
# Source: https://psa.gov.ph/sites/default/files/attachments/ird/pressrelease/tab2.pdf 

###Data Inputs####
# Find this data for your chosen country: 
#Income thresholds from data (per annum per capita in yen)
 inc_1 <- 997420.46  #Lowest Income Quintile
 inc_2 <- 1521926.05  #Second Quintile
 inc_3 <- 2145313.84  #Third quintile
 inc_4 <- 3172828.89  #Fourth Quintile
 inc_5 <- 4299226.14  #Fifth quintile taking 10,000,000 yen per household
#above is per person in household

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

## Orginial Income Data
Income_Data <- data.frame(income.vec)
colnames(Income_Data) <- c("Income")
Income_Data <- data.frame(Income_Data[order(Income_Data$Income),])
#View(Income_Data)

##Miscellaneous
# elements of each quintile
length(income)
length(income_1)
length(income_2)
length(income_3)
length(income_4)
length(income_5)

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
colnames(Income_Data) <- c("Income","APC_Value","Old_Tax","New_Tax","Consumption_Expenditure","Tax_Amount(Pre)","Tax_Amount(Post)")
Income_Data <- Income_Data[c("Income","APC_Value", "Consumption_Expenditure", "Old_Tax","New_Tax","Tax_Amount(Pre)","Tax_Amount(Post)")]
View(Income_Data)

PretaxQ1=sum(Income_Data[1:length(income_1),6])
PretaxQ2=sum(Income_Data[(length(income_1)+1):length(income_2),6])
PretaxQ3=sum(Income_Data[(length(income_2)+1):length(income_3),6])
PretaxQ4=sum(Income_Data[(length(income_3)+1):length(income_4),6])
PretaxQ5=sum(Income_Data[(length(income_4)+1):length(income),6])
PretaxTot=sum(Income_Data[,6])

PosttaxQ1=sum(Income_Data[1:length(income_1),7])
PosttaxQ2=sum(Income_Data[(length(income_1)+1):length(income_2),7])
PosttaxQ3=sum(Income_Data[(length(income_2)+1):length(income_3),7])
PosttaxQ4=sum(Income_Data[(length(income_3)+1):length(income_4),7])
PosttaxQ5=sum(Income_Data[(length(income_4)+1):length(income),7])
PosttaxTot=sum(Income_Data[,7])

IncomeQ1=sum(Income_Data[1:length(income_1),1])
IncomeQ2=sum(Income_Data[(length(income_1)+1):length(income_2),1])
IncomeQ3=sum(Income_Data[(length(income_2)+1):length(income_3),1])
IncomeQ4=sum(Income_Data[(length(income_3)+1):length(income_4),1])
IncomeQ5=sum(Income_Data[(length(income_4)+1):length(income),1])
IncomeTot=sum(Income_Data[,1])

AveragepretaxQ1= PretaxQ1/IncomeQ1 *100
AveragepretaxQ2= PretaxQ2/IncomeQ2 *100
AveragepretaxQ3= PretaxQ3/IncomeQ3 *100
AveragepretaxQ4= PretaxQ4/IncomeQ4 *100
AveragepretaxQ5= PretaxQ5/IncomeQ5 *100
AveragepretaxTot= PretaxTot/IncomeTot *100

AverageposttaxQ1= PosttaxQ1/IncomeQ1 *100
AverageposttaxQ2= PosttaxQ2/IncomeQ2 *100
AverageposttaxQ3= PosttaxQ3/IncomeQ3 *100
AverageposttaxQ4= PosttaxQ4/IncomeQ4 *100
AverageposttaxQ5= PosttaxQ5/IncomeQ5 *100
AverageposttaxTot= PosttaxTot/IncomeTot *100

Changeinrev1=(PosttaxQ1-PretaxQ1)/PretaxQ1*100
Changeinrev2=(PosttaxQ2-PretaxQ2)/PretaxQ2*100
Changeinrev3=(PosttaxQ3-PretaxQ3)/PretaxQ3*100
Changeinrev4=(PosttaxQ4-PretaxQ4)/PretaxQ4*100
Changeinrev5=(PosttaxQ5-PretaxQ5)/PretaxQ5*100
ChangeinrevTot=(AverageposttaxTot-AveragepretaxTot)/AveragepretaxTot*100

IncomeQuintile<-c("Income Quintile 1","Income Quintile 2","Income Quintile 3","Income Quintile 4","Income Quintile 5","Total")
AmPretax<-c(PretaxQ1,PretaxQ2,PretaxQ3,PretaxQ4,PretaxQ5,PretaxTot)
AmPosttax<-c(PosttaxQ1,PosttaxQ2,PosttaxQ3,PosttaxQ4,PosttaxQ5,PosttaxTot)
AveragePretax<-c(AveragepretaxQ1,AveragepretaxQ2,AveragepretaxQ3,AveragepretaxQ4,AveragepretaxQ5,AveragepretaxTot)
AveragePosttax<-c(AverageposttaxQ1, AverageposttaxQ2, AverageposttaxQ3,AverageposttaxQ4,AverageposttaxQ5,AverageposttaxTot)
Changeinrev<-c(Changeinrev1,Changeinrev2,Changeinrev3,Changeinrev4,Changeinrev5,ChangeinrevTot)
Summary_statistics<-data.frame(IncomeQuintile, AmPretax, AmPosttax, AveragePretax,AveragePosttax,Changeinrev)
colnames(Summary_statistics) <- c("Income_Bracket","Amount_PreTaxChange","Amount_PostTaxChange", "EffectiveAveragePreTaxRate", "EffectiveAveragePostTaxRate","PercentageChange_TaxCollected")
View(Summary_statistics)