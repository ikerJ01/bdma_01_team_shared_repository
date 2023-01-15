# Iker Gonzalez

setwd("D:/iCloud/iCloudDrive/Master Big Data/Data Analytics - Teoria/Introduction R_/ExerciseIntroR")

# Block 1

#Exercise B1.1
#####
E1V = c(1:20)            #Creates list 1 to 20
E1V[2] = 20              #Swaps value 20 to position 2
E1V[20] = 2              #Swaps value 2 to position 20

#Exercise B1.2
#####
E2V = rep(c(1,2,3,NA),5)
E2V[is.na(E2V)] = mean(E2V, na.rm = TRUE)

#Exercise B1.3
#####
E3Df = data.frame(Age=c(22, 25, 18, 20), Name=c("James", "Mathew", "Olivia", "Stella"), Gender=c("M", "M", "F", "F"))


#Exercise B1.4
#####

E3Df[E3Df$Age >= 21,]

#Exercise B1.5
#####
E3Df["Adult"] = FALSE
E3Df[which(E3Df[, 1] >= 21, arr.ind=TRUE), 4] = TRUE

#Exercise B1.6
#####
data("iris")
?iris
IrisNumeric = iris[1:4]

ApplyColumnsMean = apply(IrisNumeric, 2, mean)         #Mean by columns using Apply
ApplyRowsMean = apply(IrisNumeric, 1, mean)            #Mean by rows using Apply
V1IrisTotals = rbind(cbind(IrisNumeric, Rtot = ApplyRowsMean), Ctot = c(ApplyColumnsMean, mean(ApplyColumnsMean)))   #Appending Row & Columns means to source table

rowMeans = rowMeans(IrisNumeric)          #Mean by rowMeans
columnMeans = colMeans(IrisNumeric)       #Mean by colMeans
V2IrisTotals = rbind(cbind(IrisNumeric, Rtot = rowMeans), Ctot = c(columnMeans, mean(columnMeans)))   #Appending Row & Columns means to source table

#Exercise B1.7
#####
x = IrisNumeric
ColMeanFunction = function(x) {
  return(colSums(x)/nrow(x))
}
RowMeanFunction = function(x) {
  return(rowSums(x)/ncol(x))
}

V3IrisTotals = rbind(cbind(IrisNumeric, Rtot = RowMeanFunction(x)), Ctot = c(ColMeanFunction(x), mean(ColMeanFunction(x))))   #Appending Row & Columns means to source table using user defined function


#Exercise B1.8
#####
n = 1:20
RecursiveFib = function(n) {
  if (n == 1 || n == 2) {1}
  else {
    RecursiveFib(n - 1) + RecursiveFib(n - 2)
  }
}
RecursiveFib = Vectorize(RecursiveFib)
print(RecursiveFib(n))


#Exercise B1.9
#####

# Importing Source Tables 
BcnHousingPrice = read.csv("_repository/2019_comp_vend_preu_trim.csv", header = TRUE, sep = ",")
BcnAnnualIncome = read.csv("_repository/2019_atles_renda_bruta_persona.csv", header = TRUE, sep = ",")


# General parameter setup
DataSetYear = BcnHousingPrice[1,1]
SavingsToIncomeRatio = 0.4     #Ratio of yearly income destined to save and pay house purchase in full
WorkLifeExpectancySpain = 35     # Expected work life span before retirement in Spain

# BCN Housing price clean-up: Filtering table to keep necessary observations, replacing n/a values with the global mean of Housing value & grouping values and calculating their mean by trimester & district code

BcnHousingPrice = BcnHousingPrice[BcnHousingPrice$Preu_mitja_habitatge == "Total. Milers d'euros",]
BcnHousingPrice[is.na(BcnHousingPrice$Valor), 8] = mean(BcnHousingPrice$Valor, na.rm = TRUE)
BcnHousingPrice = aggregate(BcnHousingPrice$Valor~BcnHousingPrice$Codi_Barri+BcnHousingPrice$Nom_Barri+BcnHousingPrice$Nom_Districte, list(BcnHousingPrice$Codi_Barri), FUN=mean) 

colnames(BcnHousingPrice) = c("Codi_Barri",	"Nom_Barri",	"Nom_Districte",	"Valor_finca_milers_EUR")

# BCN Annual Income clean-up: grouping values and calculating their mean by trimester & district code

BcnAnnualIncome = aggregate(BcnAnnualIncome$Import_Renda_Bruta_EUR~BcnAnnualIncome$Codi_Barri+BcnAnnualIncome$Nom_Barri+BcnAnnualIncome$Nom_Districte, list(BcnAnnualIncome$Codi_Barri), FUN=mean) 
colnames(BcnAnnualIncome) = c("Codi_Barri",	"Nom_Barri",	"Nom_Districte",	"Import_Renda_Bruta_EUR")


# Joining BCN Annual Income to BCN Housing Price
BcnHousingVsIncome = merge(BcnHousingPrice, BcnAnnualIncome, by = "Codi_Barri", all.x = TRUE)
BcnHousingVsIncome[5:6] = list(NULL)          #Dropping duplicated columns

# Adding new variables
BcnHousingVsIncome['Valor_mitja_finca_EUR'] = BcnHousingVsIncome$Valor_finca_milers_EUR * 1000
BcnHousingVsIncome[4] = list(NULL)          #Dropping initial Valor column to keep 'Valor_mitja_finca_EUR' as final reference
BcnHousingVsIncome["years_to_purchase_in_full"] = BcnHousingVsIncome[5] / (BcnHousingVsIncome[4]*SavingsToIncomeRatio)   # Total years needed to save enough money to purchase house in full


# Calculating global mean of the data set as a data frame
BcnMeans = data.frame(apply(BcnHousingVsIncome[4:6], 2, FUN = mean))
colnames(BcnMeans) = "mean"
BcnMeans = t(BcnMeans)

# Global description of the data set
BcnHousingDescription = function(param1, param2, param3, param4, param5, param6) {
  cat("In Barcelona,", param1, ",the average house/flat price was at", param3, "EUR and the average annual income was at", param2, "EUR.\n\nThis meant a person with an average income would need ", param4, "years to purchase a house in full the city.\n\n")
  cat("This is assuming %", param5*100, " of the annual income is saved to purchase a house/flat during the given period of time.\n\n")
  cat("A spanish worker is expected to work", param6, "years, meaning a worker would need to work", param4-param6, "more years than the working life expectancy to save enough money to pay in full an average priced house.\n\n This assumes income and housing price remain consistent during the given period of time and no loans are taken\n\n")
}

print(BcnHousingDescription(DataSetYear, BcnMeans[1], BcnMeans[2], BcnMeans[3], SavingsToIncomeRatio, WorkLifeExpectancySpain))


# House price boxplot chart groupped by district
plot(factor(BcnHousingVsIncome$Nom_Districte.x), BcnHousingVsIncome$Valor_mitja_finca_EUR, type = "l",col = 1, xlab = "Districte", ylab = "EUR", ylim = c(0,1200000), main = "Housing price by district")

# Annual Income boxplot chart groupped by district
plot(factor(BcnHousingVsIncome$Nom_Districte.x), BcnHousingVsIncome$Import_Renda_Bruta_EUR, type = "l", col = 1, xlab = "Districte", ylab = "EUR", ylim = c(0,50000), main = "Annual Income by district")

# Yeats to purchase boxplot chart groupped by district
plot(factor(BcnHousingVsIncome$Nom_Districte.x), BcnHousingVsIncome$years_to_purchase_in_full, type = "l",col = 1, xlab = "Districte", ylab = "Anys", ylim = c(0,100), main = "Years to Purchase by district")

