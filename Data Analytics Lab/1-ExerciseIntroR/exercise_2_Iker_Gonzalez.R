# Iker Gonzalez

setwd("D:/iCloud/iCloudDrive/Master Big Data/Data Analytics - Teoria/Introduction R_/ExerciseIntroR")

#Exercise 1
#####
choices = read.csv("_repository/Choices.csv", header = TRUE, sep = ";")

summary(choices)
View(choices)

#Exercise 2
#####
conTable = table(choices$INFO, choices$MEASURE)
conTableWithSums = addmargins(conTable)


#Exercise 3
#####
conTableWithSums["Sum",1:6]
conTableWithSums[1:5,"Sum"]

#Exercise 4
#####


#Independence Evaluation
#------------------------
#Step 1: Define hypothesis

## H0 = Measure and Info are independent
## H1 = Measure and info are dependent

#Step 2: Compute Chi Squared (x^2)

chiSq = chisq.test(conTable)

#Step 3: Compute p-value and degrees of freedom

pValue = pchisq(q=860.6619, df=12, lower.tail = FALSE)      # Degrees of Freedom = (5-1)*(4-1) = 12 //  q = chiSq
format(pValue, scientific = FALSE)                          # Converting pValue to standard notation



#Step 4: Obtain Critic Value at 99% confidence and 12 Degrees of Freedom

confidence = 0.01

CriticValue = .661
xSquared = 860.6619

#Step 5: Analyze results

hypotesisAnswer = if (pValue >= confidence){
  print("H0 is accepted. The variables are independent")
} else {
    print("H0 is rejected, and H1 is accepted. The variables are dependent.\n")
  }


justifiedAnswer = function(p1, p2, p3, p4) {
  cat(p1, "\n P-Value =", p3, "which is less than 0.01, thus rejecting H0. On the other hand x^2 = ", p2, "is greater than the critic value =", p4, "sustaining the rejection of H0.\n")
}

print(justifiedAnswer(hypotesisAnswer, xSquared, pValue, CriticValue))


# H0 is rejected, and H1 is accepted. The variables are dependent.
# P-Value = 1.601542e-176 which is less than 0.01, thus rejecting H0. 
# On the other hand x^2 =  860.6619 is greater than the critic value = 0.661 sustaining the rejection of H0.
