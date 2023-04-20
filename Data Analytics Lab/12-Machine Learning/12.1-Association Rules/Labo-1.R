########################################################################
# UPC School - Master in Big Data Management, Technologies and Analytics
# Data Analytics Laboratory 1: Association rules
########################################################################

library (arules)

################################################
# Example 1: Epub dataset

data ("Epub")
Epub

summary(Epub)

# first 5 transactions
inspect (Epub[1:5])

# examine the years
year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")
table(year)

# try to visualize the data
image(Epub)
Epub.short = Epub[1:1000,]
dim(Epub.short)
image(Epub.short, xlab = "Items (Documents)", ylab = "Elements (Downloads)")

# get those transactions with more than 40 documents
transactionInfo (Epub[size(Epub) > 40])

# item Frequency Plot
itemFrequencyPlot(Epub, support=0.01, cex.names = 0.5)

# obtaining rules
rules <- apriori (Epub, parameter = list (support=0.001, confidence=0.55))
rules # gives 12 rules
summary(rules)
inspect(rules)

# use maxlen in parameter list to limit the max size of LHS (maxlen-1)
rules2 <- apriori (Epub, parameter = list (support=0.001, confidence=0.55, maxlen = 2))
rules2 # gives 9 such rules (a subset of the previous 9)

inspect(rules2)

# filter the "best" 10 rules
myrules <- sort(rules, by = "lift")[1:10]
inspect(myrules)

# we can see that there are (at least) three "clusters" of frequently downloaded pubs

################################################
# Example 2: Promoter Gene dataset

pg <- read.csv2("promotergene.csv")

(p <- ncol(pg))
(n <- nrow(pg))

str(pg)
# Needs conversion to factor
pg[sapply(pg, is.character)] <- lapply(pg[sapply(pg, is.character)], as.factor)

summary(pg)

pg.apriori <- as(pg, "transactions")

summary(pg.apriori)

image(pg.apriori, xlab = "Items (Values)", ylab = "Elements (DNA sequences)")

itemFrequencyPlot(pg.apriori, support=0.35, cex.names = 0.7)

pg.rules <- apriori (pg.apriori, parameter = list (support=0.2, confidence=0.9))
summary(pg.rules)

pg.myrules <- sort(pg.rules, by = "lift")[1:10]
inspect(pg.myrules)

# Aha! It seems that the strongest correlations correspond to positions related to the '+' class. Could we use 
# arules to create a "classifier"? That would depend, among other things, on the possibility of finding rules
# related to the '-' class. Even with those rules, it is not trivial to build such a classifier.

################################################
# Example 3: Adult dataset

data("AdultUCI")
dim(AdultUCI)

summary(AdultUCI)

### irrelevant
AdultUCI[["fnlwgt"]] <- NULL
### redundant 
AdultUCI[["education-num"]] <- NULL

AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[["age"]], c(15,25,45,65,100)),
                              labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]], 
                                             c(0,25,40,60,168)),
                        labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
			    
# Probably 99999 is a NA, but let's leave it as it is
AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]],
      c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)),
               labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
      c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)), labels = c("none", "low", "high"))

summary(AdultUCI)

# what to do with NA's (especially those in 'income' and 'native-country'?) let's leave them to try to find relationships

### coerce to transactions
Adult <- as(AdultUCI, "transactions")
Adult

summary(Adult)

itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8)

# first attempt is absolutely crazy
rules1 <- apriori(Adult, parameter = list(support = 0.01, confidence = 0.6))
rules1

summary(rules1)

### one option is obviously to increase the support and confidence to get less rules
rules2 <- apriori(Adult, parameter = list(support = 0.2, confidence = 0.9))
rules2

summary(rules2)

### another option is to sort by lift

rules3 <- sort(rules1, by = "lift")[1:10]
inspect(rules3)

### still another option is to zoom on some specific rules
rulesIncomeSmall <- subset(rules1, subset = rhs %in% "income=small" & lift > 1.2)
rulesIncomeLarge <- subset(rules1, subset = rhs %in% "income=large" & lift > 1.2)

inspect(head(sort(rulesIncomeSmall, by = "confidence"), n = 3))
inspect(head(sort(rulesIncomeLarge, by = "confidence"), n = 3))

### if you wish to save the results on a file
write (rulesIncomeSmall, file = "ADULT_rulesIncomeSmall.csv", sep = ",", col.names = NA)

# Or convert to a data frame
rulesIncomeSmall.df <- as (rulesIncomeSmall, "data.frame")

########################################################################
# Exercise: analyze the Groceries dataset
########################################################################

data ("Groceries")
summary(Groceries)
