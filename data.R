
# Data Importation & Transformation
rm(list = ls())

setwd("D:/Dataiku/us_census_full") 

# import 
train <- read.csv("census_income_learn.csv", header = FALSE,strip.white=TRUE) # around 9 s. 
test <- read.csv("census_income_test.csv", header = FALSE,strip.white=TRUE)

save(train, test, file = "rawdata.Rdata")

# dim(train) # 199 523 individuals and 42 variables...
# by checking the meta data file, we can infer that the data correspond to the last table of the file
# with 2 lignes for instance_weight. From there we create the "names.csv" file.  

varnames <- as.vector(read.csv("names.csv",header=FALSE,strip.white=TRUE)[[1]])

names(train)<-varnames
names(test)<-varnames

# in csv we can find "?" => NA in R
train[train=="?"]= NA
test[test=="?"] = NA

# it is easier to work with 0 and 1 for the salary variable. 
train$salary = as.numeric(train$salary) - 1
test$salary = as.numeric(test$salary) - 1

# summary(train) shows that some variables are considered continuous although they are factors (ex : year)
# also fore some variables "?" is still a level in factors.
# we change all supposed factor variables to factor, using the metadata description 

continuous = rep(FALSE,42)
continuous[c(1,6,17,18,19,25,31,40)] = TRUE
for (i in 1:ncol(train)){
	if (!continuous[i]){
		train[,i]=factor(train[,i])
		test[,i]=factor(test[,i])
	}
}

train <- as.data.frame(train)
test <- as.data.frame(test)

# - we have 3229 exact duplicates in train set.
# we choose to delete these because of what the metadata file tells us (Duplicate or conflicting instances : 46716)
# and because we find unlikely that two lignes have the same continuous values and are not the same. (ex : weights) 

train = unique(train)

save(train,test,varnames, file = "data.Rdata")

rm(list=ls())
load(file = "data.Rdata") # 0.17 s