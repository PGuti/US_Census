
rm(list=ls())
setwd("D:/Dataiku/us_census_full")

load("data.Rdata")

# NB and meta data comments:
# - response is the 42sd column and weight the 25. We will try to use the weights in the models and in performance testing (using weighted errors)
#   weight will not be considered a variable
# - a quick and basic classif tree using rpart gave us a few variable of interest :
#   age, capital_gains, detailed_industry_recode, detailed_occupation_recode, dividends_from_stocks, education, sex, tax_filer_stat 
#   we may focus more on these

#########################
# univariate analysis ###
#########################
 
# a simple function that give a summary as output and plot some information
# input is a string name that refers to a name of data frame. plotting is a boolean for plot or not. pdfplot boolean
univariate<- function(column,plotting=TRUE,pdfplot = FALSE,norm=TRUE){

	x = train[,column]
	tabl = NULL
	if (plotting){
		if (pdfplot){pdf(file= paste(varnames[column],".pdf",sep=""))}
		if (is.factor(x)){ # data is labels
			par(mfrow=c(1,1))
			tabl = table(x,train[,42])
			if (norm){ 
				tabl = apply(tabl,2,function(c){c/sum(c)})
			}
			dotchart(tabl)		
			
		}else{ # data is "continuous"
			x=unlist(x)
			par(mfrow=c(2,1))
			hist(x, main = paste("Histogram of" , varnames[column]), xlab = varnames[column],freq=FALSE)
			lines(density(x), col = 1)
			lines(density(x[which(train[,42]==0)]),col=2)
			lines(density(x[which(train[,42]==1)]),col=3)
			legend(x=0,y=0.01,legend = c("global","<50000",">50000"),col = c(1,2,3),lty=1 )
			hist(x[which(train[,42]==0)], col='blue',main = "Comparative Histogram. Red is Earning above 50000")
			hist(x[which(train[,42]==1)], col='red', add=T)
		}
		if(pdfplot){dev.off()}
	}
	
	missing = sum(is.na(x))
	if (is.null(tabl)){
		return(list(name =varnames[column],sumary = summary(x), missing_entry = missing))
	}else{
		return(list(name =varnames[column],sumary = summary(x), missing_entry = missing,comparativetable =tabl,chi = chisq.test(tabl) ))
	}
} 
 
univariate(1) #examples
univariate(26)

discrete<-function(column,categories=20,method="interval"){
	require(arules)
	x = train[,column]
	tmp = discretize(x, method=method, categories = categories, labels = NULL, onlycuts=FALSE) 
	tabl = table(train$salary,tmp)
	return (list(tabl = tabl, normtabl = apply(tabl,1,function(c){c/sum(c)}),chi = chisq.test(tabl)))
}

# a few results/remarks

# - univariate(1) age :
# the univariate function shows that some individuals have age 0 (probably not a missing value)
# in fact dim(train[train$education==" Children",]) is 44347 and it exactly corresponds to age<15. (by testing min and max on each case)
# those children have always salary <50000 so
# we may want not to take theme into account and predict -50000 for any children (that is 23.8 % less individuals, usefull for costly methods)
# of course we loose some info if in test set there is a children already earning more than 50000

# - univariate(2) : big proportion of private in >50000. 
# - univariate(3) : detailed industry code : way less 0 category for >50000 (may correspond to children or unemployed => redundant ?)
# - univariate(4) : detailed industry code : way less 0 category for >50000 but also a lot of "2" for >50000
# => may be interesting 
# education : the two distributions are quite different => may be interesting 
 
# - univariate(6) : wage_per_hour : skewed distribution. 
# we may want to check a table of this variable after discretization
discrete(6) # => wage_per_hour is not a very good predictor since most of the 1 correpond to a 0-500 value...

# - univariate(7) enroll_in_edu_inst_last_week does not contain much information => ? discard
# - univariate(10) : major occupation code  may be interesting since there are a lot of 1 <-> Executives or professional specialty
# - univariate(11) => comparatively, minorities are a bit less likely than whites to be well paid... univariate(12) useless
# - univariate(13) => comparatively, women are way less likely than wites to be well paid... => Very Important Variable
# - univariate(15) => too many "Not in universe" => discard
# - univariate(17,18) => skewed. capital_gains is not very good beacause it may compensate with capital_losses
# => creation of a new variable as the difference. By applying same methodology as wage_per_hour we see it may be a good predictor

# - univariate(21) => discard : not in universe
# - univariate(22) => discard : no effect
# - univariate(26->28) a lot of missing entries. Most of people are non movers in each case => may be discarded

# after that, variables does not seem to be usefull but we may keep 31, 36, 37, 40
# - univariate(40) : clearly interesting to keep

# all missing entries : 
apply(train,2,function(x){sum(is.na(x))}) # luckily the variable with lot's of missing entry are the one we planned to discard

# As a result we can infer some interesting variables that may be used : 
# columns 1,2,3,4,5, 8,9, 10,11,13, new variable diff 17,18 ; 19, 20, 23, 24, 31, 36, 37, 40
# 42 is to predict, 25 is the weights

# correlations between continuous variables
continuous = rep(FALSE,42)
continuous[c(1,6,17,18,19,25,31,40)] = TRUE
correlation = cor(train[,continuous]) # correlation within continuous values are small. 
# weeks_worked_in_year 0.744 with num_persons_worked_for_employer and  0.19 with wage_per_our and capital_gains
 
# replace the 18 column (capital losses so we don't move the dataframe indexes)
varnames[18] = "capital_balance"
tmp =  train$capital_gains - train$capital_losses
names(train)<-varnames
train$capital_balance = tmp
tmp =  test$capital_gains - test$capital_losses
names(test)<-varnames
test$capital_balance = tmp

new = rep(FALSE,42)
new[c(1,2,3,4,5,8,9,10,11,13,18 , 19, 20,25, 23, 24, 31, 36, 37, 40,42)] =TRUE
train= train[,new] # now 21 column so 19 variables kept
test=test[,new] 
 
save(train,test,varnames, file = "newdata.Rdata") 
# NB : weight is now 16 and response 21
 
trainNC = train[train$education!="Children",]
testNC = test[test$education!="Children",]
testC = test[test$education=="Children",]
save(trainNC,testNC,testC,file="datanoChild.Rdata") 
 

# apply MCA (I did no manage to make this package work). even with subsampling it still costs too much


#####################
# decision tree #####
#####################
library(rpart)
load("data.Rdata")

dftrain = train[,c(1:24,26:42)]
dftest = test[,c(1:24,26:41)] 

ytest= test[,42]
wtrain = train[,25]
wtest = test[,25]

# Usefull functions (to get only performance.)
# NB : due to the ratio between positive and negative, F1-score may be a better indicator
FOneScore <- function (y,ytest,wtest){ # weighted version of F1-score. To verify, I guessed a weighted sum would do
	
	P = sum(wtest[which(ytest == 1)]/sum(wtest)) # proportion of positive
	TP = sum((y==1) * (ytest ==1)*wtest/sum(wtest)) # proportion of true positive
	
	precision = TP/sum((y==1)*wtest/sum(wtest)) # proportion of True positive / proportion of predicted true
	recall = TP/P
	fonescore = 2*precision*recall/(precision+recall)
	perf = sum(wtest[which(y==ytest)])/sum(wtest)
	return(list(precision = precision, recall = recall, fonescore = fonescore,perf=perf))
} 

fitb <- rpart(salary ~ ., method="class", weights=wtrain, data=dftrain)
plot(fitb)
text(fitb,use.n=TRUE)
ypredb= predict(fitb, dftest,type = c("class"),na.action = na.pass)
fonescoreb = FOneScore(ypredb,ytest,wtest) # -> recall is not very good !!! 
plot(fit)
text(fit, use.n = TRUE)
printcp(fit, digits=getOption("digits") - 2)
# summary(fitb) => all the used variables are part of the set I selected. 

#require(ROCR)
#ypredb = predict(fitb, dftest)
#pred<-prediction(ypredb[,2],ytest)
#perf <- performance(pred, "tpr", "fpr")
#plot(perf)

