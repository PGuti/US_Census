
setwd("D:/dataiku/us_census_full")
load("datanoChild.Rdata")

# creating our train and test on train to choose the model
# hope that in 80 % of 150000 ind all factors will be present
sub = sample.int(nrow(trainNC),size = round(80/100 * nrow(trainNC)))
dftrain = trainNC[sub,c(1:15,17:21)]
dftest = testNC[-sub,c(1:15,17:21)] 

ytest= testNC[-sub,21]

wtrain = trainNC[sub,16]
wtest = testNC[-sub,16]

#####################
# decision tree #####
#####################
library(rpart)

# Usefull functions (to get only performance.)
# NB : due to the ratio between positive and negative, F1-score may be a better indicator
FOneScore <- function (ypred,ytest,wtest){ # weighted version of F1-score. To verify, I guessed a weighted sum would do
	
	P = sum(wtest[which(ytest == 1)]/sum(wtest)) # proportion of positive
	TP = sum((ypred==1) * (ytest == 1)*wtest/sum(wtest)) # proportion of true positive
	
	precision = TP/sum((ypred==1)*wtest/sum(wtest)) # proportion of True positive / proportion of predicted true
	recall = TP/P
	fonescore = 2*precision*recall/(precision+recall)
	perf = sum(wtest[which(ypred==ytest)])/sum(wtest)
	return(list(precision = precision, recall = recall, fonescore = fonescore,perf=perf))
} 

getROC<-function(pval,ytest,wtest,nbpoints=100){

	l = (nbpoints:1)/nbpoints
	P = sum(wtest[which(ytest == 1)]/sum(wtest)) # proportion of positive
    N = 1 - P # proportion of negative
	TP = 0 
	FP = 0
	fonescore= numeric(0)
	for (i in (1:nbpoints)){
		tp = sum((pval>l[i])*(ytest == 1)*wtest/sum(wtest))
		TP = c(TP,tp)
		FP = c(FP,sum((pval>l[i])*(ytest == 0)*wtest/sum(wtest)))
		precision = tp/sum((pval>l[i])*wtest/sum(wtest)) # proportion of True positive / proportion of predicted true
		recall = tp/P
		if (!is.nan(precision)){
			fonescore = rbind(fonescore,c(l[i],(2*precision*recall/(precision+recall))))
		}
	}

	return(list(roc=cbind(c(FP/N,1),c(TP/P,1)),fonescore=fonescore))
}

fit <- rpart(salary ~ ., method="class", weights=wtrain, data=dftrain)
ypred= predict(fit, dftest,type = c("class"),na.action = na.pass)
fonescore = FOneScore(ypred,ytest,wtest) # -> recall is not very good !!! 
printcp(fit, digits=getOption("digits") - 2)

plot(fit)
text(fit)

ypredtree= predict(fit, dftest,na.action = na.pass)
roctree =  getROC(ypredtree[,2],ytest,wtest,nbpoints=1000)
plot(roctree$roc[,1],roctree$roc[,2],type="l")

# I actually tried to play with rpart parameters like "parms" and "control" leading to no significative difference


############
# RF #######
############
require(randomForest)
fit <- randomForest(salary ~ ., data=dftrain)
# not working cause "cannot handle categorical predictor with more than 32 categories" pb : detailed_occupation_recode is important 
# we would work on that when we have time. 

#######
# glm #
#######

# NB : this analysis do not take into account the weights ... to modify later
# glm will have a hard time taking everything into account because factors may have lot'sb of level. 
# as a consequence we only use the variables we have seen usefull in the several trees created

dftrain$salary = as.numeric(dftrain$salary)-1
myglm <- glm(salary ~ age + capital_balance + detailed_industry_recode + 
detailed_occupation_recode + dividends_from_stocks +
education + sex + tax_filer_stat + weeks_worked_in_year, data=dftrain,family=binomial) 

# summary(myglm) => all variables are relevant although many factors are not. 
# to go further we could create a variable for each relevant factor and try again. 

ypredglm = predict(myglm,dftest,type="response")

rocglm =  getROC(as.vector(ypredglm),ytest,wtest,nbpoints=1000)
fonescoreglm = FOneScore(as.vector(ypredglm>0.5),ytest,wtest) # -> recall is not very good !!! 

# comparaison between rpart and glm : 
plot(x=rocglm$roc[,1],y=rocglm$roc[,2],col=2,type="l",xlab="False Positive rate",ylab="True Positive Rate", main = "RoC Curve")
lines(x=roctree$roc[,1],y=roctree$roc[,2],col=3) 
legend(x=0.5,y=0.2,legend = c("glm","rpart"),col = c(2,3),lty=1 )

# what we can see is that the tree method is not flexible if we want to capture most of the >50000 by accepting more false positive
# the result of tree will be bad.

# we may choose a model for wich the f1score is maximal. and not the performance. 
# as a result : we may want to find the corresponding pvalue.
roctree$fonescore[which(roctree$fonescore[,2]==max(roctree$fonescore[,2])),]
rocglm$fonescore[which(rocglm$fonescore[,2]==max(rocglm$fonescore[,2])),] # 0.261

#########
# SVM ###
#########
library(kernlab)
fitsvm <- ksvm(salary ~ age + capital_balance + detailed_industry_recode + 
detailed_occupation_recode + dividends_from_stocks +
education + sex + tax_filer_stat + weeks_worked_in_year,type="C-svc",data=dftrain)

ypredsvm= predict(fit, dftest,type = c("class"),na.action = na.pass)
fonescore = FOneScore(ypred,ytest,wtest)

ypredsvm= predict(fit, dftest,type = c("prob"),na.action = na.pass)
rocsvm =  getROC(ypredsvm[,2],ytest,wtest,nbpoints=1000)
plot(x=rocsvm$roc[,1],y=rocsvm$roc[,2],col=2,type="l",xlab="False Positive rate",ylab="True Positive Rate", main = "RoC Curve")
rocsvm$fonescore[which(rocsvm$fonescore[,2]==max(rocsvm$fonescore[,2])),]
# without doing something more intelligent, svm has the same perf as rpart in this case. 


################################################################################################


# Conclusion : Our best fit is obtained with logistic regression 
# for our pval we may choose 0.261 
# We may want to do a cross validation to get this pval but it is time consuming. 

# Final model :

dftrainf = trainNC[,c(1:15,17:21)]
dftestf = testNC[,c(1:15,17:21)]

ytestf= c(as.numeric(testNC[,21])-1,as.numeric(testC[,21])-1)

wtrainf = trainNC[,16]
wtestf = c(testNC[,16],testC[,16])

dftrainf$salary = as.numeric(dftrainf$salary)-1
myglm <- glm(salary ~ age + capital_balance + detailed_industry_recode + 
detailed_occupation_recode + dividends_from_stocks +
education + sex + tax_filer_stat + weeks_worked_in_year, data=dftrainf,family=binomial) 

# summary(myglm) => all variables are relevant although many factors are not. 
# to go further we could create a variable for each relevant factor and try again. 

ypredglm = predict(myglm,dftestf,type="response")
ypredglm = c(ypredglm, rep(0,nrow(testC)))

rocglm =  getROC(ypredglm,ytestf,wtestf,nbpoints=1000)
plot(rocglm$roc[,1],rocglm$roc[,2])

fonescoreglm1 = FOneScore(as.vector(ypredglm>0.5),ytestf,wtestf) # precision = 0.733, recall = 0.367, f1 = 0.487, perf =0.951
fonescoreglm2 = FOneScore(as.vector(ypredglm>0.261),ytestf,wtestf) # precision = 0.533, recall = 0.610, f1 = 0.569, perf =0.941
# with the second one we may loose some perf but we have a better f1 score and obviously a way better recall
