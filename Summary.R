
# US_Census analysis.

# the goal was to predict for individuals if they earn more than 50000$ per year or not. 

# I used R because I still need some training with python to work as fast as using R. 

# My approach : 

# I first look at the metadata file and noticed two things :
# - there was a weight column which had to be taken into account 
# - there was many more "0" than "1" so obviously the percentage of correct value is not a good indicator of performance.
# As a result I tried to take the weights into account in the analysis (when I could) and in the performances indicators (always).
# I choose to always look at precision, recall and F1-score to choose the best fit rather than the classical performance estimator. 
# That lead me to recode average versions of F1-score, ROC curve, etc. ... 
# I also threw away the doubles (because I considered that having the exact same weight, age, year... was not possible. and I took the metadata file into account)

# A basic analysis of each variable and it's comparaison with the response variable made me discard half of the variables 
# and create one (difference between the capital gains and loss).
# by runing a rpart tree on the whole train set, I manage to confirm that the variable used in this model were all part of the set I selected.
# I also considered that all children under 15 have a response "0" in order to have a smaller sample of individuals. 
# => In the final tests, all <15 will have a response of 0.

# In order to choose a model, I separated the train set into a smaller train set and a test one. 
# I then tried different algorithms :  
# - a rpart tree with all the variables and taking into account the weights in the tree construction. 
# - a logistic regression taking a few variables into account (and not using the weights...). 
#   I took a limited set of variables because factors with lot's of labels and the number of individuals made the
#   running time a bit long. I checked that all the variables were relevant even though some factors happened not to be. 
# - a basic svn with default parameter (I did not worked on parameters because running time was a bit long)
#   that gaves me similar performance results with the rpart tree so I did not explore further. 
# 
# The logistic regression had the better performance (f1 score and ROC curve).
# I selected a "pvalue" of 0.261 (corresponding to the max f1 score)
# to apply later on the test data set. (ypred = 1 if ypred_proba > 0.261). 
# I could have explore the result of a cross validation on this parameter. 

# I finally applied the model on the whole data set and got : precision = 0.533, recall = 0.610, f1 = 0.569, perf =0.941
# the performance (proba of correctness) is not impressive but it is also not a good indicator. 
# The f1-score is not that good, due to a poor precision (and rather small recall).
# But I tried to maximize it and the model i selected corresponded to the higher recall because i thought 
# that finding the over 50000 was more important than the under 50000.   

# I found that the variable that were the most discriminating were : age, capital_balance, detailed_industry_recode, detailed_occupation_recode, dividends_from_stocks, education, sex, tax_filer_stat 

# To go further :
# - It may be possible to use weights values in glm. I'm working on it.
# - With the summary(glm), it is possible to select a smaller number of factors of interest per variable.
#   Then it is possible to create factor variables containing 1 or 0 (not so many of them because of the selection).
#   Then the runing time should shrink and a more detailed work could be done one glm but also on
#   svm, knn or randomForest (which cannot take into account more than 50 factors in a variable)
# - I have not thought of any interesting new variables. But it must be possible to find some. 


