rm(list=ls())
library(readxl)
library(DataExplorer)
library(dplyr)
require(ggplot2)
library(purrr)
library(Hmisc)
library(MASS)
require(XLConnect)
credit_linear<-read_excel("Linear Regression Case.xlsx",col_names = T,col_types = NULL)

summary(credit_linear)
credit_linear<-as_tibble(credit_linear)
glimpse(credit_linear)
# categoric variables
cat_vars <- subset(credit_linear,select =c(region,townsize,gender,agecat,birthmonth,edcat,
                                  jobcat,union,empcat,retire,
                                  inccat,default,jobsat,marital,
                                  spousedcat,homeown,hometype,
                                  addresscat,carown,cartype,
                                  carcatvalue,carbought,carbuy,
                                  commutecat,commutecar,commutemotorcycle,commutecarpool,
                                  commutebus,commuterail,
                                  commutepublic,commutebike,commutewalk,
                                  commutenonmotor,telecommute,reason,
                                  polview,polparty,polcontrib,vote,card,
                                  cardtype,cardbenefit,cardfee,cardtenure,card2tenure,
                                  cardtenurecat,card2,card2type,card2benefit,
                                  card2fee,card2tenurecat,
                                  active,bfast,churn,tollfree,equip,callcard,
                                  wireless,multline,voice,pager,internet,
                                  callid,callwait,forward,confer,ebill,owntv,
                                  ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,
                                  owngame,ownfax,news,response_01,response_02,response_03))


# numeric variables
num_vars<- subset(credit_linear,select = -c(region,townsize,gender,agecat,birthmonth,
                                  edcat,jobcat,union,
                                  empcat,retire,inccat,default,jobsat,
                                  marital,spousedcat,homeown,hometype,
                                  addresscat,cars,carown,cartype,carcatvalue,
                                  carbought,carbuy,
                                  commutecat,commutecar,
                                  commutemotorcycle,commutecarpool,
                                  commutebus,commuterail,
                                  commutepublic,commutebike,commutewalk,
                                  commutenonmotor,telecommute,reason,
                                  polview,polparty,polcontrib,vote,card,
                                  cardtype,cardbenefit,cardfee,
                                  cardtenurecat,card2,card2type,card2benefit,
                                  card2fee,card2tenurecat,
                                  active,bfast,churn,tollfree,equip,callcard,
                                  wireless,multline,voice,pager,internet,
                                  callid,callwait,forward,confer,ebill,
                                  owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,
                                  owngame,ownfax,news,response_01,
                                  response_02,response_03))
# -----------------------Missing value treatment and Outlier treatment-----------------------------------

# plot depicting missing values in numerical variables
plot_missing(num_vars) #plot_missing from Dataexplorer package

plot_missing(cat_vars)
#Categorical variables having no missing values other than 
#0.04% in "townsize"


# Storing columns with more than 50% missing values in a character vector as we don't know why they are missing
remove_vars<- c('lntollmon','lntollten','lnequipmon','lnequipten','lnwiremon',
               'lnwireten')

# removing columns with more than 50% missing values
num_vars <- num_vars[,-which(names(num_vars) %in% remove_vars)]
rm(remove_vars)


# creating variable `total_spent` from `cardspent` & `card2spent` as we are interested inferring what factors 
#are guiding total credit card spend
num_vars$total_spent<-num_vars$cardspent+num_vars$card2spent

# These variables already have a categorical counterpart or 
# directly affect the target variable(i.e like carditems variable is most likely to be highly
#correlated with cardspent,similarly for card2items with card2spent
#hence tagging them as redundant variables
#like pets variable has 8 different variables so keeping the main 'pets' 
#variable as it has the same information.Similarly with commute,cardtenure,card2tenure,etc.

remove_vars2<- c("age","ed","address","employ",
                "custid","commutecat","commutecar","commutemotorcycle",
                "commutecarpool","commutebus","commuterail","commutepublic",
                "commutebike","commutewalk","commutenonmotor",
                "address","carditems","cardspent","card2items","card2spent",
                "pets_dogs","pets_cats","pets_birds",
                "pets_reptiles","pets_small","pets_saltfish","pets_freshfish",
                "cardtenure","card2tenure")

# removing redundant variables
num_vars <- num_vars[,-which(names(num_vars) %in% remove_vars2)]
cat_vars <- cat_vars[,-which(names(cat_vars) %in% remove_vars2)]

# user written function for creating descriptive statistics
mystats <- function(x) {
        nmiss<-sum(is.na(x))
        a <- x[!is.na(x)]
        m <- mean(a)
        n <- length(a)
        s <- sd(a)
        min <- min(a)
        p1<-quantile(a,0.01)
        p5<-quantile(a,0.05)
        p10<-quantile(a,0.10)
        q1<-quantile(a,0.25)
        q2<-quantile(a,0.5)
        q3<-quantile(a,0.75)
        p90<-quantile(a,0.90)
        p95<-quantile(a,0.95)
        p99<-quantile(a,0.99)
        max <- max(a)
        UC <- m+3*s
        LC <- m-3*s
        outlier_flag<- max>UC | min<LC
        return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}
diag_stats<-t(data.frame(apply(num_vars, 2, mystats)))
write.csv(diag_stats,"statistics.csv")
View(diag_stats)
#---------------- Outlier detection & treatment--------------------------------------


# Custom function for capping the outliers with lower and upper quantile values
Out_fun <- function(x){
        quantiles <- quantile( x, c(.05, .95 ),na.rm=TRUE)
        x[ x < quantiles[1] ] <- quantiles[1]
        x[ x > quantiles[2] ] <- quantiles[2]
        x
}
num_vars<- data.frame(apply(num_vars,2,Out_fun))
diag_stats<-t(data.frame(apply(num_vars, 2, mystats)))
View(diag_stats)     #No outliers in numeric variables now.
summary(num_vars)
# plotting the missing values
plot_missing(num_vars)



#Imputing missing values with mean value as they are missing at random
num_vars<-data.frame(apply(num_vars,2, function(x) impute(x,mean)))
diag_stats<-t(data.frame(apply(num_vars, 2, mystats)))
View(diag_stats)
#No missing values and outliers in numeric variables now
summary(num_data)
# final count of missing values=0
sum(is.na(num_vars))


#--------converting remaining categorical variables--------#
cat_vars<-map(cat_vars,as.factor)
cat_vars<-as.data.frame(cat_vars)
#-------investigating distributions of categorical variables------#
plot_bar(cat_vars,maxcat = 50)
#-------No outliers------#


#-----Imputing missing values in categorical values as they are missing at random--------#
cat_vars<-data.frame(apply(cat_vars,2, function(x) impute(x, mode)))
#------No missing values in categorical variables now------#
sum(is.na(cat_vars))


#------------------------Data exploration-------------------------------------

# Scatter plot of each numeric variable w.r.t target variable
# `plot_scatterplot()` from package DataExplorer

plot_scatterplot(num_vars,by ='total_spent',title = "Scatterplot of each feature w.r.t target variable")

# Creating a custom function to check which transformation gives best 
# correlation with the target variable

trans_func<-function(x){
        log10_trans= cor(num_vars$total_spent,log10(1+x))
        sqrt_trans= cor(num_vars$total_spent,sqrt(x))
        best_trans= ifelse(log10_trans>sqrt_trans,"log10","sqrt_trans")
        return(c(log10_trans= log10_trans,sqrt_trans= sqrt_trans,best_trans= best_trans))
} 

# applying the transformation function
trans_coeff<- data.frame(apply(num_vars,2,trans_func))

# Applying transformation to a few variables
num_vars$creddebt<- sqrt(num_vars$creddebt)
num_vars$othdebt<-log(1+num_vars$othdebt)
num_vars$longten<-log10(1+num_vars$longten)
num_vars$cardmon<-log10(1+num_vars$cardmon)
num_vars$ln_total_spent<-NULL
# ------------Analysing Categorical variables--------------------------------
#------assigning another variable to store the target variable----#
target_var<-num_vars$total_spent
# the nos of missing values is 2 we will omit these observations later

# Converting categorical variables in to factor
cat_vars<-data.frame(apply(cat_vars,2,as.factor))


#------ Selecting significant categoric vraiables using ANOVA------

# assigning the target variable to cat_vars for anova
cat_vars$target_var<- target_var

# ANOVA model to measure significance of variables
a1<-aov(formula <- target_var ~ ., data = cat_vars)
summary(a1)


# Selecting the significant categorical variables from anova analysis

imp_cat<- cat_vars[,c("region","gender","agecat",
                     "edcat","jobcat","empcat","retire",
                     "inccat","jobsat","carown",
                     "reason","card","card2")]

# Combining the final data frame
final_data<-cbind(num_vars,imp_cat)
# Splitting data into training and validation set
set.seed(552)

# creating an index
train_data<- sample(1:nrow(final_data), size = floor(0.70 * nrow(final_data)))
credit_train<-final_data[train_data,]
credit_test<-final_data[-train_data,]

summary(credit_train)

##--------------------------Modelling--------------------------------------
library(car)
# Building a linear regression mode using Lm()
model1<-lm(total_spent~.,data = credit_train)
summary(model1)
#Calculating leverage statistic for removing high leverage points
hl<-hatvalues(model1)
plot(hatvalues(model1),type = "h")
credit_train<-credit_train[hl<=(3*44)/3500,]
vif(model1)
model2<-lm(total_spent~.,data = credit_train)
models <- stepAIC(model2, trace = F,
                     direction = "both")
summary(models)
vif(models)
#Further improving the model
#variance inflation factor for longten is 150 ,so removing it
model3<-lm(total_spent~income+lninc+carvalue+lnlongten+tollten+cardmon
           +cardten+lncardten+wiremon+gender+jobsat+reason+card+card2,data=credit_train)
summary(model3)
vif(model3)
#income and cardten have high vif as well income is the same thing as lninc as lninc has a lower pvalue 
#so we keep it instead of inc

model4<-lm(total_spent~lninc+carvalue+lnlongten+tollten+cardmon
           +lncardten+wiremon+gender+jobsat+reason+card+card2,data=credit_train)
summary(model4)
vif(model4)
##As carvalue,lnlongten,cardmon,lncardten,jobsat have high p value ,so they are 
##insignificant in presence of other variables as well the vif for all the variables is below 2.
##So removing them
model5<-lm(total_spent~lninc
          +gender+reason+card+card2,data=credit_train)
reg_result<-summary(model5)
vif(model5)
#Checking Assumptions made in Linear regression
#1) Errors term should be normally distributed  
qqnorm(credit_train$total_spent)
qqline(credit_train$total_spent)
#Errors are not normally distributed as seen in the quantile-quantile plot ,they don't 
#fall on a straight line

#2) Non constant variance of error terms i.e heteroscedastic data 
plot(model5$fitted.values,rstandard(model5))

##although residual plot doesn't follow 
#a funnel like shape but it surely has some other pattern.

#3) Multi collinearity in model
vif(model5)

#No vif above 1.3 so there is no problem with multicollinearity in our model


#4) High leverage points and outliers are another things that needed to be taken into consideration,
#but they have been taken care of in earlier steps of models.

## Calculating Mean absolute Percentage Error (MAPE)

# MAPE for training
library(MLmetrics)

MAPE(y_pred = model5$fitted.values,y_true=credit_train$total_spent)
train_spent<-predict(model5,credit_train)
t1<-cbind(credit_train,train_spent)
#------------------------
# MAPE for testing
test_spent<-predict(model5,credit_test)
APE = abs((test_spent - credit_test$total_spent)/credit_test$total_spent)
mean(APE)
t2<-cbind(credit_test,test_spent)

# -----Getting the Root mean square error (RMSE) ------------------------------------------ 

# calculating RMSE for training set
RMSE(train_spent,credit_train$total_spent) 

# calculating RMSE for validation set
RMSE(test_spent,credit_test$total_spent) 

# Since both the rmse scores are nearly close to each other our model is not overfitting

# finding the decile locations 
decLocations <- quantile(t1$train_spent, probs = seq(0.1,0.9,by=0.1))

# using findInterval() with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$train_spent,c(-Inf,decLocations, Inf))

require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count,
               avg(total_spent) as avg_total_spent,
               avg(train_spent) as avg_predicted_spent from t1
               group by decile
               order by decile desc")

# decile analysis output for training
head(t1_DA)
write.csv(t1_DA,"train_DA.csv")

###-------- decile analysis for Validation set-----------------------

# finding the decile locations 
decLocations <- quantile(t2$test_spent, probs = seq(0.1,0.9,by=0.1))

# using findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$test_spent,c(-Inf,decLocations, Inf))

# decile analysis output for validation

t2_DA <- sqldf("select decile, count(decile) as count,
               avg(total_spent) as avg_total_spent,
               avg(test_spent) as avg_predicted_spent from t2
               group by decile
               order by decile desc")

head(t2_DA)
write.csv(t2_DA,"test_DA.csv")