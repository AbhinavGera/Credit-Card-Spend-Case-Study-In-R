# CreditCardSpendCaseStudyInR
## AIM
**The objective of this case study is to understand what's driving the credit card spend and prioritizing the drivers based on the importance. It has data of about 5000 customers and 131 features**

## Implementation
### Understanding the problem statement
Firstly we understand the problem statement and the data dictionary (i.e. what each variable means) and what is the question we are trying to answer.

### Loading and Munging the dataset
First we load the necessary packages and then import the dataset from out working directory using read_excel() function of readxl package.
Then we briefly have a look on the dataset using glimpse() function on dplyr package and summary() function of base package.Then we do some explocot datatype changes.

Then we segregate the variables into categorical and numerical for the analysis. Then we plot missing values in each variable using plot_missing() from dataexplorer package and then we came to know that some variables have more than 50% of entries missing, even if it is missing at random we'll have to make a choice are they very important for the analysis or not depending on variable we are trying to predict. If, yes then we must get aqcuire those entries before proceeding if, not then we can simply remove those variables as I did in the case study.
Then, we remove redundant variables. Then we create a user defined function to have a look at descriptive statistics, outliers, missing values,quartiles,etc. Then, as we know that the values are missing at random we imppute numerical variables with corresponding mean and categorical variable with corresponding mode. Then we cap the outliers at 5th percentile for lower bound and 95 percentile as upper bound for outliers.
### Data Exploration
Then we explore each variable by plotting a scatterplot along with the target variable.
As we have already have dealt with outliers and missing data we don't need to plot distribution plots for variables.

### Fitting the model
We use anova to check for important categorical variables and transform some of the numerical variables.
Then we fit a linear regression model using lm() function. Then we calculate leverage statistic for removing high leverage points.
Then we use vif and p value to subsequently improve the model (i.e Higher adj R squared and low multicollinearity).
And then we check our assumptions which are:
                                  **1)** The errors term should be normally distributed using qqplot().
                                  **2)** Non constant variance of error terms i.e heteroscedastic data.
                                  **3)** Multicollinearlity check using VIF.
                                  **4)** High leverage points(we had already removed the points with high leverage statistic)

### Metrics
Then we check the metrics using decile analysis, R squared,adjusted R squared, checking test error to be sure that we aren't overfitting.

### Prerequisites
1) Installing R.
2) Knowledge of R programming.
3) Knowledge of Data Munging.
4) Knowledge of Linear regression

### Author
**Abhinav Gera**



