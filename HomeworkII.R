library(ggplot2)
library(pander)
library(grid)
library(gridExtra)
library(pastecs)


###################################################################################
################################# HOMEWORK II #####################################
###################################################################################

##########################################################
# Q 1. Plasma data set outliers                          #
##########################################################

# Collett argues that two outliers need to be removed from the plasma data
# Try to identify those two unusual observations by means of scatterplot

data("plasma", package = "HSAUR3")
head(plasma)
ggplot(plasma, aes(x=fibrinogen,y=globulin)) + geom_point()

##########################################################
# Q 2. Multiple Regression using the 'hubble' data set   #
##########################################################

data("hubble", package="gamair")
summary(hubble)
head(hubble)

# a. Fit a quadratic regression model to the data set

velocity <- hubble$y
distance <- hubble$x
Model_2 <- lm(velocity ~ distance + I(distance^2), data=hubble)
summary(Model_2)
pander(Model_2)

# b. Plot the fitted curve from Model_2 on the scatterplot of the data

plt <- ggplot(hubble, aes(x=distance,y=velocity)) + geom_point() + geom_smooth(method="lm",formula=y~x+I(x^2),se=FALSE,aes(col='Quadratic'))
plt

# c. Add a simple linear regression fit on this plot, using a diff color, line type
#    and add a legend

plt + stat_smooth(method='lm', se=FALSE, aes(col='Linear'), lty=2) + theme(legend.title=element_text(color ='black',face='bold'), legend.text=element_text(color='blue'))


SLR <- lm(velocity ~ distance, data=hubble)

# d. Which model do you consider most sensible considering the nature of the data

# after plotting the quadratic line and linear line, the linear line fits the data better.

# e. Which model is better? Provide a statistic for support
summary(Model_2)
summary(SLR)

# SLR provides the better fit as the P-Value is lower and higher F Score
anova(SLR,Model_2)

########################################
# Q 3: The leuk data from package MASS #
########################################

data("leuk", package = "MASS")
head(leuk, n=15)
summary(leuk)

# a. Define a binary outcome variable according to whether or not patients
#    lived for at least 24 weeks after diagnosis

leuk2 <- leuk
leuk2$surv24 <- ifelse(leuk2$time>=24,1,0)

# b. Fit a logistic regression model to the data with surv24 as response.
#    Transform very large wbc counts to avoid coeff results close to 0.

# log transform wbc
leuk2$wbc <- log(leuk2$wbc)
leuk2

# fit logistic regression

mylogit <- glm(surv24 ~ wbc + ag, data = leuk2, family = binomial())
tst <- glm(surv24 ~ ag, data=leuk2, family=binomial())
summary(tst)
summary(mylogit)
pander(mylogit)

# c. Construct some graphics usefull in interpreting of the final model you fit

mylogit_fit <- predict(mylogit, type='response')
p <- leuk2$ag == "present"

p1 <- ggplot(subset(leuk2, ag=='present'), aes(x=wbc,y=mylogit_fit[p])) + geom_point() + stat_smooth(method="glm",col='purple',se=FALSE) + 
  xlab("White Blood Cell Count") + ylab("Prob of Surviving 24 Weeks") + ggtitle("Morphological Char Present")
p1

p2 <- ggplot(subset(leuk2, ag=='absent'), aes(x=wbc,y=mylogit_fit[!p])) + geom_point() + stat_smooth(method='glm', col='green',se=FALSE) +
  xlab("White Blood Cell Count") + ylab("Prob of Surviving 24 Weeks") + ggtitle("Morphological Char Absent")
p2

# From the graph, it appears that as while blood cell count increases, the probability
# of living past 24 weeks is reduced. This effect is even more so for those with a absence
# of a morphological characteristic of white blood cells

# d. Fit a model with an interaction term between the two predictors. 

mylogit2 <- glm(surv24 ~ wbc * ag, data=leuk2, family=binomial())
mylogit2_fit <- predict(mylogit2, type='response')
summary(mylogit2)
pander(mylogit2)
pander(mylogit)

1-pchisq(45.475-34.167,32-29)
1-pchisq(45.475-37.498,32-30)

########################################
# Q 4: The Default data set from ISLR #
########################################

data("Default", package="ISLR")
head(Default)


# a. Perform descriptive analysis on the data set to have an insight
#    Use summaries and appropriate exploratory graphics to answer the
#    question of interest.

# summary stats
pander(summary(Default))

#graphics

ggplot(Default, aes(x=default, y=balance, fill=student)) + geom_boxplot()

x <- qplot(income, balance, data=Default, color=default, shape=student, geom='point')+scale_shape(solid=FALSE)
x
y <- qplot(income, balance, data=Default, color=default, shape=default, geom='point')+scale_shape(solid=FALSE)
y


# b. Use R to build a logistic regression model

head(Default)

# Models by number of independent variables
def_glm3 <- glm(default ~ student + balance + income, data=Default, family = binomial())

pander(summary(def_glm3))

def_interaction_glm <- glm(default ~ student*balance + income, data=Default, family=binomial())

summary(def_interaction_glm)

fitted.results <- predict(def_glm3, type='response')

plot(Default$balance,fitted.results)
plot(Default$income,fitted.results)

1-pchisq(26.9484 - 6.8728,99 - 96)

# c. Discuss your result. Which predictor variables were important.

# The variable 'balance' appears to be the main contributor to defaulting while income does not
# appear to play much of a factor.
# Overall, the larger the 'balance' variable becomes, the more likely the individual will default.


# d. How good is your model?

fitted.results <- predict(def_glm3, type='response')
lev <- c("Yes", "No")
testing <- Default
testing$def_yes <- ifelse(testing$default=="Yes",1,0)
testing$def_no <- ifelse(testing$default=="No",1,0)
tr <- testing$def_yes / (testing$def_yes + testing$def_no)
pred <- factor(ifelse(fitted.results>=0.50,"Yes","No"),levels=lev)
pred


tr1 <- factor(ifelse(tr>=.5,"Yes","No"),levels=lev)
table(pred,True=tr1)
error.rate <- 1 - (9627 + 105) / 10000
error.rate # Low error rate



