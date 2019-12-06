###############getting started#################
rm(list=ls(all=TRUE)) # clears the workspace
graphics.off() # clears graphics

#activate necessary packages 
library(psych) # for describe	
library(tidyverse) # for tidy code	
library(lmtest) #for the homoscedasticity test
library(car) #needed for the for the VIF test

#import dataset
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")
str(data_sample_1) #sex coded: female=2, male= 1 

#delete participants with errors in the dataset (participant 18 on STAI + participant 49 on household income)
data_sample_clean=data_sample_1
data_sample_clean=data_sample_clean[!data_sample_clean$STAI_trait<=20,] #delete participant 18
data_sample_clean=data_sample_clean[!data_sample_clean$household_income<=0,] #delete participant 49

#############make our linear regression model3 (with all predictors)################
#Model 3
model_3 <- lm(pain ~ sex + age + STAI_trait + pain_cat 
              + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_clean)
summary(model_3)

#############check boxplots of outliers for ALL variables in exercise 1##############

#Check for outliers with Cook's distance 
cooksd3 <- cooks.distance(model_3) 
View(cooksd3) 
sample_size <- nrow(data_sample_clean) 
plot(cooksd3, pch="*", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4/sample_size, col="red") 
text(x=1:length(cooksd3)+1, y=cooksd3, labels=ifelse(cooksd3>4/sample_size, names(cooksd3),""), col="red") 

#make a new dataset with excluding the Cook's distance- outlier with the rule: (4/n)
data_sample_1mod3NO=data_sample_clean 
data_sample_1mod3NO=data_sample_1mod3NO[!cooksd3> (4/sample_size),]  
View(data_sample_1mod3NO) 

##############create a linear regression model with dataset without outliers##############
#Model 3 
model_3NO <- lm(pain ~ sex + age + STAI_trait + pain_cat 
                        + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1mod3NO)
summary(model_3NO)

##############Test the assumptions for Model 3#############
#1. Normality of the residuals
#2. Linearity
#3. Homoscedasiticy 
#4. Multicollinearity 

# for dataset with outliers
#1. Normality =met (see below)
residuals(model_3)
shapiro.test(residuals(model_3)) #has to be non-significant (means= there are no differences, we want that)-->it is p=0.81
hist(residuals(model_3)) 

#2. Linearity 
plot(model_3, 2) #check Q-Q-Plot  for linearity 
residualPlots(model_3) #check for Tukey test 

#3. Homoscedasticity 
plot(model_3, 1)
plot(model_3, 3)
bptest(model_3) #Breusch-Pagan test: p-value non-significant (p=0.1) = homoscedasticity is met (there are no differences, however very low! but we use cutoff score of p<.05 so still ok here)

#4. Mulitcollinearity (check with VIF)
vif(model_3) 

# for dataset without outliers 
#1. Normality = met (see below)
residuals(model_3NO)
shapiro.test(residuals(model_3NO)) 
hist(residuals(model_3NO))

#2. Linearity 
plot(model_3NO, 2) #check Q-Q-Plot  for linearity 
residualPlots(model_3NO)

#3. Homoscedasticity 
plot(model_3NO, 1)
plot(model_3NO, 3)
bptest(model_3NO) 

#4. Mullticolinearity (check with VIF)
vif(model_3NO)

#-->assumptions are met in both datasets (outliers/no outliers) (Linearity in NO = better)
#-->but seems as if our assumptions are slightly worse with outliers in the model + significant model coefficients change slightly 

##############Test impact on outliers on our model#############
summary(model_3)
summary(model_3NO)

#-->no clear difference between the two models (still the same significant predictors, p-values vary slightly,R^2 quite similar)
#-->thus decision: we will work with the dataset including the outliers (as they might add interesting information + we do not see any reason to exclude them)

###############Backward Regression###############
backwardmodel <- step(model_3, direction= "backward") #2 possibtilities: run every step seperately or run it all in one (decided for the latter)
summary(backwardmodel) 

#to get APA- style coefficients 
coef_table = function(backwardmodel){
  require(lm.beta)
  mod_sum = summary(backwardmodel)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(backwardmodel), confint(backwardmodel), c(0, lm.beta(backwardmodel)$standardized.coefficients[c(2:length(backwardmodel$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}
View(coef_table)
coef_table(backwardmodel)

###############compare Model 3 and Backward Model ##########################
summary(model_3)
summary(backwardmodel)

anova(backwardmodel, model_3)

AIC(model_3, backwardmodel)

#reminder to myself: the lower the AIC= the better the model 

###############include the final theory-based model (from previous assignment)
model_2FINAL <- lm(pain ~ sex + age + STAI_trait + pain_cat 
                    + mindfulness + cortisol_saliva, data = data_sample_clean)

###############AIC + anova to compare Model 2 FINAL and backward model####################
summary(model_2FINAL)
summary(backwardmodel)

anova(model_2FINAL, backwardmodel) # NOT POSSIBLE, as both models are not nested!!! 

AIC(model_2FINAL, backwardmodel)

################import dataset 2#######################
#import dataset
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
View(data_sample_2)

##############make predictions on our models (use values in data set2 + participant 157-160)#######

#model_2Final (final model)
sex = c("female", "female", "male", "female")
age = c(39, 32, 38, 40)
STAI_trait = c(41, 44, 37, 40)
pain_cat = c(26, 30, 29, 25)
mindfulness = c(3.5, 2.5, 2.0, 3.7)
cortisol_saliva = c(4.7, 6.1, 4.5, 5.6)

data_forprediction2 = as_tibble(sex, age, STAI_trait, pain_cat, mindfulness, cortisol_saliva)
predictions = predict(model_2FINAL, newdata = data_forprediction2)

data_new_incl_predicted2 = cbind(data_forprediction2, predictions)
data_new_incl_predicted2

#use function to predict values with model_2FINAL on datset 2 
Model_2_Predictions <- predict(model_2FINAL, data_sample_2, allow.new.levels = TRUE)
Model_2_Predictions

#backwards model (backwards regression model)
sex = c("female", "female", "male", "female")
age = c(39, 32, 38, 40)
pain_cat = c(26, 30, 29, 25)
mindfulness = c(3.5, 2.5, 2.0, 3.7)
cortisol_serum = c(4.0, 5.5, 4.1, 5.8)
weight = c(68, 53, 64, 59)

data_forprediction3 = as_tibble(sex, age, pain_cat, mindfulness, cortisol_serum, weight)
predictions = predict(backwardmodel, newdata = data_forprediction3)

data_new_incl_predicted3 = cbind(data_forprediction3, predictions)
data_new_incl_predicted3

#use function to predict values with backwardmodel on datset 2
Model_back_Predictions <- predict(backwardmodel, data_sample_2, allow.new.levels = TRUE)
Model_back_Predictions

#MODEL 2-->pain predictions:  value predictions
#157 female         3.9
#158 female         5.5
#159 male           5.1
#160 female         4.3

#BACKWARDS Model-->pain predictions:  value predictions
#157 female         3.8
#158 female         5.6
#159 male           5.1
#160 female         4.5

#-->values from datset
#157 4.1 
#158 6.0
#159 5.5
#160 5.9

#Compare the predicted values with the actual pain ratings. 
#Which model was able to predict the actual pain ratings in data file 2 better?

###########calculate the sum of squared differences between the predicted and the actual pain values#####
#(or the sum of absolute differences) for each model.
RSS2 = sum((data_sample_2$pain - predict(model_2FINAL))^2)
RSS2

RSS3 = sum((data_sample_2$pain - predict(backwardmodel))^2)
RSS3

#RSS= gives an indication of the total amount of error when using the model