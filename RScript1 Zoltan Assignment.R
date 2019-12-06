###############getting started#################
rm(list=ls(all=TRUE)) # clears the workspace
graphics.off() # clears graphics

#activate necessary packages 
library(psych) # for describe	
library(tidyverse) # for tidy code	
library(lmtest) #for the homoscedasticity test
library(car) #needed for the for the VIF test
library(stats)
library(Hmisc) #for significant values in correlation table

#import dataset
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")

##############explore dataset##################
#view our dataset
View(data_sample_1)	#checking for errors in the table, make sure all answers are correct -->participant 18 on STAI + 49= negative household income + 4x minimum IQ of 60= ethical? (intellectual disability) 
summary(data_sample_1) #check for min & max values on variables and check whether it makes sense with the scale (doesnt for participant 18,49,)
str(data_sample_1) #loot at structure of the data (although only 2 levels on gender, not ethically)
describe(data_sample_1) #no missing values (YES!)

#delete participants with errors in the dataset (participant 18 on STAI + participant 49 on household income)
data_sample_clean=data_sample_1
data_sample_clean=data_sample_clean[!data_sample_clean$STAI_trait<=20,] #delete participant 18
data_sample_clean=data_sample_clean[!data_sample_clean$household_income<=0,] #delete participant 49

View(data_sample_clean)#check table if values are excluded
summary(data_sample_clean) #check if min & max values are according to scales now 
describe(data_sample_clean)
str(data_sample_clean)

#plot data: exploring our dataset visually 
data_sample_clean %>%
  ggplot() +	
  aes(x = pain) 
  geom_histogram()	
  
##############make correlation table for important predictors in our models (exclude errors)################
data_matrix <- data.matrix(data_sample_clean)
cor(data_matrix[,c("age", "sex", "STAI_trait", "pain_cat", "mindfulness", "cortisol_saliva", "cortisol_serum", "pain", "weight", "IQ", "household_income")])

mydata.rcorr = rcorr(as.matrix(data_matrix)) #to see p-values for correlations 
mydata.rcorr  

#-->correlation of r=0.87 for cortisol saliva and cortisol serum = one of them redundant? (check later with VIF) 

#############make our linear regression models################
#Model 1 
model_1 <- lm(pain ~ sex + age, data = data_sample_clean)
model_1
#Model 2 
model_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat 
              + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_clean)
model_2

#############check for outliers##############################
#visually: 
boxplot(data_sample_clean$pain, xlab= "Pain")
boxplot(data_sample_clean$age, xlab= "Age")
boxplot(data_sample_clean$STAI_trait, xlab= "State Trait Anxiety Inventory")
boxplot(data_sample_clean$pain_cat, xlab="Pain Catastrophizing")
boxplot(data_sample_clean$cortisol_serum, xlab= "Cortisol Serum")
boxplot(data_sample_clean$cortisol_saliva, xlab= "Salvary Cortisol Level")
boxplot(data_sample_clean$mindfulness, xlab= "Mindful Attention Awareness Scale")
boxplot(data_sample_clean$weight, xlab= "Body Weight")
boxplot(data_sample_clean$IQ, xlab= "Participant IQ")
boxplot(data_sample_clean$household_income, xlab= "Household Income")

###################Model 1#####################################
#check for outliers 
#With Cook's distance (only on second model as this one includes all necessary variables)
cooksd1 <- cooks.distance(model_1) #calculate cooks distance 
View(cooksd1) #see what values we have for Cook's distance
sample_size <- nrow(data_sample_clean) #specify the sample size (needed for the rule of thumb)
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # make a plot of Cook's distance
abline(h = 4/sample_size, col="red")  # add a cutoff score based on the literature which normally is 4/n (sample size)
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4/sample_size, names(cooksd1),""), col="red")  # add labels to the plot 

#make a new dataset with excluding the Cook's distance- outlier with the rule: (4/n)
data_sample_model1NO=data_sample_clean #make a copy of the dataset (without errors)
data_sample_model1NO=data_sample_model1NO[!cooksd1> (4/sample_size),] #exclude the outliers after the 4/n rule 
View(data_sample_model1NO) #check new dataset and see if everything worked 

#create a regression Model 1 without outliers 
model_1NO<- lm(pain ~ sex + age, data = data_sample_model1NO)
model_1NO
model1 #check if model changes significantly with/without outliers

#check assumptions for model 1 with and without outliers 
#WITH outliers 
##############Test the assumptions for model 1###############
# for dataset with outliers

#1. Normality =met (see below)
residuals(model_1)
shapiro.test(residuals(model_1)) 
hist(residuals(model_1)) #look at distribution

#2. Linearity 
plot(model_1, 2) #check Q-Q-Plot  for linearity 
residualPlots(model_1)

#3. Homoscedasticity 
plot(model_1, 1)
plot(model_1, 3)
bptest(model_1) #p-value non-significant= homoscedasticity is met (there are no differences)

#4. Mulitcollinearity (check with VIF)
vif(model_1) #-->larger than 5 = multicollinearity 

#WITHOUT outliers 
#1. Normality 
residuals(model_1NO)
shapiro.test(residuals(model_1NO)) 
hist(residuals(model_1NO)) 

#2. Linearity 
plot(model_1NO, 2) 
residualPlots(model_1NO)

#3. Homoscedasticity 
plot(model_1NO, 1)
plot(model_1NO, 3)
bptest(model_1NO)

#4. Mulitcollinearity (check with VIF)
vif(model_1NO) #-->larger than 5 = multicollinearity 

#Test impact on outliers on our model
summary(model_1)
summary(model_1NO)

#-->decision: go for dataset WITH outliers (as there are no significant changes in the assumption testing ) 


###################Model 2#####################################

#With Cook's distance (only on second model as this one includes all necessary variables)
cooksd2 <- cooks.distance(model_2) #calculate cooks distance 
View(cooksd2) #see what values we have for Cook's distance
sample_size <- nrow(data_sample_clean) #specify the sample size (needed for the rule of thumb)
plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance")  # make a plot of Cook's distance
abline(h = 4/sample_size, col="red")  # add a cutoff score based on the literature which normally is 4/n (sample size)
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>4/sample_size, names(cooksd2),""), col="red")  # add labels to the plot 

#make a new dataset with excluding the Cook's distance- outlier with the rule: (4/n)
data_sample_model2NO=data_sample_clean #make a copy of the dataset (without errors)
data_sample_model2NO=data_sample_model2NO[!cooksd2> (4/sample_size),] #exclude the outliers after the 4/n rule 
View(data_sample_model2NO) #check new dataset and see if everything worked 

##############create a linear regression model2 with dataset without outliers##############
#Model 2 
model_2NO <- lm(pain ~ sex + age + STAI_trait + pain_cat 
              + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_model2NO)
model_2NO

##############Test the assumptions for the model 2 ("final model")#############
#1. Normality of the residuals
#2. Linearity
#3. Homoscedasiticy 
#4. Multicollinearity 

# for dataset with outliers
#1. Normality =met (see below)
residuals(model_2)
shapiro.test(residuals(model_2)) #has to be non-significant (means= there are no differences, we want that)-->it is p=0.81
hist(residuals(model_2)) 

#2. Linearity 
plot(model_2, 2) 
residualPlots(model_2)

#3. Homoscedasticity 
plot(model_2, 1)
plot(model_2, 3)
bptest(model_2) #p-value non-significant (p=0.33) = homoscedasticity is met (there are no differences)

#4. Mulitcollinearity (check with VIF)
vif(model_2) # using the cutoff score of VIF<5 : no multicollinearty in our datset (BUT cortisol_saliva over 5= quite high + cortisol_serum= because similar concept?)


# for dataset without outliers 
#1. Normality = met (see below)
residuals(model_2NO)
shapiro.test(residuals(model_2NO)) #non-significant so assumption is met p=0.68
hist(residuals(model_2NO))

#2.for linearity 
plot(model_2NO, 2) #check Q-Q-Plot  
residualPlots(model_2NO) #check Tukey test

#3. Homoscedasticity 
plot(model_2NO, 1)
plot(model_2NO, 3)
bptest(model_2NO) # non-significant value (p=0.8043), higher p value than for the one with outliers (makes sense as variance descreases as we remove the outliers)

#4. Mullticolinearity (check with VIF)
vif(model_2NO)

#-->assumptions are met in both datasets (outliers/no outliers)

##############Test impact on outliers on our model 2#############
summary(model_2)
summary(model_2NO)

#-->no clear difference between the two models (still the same significant predictors, p-values vary slightly)
#-->thus decision: I will work with the dataset including the outliers (as they might add interesting information + we do not see any reason to exclude them)


#############see which predictor (saliva/serum) is better#################
#model with Cortisol_saliva
model_2saliva <- lm(pain ~ sex + age + STAI_trait + pain_cat 
              + mindfulness + cortisol_saliva, data = data_sample_clean)
summary(model_2saliva)

#model with Cortisol_serum 
model_2serum <- lm(pain ~ sex + age + STAI_trait + pain_cat 
              + mindfulness + cortisol_serum, data = data_sample_clean)
summary(model_2serum)

#-->which of the two models is able to explain more: saliva actually explains more (higher R^2 for the model), thus we will exclude serum as a predictor in our final model
model_2FINAL <- model_2saliva

##############check assumptions of FINAL model#############
#With Cook's distance (only on second model as this one includes all necessary variables)
cooksd2 <- cooks.distance(model_2FINAL) 
View(cooksd2) 
sample_size <- nrow(data_sample_clean) 

plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4/sample_size, col="red") 
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>4/sample_size, names(cooksd2),""), col="red") 

#make a new dataset with excluding the Cook's distance- outlier with the rule: (4/n)
data_sample_model2FINALNO=data_sample_clean 
data_sample_model2FINALNO=data_sample_model2NO[!cooksd2> (4/sample_size),] #exclude the outliers after the 4/n rule 
View(data_sample_model2FINALNO) #check new dataset and see if everything worked 

# for dataset including outliers AND saliva (NOT SERUM)
#1. Normality =met (see below)
residuals(model_2FINAL)
shapiro.test(residuals(model_2FINAL)) 
hist(residuals(model_2FINAL)) 

#2. Linearity 
plot(model_2FINAL, 2)
residualPlots(model_2FINAL)

#3. Homoscedasticity 
plot(model_2FINAL, 1)
plot(model_2FINAL, 3)
bptest(model_2FINAL) #p-value non-significant= homoscedasticity is met (there are no differences)

#4. Mulitcollinearity (check with VIF)
vif(model_2FINAL) 

##############get R^2, F, df & pvalue of my two models########
summary(model_1)
summary(model_2FINAL)

##############compare model 1 and model 2(final)################
anova(model_1, model_2FINAL) #compare the two models , output: models differ significantly from each other (see really low p-value), look at mean difference to find which group is actually better

#Aikake 
AIC(model_1, model_2FINAL)#see how well you can generalise to the population -->lower score= better model
#95% Confidence Interval 
confint(model_1)
confint(model_2FINAL)

############APA style table################
# load custom functions for model 1
coef_table = function(model_1){
  require(lm.beta)
  mod_sum = summary(model_1)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model_1), confint(model_1), c(0, lm.beta(model_1)$standardized.coefficients[c(2:length(model_1$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}
View(coef_table)
coef_table(model_1)

# load custom functions for model 2
coef_table = function(model_2FINAL){
  require(lm.beta)
  mod_sum = summary(model_2FINAL)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model_2FINAL), confint(model_2FINAL), c(0, lm.beta(model_2FINAL)$standardized.coefficients[c(2:length(model_2FINAL$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}
View(coef_table)
coef_table(model_2FINAL)

