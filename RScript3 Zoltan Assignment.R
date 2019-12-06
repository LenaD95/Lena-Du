########Linear Mixed Effects Models- Assignment 3##########

#load important packages 
library(psych) # for describe\t
library(tidyverse) # for tidy code and ggplot\t
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath

#import dataset
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")
View(data_sample_3)

#explore the datatset 3 
describe(data_sample_3)
summary(data_sample_3)

#dataset 3: change hospital to numerical value + female= double (as female & Female)
sample3_cleaned1 <- data_sample_3 %>% 	#change Female to female 
  mutate(	
    sex = droplevels(replace(sex, sex == "Female", "female")),	
    sex = droplevels(replace(sex, sex == "Male", "male"))	
  ) 	
str(sample3_cleaned1)
summary(sample3_cleaned1)

sample3_clean <- sample3_cleaned1 %>% 	
  mutate(hospital = recode(hospital,	
                       "hospital_1" = 1,	
                       "hospital_2" = 2,	
                       "hospital_3" = 3,	
                       "hospital_4" = 4,	
                       "hospital_5" = 5,	
                       "hospital_6" = 6,
                       "hospital_7" = 7,
                       "hospital_8" = 8,
                       "hospital_9" = 9,
                       "hospital_10" =10,
  ))	
View(sample3_clean)


#build our model with linear mixed effects (accounting for clustering of the different hospital sides)#
mod_hospital_int <- lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness
                    + cortisol_saliva + (1|hospital), data = sample3_clean)	
summary(mod_hospital_int) #for coefficients (b), and p-values 

#CI 
confint(mod_hospital_int)

#standardised b 
#function to get standardised beta coefficients 
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}
stdCoef.merMod(mod_hospital_int) #to get the standardised coefficients: paincat & cortisol have a very similar value, however: cortisol= slightly better 

#residuals 
sample3_clean = sample3_clean %>% mutate(resid = residuals(mod_hospital_int))

##############################assumption checking###############################

#outliers in dataset 3
influence_observation = influence(mod_hospital_int, obs = T)$alt.fixed 
influence_group = influence(mod_hospital_int, group = "hospital")$alt.fixed

#plot the outliers 
windows()
data_plot_influence = as_tibble(influence_group) %>% gather(colnames(influence_group),
value = coefficient, key = predictor)
data_plot_influence %>% ggplot() + aes(x = 1, y = coefficient,
group = predictor) + geom_violin() + facet_wrap(~predictor,
scales = "free")

#Normality: QQplot 
#of fixed effects 
windows()
qqmath(mod_hospital_int, id = 0.05)

#of random effects (intercept)
windows()
qqmath(ranef(mod_hospital_int))

#Linearity 
windows()
plot(mod_hospital_int, arg = "pearson")

#look at the scatterplot of the residuals and the fixed predictors separately.
windows()
sample3_clean %>% ggplot() + aes(x = hospital, y = resid) +
  geom_point()

windows()
sample3_clean %>% ggplot() + aes(x = age, y = resid) +
  geom_point()

windows()
sample3_clean %>% ggplot() + aes(x = sex, y = resid) +
  geom_point()

windows()
sample3_clean %>% ggplot() + aes(x = STAI_trait, y = resid) +
  geom_point()

windows()
sample3_clean %>% ggplot() + aes(x = mindfulness, y = resid) +
  geom_point()

windows()
sample3_clean %>% ggplot() + aes(x = cortisol_saliva, y = resid) +
  geom_point()

windows()
sample3_clean %>% ggplot() + aes(x = pain_cat, y = resid) +
  geom_point()

#Homoscedasticity 
windows()
plot(mod_hospital_int, arg = "pearson")
homosced_mod = lm(resid^2 ~ hospital, data = sample3_clean)
summary(homosced_mod)

# caluclate interquartile range within each cluster
IQR_of_residuals_by_hospital = sapply(split(sample3_clean,
f = sample3_clean$hospital), function(x) IQR(x$resid))
# rank ordering them
rank = rank(IQR_of_residuals_by_hospital)
# adding rank to the dataframe containing the residuals
sample3_clean$rank = rep(rank, each = length(c("hospital_1", "hospital_2",
 "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10")))
# creating a vector of participant IDs ordered based on the
# rank, this will be used as labels
Hforplot = unique(sample3_clean$hospital[order(sample3_clean$rank)])
# create the plot
windows()
ggplot(sample3_clean, aes(y = resid, x = factor(rank), labels = hospital)) +
  geom_boxplot() + scale_x_discrete(labels = Hforplot) + coord_flip()


#No Multicollinearity  
windows()
pairs.panels(sample3_clean[, c("sex", "age","mindfulness", "STAI_trait", "cortisol_saliva",
                                 "pain_cat")], col = "red", lm = T)


#################model coefficients and the confidence intervals of the coefficients for all 
#fixed effect predictors, and compare them to the ones obtained in assignment 1.################
summary(mod_hospital_int)
confint(mod_hospital_int)

#compute the variance explained by the fixed effect predictors using marginal R^2
#the variance explained by the fixed and random effect terms combined using conditional R^2
# Marginal R squared	
r2beta(mod_hospital_int, method = "nsj", data = sample3_clean)# for variance explained by the fixed effects (for residuals & random effect see the summary(MODEL) function)
summary(mod_hospital_int)

############################calculate variance components ###################
VarCorr(mod_hospital_int) 
Model_noLMEM <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness
                    + cortisol_saliva, data = sample3_clean)
summary(Model_noLMEM)

#calculate variance components, SOURCE: https://sites.google.com/site/alexandrecourtiol/what-did-i-learn-today/inrhowtoextractthedifferentcomponentsofvarianceinalinearmixedmodel
Var_Random_effect <- as.numeric(VarCorr(mod_hospital_int))
Var_Residual <- attr(VarCorr(mod_hospital_int), "sc")^2
Var_Fixed <- var(predict(Model_noLMEM)) #compute variance of only the fixed effects by making a model without random effect (correct?)

var(sample3_clean$pain)
Var_Random_effect + Var_Residual + Var_Fixed

# marginal and conditional R squared values	
r.squaredGLMM(mod_hospital_int)	

#predict pain levels on dataset 4 with our LMEM
data_sample_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")
View(data_sample_4)
summary(data_sample_4)
str(data_sample_4)

#change the hospital_X into numerical 
sample4_clean <- data_sample_4 %>% 	
  mutate(hospital = recode(hospital,	
                           "hospital_11" = 11,	
                           "hospital_12" = 12,	
                           "hospital_13" = 13,	
                           "hospital_14" = 14,	
                           "hospital_15" = 15,	
                           "hospital_16" = 16,
                           "hospital_17" = 17,
                           "hospital_18" = 18,
                           "hospital_19" = 19,
                           "hospital_20" = 20,
  ))	
View(sample4_clean)

#make prediction with our mixed model on dataset 4 
Predict_mixedmod<- predict(mod_hospital_int, sample4_clean, allow.new.levels = TRUE)
Predict_mixedmod

# Compute the variance explained by the model on data file 4
#make a Mean model 
BasicMod <- lmer(pain ~ 1 + (1|hospital), data = sample4_clean, REML = FALSE)

RSS = sum((sample4_clean$pain - predict(mod_hospital_int, sample4_clean, allow.new.levels = TRUE))^2)	
SST = sum((sample4_clean$pain - predict(BasicMod, sample4_clean, allow.new.levels = TRUE ))^2)

#compute the variance explained by the model on data file 4
1-(RSS/SST)

#see standardised beta to be able to compare the coefficients and see which one is the best predictor 
#AGAIN (see above)function to get standardised beta coefficients 
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}
stdCoef.merMod(mod_hospital_int) #to get the standardised coefficients: paincat & cortisol have a very similar value, however: cortisol= slightly better 

#Allow for both random intercept and random slope for the model with the best predictor 
mod_cortisol_slope <- lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital), data = sample3_clean)	
summary(mod_cortisol_slope) #random slope model allows for both the random intercept and the random slope (exercise 17, page 8)

#visualize the fitted regression lines for each hospital separately (intercept + slope)
sample3_clean_slope = sample3_clean %>% mutate(pred_slope = predict(mod_cortisol_slope))

windows()
sample3_clean_slope %>% ggplot() + aes(y = pain, x = cortisol_saliva,
                                    group = hospital) + geom_point(aes(color = hospital), size = 4) +
  geom_line(color = "red", aes(y = pred_slope, x = cortisol_saliva)) +
  facet_wrap(~hospital, ncol = 2)