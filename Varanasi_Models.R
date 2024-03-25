#MULTIVARIABLE LOGISTIC REGRESSION

#Model 1a: hcv, with all variables
model_hcv1 <- glm(hcv ~ age + relationship + education + earnings + incarceration + homelessness + shared_needle + wsexnum + hiv + at_risk_alcohol_use + depressed + transactional_sex + condom_use, data=data_filtered2, family = "binomial")
summary(model_hcv1)
##exponentiate coefficients that are logs to get the odds ratios. Higher than 1
exp(coef(model_hcv1))

#Model 1b: hiv, with all variables
model_hiv1 <- glm(hiv ~ age + relationship + education + earnings + incarceration + homelessness + shared_needle + wsexnum + hcv + at_risk_alcohol_use + depressed + transactional_sex + condom_use, data=data_filtered2, family = "binomial")
summary(model_hiv1)
##exponentiate coefficients that are logs to get the odds ratios. Higher than 1
exp(coef(model_hiv1))


#Model 2a: hcv, without depression, alcohol, transactional sex, condom use
model_hcv2 <- glm(hcv ~ age + relationship + education + earnings + incarceration + homelessness + shared_needle + wsexnum + hiv, data=data_filtered2, family = "binomial")
summary(model_hcv2)
##exponentiate coefficients that are logs to get the odds ratios. Higher than 1
exp(coef(model_hcv2))

#Model 2b: hiv, without depression and alcohol
model_hiv2 <- glm(hiv ~ age + relationship + education + earnings + incarceration + homelessness + shared_needle + wsexnum + hcv, data=data_filtered2, family = "binomial")
summary(model_hiv2)
##exponentiate coefficients that are logs to get the odds ratios. Higher than 1
exp(coef(model_hiv2))


#ASSESS PREDICTIVE PERFORMANCE
AIC(model_hcv1, model_hiv1)




