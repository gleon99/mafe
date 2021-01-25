# TODO: Clean
require(car)
require(nlme)
require(sandwich)
require(lmtest)
require(lfe)
require(stargazer)
require(aod)

# 1.1
JTRAIN2 = read.table("JTRAIN2.txt", header=FALSE, as.is=FALSE)
colnames(JTRAIN2) = c("TRAIN", "AGE", "EDUC", "BLACK", "HISP",
                     "MARRIED", "NODEGREE", "MOSINEX", "RE74",
                     "RE75", "RE78", "UNEM74", "UNEM75", "UNEM78",
                     "LRE74", "LRE75", "LRE78", "AGESQ", "MOSTRN")
JTRAIN2_New = JTRAIN2[c("UNEM78", "TRAIN", "AGE", "EDUC", "BLACK",
                      "HISP", "MARRIED", "UNEM74", "UNEM75")]

# 1.2
print(nrow(JTRAIN2_New[JTRAIN2_New$TRAIN == 1,]))

# 1.3
print(max(JTRAIN2$MOSTRN))
                     
# 1.4
fit1_lpm = lm("UNEM78 ~ TRAIN + AGE + EDUC + BLACK + HISP + MARRIED + UNEM74 + UNEM75",
              data=JTRAIN2_New)
print(summary(fit1_lpm))

fit1_logit = glm("UNEM78 ~ TRAIN + AGE + EDUC + BLACK + HISP + MARRIED + UNEM74 + UNEM75",
                 data=JTRAIN2_New, family=binomial(link="logit"))
print(summary(fit1_logit))

fit1_probit = glm("UNEM78 ~ TRAIN + AGE + EDUC + BLACK + HISP + MARRIED + UNEM74 + UNEM75",
                  data=JTRAIN2_New, family=binomial(link="probit"))
print(summary(fit1_probit))

# 1.5.1
means_TRAIN_0 = data.frame(TRAIN=0, AGE=mean(JTRAIN2_New$AGE), EDUC=mean(JTRAIN2_New$EDUC),
                           BLACK=mean(JTRAIN2_New$BLACK), HISP=mean(JTRAIN2_New$HISP),
                           MARRIED=mean(JTRAIN2_New$MARRIED), UNEM74=mean(JTRAIN2_New$UNEM74),
                           UNEM75=mean(JTRAIN2_New$UNEM75))
means_TRAIN_1 = data.frame(TRAIN=1, AGE=mean(JTRAIN2_New$AGE), EDUC=mean(JTRAIN2_New$EDUC),
                           BLACK=mean(JTRAIN2_New$BLACK), HISP=mean(JTRAIN2_New$HISP),
                           MARRIED=mean(JTRAIN2_New$MARRIED), UNEM74=mean(JTRAIN2_New$UNEM74),
                           UNEM75=mean(JTRAIN2_New$UNEM75))
CDF_Logit_TRAIN_0 = plogis(predict(fit1_logit, means_TRAIN_0), location=0, scale=1)
CDF_Logit_TRAIN_1 = plogis(predict(fit1_logit, means_TRAIN_1), location=0, scale=1)
print(CDF_Logit_TRAIN_1 - CDF_Logit_TRAIN_0)

CDF_Probit_TRAIN_0 = plogis(predict(fit1_probit, means_TRAIN_0), location=0, scale=1)
CDF_Probit_TRAIN_1 = plogis(predict(fit1_probit, means_TRAIN_1), location=0, scale=1)
print(CDF_Probit_TRAIN_1 - CDF_Probit_TRAIN_0)

JTRAIN2_TRAIN_0 = data.frame(TRAIN=0, AGE=JTRAIN2_New$AGE, EDUC=JTRAIN2_New$EDUC,
                             BLACK=JTRAIN2_New$BLACK, HISP=JTRAIN2_New$HISP,
                             MARRIED=JTRAIN2_New$MARRIED, UNEM74=JTRAIN2_New$UNEM74,
                             UNEM75=JTRAIN2_New$UNEM75)
JTRAIN2_TRAIN_1 = data.frame(TRAIN=1, AGE=JTRAIN2_New$AGE, EDUC=JTRAIN2_New$EDUC,
                             BLACK=JTRAIN2_New$BLACK, HISP=JTRAIN2_New$HISP,
                             MARRIED=JTRAIN2_New$MARRIED, UNEM74=JTRAIN2_New$UNEM74,
                             UNEM75=JTRAIN2_New$UNEM75)
Mean_CDF_Logit_TRAIN_0 = mean(plogis(predict(fit1_logit, JTRAIN2_TRAIN_0), location=0, scale=1))
Mean_CDF_Logit_TRAIN_1 = mean(plogis(predict(fit1_logit, JTRAIN2_TRAIN_1), location=0, scale=1))
print(Mean_CDF_Logit_TRAIN_1 - Mean_CDF_Logit_TRAIN_0)
Mean_CDF_Probit_TRAIN_0 = mean(plogis(predict(fit1_probit, JTRAIN2_TRAIN_0), location=0, scale=1))
Mean_CDF_Probit_TRAIN_1 = mean(plogis(predict(fit1_probit, JTRAIN2_TRAIN_1), location=0, scale=1))
print(Mean_CDF_Probit_TRAIN_1 - Mean_CDF_Probit_TRAIN_0)

# 1.5.2
means = data.frame(TRAIN=mean(JTRAIN2_New$TRAIN), AGE=mean(JTRAIN2_New$AGE), EDUC=mean(JTRAIN2_New$EDUC),
                   BLACK=mean(JTRAIN2_New$BLACK), HISP=mean(JTRAIN2_New$HISP), MARRIED=mean(JTRAIN2_New$MARRIED),
                   UNEM74=mean(JTRAIN2_New$UNEM74), UNEM75=mean(JTRAIN2_New$UNEM75))
scale_factor_logit = dlogis(predict(fit1_logit, means), location=0, scale=1)
print(scale_factor_logit)
scale_factor_probit = dlogis(predict(fit1_probit, means), location=0, scale=1)
print(scale_factor_probit)

scale_factor_logit = mean(dlogis(predict(fit1_logit), location=0, scale=1))
print(scale_factor_logit)
scale_factor_probit = mean(dlogis(predict(fit1_probit), location=0, scale=1))
print(scale_factor_probit)

# 1.6
print(min(fit1_logit$fitted.values))
print(quantile(fit1_logit$fitted.values, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))
print(max(fit1_logit$fitted.values))

print(min(fit1_probit$fitted.values))
print(quantile(fit1_probit$fitted.values, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))
print(max(fit1_probit$fitted.values))

print(min(fit1_lpm$fitted.values))
print(quantile(fit1_lpm$fitted.values, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)))
print(max(fit1_lpm$fitted.values))

# 1.7
sample = data.frame(TRAIN=1, EDUC=16, AGE=52, MARRIED=1, BLACK=0, HISP=0, UNEM74=1, UNEM75=1)
print(predict(fit1_lpm, sample, type="response"))
print(predict(fit1_logit, sample, type="response"))
print(predict(fit1_probit, sample, type="response"))

# 1.8
print(table(fit1_lpm$fitted.values > 0.5, JTRAIN2_New$UNEM78))
print(table(fit1_logit$fitted.values > 0.5, JTRAIN2_New$UNEM78))
print(table(fit1_probit$fitted.values > 0.5, JTRAIN2_New$UNEM78))

fit1_lpm_null = lm("UNEM78 ~ 1", data=JTRAIN2_New)
fit1_lpm_null_deviance = -2*logLik(fit1_lpm_null)
fit1_lpm_deviance = -2*logLik(fit1_lpm)
print(1 - fit1_lpm_deviance / fit1_lpm_null_deviance)
print(1 - fit1_logit$deviance / fit1_logit$null.deviance)
print(1 - fit1_probit$deviance / fit1_probit$null.deviance)

# 1.10
fit1_logit_r = glm("UNEM78 ~ TRAIN + AGE + EDUC + UNEM74 + UNEM75", data=JTRAIN2_New, family=binomial(link="logit"))
print(lrtest(fit1_logit, fit1_logit_r))
print(wald.test(b=fit1_logit$coefficients, Sigma=vcov(fit1_logit), Terms=c(5, 6, 7)))

fit1_probit_r = glm("UNEM78 ~ TRAIN + AGE + EDUC + UNEM74 + UNEM75", data=JTRAIN2_New, family=binomial(link="probit"))
print(lrtest(fit1_probit, fit1_probit_r))
print(wald.test(b=fit1_probit$coefficients, Sigma=vcov(fit1_probit), Terms=c(5, 6, 7)))

# 1.11
thres = mean(JTRAIN2_New$UNEM78)

fit1_lpm_goodness = table(fit1_lpm$fitted.values > thres, JTRAIN2_New$UNEM78)
print(fit1_lpm_goodness)
print(fit1_lpm_goodness[2,2] / (fit1_lpm_goodness[1,2] + fit1_lpm_goodness[2,2]))
print(fit1_lpm_goodness[1,1] / (fit1_lpm_goodness[2,1] + fit1_lpm_goodness[1,1]))
print((fit1_lpm_goodness[2,2] + fit1_lpm_goodness[1,1]) / (fit1_lpm_goodness[1,1] +
       fit1_lpm_goodness[1,2] + fit1_lpm_goodness[2,1] + fit1_lpm_goodness[2,2]))

fit1_logit_goodness = table(fit1_logit$fitted.values > thres, JTRAIN2_New$UNEM78)
print(fit1_logit_goodness)
print(fit1_logit_goodness[2,2] / (fit1_logit_goodness[1,2] + fit1_logit_goodness[2,2]))
print(fit1_logit_goodness[1,1] / (fit1_logit_goodness[2,1] + fit1_logit_goodness[1,1]))
print((fit1_logit_goodness[2,2] + fit1_logit_goodness[1,1]) / (fit1_logit_goodness[1,1] +
       fit1_logit_goodness[1,2] + fit1_logit_goodness[2,1] + fit1_logit_goodness[2,2]))

fit1_probit_goodness = table(fit1_probit$fitted.values > thres, JTRAIN2_New$UNEM78)
print(fit1_probit_goodness)
print(fit1_probit_goodness[2,2] / (fit1_probit_goodness[1,2] + fit1_probit_goodness[2,2]))
print(fit1_probit_goodness[1,1] / (fit1_probit_goodness[2,1] + fit1_probit_goodness[1,1]))
print((fit1_probit_goodness[2,2] + fit1_probit_goodness[1,1]) / (fit1_probit_goodness[1,1] +
       fit1_probit_goodness[1,2] + fit1_probit_goodness[2,1] + fit1_probit_goodness[2,2]))