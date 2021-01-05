require(car)
require(nlme)
require(sandwich)
require(lmtest)
require(lfe)

# 1.1
Signals = read.csv(file="Signals_A_62016.csv", header=TRUE, as.is=TRUE, na.strings=c("NA"))
Signals$FDate = as.Date(Signals$FDate, format="%d/%m/%Y")
Signals$PFormDate = as.Date(Signals$PFormDate, format="%d/%m/%Y")
Signals_1992 = Signals[Signals$PFormDate == "1992/06/30", ]

fit1 = lm("AvgMonRet ~ log(Size) + log(BTM) + ROE + NSI + NXF", data=Signals_1992)
print(summary(fit1))

# 1.2
fit1_residuals = fit1$residuals
fit1_fitted = fit1$fitted.values
fit1_fitted_squared = fit1_fitted ^ 2
fit1_white_test_data = data.frame(fit1_residuals, fit1_fitted, fit1_fitted_squared)
fit1_white_test = lm("fit1_residuals ~ fit1_fitted + fit1_fitted_squared", data=fit1_white_test_data)
print(linearHypothesis(fit1_white_test, c("fit1_fitted=0", "fit1_fitted_squared=0"), test="F"))

# 1.3.1
print(confint(fit1, c("log(Size)", "log(BTM)", "ROE", "NSI", "NXF"), level=0.95, alternative="two.sided"))

# 1.3.2
Size = quantile(Signals_1992$Size, prob=0.2, na.rm=T)
NSI = quantile(Signals_1992$NSI, prob=0.2, na.rm=T)
NXF = quantile(Signals_1992$NXF, prob=0.2, na.rm=T)
BTM = quantile(Signals_1992$BTM, prob=0.8, na.rm=T)
ROE = quantile(Signals_1992$ROE, prob=0.5, na.rm=T)
data = data.frame(Size, BTM, ROE, NSI, NXF)
print(predict(fit1, data))

Signals_1992$Size_new = Signals_1992$Size - Size
Signals_1992$NSI_new = Signals_1992$NSI - NSI
Signals_1992$NXF_new = Signals_1992$NXF - NXF
Signals_1992$BTM_new = Signals_1992$BTM - BTM
Signals_1992$ROE_new = Signals_1992$ROE - ROE
fit1_new = lm("AvgMonRet ~ log(Size_new) + log(BTM_new) + ROE_new + NSI_new + NXF_new", data=Signals_1992)
print(confint(fit1_new, "(Intercept)", level=0.95, alternative="two.sided"))

se_spc = sqrt(vcov(fit1_new)[1,1] + summary(fit1)$sigma ^ 2)
print(fit1_new$coefficients["(Intercept)"] - qt(p=0.975, df=1875) * se_spc)
print(fit1_new$coefficients["(Intercept)"] - qt(p=0.025, df=1875) * se_spc)

# 1.3.3
print(linearHypothesis(fit1, c("ROE=0", "NSI=0", "NXF=0"), test="F"))
print(linearHypothesis(fit1, c("ROE=0", "NSI=0", "NXF=0"), test="Chisq"))

# 1.3.4
print(linearHypothesis(fit1, c("NSI=3*log(Size)"), test="F"))
print(linearHypothesis(fit1, c("NSI=3*log(Size)"), test="Chisq"))

# 1.3.5
print(linearHypothesis(fit1, c("NSI=3*log(Size)", "ROE=0", "NXF=0"), test="F"))
print(linearHypothesis(fit1, c("NSI=3*log(Size)", "ROE=0", "NXF=0"), test="Chisq"))

# 1.4
print(bptest(fit1, studentize=TRUE))

# 1.5.1
fit1_vcov = vcovHC(fit1, type="HC0")
print(coefci(fit1, c("log(Size)", "log(BTM)", "ROE", "NSI", "NXF"), level=0.95, vcov.=fit1_vcov))

# 1.5.2
fit1_new = lm("AvgMonRet ~ log(Size_new) + log(BTM_new) + ROE_new + NSI_new + NXF_new", data=Signals_1992)
fit1_new_vcov = vcovHC(fit1_new, type="HC0")
print(coefci(fit1_new, "(Intercept)", level=0.95, vcov.=fit1_new_vcov))

se_spc = sqrt(fit1_new_vcov[1,1] + summary(fit1)$sigma ^ 2)
print(fit1_new$coefficients["(Intercept)"] - qt(p=0.975, df=1875) * se_spc)
print(fit1_new$coefficients["(Intercept)"] - qt(p=0.025, df=1875) * se_spc)

# 1.5.3
print(linearHypothesis(fit1, c("ROE=0", "NSI=0", "NXF=0"), test="F", vcov.=fit1_vcov))

# 1.5.4
print(linearHypothesis(fit1, c("NSI=3*log(Size)"), test="F", vcov.=fit1_vcov))

# 1.5.5
print(linearHypothesis(fit1, c("NSI=3*log(Size)", "ROE=0", "NXF=0"), test="F", vcov.=fit1_vcov))

# 1.6.1
fit2 = felm(AvgMonRet ~ log(Size) + log(BTM) + ROE + NSI + NXF | 0 | 0 | BBHL, data=Signals_1992)
print(summary(fit2))

# 1.6.2
print(linearHypothesis(fit2, c("ROE=0", "NSI=0", "NXF=0"), vcov.=fit2$robustvcv, test="F"))

# 2.1
Signals = read.csv(file="Signals_A_62016.csv", header=TRUE, as.is=TRUE, na.strings=c("NA"))
Signals$FDate = as.Date(Signals$FDate, format="%d/%m/%Y")
Signals$PFormDate = as.Date(Signals$PFormDate, format="%d/%m/%Y")
fit3 = lm("AvgMonRet ~ log(Size) + log(BTM) + ROE + NSI + NXF", data=Signals)
print(summary(fit3))

# 2.3.1
fit4 = felm(AvgMonRet ~ log(Size) + log(BTM) + ROE + NSI + NXF | 0 | 0 | PermNo, data=Signals)
fit4_vcov = fit4$clustervcv
print(coeftest(fit4, vcov.=fit4_vcov))

# 2.3.2
fit5 = felm(AvgMonRet ~ log(Size) + log(BTM) + ROE + NSI + NXF | 0 | 0 | PFormDate, data=Signals)
fit5_vcov = fit5$clustervcv
print(coeftest(fit5, vcov.=fit5_vcov))

# 2.3.3
fit6 = felm(AvgMonRet ~ log(Size) + log(BTM) + ROE + NSI + NXF | 0 | 0 | PFormDate + PermNo, data=Signals)
fit6_vcov = fit6$clustervcv
print(coeftest(fit6, vcov.=fit6_vcov))

# 2.4
fit7 = felm(AvgMonRet ~ log(Size) + log(BTM) + ROE + NSI + NXF | PFormDate + PermNo | 0 | PFormDate + PermNo, data=Signals)
fit7_vcov = fit7$clustervcv
print(coeftest(fit7, vcov.=fit7_vcov))
