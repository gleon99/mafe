# 1.1

require(Hmisc)
require(stargazer)
require(car)
require(sandwich)
require(lmtest)

FERTIL = read.table(file="FERTIL.txt",  header=FALSE, sep="", as.is=TRUE, na.strings=c("NA", ".", ""))

colnames(FERTIL) = c("GFR", "PE", "YEAR")

FERTIL = FERTIL[order(FERTIL$YEAR), ]

FERTIL$PE_LAG1 = Lag(FERTIL$PE, 1)

FERTIL$PE_LAG2 = Lag(FERTIL$PE, 2)

# 1.2

fit1 = lm(GFR ~ PE + PE_LAG1 + PE_LAG2, data=FERTIL)

stargazer(fit1, type="html", out="hw2q1p2.html")

summary(fit1)

# 1.2.1

linearHypothesis(fit1, c("PE=0", "PE_LAG1=0", "PE_LAG2=0"), test="F")

# 1.2.3

MPL = fit1$coefficients["PE"] + fit1$coefficients["PE_LAG1"] + fit1$coefficients["PE_LAG2"]

print(MPL)

# 1.3

FERTIL$X1 = FERTIL$PE_LAG1 - FERTIL$PE

FERTIL$X2 = FERTIL$PE_LAG2 - FERTIL$PE

fit2 = lm(GFR ~ PE + X1 + X2, data=FERTIL)

print(confint(fit2, "PE", level=0.95, alternative="two.sided"))

# 1.4.1

FERTIL$WW2 = ifelse(FERTIL$YEAR>=1941 & FERTIL$YEAR<=1945, 1, 0)

FERTIL$PILL = ifelse(FERTIL$YEAR>=1963, 1, 0)

FERTIL$t = 1:length(FERTIL$YEAR)

FERTIL$t_2 = FERTIL$t * FERTIL$t

fit3 = lm(GFR ~ PE + WW2 + PILL, data=FERTIL)

fit4 = lm(GFR ~ PE + WW2 + PILL + t, data=FERTIL)

fit5 = lm(GFR ~ PE + WW2 + PILL + t + t_2, data=FERTIL)

stargazer(fit3, fit4, fit5, type="html", out="hw2q1p4.html")

summary(fit3)

summary(fit4)

summary(fit5)

# 1.4.3

fit6 = lm(GFR ~ t + t_2 , data=FERTIL)

GFR_resid = fit6$residuals

fit7 = lm(GFR_resid ~ PE + WW2 + PILL + t + t_2, data=FERTIL)

print(summary(fit7)$adj.r.squared)

# 2.1

TRAFFIC2_R = read.csv(file="TRAFFIC2_R.csv", header = FALSE, sep=",", as.is =TRUE, na.strings = c("NA", "", "."))

colnames(TRAFFIC2_R) = c("YEAR", "MONTH", "TOTACC", "FATACC", "INJACC", "PDOACC", "UNEM", "SPDLAW", "BELTLAW", "WKENDS")

TRAFFIC2_R = TRAFFIC2_R[order(TRAFFIC2_R$YEAR, TRAFFIC2_R$MONTH), ]

TRAFFIC2_R[TRAFFIC2_R$BELTLAW == 1,][1,c("YEAR", "MONTH")]

TRAFFIC2_R[TRAFFIC2_R$SPDLAW == 1,][1,c("YEAR", "MONTH")]

# 2.2

TRAFFIC2_R$FEB = ifelse(TRAFFIC2_R$MONTH == 2, 1, 0)

TRAFFIC2_R$MAR = ifelse(TRAFFIC2_R$MONTH == 3, 1, 0)

TRAFFIC2_R$APR = ifelse(TRAFFIC2_R$MONTH == 4, 1, 0)

TRAFFIC2_R$MAY = ifelse(TRAFFIC2_R$MONTH == 5, 1, 0)

TRAFFIC2_R$JUN = ifelse(TRAFFIC2_R$MONTH == 6, 1, 0)

TRAFFIC2_R$JUL = ifelse(TRAFFIC2_R$MONTH == 7, 1, 0)

TRAFFIC2_R$AUG = ifelse(TRAFFIC2_R$MONTH == 8, 1, 0)

TRAFFIC2_R$SEP = ifelse(TRAFFIC2_R$MONTH == 9, 1, 0)

TRAFFIC2_R$OCT = ifelse(TRAFFIC2_R$MONTH == 10, 1, 0)

TRAFFIC2_R$NOV = ifelse(TRAFFIC2_R$MONTH == 11, 1, 0)

TRAFFIC2_R$DEC = ifelse(TRAFFIC2_R$MONTH == 12, 1, 0)

TRAFFIC2_R$t = 1:nrow(TRAFFIC2_R)

fit8 = lm(log(TOTACC) ~ t + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + DEC, data=TRAFFIC2_R)

summary(fit8)

linearHypothesis(fit8, c("FEB=0", "MAR=0", "APR=0", "MAY=0", "JUN=0", "JUL=0", "AUG=0", "SEP=0", "OCT=0", "NOV=0", "DEC=0"), test="F")

# 2.3

fit9 = lm(log(TOTACC) ~ t + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + DEC + UNEM + SPDLAW + BELTLAW + WKENDS, data=TRAFFIC2_R)

summary(fit9)

# 2.4

TRAFFIC2_R$PRCFAT = 100 * TRAFFIC2_R$FATACC / TRAFFIC2_R$TOTACC

mean(TRAFFIC2_R$PRCFAT)

# 2.5

fit10 = lm(PRCFAT ~ t + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + DEC + UNEM + SPDLAW + BELTLAW + WKENDS, data=TRAFFIC2_R)

summary(fit10)

# 2.6

TRAFFIC2_R$PRCFAT_LAG1 = Lag(TRAFFIC2_R$PRCFAT, 1)

TRAFFIC2_R$UNEM_LAG1 = Lag(TRAFFIC2_R$UNEM, 1)

fit11 = lm(PRCFAT ~ PRCFAT_LAG1, data=TRAFFIC2_R)

summary(fit11)

fit12 = lm(UNEM ~ UNEM_LAG1, data=TRAFFIC2_R)

summary(fit12)

# 2.7

TRAFFIC2_R$PRCFAT_DIF = TRAFFIC2_R$PRCFAT - TRAFFIC2_R$PRCFAT_LAG1

TRAFFIC2_R$UNEM_DIF = TRAFFIC2_R$UNEM - TRAFFIC2_R$UNEM_LAG1

fit13 = lm(PRCFAT_DIF ~ t + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + DEC + UNEM_DIF + SPDLAW + BELTLAW + WKENDS, data=TRAFFIC2_R)

summary(fit13)

# 2.9

fit14 = lm(PRCFAT ~ t + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + DEC + UNEM + SPDLAW + BELTLAW + WKENDS, data=TRAFFIC2_R)

TRAFFIC2_R$RESID = fit14$residuals

TRAFFIC2_R$RESID_LAG1 = Lag(TRAFFIC2_R$RESID, 1)

fit15 = lm(RESID ~ RESID_LAG1, data=TRAFFIC2_R)

summary(fit15)

fit16 = lm(RESID ~ t + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + DEC + UNEM + SPDLAW + BELTLAW + WKENDS + RESID_LAG1, data=TRAFFIC2_R)

summary(fit16)

# 2.10

fit17 = lm(PRCFAT ~ t + FEB + MAR + APR + MAY + JUN + JUL + AUG + SEP + OCT + NOV + DEC + UNEM + SPDLAW + BELTLAW + WKENDS, data=TRAFFIC2_R)

summary(fit17)

vcov_NW = vcovHAC(fit17, weights=bwNeweyWest)

coeftest(fit17, vcov.=vcov_NW)

vcov_A = vcovHAC(fit17, weights=bwAndrews)

coeftest(fit17, vcov.=vcov_A)

# 3.3

NYSE = read.csv(file="NYSE.csv", header = TRUE, sep=",", as.is =TRUE, na.strings = c("NA", "", "."))

NYSE$RET_LAG1 = Lag(NYSE$RET, 1)

NYSE$RET_LAG2 = Lag(NYSE$RET, 2)

# 3.4

fit18 = lm(RET ~ RET_LAG1, data=NYSE)

fit19 = lm(RET ~ RET_LAG1 + RET_LAG2, data=NYSE)

stargazer(fit18, fit19, type="html", out="hw2q2p3.html")

linearHypothesis(fit18, "RET_LAG1=0", test="F")

linearHypothesis(fit19, c("RET_LAG1=0", "RET_LAG2=0"), test="F")

# 3.6

bptest(fit18)

bptest(fit19)
