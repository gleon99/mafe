require(car)
require(stargazer)
require(nlme)

# 1.1
Signals = read.csv(file="Signals_A_62016.csv", header=TRUE, as.is=TRUE, na.strings=c("NA"))
Signals$FDate = as.Date(Signals$FDate, format="%d/%m/%Y")
Signals$PFormDate = as.Date(Signals$PFormDate, format="%d/%m/%Y")
Signals$ETP_IndNeg[is.na(Signals$ETP)] = 1
Signals$ETP_IndNeg[!is.na(Signals$ETP)] = 0
Signals$ETP_ValPos[is.na(Signals$ETP)] = 0
Signals$ETP_ValPos[!is.na(Signals$ETP)] = Signals$ETP[!is.na(Signals$ETP)]

Signals_1990 = Signals[Signals$PFormDate == "1990/06/30", ]

fit1 = lm("AvgMonRet ~ log(Size) + log(BTM) + ETP_IndNeg + ETP_ValPos", data=Signals_1990)
stargazer(fit1, type="html", out="fit1.html")

# 1.2
L = 0.005
H = 0.995
Size_L = quantile(Signals_1990$Size, prob=L, na.rm=T)
Size_H = quantile(Signals_1990$Size, prob=H, na.rm=T)
Signals_1990$Size[Signals_1990$Size<= Size_L] = Size_L
Signals_1990$Size[Signals_1990$Size>= Size_H] = Size_H

BTM_L = quantile(Signals_1990$BTM, prob=L, na.rm=T)
BTM_H = quantile(Signals_1990$BTM, prob=H, na.rm=T)
Signals_1990$BTM[Signals_1990$BTM<= BTM_L] = BTM_L
Signals_1990$BTM[Signals_1990$BTM>= BTM_H] = BTM_H

ETP_ValPos_L = quantile(Signals_1990$ETP_ValPos, prob=L, na.rm=T)
ETP_ValPos_H = quantile(Signals_1990$ETP_ValPos, prob=H, na.rm=T)
Signals_1990$ETP_ValPos[Signals_1990$ETP_ValPos<= ETP_ValPos_L] = ETP_ValPos_L
Signals_1990$ETP_ValPos[Signals_1990$ETP_ValPos>= ETP_ValPos_H] = ETP_ValPos_H

fit2 = lm("AvgMonRet ~ log(Size) + log(BTM) + ETP_IndNeg + ETP_ValPos", data=Signals_1990)
stargazer(fit2, type="html", out="fit2.html")

# 1.3
BTM = quantile(Signals$BTM, prob=0.9, na.rm=T)
ETP_ValPos = quantile(Signals_1990$ETP_ValPos, prob=0.9, na.rm=T)
ETP_IndNeg = 0
Size = quantile(Signals_1990$Size, prob=0.1, na.rm=T)
df = data.frame(BTM, ETP_ValPos, ETP_IndNeg, Size)
avg_ret = predict(fit2, df)
print(avg_ret)
print(nrow(Signals_1990[Signals_1990$AvgMonRet >= avg_ret ,]))
print(nrow(Signals_1990[Signals_1990$BTM == BTM ,]))

BTM = quantile(Signals_1990$BTM, prob=0.1, na.rm=T)
ETP_ValPos = quantile(Signals_1990$ETP_ValPos, prob=0.1, na.rm=T)
ETP_IndNeg = 0
Size = quantile(Signals_1990$Size, prob=0.9, na.rm=T)
df = data.frame(BTM, ETP_ValPos, ETP_IndNeg, Size)
avg_ret = predict(fit2, df)
print(avg_ret)
print(nrow(Signals_1990[Signals_1990$AvgMonRet >= avg_ret ,]))
print(nrow(Signals_1990[Signals_1990$BTM == BTM ,]))

BTM = quantile(Signals_1990$BTM, prob=0.1, na.rm=T)
ETP_ValPos = 0
ETP_IndNeg = 1
Size = quantile(Signals_1990$Size, prob=0.9, na.rm=T)
df = data.frame(BTM, ETP_ValPos, ETP_IndNeg, Size)
avg_ret = predict(fit2, df)
print(avg_ret)
print(nrow(Signals_1990[Signals_1990$AvgMonRet >= avg_ret ,]))
print(nrow(Signals_1990[Signals_1990$BTM == BTM ,]))

# 1.5
fit3 = lm("AvgMonRet ~ log(Size) + log(BTM)", data=Signals_1990)
stargazer(fit3, type="html", out="fit3.html")

OACC_L = quantile(Signals_1990$OACC, prob=L, na.rm=T)
OACC_H = quantile(Signals_1990$OACC, prob=H, na.rm=T)
Signals_1990$OACC[Signals_1990$OACC<= OACC_L] = OACC_L
Signals_1990$OACC[Signals_1990$OACC>= OACC_H] = OACC_H

fit4 = lm("AvgMonRet ~ log(Size) + log(BTM) + OACC", data=Signals_1990)
stargazer(fit4, type="html", out="fit4.html")
print(vif(fit4))

# 1.6
Signals_New = Signals[!is.na(Signals$Size) & !is.na(Signals$BTM) & 
                      Signals$PFormDate >= as.Date("1963-06-30") &
                      Signals$PFormDate <= as.Date("2015-06-30") &
                      !is.na(Signals$AvgMonRet), c("PFormDate", "Size", "BTM", "AvgMonRet")]

Size_L = quantile(Signals_New$Size, prob=L, na.rm=T)
Size_H = quantile(Signals_New$Size, prob=H, na.rm=T)
Signals_New$Size[Signals_New$Size<= Size_L] = Size_L
Signals_New$Size[Signals_New$Size>= Size_H] = Size_H

BTM_L = quantile(Signals_New$BTM, prob=L, na.rm=T)
BTM_H = quantile(Signals_New$BTM, prob=H, na.rm=T)
Signals_New$BTM[Signals_New$BTM<= BTM_L] = BTM_L
Signals_New$BTM[Signals_New$BTM>= BTM_H] = BTM_H
fit5 = lmList(AvgMonRet ~ log(Size) + log(BTM) | factor(PFormDate), data=Signals_New)
fit_summary = summary(fit5)
cf = coefficients(fit_summary)
print(summary(cf[,1,]))
print(summary(cf[,2,]))

# 1.7
fit6 = lm(AvgMonRet ~ 0 + log(Size) + log(BTM) + factor(EXCHG) + factor(EXCHG):log(BTM), data=Signals_1990)
print(summary(fit6))

# 2.2
CEOSAL = read.table(file="CEOSAL2.txt", header=TRUE, as.is=TRUE, na.strings=c("NA"))
colnames(CEOSAL) = c("salary", "age", "college", "grad", "comten", "ceoten",
  "sales", "profits", "mktval", "lsalary", "lsales",
  "lmktval", "comtensq", "ceotensq", "profmarg")                  
CEOSAL = CEOSAL[CEOSAL$profits > 0 & CEOSAL$sales > 0,]
fit7 = lm(log(salary) ~ college + log(sales) + log(profits) + ceoten + ceotensq, data=CEOSAL)

# 2.3
sales = quantile(CEOSAL$sales, prob=0.5, na.rm=T)
profits = quantile(CEOSAL$profits, prob=0.7, na.rm=T)
CEO = data.frame(ceoten=10, ceotensq=100, college=0, sales=sales, profits=profits)
fitted = predict(fit7, CEO)
mom_est = mean(exp(fit7$residuals))
mom_fitval = mom_est * exp(fitted)
print(mom_fitval)

# 2.4
fit8 = lm(salary ~ college + sales + profits + ceoten + ceotensq, data=CEOSAL)
CEOSAL$fit7_salary = mom_est * exp(fit7$fitted.values)
print((cor(CEOSAL$salary, CEOSAL$fit7_salary))^2)
print((cor(CEOSAL$salary, fit8$fitted.values))^2)