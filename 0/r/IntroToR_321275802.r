library(lubridate)

# Question 1
# 1
VWRet = c(0.931, −0.105, −0.470, −0.149, −0.218, 0.250, 1.171, −7.162)
# 2
print(mode(VWRet))
# 3
TrdDate = as.Date(c("2015.03.20", "2015.03.23", "2015.03.24", "2015.03.25",
"2015.03.26", "2015.03.27", "2015.03.30", "2015.03.31"), format="%Y.%m.%d")
# 4
stopifnot(class(TrdDate) == "Date")
# 5
DF = data.frame(VWRet_DF = VWRet, TrdDate_DF = TrdDate)
# 6
stopifnot(colnames(DF)[1] == "VWRet_DF")
stopifnot(colnames(DF)[2] == "TrdDate_DF")
# 7
print(max(DF$VWRet_DF))
print(min(DF$VWRet_DF))
# 8
DF$TrdDate_Weekday = weekdays(DF$TrdDate_DF)
# 9
print(DF[which.max(DF$VWRet), "TrdDate_Weekday"])

# Question 2
# 1
IndexesDF = read.csv("Indexes_D_122016.csv")
VIXDF = read.csv("VIX_D_72017.csv")
# 2
print(dim(IndexesDF))
print(dim(VIXDF))
# 3
IndexesDF$TrdDate = as.Date(as.character(IndexesDF$TrdDate), format = "%Y%m%d")
VIXDF$TrdDate = as.Date(as.character(VIXDF$TrdDate), format = "%d%b%Y")
# 4
COMBINED = merge(VIXDF, IndexesDF, by="TrdDate")
# 5
print(nrow(COMBINED[year(COMBINED$TrdDate) == 1996,]))
# 6
COMBINED$TrdMonth = month.abb[month(COMBINED$TrdDate)]
# 7
print(nrow(COMBINED[year(COMBINED$TrdDate) == 1996 & COMBINED$TrdMonth == "Apr",]))
print(nrow(COMBINED[COMBINED$TrdMonth == "Apr",]))
# 8
print(tapply(COMBINED$SP_VWRet, weekdays(COMBINED$TrdDate), mean)) # Tuesday is the best day, Friday is the worst
# 9
vix_avg = mean(COMBINED$VIX)
COMBINED_VIX_BELOW_AVG = COMBINED[COMBINED$VIX < vix_avg,]
print(mean(COMBINED_VIX_BELOW_AVG$SP_VWRet))
COMBINED_VIX_ABOVE_AVG = COMBINED[COMBINED$VIX > vix_avg,]
print(mean(COMBINED_VIX_ABOVE_AVG$SP_VWRet)) # Conclusion: inverse correlation (?)

# Question 3
TITLE = "Leonid Genkin 321275802"
# 1
Indexes = read.csv("Indexes_M_122016.csv")
RED = Indexes[,c("TrdDate","VWRet", "SP_VWRet", "SP_Ind"), drop=FALSE]
RED$TrdDate = as.Date(as.character(RED$TrdDate), format = "%Y%m%d")
# 2
print(nrow(RED[RED$VWRet > RED$SP_VWRet & year(RED$TrdDate) == 1990,]))
# 3
# Conclustion: Most of the values are in the range (-8, +8), there is no specific trend over time,
# average seems to be around 0
plot(RED$TrdDate, RED$VWRet, main=TITLE, xlab="Trade Date", ylab="Return Value")
# 4
RED$Cum_VWRet = cumprod(1 + RED$VWRet / 100)
RED$Cum_SP_VWRet = cumprod(1 + RED$SP_VWRet / 100)
# 5
pdf(file="q3p5.pdf")
par(mfrow=c(nr=2, nc=1))
# We learn from the plots:
# 1. The indices behave in a similar manner
# 2. Until ~1980s there was quite slow growth, which accelerated exponentially afterwards
# 3. We can clearly sett the ".net technology peak" around year 2000, and the 2008 crisis
plot(RED$TrdDate, RED$Cum_VWRet, xlab = "Trade Date", ylab = "Cumulative Return",
main=TITLE, type="l", col="red")
legend("topleft", legend="VW", col="red", lty=1)
plot(RED$TrdDate, RED$Cum_SP_VWRet, xlab = "Trade Date", ylab = "Cumulative Return",
main=TITLE, type="l", col="blue")
legend("topleft", legend="SP_VW", col="blue", lty=1)
dev.off()
# 6
pdf(file="q3p6.pdf")
plot(RED$TrdDate, RED$Cum_VWRet, xlab = "Trade Date", ylab = "Cumulative Return",
main=TITLE, type="l", col="red")
legend("topleft", legend=c("VW", "SP_VW"), col=c("red", "blue"), lty=1)
lines(RED$TrdDate, RED$Cum_SP_VWRet, xlab = "Trade Date", ylab = "Cumulative Return",
main=TITLE, type="l", col="blue")
dev.off()
# 7
write.csv(RED, file="q3p7.csv")