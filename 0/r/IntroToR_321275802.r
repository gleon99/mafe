library(lubridate)

# Part 1
VWRet = c(0.931, −0.105, −0.470, −0.149, −0.218, 0.250, 1.171, −7.162)
print(mode(VWRet))
TrdDate = as.Date(c("2015.03.20", "2015.03.23", "2015.03.24", "2015.03.25", "2015.03.26", "2015.03.27", "2015.03.30", "2015.03.31"), format="%Y.%m.%d")
print(class(TrdDate))
DF = data.frame(VWRet_DF = VWRet, TrdDate_DF = TrdDate)
print(max(DF$VWRet_DF))
print(min(DF$VWRet_DF))
DF$TrdDate_Weekday = weekdays(DF$TrdDate_DF)
print(DF[which.max(DF$VWRet), "TrdDate_Weekday"])

# Part 2
IndexesDF = read.csv("Indexes_D_122016.csv")
VIXDF = read.csv("VIX_D_72017.csv")
print(dim(IndexesDF))
print(dim(VIXDF))
IndexesDF$TrdDate = as.Date(as.character(IndexesDF$TrdDate), format = "%Y%m%d")
VIXDF$TrdDate = as.Date(as.character(VIXDF$TrdDate), format = "%d%b%Y")
COMBINED = merge(VIXDF, IndexesDF, by="TrdDate")
print(nrow(COMBINED[year(COMBINED$TrdDate) == 1996,]))
COMBINED$TrdMonth = month.abb[month(COMBINED$TrdDate)]
print(nrow(COMBINED[year(COMBINED$TrdDate) == 1996 & COMBINED$TrdMonth == "Apr",]))
print(nrow(COMBINED[COMBINED$TrdMonth == "Apr",]))
tapply(COMBINED$SP_VWRet, weekdays(COMBINED$TrdDate), mean)) # Tuesday is the best day, Friday is the worst
vix_avg = mean(COMBINED$VIX)
COMBINED_VIX_BELOW_AVG = COMBINED[COMBINED$VIX < vix_avg,]
print(mean(COMBINED_VIX_BELOW_AVG$SP_VWRet))
COMBINED_VIX_ABOVE_AVG = COMBINED[COMBINED$VIX > vix_avg,]
print(mean(COMBINED_VIX_ABOVE_AVG$SP_VWRet)) # Conclusion: inverse correlation (?)

