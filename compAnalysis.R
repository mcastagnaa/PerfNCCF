library(RODBC)
library(lattice)
library(ggplot2)
library(scales)
#library(zoo)
library(car)

channel <- odbcConnect("SQLServerPerfRep")

compdata <- sqlQuery(channel, "SELECT * FROM vw_CompNCCF ORDER BY FundId, RefDate")

close(channel)
rm(channel)

save(rawdata, file = "COMPset.Rda")
load("COMPset.Rda")


str(compdata)
#remove outliers
threshold <- 50
compdata1 <- compdata[compdata$RatioToBS1m > -threshold & 
                        compdata$RatioToBS1m < threshold,]

compdata1 <- compdata1[compdata1$RatioToBS3m > -threshold & 
                        compdata1$RatioToBS3m < threshold,]
#remove Best sellers
compdata1 <- compdata1[! compdata1$ItemId == 'BS.', ]

#Filter out small samples
compdata1 <- compdata1[compdata1$CompNo > 2,]


par(mar = c(4,4,2,2))
par(mfrow = c(2,2))
plot(compdata1$Perf1y1mRatio, compdata1$RatioToBS1m, 
     col=rgb(0,100,0,50,maxColorValue=255), pch=16,
     xlab = "1y perf. ratio to 1m best NCCF 1y perf.",
     ylab = "1m NCCF ratio to 1m best NCCF", 
     cex.lab = 0.7,
     cex.axis = 0.7, 
     bty="n")
mtext("NCCF to best seller (among selected competitors) vs. performance ratios", 
      side = 3, adj = 0, cex = 0.7, line = 1, font = 4)
plot(compdata1$Perf3y1mRatio, compdata1$RatioToBS1m, 
     col=rgb(0,100,0,50,maxColorValue=255), pch=16,
     xlab = "3y perf. ratio to 1m best NCCF 3y perf.",
     ylab = "1m NCCF ratio to 1m best NCCF", 
     cex.lab = 0.7,
     cex.axis = 0.7, 
     bty="n")
plot(compdata1$Perf1y3mRatio, compdata1$RatioToBS3m, 
     col=rgb(0,100,0,50,maxColorValue=255), pch=16,
     xlab = "1y perf. ratio to 1m best NCCF 1y perf.",
     ylab = "3m NCCF ratio to 3m best NCCF", 
     cex.lab = 0.7,
     cex.axis = 0.7, 
     bty="n")
plot(compdata1$Perf3y3mRatio, compdata1$RatioToBS3m, 
     col=rgb(0,100,0,50,maxColorValue=255), pch=16, 
     xlab = "3y perf. ratio to 3m best NCCF 3y perf.",
     ylab = "3m NCCF ratio to 3m best NCCF", 
     cex.lab = 0.7,
     cex.axis = 0.7, 
     bty="n")
par(mar = c(5,4,4,2) + 0.1)
par(mfcol = c(1,1))


#numerical analysis

model1m <- lm(RatioToBS1m ~
                Perf3y1mRatio
              + StDev3y
              , data = compdata1)
summary(model1m)

model3m <- lm(RatioToBS3m ~
                Perf3y3mRatio
              + StDev3y
              , data = compdata1)
summary(model3m)



# example of NCCF vs. Performance bubble chart
bubexset <- compdata[compdata$FundId == 314 & 
                       as.Date(compdata$RefDate) == as.Date('2013-10-31'),]

plot(bubexset$Perf1y, bubexset$NCCF3mGBPmn 
     , col = (bubexset$ItemId == 'OMGI') + 1
     , cex = bubexset$StDev1y/3)
abline(h=0, col = "black")


symbols(bubexset$Perf3y, bubexset$NCCF3mGBPmn 
     , bg = "white"
     , fg = ((bubexset$ItemId == 'OMGI') + 2)
     , circles = bubexset$StDev1y, inches = 0.2
     , xlab = "Performance 3y"
        , ylab = "NCCF 3m (GBP mn)"
        , bty = "n"
        , main = bubexset[bubexset$ItemId == 'OMGI', "FundName"]
        , cex.main = 0.75)
text(bubexset$Perf3y, bubexset$NCCF3mGBPmn, bubexset$ItemId, cex=0.5)
abline(h=0, col = "black", lty = 3)
