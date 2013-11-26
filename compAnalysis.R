# library(lattice)
library(ggplot2)
library(scales)
#library(zoo)
library(car)

# library(RODBC)
# channel <- odbcConnect("SQLServerPerfRep")
# compdata <- sqlQuery(channel, "SELECT * FROM vw_CompNCCF ORDER BY FundId, RefDate")
# close(channel)
# rm(channel)
# save(compdata, file = "COMPset.Rda")

load("COMPset.Rda")

#add variable
compdata$ItemType <- compdata$ItemId
compdata$ItemType <- sub("Comp.", "Competitor", compdata$ItemType)
compdata$ItemType <- sub("BS", "BestSeller", compdata$ItemType)

print(paste0("Raw number of products: ",
             as.character(length(unique(compdata$FundId)))))
print(paste0("Observations: ",
             as.character(length(compdata$FundId))))


str(compdata)
#remove outliers
threshold <- 50
panelB <- compdata[compdata$RatioToBS1m > -threshold & 
                        compdata$RatioToBS1m < threshold,]

panelB <- panelB[panelB$RatioToBS3m > -threshold & 
                        panelB$RatioToBS3m < threshold,]
#remove Best sellers
panelB <- panelB[! panelB$ItemId == 'BS.', ]

#Filter out small samples
panelB <- panelB[panelB$CompNo > 2,]


par(mar = c(4,4,2,2))
par(mfrow = c(2,2))
plot(panelB$Perf1y1mRatio, panelB$RatioToBS1m, 
     col=rgb(0,100,0,50,maxColorValue=255), pch=16,
     xlab = "1y perf. ratio to 1m best NCCF 1y perf.",
     ylab = "1m NCCF ratio to 1m best NCCF", 
     cex.lab = 0.7,
     cex.axis = 0.7, 
     bty="n")
mtext("NCCF to best seller (among selected competitors) vs. performance ratios", 
      side = 3, adj = 0, cex = 0.7, line = 1, font = 4)
plot(panelB$Perf3y1mRatio, panelB$RatioToBS1m, 
     col=rgb(0,100,0,50,maxColorValue=255), pch=16,
     xlab = "3y perf. ratio to 1m best NCCF 3y perf.",
     ylab = "1m NCCF ratio to 1m best NCCF", 
     cex.lab = 0.7,
     cex.axis = 0.7, 
     bty="n")
plot(panelB$Perf1y3mRatio, panelB$RatioToBS3m, 
     col=rgb(0,100,0,50,maxColorValue=255), pch=16,
     xlab = "1y perf. ratio to 1m best NCCF 1y perf.",
     ylab = "3m NCCF ratio to 3m best NCCF", 
     cex.lab = 0.7,
     cex.axis = 0.7, 
     bty="n")
plot(panelB$Perf3y3mRatio, panelB$RatioToBS3m, 
     col=rgb(0,100,0,50,maxColorValue=255), pch=16, 
     xlab = "3y perf. ratio to 3m best NCCF 3y perf.",
     ylab = "3m NCCF ratio to 3m best NCCF", 
     cex.lab = 0.7,
     cex.axis = 0.7, 
     bty="n")
par(mar = c(5,4,4,2) + 0.1)
par(mfcol = c(1,1))


#numerical analysis on Panel B

model1m <- lm(RatioToBS1m ~
                Perf3y1mRatio
              + StDev3y
              , data = panelB)
summary(model1m)

model3m <- lm(RatioToBS3m ~
                Perf3y3mRatio
              + StDev3y
              , data = panelB)
summary(model3m)

rm(list= ls(pattern = "model."))

# example of NCCF vs. Performance bubble chart
# bubexset <- compdata[compdata$FundId == 314 & 
#                        as.Date(compdata$RefDate) == as.Date('2013-10-31'),]
# 
# plot(bubexset$Perf1y, bubexset$NCCF3mGBPmn 
#      , col = (bubexset$ItemId == 'OMGI') + 1
#      , cex = bubexset$StDev1y/3)
# abline(h=0, col = "black")
# 
# 
# symbols(bubexset$Perf3y, bubexset$NCCF3mGBPmn 
#      , bg = "white"
#      , fg = ((bubexset$ItemId == 'OMGI') + 2)
#      , circles = bubexset$StDev1y, inches = 0.2
#      , xlab = "Performance 3y"
#         , ylab = "NCCF 3m (GBP mn)"
#         , bty = "n"
#         , main = bubexset[bubexset$ItemId == 'OMGI', "FundName"]
#         , cex.main = 0.75)
# text(bubexset$Perf3y, bubexset$NCCF3mGBPmn, bubexset$ItemId, cex=0.5)
# abline(h=0, col = "black", lty = 3)
