#setwd("g:/GitHub/PerfNCCF/")

library(ggplot2)
library(scales)
library(car)

# library(RODBC)
# channel <- odbcConnect("SQLServerPerfRep")
# rawdata <- sqlQuery(channel, "SELECT * FROM vw_EstimatedNCCF ORDER BY FundCode, RefDate")
# close(channel)
# rm(channel)
# save(rawdata, file = "OMGIset.Rda")

load("OMGIset.Rda")

#str(rawdata)
#summary(rawdata)
#change names
colnames(rawdata)[names(rawdata) == "NCCFEstimate"] <- "NCCF"
rawdata$RefDate <- as.Date(rawdata$RefDate)
print(paste0("Raw number of products: ",
             as.character(length(unique(rawdata$FundCode)))))

#remove outliers
NCCFtolerance <- 0.5
print(paste0("Outliers with |NCCF| > ", 
            as.character(NCCFtolerance*100), "%: " ,
            sum(rawdata$NCCF > NCCFtolerance |  
                  rawdata$NCCF < -NCCFtolerance, na.rm = TRUE)))
panelA <- rawdata[rawdata$NCCF < NCCFtolerance & 
                    rawdata$NCCF > -NCCFtolerance, ]

ExcludeFunds <- c("SKPROP")
print(paste0("Excluded funds: ", ExcludeFunds))
sum(panelA$FundCode %in% ExcludeFunds, na.rm=TRUE)
panelA <- panelA[! panelA$FundCode %in% ExcludeFunds, ]
#exclude Select seeded

#summary(panelA$IsSelect)
print(paste0("Seeded Select funds obs.: ", as.character(
  sum(panelA$IsSelect == 1, na.rm=TRUE))))
panelA <- panelA[! panelA$IsSelect == 1, ]
print(paste0("Final number of products: ",
             as.character(length(unique(panelA$FundCode)))))

print(paste0("Final number of observations: ",
             as.character(length(panelA$FundCode))))

#summary(panelA)
#######ADDED SETS
#extreme performers
extremeThresh <- 0.15
panelA_extr <- panelA[panelA$Rank1y < extremeThresh | 
                          panelA$Rank1y > 1- extremeThresh, ]

#normal performers
panelA_mid <- panelA[panelA$Rank1y > extremeThresh & 
                       panelA$Rank1y < 1- extremeThresh, ]

#scaled sets
# dataScaled <- data.frame(scale(panelA[, c("NCCF", 
#                                              "RelPerf1y", 
#                                              "RelPerf2y", 
#                                              "RelPerf3y",
#                                              "RelPerf3m", 
#                                              "RelPerf6m",
#                                              "Rank1y", 
#                                              "Rank3y")]))
# 
