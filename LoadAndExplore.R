#setwd("g:/GitHub/PerfNCCF/")

library(RODBC)
library(lattice)
library(ggplot2)
library(scales)
#library(zoo)
library(car)

channel <- odbcConnect("SQLServerPerfRep")

rawdata <- sqlQuery(channel, "SELECT * FROM vw_EstimatedNCCF ORDER BY FundCode, RefDate")

close(channel)
rm(channel)

save(rawdata, file = "OMGIset.Rda")
load("OMGIset.Rda")

#str(rawdata)
#summary(rawdata)
#change names
colnames(rawdata)[names(rawdata) == "NCCFEstimate"] <- "NCCF"

#remove outliers
NCCFtolerance <- 0.5
sum(rawdata$NCCF > NCCFtolerance |  rawdata$NCCF < -NCCFtolerance, na.rm = TRUE)
rawdata1 <- rawdata[rawdata$NCCF < NCCFtolerance & rawdata$NCCF > -NCCFtolerance, ]

ExcludeFunds <- c("SKPROP")
sum(rawdata1$FundCode %in% ExcludeFunds, na.rm=TRUE)
rawdata1 <- rawdata1[! rawdata1$FundCode %in% ExcludeFunds, ]
#exclude Select seeded

#summary(rawdata1$IsSelect)
rawdata1 <- rawdata1[! rawdata1$IsSelect == 1, ]

summary(rawdata1)
#######ADDED SETS
#extreme performers
extremeThresh <- 0.15
rawdata2 <- rawdata1[rawdata1$Rank1y < extremeThresh | 
                       rawdata1$Rank1y > 1- extremeThresh, ]

#normal performers
rawdata3 <- rawdata1[rawdata1$Rank1y > extremeThresh & 
                       rawdata1$Rank1y < 1- extremeThresh, ]

#scaled sets
modelScaled <- data.frame(scale(rawdata1[, c("NCCF", 
                                             "RelPerf1y", 
                                             "RelPerf2y", 
                                             "RelPerf3y",
                                             "RelPerf3m", 
                                             "RelPerf6m",
                                             "Rank1y", 
                                             "Rank3y")]))

#exploring plots
hist(rawdata1$NCCF, prob = TRUE)
curve(dnorm(x, mean = mean(rawdata1$NCCF, na.rm = TRUE), sd = sd(rawdata1$NCCF, na.rm = TRUE)), add=TRUE, col = "red")

plot(rawdata1$RelPerf3y, rawdata1$NCCF)
plot(rawdata1$RelPerf1y, rawdata1$NCCF)
plot(rawdata1$RelPerf6m, rawdata1$NCCF)
plot(rawdata1$Rank3y, rawdata1$NCCF)
plot(rawdata1$Rank1y, rawdata1$NCCF)


modelKitchen <- lm(NCCF ~
                     AbsPerf3m +
                     AbsPerf6m +
                     AbsPerf1y +
                     AbsPerf2y +
                     AbsPerf3y +
                     RelPerf3m +
                     RelPerf6m +
                     RelPerf1y +
                     RelPerf2y +
                     RelPerf3y +
                     Rank3m +
                     Rank6m +
                     Rank1y +
                     Rank2y +
                     Rank3y,
                     data = rawdata1, na.action = na.omit)
summary(modelKitchen)

step(modelKitchen, direction="both")


model1 <- lm(NCCF ~ AbsPerf3m +
               AbsPerf6m + 
               AbsPerf1y + 
               AbsPerf2y + 
               AbsPerf3y, data = rawdata1, na.action = na.omit)
summary(model1)

model2 <- lm(NCCF ~ RelPerf3m +
               RelPerf6m + 
               RelPerf1y + 
               RelPerf2y + 
               RelPerf3y, data = rawdata1, na.action = na.omit)
summary(model2)

model3 <- lm(NCCF ~ Rank3m +
               Rank6m + 
               Rank1y + 
               Rank2y + 
               Rank3y, data = rawdata1, na.action = na.omit)
summary(model3)

model4 <- lm(NCCF ~ 
               RelPerf3y + 
               Rank1y, data = rawdata1, na.action = na.omit)

summary(model4)
confint(model4)

model4s <- lm(NCCF ~ 
               RelPerf3y + 
               Rank1y, 
              data = modelScaled, na.action = na.omit)
summary(model4s)
confint(model4s)


#nice plots

xyplot(NCCF ~ RelPerf3y | Vehicle, 
       data=rawdata1, #type=c("p","r"),
       panel = function(x,y,...){
         panel.xyplot(x, y, pch = 21,col = "black")
         panel.lmline(x,y,col = "red")}
)

ggplot(na.omit(rawdata1[rawdata1$Vehicle == "OEIC" | rawdata1$Vehicle == "UCITS4", ]
               [,c("NCCF", "RelPerf3y", "Vehicle")]), 
       aes(x=RelPerf3y, y=NCCF)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ Vehicle, nrow=2) + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Monthly NCCF* by Relative performance (3y) \n") +
  #theme(legend.title=element_blank()) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("NCCF/AuM") +
  xlab("Relative Performance (3y)") 

ggplot(na.omit(rawdata1[rawdata1$Vehicle == "OEIC" | rawdata1$Vehicle == "UCITS4", ]
               [,c("NCCF", "Rank1y", "Vehicle")]), 
       aes(x=Rank1y, y=NCCF)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ Vehicle, nrow=2) + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Monthly NCCF* by percentile ranking (1y) \n") +
  #theme(legend.title=element_blank()) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("NCCF/AuM") +
  xlab("1y Percentile ranking") 


ggplot(na.omit(rawdata2[rawdata2$Vehicle == "OEIC" | rawdata2$Vehicle == "UCITS4", ]
               [,c("NCCF", "Rank1y", "Vehicle")]), 
       aes(x=Rank1y, y=NCCF)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ Vehicle, ncol=2) + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Monthly NCCF* by peer rank (1y) \n (percentile < 15% or > 85%) \n") +
  #theme(legend.title=element_blank()) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("NCCF/AuM") +
  xlab("Peer rank") 

ggplot(na.omit(rawdata3[rawdata3$Vehicle == "OEIC" | rawdata3$Vehicle == "UCITS4", ]
               [,c("NCCF", "Rank1y", "Vehicle")]), 
       aes(x=Rank1y, y=NCCF)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ Vehicle, ncol=2) + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Monthly NCCF* by peer rank (1y) \n (percentile > 15% or < 85%) \n") +
  #theme(legend.title=element_blank()) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("NCCF/AuM") +
  xlab("Peer rank") 


#Predictions

completeSet <- rawdata1[,c("NCCF", 
                          "RelPerf3y", 
                          "Rank1y")]
completeSet <- completeSet[complete.cases(completeSet), ]

set.seed(12345)
index <- 1:nrow(completeSet)
trainindex <- sample(index, trunc(length(index)*2/3))
trainset <- completeSet[trainindex, ]
testset <- completeSet[-trainindex, ]

str(trainset)
str(testset)

predicted <- predict(model4, testset)
actual <- testset$NCCF

RMSD <- sqrt(sum((actual-predicted)^2)/length(actual))

rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
print(rsq)
print(RMSD)
print(RMSD/mean(actual))
print(paste0("Prediction error: ", as.character(round(RMSD/(max(actual)-min(actual)),3)*100),"%"))
#http://en.wikipedia.org/wiki/Root-mean-square_deviation

#png("IntRatePrediction.png", width=480, height=400, units= "px")
plot(actual, predicted, 
     main = "NCCF: actual vs. predicted", cex.main = 1, 
     yaxt = "n", xaxt = "n", cex.axis=0.75,
      bty= "n") #xlim=c(-.5, 0.5), ylim=c(-0.50, 0.5),
axis(2, at=pretty(predicted)
     , lab=paste0(pretty(predicted)*100, "%")
     , las = TRUE, cex.axis=0.75)
axis(1, at=pretty(actual)
     , lab=paste0(pretty(actual)*100, "%")
     , las = TRUE, cex.axis=0.75)

abline(0,1, col="red")
#dev.off()

qqPlot(model4, main="QQ Plot")
influencePlot(model4, id.method="noteworthy", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

rm(list = ls(pattern = "model."))