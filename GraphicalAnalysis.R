source("multiplot.R")

p1 <- ggplot(na.omit(panelA[panelA$Vehicle == "OEIC" | panelA$Vehicle == "UCITS4", ]
                     [,c("NCCF", "RelPerf3y", "Vehicle")]), 
             aes(x=RelPerf3y, y=NCCF)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ Vehicle, nrow=2) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 10)) +
  ggtitle("Monthly NCCF* by Relative performance (3y) \n") +
  #theme(legend.title=element_blank()) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("NCCF") +
  xlab("3y Relative Performance") 

p2 <- ggplot(na.omit(panelA[panelA$Vehicle == "OEIC" | panelA$Vehicle == "UCITS4", ]
                     [,c("NCCF", "Rank1y", "Vehicle")]), 
             aes(x=Rank1y, y=NCCF)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ Vehicle, nrow=2) + 
  theme(plot.title = element_text(lineheight=0.8, face="bold", size = 10)) +
  ggtitle("Monthly NCCF* by percentile ranking (1y) \n") +
  #theme(legend.title=element_blank()) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("NCCF") +
  xlab("1y Percentile ranking") 

multiplot(p1, p2, cols = 2)

p1 <- ggplot(na.omit(rawdata2[rawdata2$Vehicle == "OEIC" | rawdata2$Vehicle == "UCITS4", ]
                     [,c("NCCF", "Rank1y", "Vehicle")]), 
             aes(x=Rank1y, y=NCCF)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ Vehicle, nrow=2) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 10)) +
  ggtitle("Monthly NCCF* by peer rank (1y) \n (percentile < 15% or > 85%) \n") +
  #theme(legend.title=element_blank()) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("NCCF") +
  xlab("Peer rank") 

p2 <- ggplot(na.omit(rawdata3[rawdata3$Vehicle == "OEIC" | rawdata3$Vehicle == "UCITS4", ]
                     [,c("NCCF", "Rank1y", "Vehicle")]), 
             aes(x=Rank1y, y=NCCF)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ Vehicle, nrow=2) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 10)) +
  ggtitle("Monthly NCCF* by peer rank (1y) \n (percentile > 15% or < 85%) \n") +
  #theme(legend.title=element_blank()) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("NCCF") +
  xlab("Peer rank") 

multiplot(p1, p2, cols = 2)


#Predictions

completeSet <- panelA[,c("NCCF", 
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

predicted <- predict(model, testset)
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

qqPlot(model, main="QQ Plot")
influencePlot(model, id.method="noteworthy", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

rm(list = ls(pattern = "model."))