#exploring plots
hist(panelA$NCCF, prob = TRUE)
curve(dnorm(x, mean = mean(panelA$NCCF, na.rm = TRUE), 
            sd = sd(panelA$NCCF, na.rm = TRUE)), 
      add=TRUE, col = "red")

plot(panelA$RelPerf3y, panelA$NCCF)
plot(panelA$Rank1y, panelA$NCCF)


modelKitchenSink <- lm(NCCF ~
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
                       data = panelA, na.action = na.omit)
summary(modelKitchenSink)

model <- step(modelKitchenSink, direction="both")
# 
# 
# model1 <- lm(NCCF ~ AbsPerf3m +
#                AbsPerf6m + 
#                AbsPerf1y + 
#                AbsPerf2y + 
#                AbsPerf3y, data = panelA, na.action = na.omit)
# summary(model1)
# 
# model2 <- lm(NCCF ~ RelPerf3m +
#                RelPerf6m + 
#                RelPerf1y + 
#                RelPerf2y + 
#                RelPerf3y, data = panelA, na.action = na.omit)
# summary(model2)
# 
# model3 <- lm(NCCF ~ Rank3m +
#                Rank6m + 
#                Rank1y + 
#                Rank2y + 
#                Rank3y, data = panelA, na.action = na.omit)
# summary(model3)
# 
# model4 <- lm(NCCF ~ 
#                RelPerf3y + 
#                Rank1y, data = panelA, na.action = na.omit)
# 
# summary(model4)
# confint(model4)
# 
# model4s <- lm(NCCF ~ 
#                RelPerf3y + 
#                Rank1y, 
#               data = modelScaled, na.action = na.omit)

summary(model)
confint(model)

rm(modelKitchenSink)

#nice plots

# xyplot(NCCF ~ RelPerf3y | Vehicle, 
#        data=panelA, #type=c("p","r"),
#        panel = function(x,y,...){
#          panel.xyplot(x, y, pch = 21,col = "black")
#          panel.lmline(x,y,col = "red")}
# )

