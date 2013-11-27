#library(zoo)
library(plm)

panelSet <- na.omit(panelA[, c("FundCode"
                         , "RefDate"
                         , "NCCF"
                         , "AbsPerf1m"
                         , "RelPerf1m"
                         , "AbsPerf3m"
                         , "RelPerf3m"
                         , "Rank3m"                               
                         , "Rank1m"
                         , "Rank1y"
                         , "RelPerf3y")])

pdataSet <- pdata.frame(panelSet
                        , index = c("FundCode", "RefDate")
                        , drop.index = FALSE
                        , row.names = TRUE)
rm(panelSet)

head(pdataSet)
length(unique(pdataSet$FundCode))
# 
# laggedMod <- lm(NCCF ~ 
#                   RelPerf3m
#                 + lag(pdataSet$RelPerf3m, 1)
#                 + lag(pdataSet$RelPerf3m, 2)
#                 + lag(pdataSet$RelPerf3m, 3)
#                 + lag(pdataSet$RelPerf3m, 4)
#                 + lag(pdataSet$RelPerf3m, 5)
#                 + lag(pdataSet$RelPerf3m, 6)
#                 + AbsPerf3m
#                 + lag(pdataSet$AbsPerf3m, 1)
#                 + lag(pdataSet$AbsPerf3m, 2)
#                 + lag(pdataSet$AbsPerf3m, 3)
#                 + lag(pdataSet$AbsPerf3m, 4)
#                 + lag(pdataSet$AbsPerf3m, 5)
#                 + lag(pdataSet$AbsPerf3m, 6)
#                 + Rank3m
#                 + lag(pdataSet$Rank3m, 1)
#                 + lag(pdataSet$Rank3m, 2)
#                 + lag(pdataSet$Rank3m, 3)
#                 + lag(pdataSet$Rank3m, 4)
#                 + lag(pdataSet$Rank3m, 5)
#                 + lag(pdataSet$Rank3m, 6)
#                 , data=pdataSet, na.action=na.omit)


# laggedMod <- lm(NCCF ~ 
#                   RelPerf3m
#                 + lag(pdataSet$RelPerf1m, 1)
#                 + lag(pdataSet$RelPerf1m, 2)
#                 + lag(pdataSet$RelPerf1m, 3)
#                 + lag(pdataSet$RelPerf1m, 4)
#                 + lag(pdataSet$RelPerf1m, 5)
#                 + lag(pdataSet$RelPerf1m, 6)
#                 + AbsPerf1m
#                 + lag(pdataSet$AbsPerf1m, 1)
#                 + lag(pdataSet$AbsPerf1m, 2)
#                 + lag(pdataSet$AbsPerf1m, 3)
#                 + lag(pdataSet$AbsPerf1m, 4)
#                 + lag(pdataSet$AbsPerf1m, 5)
#                 + lag(pdataSet$AbsPerf1m, 6)
#                 + Rank1m
#                 + lag(pdataSet$Rank1m, 1)
#                 + lag(pdataSet$Rank1m, 2)
#                 + lag(pdataSet$Rank1m, 3)
#                 + lag(pdataSet$Rank1m, 4)
#                 + lag(pdataSet$Rank1m, 5)
#                 + lag(pdataSet$Rank1m, 6)
#                 , data=pdataSet, na.action=na.omit)



laggedMod <- lm(NCCF ~ 
                + lag(pdataSet$RelPerf1m, 1)
                + lag(pdataSet$RelPerf1m, 2)
                + lag(pdataSet$RelPerf1m, 3)
#                 + lag(pdataSet$RelPerf1m, 4)
#                 + lag(pdataSet$RelPerf1m, 5)
#                 + lag(pdataSet$RelPerf1m, 6)
                + lag(pdataSet$AbsPerf1m, 1)
                + lag(pdataSet$AbsPerf1m, 2)
                + lag(pdataSet$AbsPerf1m, 3)
#                 + lag(pdataSet$AbsPerf1m, 4)
#                 + lag(pdataSet$AbsPerf1m, 5)
#                 + lag(pdataSet$AbsPerf1m, 6)
                + lag(pdataSet$Rank1m, 1)
                + lag(pdataSet$Rank1m, 2)
                + lag(pdataSet$Rank1m, 3)
#                 + lag(pdataSet$Rank1m, 4)
#                 + lag(pdataSet$Rank1m, 5)
#                 + lag(pdataSet$Rank1m, 6)
                + lag(pdataSet$RelPerf3m, 1)
                + lag(pdataSet$RelPerf3m, 2)
                + lag(pdataSet$RelPerf3m, 3)
#                 + lag(pdataSet$RelPerf3m, 4)
#                 + lag(pdataSet$RelPerf3m, 5)
#                 + lag(pdataSet$RelPerf3m, 6)
                + lag(pdataSet$AbsPerf3m, 1)
                + lag(pdataSet$AbsPerf3m, 2)
                + lag(pdataSet$AbsPerf3m, 3)
#                 + lag(pdataSet$AbsPerf3m, 4)
#                 + lag(pdataSet$AbsPerf3m, 5)
#                 + lag(pdataSet$AbsPerf3m, 6)
                + lag(pdataSet$Rank3m, 1)
                + lag(pdataSet$Rank3m, 2)
                + lag(pdataSet$Rank3m, 3)
#                 + lag(pdataSet$Rank3m, 4)
#                 + lag(pdataSet$Rank3m, 5)
#                 + lag(pdataSet$Rank3m, 6)                
                , data=pdataSet, na.action=na.omit)


#summary(laggedMod)
finalLags <- step(laggedMod, direction="both")
summary(finalLags)
rm(laggedMod)

discMod <- lm(NCCF ~ 
                + Rank1y
                + RelPerf3y
                #+ lag(pdataSet$Rank1m, 2)
                #+ lag(pdataSet$Rank3m, 2)
                #+ lag(pdataSet$RelPerf3m, 1)
                #+ lag(pdataSet$RelPerf3m, 3)
                , data=pdataSet, na.action=na.omit)
summary(discMod)


par(mfrow = c(2,2))
#plot(pdataSet$RelPerf3m, pdataSet$NCCF)
plot(lag(pdataSet$Rank1m, 2), pdataSet$NCCF)
plot(lag(pdataSet$Rank1m, 3), pdataSet$NCCF)
plot(lag(pdataSet$RelPerf3m, 3), pdataSet$NCCF)
plot(lag(pdataSet$RelPerf1m, 3), pdataSet$NCCF)
par(mfrow = c(1,1))

#lagSetZ <-zoo(na.omit(laggedSet))
#lagSetZ

#head(lagSetZ)
#NCCF_1 <-lag(lagSetZ,-1)[lagSetZ$FundCode==lag(lagSetZ$FundCode)]
#NCCF_2 <-lag(lagSetZ,-2)[lagSetZ$FundCode==lag(lagSetZ$FundCode)]
#NCCF_3 <-lag(lagSetZ,-3)[lagSetZ$FundCode==lag(lagSetZ$FundCode)]

#res <-merge.zoo(lagSetZ,NCCF_1)#, NCCF_2, NCCF_3)
#head(res)
# 
# fixed <- plm(NCCF ~ Rank3m + AbsPerf3m + RelPerf3m, data=panelSet, index=c("FundCode", "RefDate"), model="within")
# summary(fixed)
# fixef(fixed)
# random <- plm(NCCF ~ Rank3m, data=panelSet, index=c("FundCode", "RefDate"), model="random")
# summary(random)
# ols <-lm(NCCF ~ Rank3m + AbsPerf3m + RelPerf3m, data=panelSet)
# summary(ols)
# pFtest(fixed, ols)





