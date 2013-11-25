library(ggplot2)
library(gridExtra)


# example of NCCF vs. Performance bubble chart
bubexset <- compdata[compdata$FundId == 314 & 
                       as.Date(compdata$RefDate) == as.Date('2013-10-31'),]

#png("testQuality.png", width = 640, height = 640)

ggplot(bubexset, aes(Perf3y, 
                     NCCF3mGBPmn,
                     size = StDev3y, 
                     label=ItemId)) +
  geom_point(aes(colour = ItemType)) +
  geom_text(size=3) +
  xlab("Performance 3y (annualized)") + ylab("NCCF 3m (GBP mn)") +
  labs(title = bubexset[bubexset$ItemId == 'OMGI', "FundName"]) + 
  theme(plot.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("red","orange", "cyan", "green")) + 
  scale_size_continuous(range = c(6, 20), 
                        name = "Standard Dev. 3y\n(annualized)") +
  annotation_custom(
    grob = tableGrob(bubexset[! bubexset$ItemType =="OMGI",
                              c("ItemId", "FundName")],
    show.rownames = FALSE,
    show.box      = FALSE,
    gpar.coretext = gpar(fontsize = 7),
                     show.colnames = FALSE,),  
    xmin = 16, xmax = 25, ymin = 300, ymax = 350)

rm(bubexset)

#dev.off()



