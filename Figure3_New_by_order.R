#For BIN-based DB results, create line graph of numbers of BINs of each of
# four major orders detected throughout the two years.
setwd("/home/laur/Schreibtisch/NPBW_raw_data/allNPBW")
data <- read.table("ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)

head(data)
colnames(data)
#Convert blank spaces to "NA":
data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA
data$Phylum[data$Phylum == ""] <- NA
data$Class[data$Class == ""] <- NA
data$Order[data$Order == ""] <- NA

#Subset to only the powder ones:
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
names(data)
ncol(data)
names(data)
# Make it presence absence
# Change values greater than 0 to 1:
data[,c(14:ncol(data))] <- (data[,c(14:ncol(data))] != 0)*1
#Group column data by Month I and II (all traps together)
#first by year:
## Subset to contain columns 1 to (14) and 2016:
data2016 <- cbind.data.frame(data[,1:13], data[,grepl("2016", names(data))] )
## Columns 1 to 14 and 2018:
data2018 <- cbind.data.frame(data[,1:13], data[,grepl("2018", names(data))])

#Reorder by collection period
data2016_tax <- data2016[,1:13]
data2016_1Mai <- data2016[, grepl("1LMai", names(data2016))]
data2016_2Mai <- data2016[, grepl("2LMai", names(data2016))]
data2016_1Juni <- data2016[, grepl("1LJuni", names(data2016))]
data2016_2Juni <- data2016[, grepl("2LJuni", names(data2016))]
data2016_1Juli <- data2016[, grepl("1LJuli", names(data2016))]
data2016_2Juli <- data2016[, grepl("2LJuli", names(data2016))]
data2016_1Aug <- data2016[, grepl("1LAug", names(data2016))]
data2016_2Aug <- data2016[, grepl("2LAug", names(data2016))]
data2016_1Sep <- data2016[, grepl("1LSep", names(data2016))]
data2016_2Sep <- data2016[, grepl("2LSep", names(data2016))]

data2018_tax <- data2018[,1:13]
data2018_1Mai <- data2018[, grepl("1LMai", names(data2018))]
data2018_2Mai <- data2018[, grepl("2LMai", names(data2018))]
data2018_1Juni <- data2018[, grepl("1LJuni", names(data2018))]
data2018_2Juni <- data2018[, grepl("2LJuni", names(data2018))]
data2018_1Juli <- data2018[, grepl("1LJuli", names(data2018))]
data2018_2Juli <- data2018[, grepl("2LJuli", names(data2018))]
data2018_1Aug <- data2018[, grepl("1LAug", names(data2018))]
data2018_2Aug <- data2018[, grepl("2LAug", names(data2018))]
data2018_1Sep <- data2018[, grepl("1LSep", names(data2018))]
data2018_2Sep <- data2018[, grepl("2LSep", names(data2018))]


data2016_ro <- cbind.data.frame(data2016_tax, rowSums(data2016_1Mai), rowSums(data2016_2Mai),
                                rowSums(data2016_1Juni), rowSums(data2016_2Juni),
                                rowSums(data2016_1Juli), rowSums(data2016_2Juli),
                                rowSums(data2016_1Aug),  rowSums(data2016_2Aug),
                                rowSums(data2016_1Sep), rowSums(data2016_2Sep) )

data2018_ro <- cbind.data.frame(data2018_tax, rowSums(data2018_1Mai), rowSums(data2018_2Mai),
                                rowSums(data2018_1Juni), rowSums(data2018_2Juni),
                                rowSums(data2018_1Juli), rowSums(data2018_2Juli),
                                rowSums(data2018_1Aug),  rowSums(data2018_2Aug),
                                rowSums(data2018_1Sep), rowSums(data2018_2Sep) )

newdf <- cbind.data.frame(data2016_ro$BIN, data2016_ro$Order, data2016_ro[14:ncol(data2016_ro)],data2018_ro[14:ncol(data2018_ro)])

colnames(newdf) <- c("BIN", "Order", "MayI_2016", "MayII_2016", "JuneI_2016", "JuneII_2016", "JulyI_2016", "JulyII_2016", "AugI_2016", "AugII_2016", "SepI_2016", "SepII_2016", "MayI_2018", "MayII_2018", "JuneI_2018", "JuneII_2018", "JulyI_2018", "JulyII_2018", "AugI_2018", "AugII_2018", "SepI_2018", "SepII_2018")




#Create a presence-absence version at this point for additional line.
pa <- (newdf[3:ncol(newdf)] != 0)*1
padf <- cbind.data.frame(newdf[1:2], pa)


#Add shared data columns
newdf$MayI_both <- as.numeric((newdf$MayI_2016 > 0) & (newdf$MayI_2018 >0))
newdf$MayII_both <- as.numeric((newdf$MayII_2016 > 0) & (newdf$MayII_2018 >0))
newdf$JuneI_both <- as.numeric((newdf$JuneI_2016 > 0) & (newdf$JuneI_2018 >0))
newdf$JuneII_both <- as.numeric((newdf$JuneII_2016 > 0) & (newdf$JuneII_2018 >0))
newdf$JulyI_both <- as.numeric((newdf$JulyI_2016 > 0) & (newdf$JulyI_2018 >0))
newdf$JulyII_both <- as.numeric((newdf$JulyII_2016 > 0) & (newdf$JulyII_2018 >0))
newdf$AugI_both <- as.numeric((newdf$AugI_2016 > 0) & (newdf$AugI_2018 >0))
newdf$AugII_both <- as.numeric((newdf$AugII_2016 > 0) & (newdf$AugII_2018 >0))
newdf$SepI_both <- as.numeric((newdf$SepI_2016 > 0) & (newdf$SepI_2018 >0))
newdf$SepII_both <- as.numeric((newdf$SepII_2016 > 0) & (newdf$SepII_2018 >0))

#### Add 2016 and 2018 columns Presence-Absence
newdf$MayI_2016_PA <- padf$MayI_2016
newdf$MayII_2016_PA <- padf$MayII_2016
newdf$JuneI_2016_PA <- padf$JuneI_2016
newdf$JuneII_2016_PA <- padf$JuneII_2016
newdf$JulyI_2016_PA <- padf$JulyI_2016
newdf$JulyII_2016_PA <- padf$JulyII_2016
newdf$AugI_2016_PA <- padf$AugI_2016
newdf$AugII_2016_PA <- padf$AugII_2016
newdf$SepI_2016_PA <- padf$SepI_2016
newdf$SepII_2016_PA <- padf$SepII_2016

newdf$MayI_2018_PA <- padf$MayI_2018
newdf$MayII_2018_PA <- padf$MayII_2018
newdf$JuneI_2018_PA <- padf$JuneI_2018
newdf$JuneII_2018_PA <- padf$JuneII_2018
newdf$JulyI_2018_PA <- padf$JulyI_2018
newdf$JulyII_2018_PA <- padf$JulyII_2018
newdf$AugI_2018_PA <- padf$AugI_2018
newdf$AugII_2018_PA <- padf$AugII_2018
newdf$SepI_2018_PA <- padf$SepI_2018
newdf$SepII_2018_PA <- padf$SepII_2018
######

#Aggregate by order
ag <- aggregate(newdf[3:ncol(newdf)], by=list(newdf$Order), FUN=sum)
#Keep only orders of interest
ag_ord <- subset(ag, ag$Group.1=="Diptera" | ag$Group.1=="Coleoptera" | ag$Group.1=="Lepidoptera" | ag$Group.1=="Hymenoptera")
rownames(ag_ord) <- c()

names(ag_ord)[names(ag_ord)=="Group.1"] <- "Order"

#Transpose for ggplot2
orders <- ag_ord$Order
orders <- c("Coleoptera", "Diptera", "Hymenoptera", "Lepidoptera")
tdf <- as.data.frame(t(ag_ord[,-1]))
colnames(tdf) <- orders
library(data.table)
setDT(tdf, keep.rownames = TRUE)[]
tdf
tdf$year <- c(rep("2016",10),rep("2018",10),rep("shared",10),rep("2016_PA",10), rep("2018_PA",10))
tdf$collection <- rep(c("May_I","May_II","June_I","June_II","July_I","July_II","Aug_I","Aug_II","Sep_I","Sep_II"),5)


tdf <- tdf[,-1]
library(ggplot2)
collection_order <- c("May_I","May_II","June_I","June_II","July_I","July_II","Aug_I","Aug_II","Sep_I","Sep_II")
year_order <- c("2016", "2016_PA", "2018", "2018_PA", "shared")

g <- ggplot(data=tdf, aes(x=factor(collection, level=collection_order), y=Diptera, group=year ))+
  geom_line(aes(linetype=year, color=factor(year,levels=year_order))) +
  xlab(NULL) + ylab("BINs detected") + labs(color='year') +
  scale_linetype_manual(values=c("solid","dotted","solid","dotted","solid")) +
  scale_color_manual(values=c("#56B4E9", "#56B4E9", "#E69F00", "#E69F00", "#999999"), labels=c("2016","2016_PA","2018","2018_PA","shared") ) +
  theme_classic()

