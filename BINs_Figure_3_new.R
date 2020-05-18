# Create line graph of numbers of BINs of each of
# four major orders detected throughout the two years.
# 17.5.2020 Version to show overlap of BINs between the two years.

#setwd("/media/laur/wdhdd1/allNPBW/")
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
# Make it presence absence (if better for plots) -
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


#df_2016 <- data2016_ro[ , c(4,9,14:23)]
#df_2018 <- data2018_ro[ , c(4,9,14:23)]
bindata_2016 <- data2016_ro[,c(9,14:23)]
bindata_2018 <- data2018_ro[,c(9,14:23)]

agg2016 <- aggregate(bindata_2016[2:11], by=list(bindata_2016$BIN), FUN=sum )
agg2018 <- aggregate(bindata_2018[2:11], by=list(bindata_2018$BIN), FUN=sum )

# Create vectors of BINs for each year, for each collection, so the intersections can be determined.
BINS_MayI_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_1Mai)` >0)]
BINS_MayII_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_2Mai)` >0)]
BINS_JuneI_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_1Juni)` >0)]
BINS_JuneII_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_2Juni)` >0)]
BINS_JulyI_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_1Juli)` >0)]
BINS_JulyII_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_2Juli)` >0)]
BINS_AugI_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_1Aug)` >0)]
BINS_AugII_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_2Aug)` >0)]
BINS_SepI_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_1Sep)` >0)]
BINS_SepII_2016 <- agg2016$Group.1[which(agg2016$`rowSums(data2016_2Sep)` >0)]

BINS_MayI_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_1Mai)` >0)]
BINS_MayII_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_2Mai)` >0)]
BINS_JuneI_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_1Juni)` >0)]
BINS_JuneII_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_2Juni)` >0)]
BINS_JulyI_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_1Juli)` >0)]
BINS_JulyII_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_2Juli)` >0)]
BINS_AugI_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_1Aug)` >0)]
BINS_AugII_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_2Aug)` >0)]
BINS_SepI_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_1Sep)` >0)]
BINS_SepII_2018 <- agg2018$Group.1[which(agg2018$`rowSums(data2018_2Sep)` >0)]

# Count BINs for each collection, and how many overlapped with the same collection in the other year.
BIN_count_MayI_both <- length(intersect(BINS_MayI_2016, BINS_MayI_2018))
BIN_count_MayII_both <- length(intersect(BINS_MayII_2016, BINS_MayII_2018))
BIN_count_JuneI_both <- length(intersect(BINS_JuneI_2016, BINS_JuneI_2018))
BIN_count_JuneII_both <- length(intersect(BINS_JuneII_2016, BINS_JuneII_2018))
BIN_count_JulyI_both <- length(intersect(BINS_JulyI_2016, BINS_JulyI_2018))
BIN_count_JulyII_both <- length(intersect(BINS_JulyII_2016, BINS_JulyII_2018))
BIN_count_AugI_both <- length(intersect(BINS_AugI_2016, BINS_AugI_2018))
BIN_count_AugII_both <- length(intersect(BINS_AugII_2016, BINS_AugII_2018))
BIN_count_SepI_both <- length(intersect(BINS_SepI_2016, BINS_SepI_2018))
BIN_count_SepII_both <- length(intersect(BINS_SepII_2016, BINS_SepII_2018))

BIN_count_MayI_2016 <- length(BINS_MayI_2016)
BIN_count_MayII_2016 <- length(BINS_MayII_2016)
BIN_count_JuneI_2016 <- length(BINS_JuneI_2016)
BIN_count_JuneII_2016 <- length(BINS_JuneII_2016)
BIN_count_JulyI_2016 <- length(BINS_JulyI_2016)
BIN_count_JulyII_2016 <- length(BINS_JulyII_2016)
BIN_count_AugI_2016 <- length(BINS_AugI_2016)
BIN_count_AugII_2016 <- length(BINS_AugII_2016)
BIN_count_SepI_2016 <- length(BINS_SepI_2016)
BIN_count_SepII_2016 <- length(BINS_SepII_2016)

BIN_count_MayI_2018 <- length(BINS_MayI_2018)
BIN_count_MayII_2018 <- length(BINS_MayII_2018)
BIN_count_JuneI_2018 <- length(BINS_JuneI_2018)
BIN_count_JuneII_2018 <- length(BINS_JuneII_2018)
BIN_count_JulyI_2018 <- length(BINS_JulyI_2018)
BIN_count_JulyII_2018 <- length(BINS_JulyII_2018)
BIN_count_AugI_2018 <- length(BINS_AugI_2018)
BIN_count_AugII_2018 <- length(BINS_AugII_2018)
BIN_count_SepI_2018 <- length(BINS_SepI_2018)
BIN_count_SepII_2018 <- length(BINS_SepII_2018)

collection <- c("May I", "May II", "June I", "June II", "July I", "July II", "Aug. I", "Aug II", "Sep. I", "Sep. II")
BIN_counts_2016 <- c(BIN_count_MayI_2016, BIN_count_MayII_2016, BIN_count_JuneI_2016, BIN_count_JuneII_2016, BIN_count_JulyI_2016, BIN_count_JulyII_2016, BIN_count_AugI_2016, BIN_count_AugII_2016, BIN_count_SepI_2016, BIN_count_SepII_2016)
BIN_counts_2018 <- c(BIN_count_MayI_2018, BIN_count_MayII_2018, BIN_count_JuneI_2018, BIN_count_JuneII_2018, BIN_count_JulyI_2018, BIN_count_JulyII_2018, BIN_count_AugI_2018, BIN_count_AugII_2018, BIN_count_SepI_2018, BIN_count_SepII_2018)
BIN_counts_shared <- c(BIN_count_MayI_both, BIN_count_MayII_both, BIN_count_JuneI_both, BIN_count_JuneII_both, BIN_count_JulyI_both, BIN_count_JulyII_both, BIN_count_AugI_both, BIN_count_AugII_both, BIN_count_SepI_both, BIN_count_SepII_both)

df <- cbind.data.frame(collection, BIN_counts_2016, BIN_counts_2018, BIN_counts_shared)

# Reshape for ggplot2
library(reshape2)
longdf <- melt(data=df, id.vars="collection", variable.name="year", value.name = "counts" )
longdf

library(ggplot2)
g <- ggplot(data=longdf, mapping=aes(x=collection, y=counts, group=year, color=year))
g + geom_line() + theme_classic()





