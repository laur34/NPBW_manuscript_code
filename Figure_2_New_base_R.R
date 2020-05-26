# Create a new clustered bar chart of 4 orders' BIN counts for 3 years.
# As well as Venn Diagrams to show the overlaps in BINs between years.
#setwd("/media/laur/wdhdd1/allNPBW/")
setwd("/home/laur/Schreibtisch/NPBW_manuscript_code/")

layout(matrix(c(1,1,1,1,2,4,3,5),2,4))

# Calculate total BINs for 4 Orders 2016 and 2018:
data <- read.table("/home/laur/Schreibtisch/NPBW_raw_data/allNPBW/ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)
head(data)
colnames(data)
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
#data <- data[, !grepl("X2018_NPBWpowder", names(data))]
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
names(data)
## Subset to contain columns 1 to (14) and 2016:
data2016 <- cbind.data.frame(data[,1:13], data[,grepl("2016", names(data))] )
## Columns 1 to 14 and 2018:
data2018 <- cbind.data.frame(data[,1:13], data[,grepl("2018", names(data))])
## Subset by Order:
data2016Cole <- data2016[which(data2016$Order=="Coleoptera"),]
data2018Cole <- data2018[which(data2018$Order=="Coleoptera"),]

data2016Dip <- data2016[which(data2016$Order=="Diptera"),]
data2018Dip <- data2018[which(data2018$Order=="Diptera"),]

data2016Hym <- data2016[which(data2016$Order=="Hymenoptera"),]
data2018Hym <- data2018[which(data2018$Order=="Hymenoptera"),]

data2016Lep <- data2016[which(data2016$Order=="Lepidoptera"),]
data2018Lep <- data2018[which(data2018$Order=="Lepidoptera"),]
## Keep only rows that are not 0 (have detections):
data2016Cole <- data2016Cole[which(rowSums(data2016Cole[14:ncol(data2016Cole)])>0),]
data2016Dip <- data2016Dip[which(rowSums(data2016Dip[14:ncol(data2016Dip)])>0),]
data2016Hym <- data2016Hym[which(rowSums(data2016Hym[14:ncol(data2016Hym)])>0),]
data2016Lep <- data2016Lep[which(rowSums(data2016Lep[14:ncol(data2016Lep)])>0),]

data2018Cole <- data2018Cole[which(rowSums(data2018Cole[14:ncol(data2018Cole)])>0),]
data2018Dip <- data2018Dip[which(rowSums(data2018Dip[14:ncol(data2018Dip)])>0),]
data2018Hym <- data2018Hym[which(rowSums(data2018Hym[14:ncol(data2018Hym)])>0),]
data2018Lep <- data2018Lep[which(rowSums(data2018Lep[14:ncol(data2018Lep)])>0),]
## Extract the BINs:
lc2016 <- length(unique(na.omit(data2016Cole$BIN))) #268
ld2016 <- length(unique(na.omit(data2016Dip$BIN))) #2118
lh2016 <- length(unique(na.omit(data2016Hym$BIN))) #730
ll2016 <- length(unique(na.omit(data2016Lep$BIN))) #328

lc2018 <- length(unique(na.omit(data2018Cole$BIN))) #234
ld2018 <- length(unique(na.omit(data2018Dip$BIN))) #1903
lh2018 <- length(unique(na.omit(data2018Hym$BIN))) #709
ll2018 <- length(unique(na.omit(data2018Lep$BIN))) #351

######## 2012
data2012 <- read.table("/home/laur/Schreibtisch/NPBW_raw_data/allNPBW/Projects_GlobalMalaise_2012.csv", header=T, sep="\t", stringsAsFactors = F)

data2012Cole <- data2012[which(data2012$Order=="Coleoptera"), ]
data2012Dip <- data2012[which(data2012$Order=="Diptera"), ]
data2012Hym <- data2012[which(data2012$Order=="Hymenoptera"), ]
data2012Lep <- data2012[which(data2012$Order=="Lepidoptera"), ]

lc2012 <- length(unique(data2012Cole$BIN[data2012Cole$BIN != ""])) #94
ld2012 <- length(unique(data2012Dip$BIN[data2012Dip$BIN != ""])) #1563
lh2012 <- length(unique(data2012Hym$BIN[data2012Hym$BIN != ""])) #678
ll2012 <- length(unique(data2012Lep$BIN[data2012Lep$BIN != ""])) #64


########################### Create Barplot #####################################
df <- cbind.data.frame(c(lc2012,ld2012,lh2012,ll2012), c(lc2016,ld2016,lh2016,ll2016), c(lc2018,ld2018,lh2018,ll2018) )
names(df) <- c("x2012", "x2016", "x2018")
Order <- (c("Coleoptera", "Diptera", "Hymenoptera", "Lepidoptera") )
df <- cbind.data.frame(Order,df)
#
df2 <- df[2:4]
row.names(df2) <- df$Order

barcolors <- c("#CC6666", "#9999CC", "#66CC99")
barplot(t(df2), beside=T, ylab="BINs", col=barcolors,
        main="BINs detected for four major insect orders", font.main=1)
legend("topright", c("2012", "2016", "2018"), fill=barcolors)

################### Create Venn Diagrams #########################
library(venneuler)
#Coleoptera
cole_2012_2016 <- length(intersect(unique(data2012Cole$BIN[data2012Cole$BIN != ""]), data2016Cole$BIN[data2016Cole$BIN != ""]))
cole_2012_2018 <- length(intersect(unique(data2012Cole$BIN[data2012Cole$BIN != ""]), data2018Cole$BIN[data2018Cole$BIN != ""]))
cole_2016_2018 <- length(intersect(unique(data2016Cole$BIN[data2016Cole$BIN != ""]), data2018Cole$BIN[data2018Cole$BIN != ""]))

cole_venn <- venneuler(c(A=lc2012, B=lc2016, C=lc2018, "A&B"=cole_2012_2016, "A&C"=cole_2012_2018, "B&C"=cole_2016_2018))
cole_venn$labels <- c("2012", "2016", "2018")
plot(cole_venn)
title(font.main=1, main="Coleoptera")


#Diptera
dip_2012_2016 <- length(intersect(unique(data2012Dip$BIN[data2012Dip$BIN != ""]), data2016Dip$BIN[data2016Dip$BIN != ""]))
dip_2012_2018 <- length(intersect(unique(data2012Dip$BIN[data2012Dip$BIN != ""]), data2018Dip$BIN[data2018Dip$BIN != ""]))
dip_2016_2018 <- length(intersect(unique(data2016Dip$BIN[data2016Dip$BIN != ""]), data2018Dip$BIN[data2018Dip$BIN != ""]))

dip_venn <- venneuler(c(A=ld2012, B=ld2016, C=ld2018, "A&B"=dip_2012_2016, "A&C"=dip_2012_2018, "B&C"=dip_2016_2018))
dip_venn$labels <- c("2012", "2016", "2018")
plot(dip_venn)
title(font.main=1, main="Diptera")

#Hymenoptera
hym_2012_2016 <- length(intersect(unique(data2012Hym$BIN[data2012Hym$BIN != ""]), data2016Hym$BIN[data2016Hym$BIN != ""]))
hym_2012_2018 <- length(intersect(unique(data2012Hym$BIN[data2012Hym$BIN != ""]), data2018Hym$BIN[data2018Hym$BIN != ""]))
hym_2016_2018 <- length(intersect(unique(data2016Hym$BIN[data2016Hym$BIN != ""]), data2018Hym$BIN[data2018Hym$BIN != ""]))

hym_venn <- venneuler(c(A=lh2012, B=lh2016, C=lh2018, "A&B"=hym_2012_2016, "A&C"=hym_2012_2018, "B&C"=hym_2016_2018))
hym_venn$labels <- c("2012", "2016", "2018")
plot(hym_venn)
title(font.main=1, main="Hymenoptera")

#Lepidoptera
lep_2012_2016 <- length(intersect(unique(data2012Lep$BIN[data2012Lep$BIN != ""]), data2016Lep$BIN[data2016Lep$BIN != ""]))
lep_2012_2018 <- length(intersect(unique(data2012Lep$BIN[data2012Lep$BIN != ""]), data2018Lep$BIN[data2018Lep$BIN != ""]))
lep_2016_2018 <- length(intersect(unique(data2016Lep$BIN[data2016Lep$BIN != ""]), data2018Lep$BIN[data2018Lep$BIN != ""]))

lep_venn <- venneuler(c(A=ll2012, B=ll2016, C=ll2018, "A&B"=lep_2012_2016, "A&C"=lep_2012_2018, "B&C"=lep_2016_2018))
lep_venn$labels <- c("2012", "2016", "2018")
plot(lep_venn)
title(font.main=1, main="Lepidoptera")

