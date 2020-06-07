# Create Venn diagrams of BINs detected in 2012, 2016, and 2018.
#setwd("/media/laur/wdhdd1/allNPBW/")
setwd("/home/laur/Schreibtisch/NPBW_raw_data/allNPBW/")

#Get count of 2012 BINs.
data2012 <- read.table("Projects_GlobalMalaise_2012.csv", header=T, sep="\t", stringsAsFactors = F)
cole2012 <- data2012[which(data2012$Order=="Coleoptera"),]
cole2012BINs <- cole2012$BIN
cole_twelve <- unique(cole2012BINs[cole2012BINs != ""])

dip2012 <- data2012[which(data2012$Order=="Diptera"),]
dip2012BINs <- dip2012$BIN
dip_twelve <- unique(dip2012BINs[dip2012BINs != ""])

hym2012 <- data2012[which(data2012$Order=="Hymenoptera"),]
hym2012BINs <- hym2012$BIN
hym_twelve <- unique(hym2012BINs[hym2012BINs != ""])

lep2012 <- data2012[which(data2012$Order=="Lepidoptera"),]
lep2012BINs <- lep2012$BIN
lep_twelve <- unique(lep2012BINs[lep2012BINs != ""])

#2016 and 2018 BINs.
data <- read.table("ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)
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
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]

names(data)
############## Coleoptera ############################
cole_data <- data[which(data$Order=="Coleoptera"), ]
## Subset to contain columns 1 to (14) and 2016:
cole_data2016 <- cbind.data.frame(cole_data[,1:13], cole_data[,grepl("2016", names(cole_data))] )
## Columns 1 to 14 and 2018:
cole_data2018 <- cbind.data.frame(cole_data[,1:13], cole_data[,grepl("2018", names(cole_data))])
## Subset to keep rows (BINs) which are not zero:
cole_data2016 <- cole_data2016[which(rowSums(cole_data2016[14:ncol(cole_data2016)])>0), ]
cole_data2018 <- cole_data2018[which(rowSums(cole_data2018[14:ncol(cole_data2018)])>0), ]

cole_sixteen <- cole_data2016$BIN
cole_eighteen <- cole_data2018$BIN
cole_sixteen <- unique(cole_sixteen[cole_sixteen != ""])
cole_eighteen <- unique(cole_eighteen[cole_eighteen != ""])

cole_twelve_and_cole_sixteen <- intersect(cole_twelve, cole_sixteen)
cole_twelve_and_cole_eighteen <- intersect(cole_twelve, cole_eighteen)
cole_sixteen_and_cole_eighteen <- intersect(cole_sixteen, cole_eighteen)

length(cole_twelve)
length(cole_sixteen)
length(cole_eighteen)
######### Diptera ##################
dip_data <- data[which(data$Order=="Diptera"), ]
dip_data <- dip_data[which(dip_data$Order=="Diptera"), ]
## Subset to contain columns 1 to (14) and 2016:
dip_data2016 <- cbind.data.frame(dip_data[,1:13], dip_data[,grepl("2016", names(dip_data))] )
## Columns 1 to 14 and 2018:
dip_data2018 <- cbind.data.frame(dip_data[,1:13], dip_data[,grepl("2018", names(dip_data))])
## Subset to keep rows (BINs) which are not zero:
dip_data2016 <- dip_data2016[which(rowSums(dip_data2016[14:ncol(dip_data2016)])>0), ]
dip_data2018 <- dip_data2018[which(rowSums(dip_data2018[14:ncol(dip_data2018)])>0), ]

dip_sixteen <- dip_data2016$BIN
dip_eighteen <- dip_data2018$BIN
dip_sixteen <- unique(dip_sixteen[dip_sixteen != ""])
dip_eighteen <- unique(dip_eighteen[dip_eighteen != ""])

dip_twelve_and_dip_sixteen <- intersect(dip_twelve, dip_sixteen)
dip_twelve_and_dip_eighteen <- intersect(dip_twelve, dip_eighteen)
dip_sixteen_and_dip_eighteen <- intersect(dip_sixteen, dip_eighteen)

############# Hymenoptera #######################
hym_data <- data[which(data$Order=="Hymenoptera"), ]
## Subset to contain columns 1 to (14) and 2016:
hym_data2016 <- cbind.data.frame(hym_data[,1:13], hym_data[,grepl("2016", names(hym_data))] )
## Columns 1 to 14 and 2018:
hym_data2018 <- cbind.data.frame(hym_data[,1:13], hym_data[,grepl("2018", names(hym_data))])
## Subset to keep rows (BINs) which are not zero:
hym_data2016 <- hym_data2016[which(rowSums(hym_data2016[14:ncol(hym_data2016)])>0), ]
hym_data2018 <- hym_data2018[which(rowSums(hym_data2018[14:ncol(hym_data2018)])>0), ]

hym_sixteen <- hym_data2016$BIN
hym_eighteen <- hym_data2018$BIN
hym_sixteen <- unique(hym_sixteen[hym_sixteen != ""])
hym_eighteen <- unique(hym_eighteen[hym_eighteen != ""])

hym_twelve_and_hym_sixteen <- intersect(hym_twelve, hym_sixteen)
hym_twelve_and_hym_eighteen <- intersect(hym_twelve, hym_eighteen)
hym_sixteen_and_hym_eighteen <- intersect(hym_sixteen, hym_eighteen)

length(hym_twelve)
length(hym_sixteen)
length(hym_eighteen)
length(hym_sixteen_and_hym_eighteen)
############## Lepidoptera #############################
lep_data <- data[which(data$Order=="Lepidoptera"), ]
## Subset to contain columns 1 to (14) and 2016:
lep_data2016 <- cbind.data.frame(lep_data[,1:13], lep_data[,grepl("2016", names(lep_data))] )
## Columns 1 to 14 and 2018:
lep_data2018 <- cbind.data.frame(lep_data[,1:13], lep_data[,grepl("2018", names(lep_data))])
## Subset to keep rows (BINs) which are not zero:
lep_data2016 <- lep_data2016[which(rowSums(lep_data2016[14:ncol(lep_data2016)])>0), ]
lep_data2018 <- lep_data2018[which(rowSums(lep_data2018[14:ncol(lep_data2018)])>0), ]

lep_sixteen <- lep_data2016$BIN
lep_eighteen <- lep_data2018$BIN
lep_sixteen <- unique(lep_sixteen[lep_sixteen != ""])
lep_eighteen <- unique(lep_eighteen[lep_eighteen != ""])

lep_twelve_and_lep_sixteen <- intersect(lep_twelve, lep_sixteen)
lep_twelve_and_lep_eighteen <- intersect(lep_twelve, lep_eighteen)
lep_sixteen_and_lep_eighteen <- intersect(lep_sixteen, lep_eighteen)

length(lep_twelve)
length(lep_sixteen)
length(lep_eighteen)
length(lep_twelve_and_lep_eighteen)
###############################################
#### New package, for more accurate proportionate visual representation
#install.packages("eulerr")
library(eulerr)
plot.new()
#par(mfrow=c(2,2))
#layout(matrix(c(1,2,3,4),2,2))

# Coleoptera plot
cole_A_and_B_and_C <- intersect(cole_twelve_and_cole_sixteen, cole_eighteen)

c <- euler(c(A=length(cole_twelve),B=length(cole_sixteen),C=length(cole_eighteen),
             "A&B"=length(cole_twelve_and_cole_sixteen),"A&C"=length(cole_twelve_and_cole_eighteen),
             "B&C"=length(cole_sixteen_and_cole_eighteen), "A&B&C"=length(cole_A_and_B_and_C)))

c1 <- plot(
  c,
  fills = c("#CC6666", "#9999CC", "#66CC99"),
  labels = c("2012", "2016", "2018"),
  edges = FALSE,
  main = "Coleoptera"
)

#Diptera plot
dip_A_and_B_and_C <- intersect(dip_twelve_and_dip_sixteen, dip_eighteen)

d <- euler(c(A=length(dip_twelve),B=length(dip_sixteen),C=length(dip_eighteen),
             "A&B"=length(dip_twelve_and_dip_sixteen),"A&C"=length(dip_twelve_and_dip_eighteen),
             "B&C"=length(dip_sixteen_and_dip_eighteen), "A&B&C"=length(dip_A_and_B_and_C)))

d1 <- plot(
  d,
  fills = c("#CC6666", "#9999CC", "#66CC99"),
  labels = c("2012", "2016", "2018"),
  edges = FALSE,
  main = "Diptera"
)

#Hymenoptera plot
hym_A_and_B_and_C <- intersect(hym_twelve_and_hym_sixteen, hym_eighteen)

h <- euler(c(A=length(hym_twelve),B=length(hym_sixteen),C=length(hym_eighteen),
             "A&B"=length(hym_twelve_and_hym_sixteen),"A&C"=length(hym_twelve_and_hym_eighteen),
             "B&C"=length(hym_sixteen_and_hym_eighteen), "A&B&C"=length(hym_A_and_B_and_C)))

h1 <- plot(
  h,
  fills = c("#CC6666", "#9999CC", "#66CC99"),
  labels = c("2012", "2016", "2018"),
  edges = FALSE,
  main = "Hymenoptera"
)

#Lepidoptera plot
lep_A_and_B_and_C <- intersect(lep_twelve_and_lep_sixteen, lep_eighteen)

l <- euler(c(A=length(lep_twelve),B=length(lep_sixteen),C=length(lep_eighteen),
             "A&B"=length(lep_twelve_and_lep_sixteen),"A&C"=length(lep_twelve_and_lep_eighteen),
             "B&C"=length(lep_sixteen_and_lep_eighteen), "A&B&C"=length(lep_A_and_B_and_C)))

l1 <- plot(
  l,
  fills = c("#CC6666", "#9999CC", "#66CC99"),
  labels = c("2012", "2016", "2018"),
  edges = FALSE,
  main = "Lepidoptera"
)

############# Plot all ##########
gridExtra::grid.arrange(grobs=list(c1,d1,h1,l1))
