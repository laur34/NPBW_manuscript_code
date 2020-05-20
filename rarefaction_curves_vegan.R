## Generate rarefaction curves for the NPBW samples, for reliability of the data.
## 13.5.2020 LH

library(vegan)
#setwd("/media/laur/wdhdd1/NPBW_manuscript_code/")
setwd("/home/laur/Schreibtisch/NPBW_manuscript_code/")
setwd("/home/laur/Schreibtisch/NPBW_raw_data/allNPBW/")

# Read in OTU table (homogenized samples only) and clean it for R.
data <- read.table("Reordered_for_pest_R_5_newFebNPBW_VL2.tsv", header=T, sep="\t", stringsAsFactors = F)
data <- read.table("ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)

data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA
data$Phylum[data$Phylum == ""] <- NA
data$Class[data$Class == ""] <- NA
data$Order[data$Order == ""] <- NA

# Subset the table to necessary columns (will be using BINs).
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]

bindata <- data[,c(9,14:ncol(data))]

# Combine by BIN to avoid duplicate row names (changes column 1 name to Group.1).
bindata1 <- aggregate(bindata[,2:ncol(bindata)], by=list(bindata[,1]), FUN=sum )

bindata2 <- bindata1[,-1]
rownames(bindata2) <- bindata1[,1]

#t(bindata2)[1:5,1:6]
# Make it a matrix, for use with rarefy
#bindata2 <- bindata2[-1]
m_bindata <- as.matrix(t(bindata2))
#remove rows (BINs) with sum of 0
m_bindata <- m_bindata[which(rowSums(m_bindata)>0),]
#transpose
#tm_bindata <- t(m_bindata)
# Rarefy
S <- specnumber(m_bindata)
raremax <- min(rowSums(m_bindata))
Srare <- rarefy(m_bindata, raremax)
# Create rarecurve
plot(S, Srare, xlab="Observed No. of BINs", ylab="Rarefied No. of BINs")
abline(0,1)
rarecurve(m_bindata, step=20, sample=raremax, col="mediumslateblue", cex=0.6, ylab="BINs", label=NULL)
title(main="Rarefaction curves for homogenized tissue samples")
