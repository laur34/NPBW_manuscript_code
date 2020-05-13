## Generate rarefaction curves for the NPBW samples, for reliability of the data.
## 13.5.2020 LH

library(vegan)
setwd("/media/laur/wdhdd1/NPBW_manuscript_code/")

# Read in OTU table (homogenized samples only) and clean it for R.
data <- read.table("/media/laur/wdhdd1/NPBW_manuscript_code/Reordered_for_pest_R_5_newFebNPBW_VL2.tsv", header=T, sep="\t", stringsAsFactors = F)

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

# Make it a matrix, for use with rarefy
bindata2 <- bindata1[-1]
m_bindata <- as.matrix(bindata2)
# Rarefy ##### Not finished!
S <- specnumber(m_bindata)
raremax <- min(rowSums(m_bindata))
Srare <- rarefy(m_bindata, raremax)
# Create rarecurve
plot(S, Srare, xlab="Observed No. of BINs", ylab="Rarefied No. of BINs")
abline(0,1)
rarecurve(m_bindata, step=20, sample=raremax, col="mediumslateblue", cex=0.6)


############ Other samples (lower read numbers) Ethanol, Filter ##################
# Read in OTU table of ethanol samples and clean it for R.
data <- read.table("/media/laur/wdhdd1/NPBW_manuscript_code/For_R_4-all.otusNPBW-AllApril2018BINsMegablast.tsv", header=T, sep="\t", stringsAsFactors = F)
