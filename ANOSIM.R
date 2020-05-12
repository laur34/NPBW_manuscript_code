## Script to perform ANOSIM test on NPBW samples inside and outside of the Park.
## 12.5.2020 LH
## Tutorial at https://jkzorz.github.io/2019/06/11/ANOSIM-test.html

# Read in an OTU table with columns as OTUs and rows as samples.
#install.packages("vegan")
library(vegan)
setwd("/media/laur/wdhdd11/allNPBW/")
data <- read.table("/media/laur/wdhdd1/NPBW_manuscript_code/Reordered_for_pest_R_5_newFebNPBW_VL2.tsv", header=T, sep="\t", stringsAsFactors = F)
# Clean/prepare table to meet above conditions.
# Replace blanks with NAs.
data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA
data$Phylum[data$Phylum == ""] <- NA
data$Class[data$Class == ""] <- NA
data$Order[data$Order == ""] <- NA
# Tissue powders only for this comparison - remove others.
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]
head(data)
head(data[13:ncol(data)])
data <- data[13:ncol(data)]

#Subset to BINs and sample data
bindata <- data[,c(9,14:ncol(data))] #See Mantel test script...

#Transpose
#data1 <- data[,-1]
#rownames(data1) <- data[,1]
#pc <- t(data1)
#class(pc)
