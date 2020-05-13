## Script to perform ANOSIM test on NPBW samples inside and outside of the Park.
## 12.5.2020 LH
## Tutorial at https://jkzorz.github.io/2019/06/11/ANOSIM-test.html

# Read in an OTU table with columns as OTUs and rows as samples, with metadata as (a) column(s).
#install.packages("vegan")
library(vegan)
#setwd("/media/laur/wdhdd11/allNPBW/")
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


#Subset to BINs and sample data
bindata <- data[,c(9,14:ncol(data))]
#Combine by BIN to avoid duplicate row names (changes column 1 name to Group.1).
bindata1 <- aggregate(bindata[,2:ncol(bindata)], by=list(bindata[,1]), FUN=sum )

#Transpose (first get rid of the column 1 name, then make it the row names).
bindata2 <- bindata1[,-1]
rownames(bindata2) <- bindata1[,1]

tbindata2 <- data.frame(t(bindata2))
#write.csv(tbindata2, file="tbindata2_int_anosim.csv")
#Add metadata into a column
tbindatam <- cbind.data.frame(tbindata2, as.vector(c(rep("outside",60),rep("inside",120))) )
ncol(tbindatam) #last col is md
#Make the data frame into a matrix of abundance info, after changing row names into a column.
tbindatam1 <- cbind(rownames(tbindatam), data.frame(tbindatam, row.names = NULL))
colnames(tbindatam1)[which(names(tbindatam1)=="rownames(tbindatam)")] <- "SampleName"
colnames(tbindatam1)[which(names(tbindatam1)=="as.vector.c.rep..outside...60...rep..inside...120...")] <- "Location"

com <- tbindatam1[-ncol(tbindatam1)]
com <- com[-1]
m_com <- as.matrix(com)

#Now use the function “anosim” from the package vegan to test whether there is a statistical difference between
#groups (inside and outside of the park).
#In the following code,  ‘m_com’ is the community abundance matrix I am testing;
#tbindatam1$Location is the column in df that contains the grouping information for samples.
#‘distance = “bray”’ is the dissimilarity measure (Bray-Curtis) that I would like to use in the analysis; 
#and ‘permutations = 9999’ is the number of permutations done with the data to determine significance.

ano = anosim(m_com, tbindatam1$Location, distance = "bray", permutations = 9999)
### wait for it! ###
ano #Reults: ANOSIM statistic: 0.2073,  Significance: 1e-04
# Reject the null.