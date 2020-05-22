## Script to perform ANOSIM test on NPBW samples inside and outside of the Park, for each study year.
## 21.5.2020 LH
## Tutorial at https://jkzorz.github.io/2019/06/11/ANOSIM-test.html

# Read in an OTU table with columns as OTUs and rows as samples, with metadata as (a) column(s).
#install.packages("vegan")
library(vegan)
#setwd("/media/laur/wdhdd11/allNPBW/")
data <- read.table("/home/laur/Schreibtisch/NPBW_manuscript_code/Reordered_for_pest_R_5_newFebNPBW_VL2.tsv", header=T, sep="\t", stringsAsFactors = F)
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
#Split the years here.
bin_year <- bindata[,!grepl("2016", names(bindata1))]
#Combine by BIN to avoid duplicate row names (changes column 1 name to Group.1).
bin_year1 <- aggregate(bin_year[,2:ncol(bin_year)], by=list(bin_year[,1]), FUN=sum )

#Transpose (first get rid of the column 1 name, then make it the row names).
bin_year2 <- bin_year1[,-1]
rownames(bin_year2) <- bin_year1[,1]

tbin_year2 <- data.frame(t(bin_year2))

#write.csv(tbindata2, file="tbindata2_int_anosim.csv")
#Add metadata into a column
tbin_yearm <- cbind.data.frame(tbin_year2, as.vector(c(rep("outside",30),rep("inside",60))) )
ncol(tbin_yearm) #last col is md


#Make the data frame into a matrix of abundance info, after changing row names into a column.
tbin_yearm1 <- cbind(rownames(tbin_yearm), data.frame(tbin_yearm, row.names = NULL))
colnames(tbin_yearm1)[which(names(tbin_yearm1)=="rownames(tbin_yearm)")] <- "SampleName"
colnames(tbin_yearm1)[which(names(tbin_yearm1)=="as.vector.c.rep..outside...30...rep..inside...60...")] <- "Location"



com <- tbin_yearm1[-ncol(tbin_yearm1)]
com <- com[-1]
m_com <- as.matrix(com)

#Now use the function “anosim” from the package vegan to test whether there is a statistical difference between
#groups (inside and outside of the park).
#In the following code,  ‘m_com’ is the community abundance matrix I am testing;
#tbindatam1$Location is the column in df that contains the grouping information for samples.
#‘distance = “bray”’ is the dissimilarity measure (Bray-Curtis) that I would like to use in the analysis; 
#and ‘permutations = 9999’ is the number of permutations done with the data to determine significance.

ano = anosim(m_com, tbin_yearm1$Location, distance = "bray", permutations = 9999)
### wait for it! ###
ano #Reults: ANOSIM statistic: 0.2073,  Significance: 1e-04
# Reject the null.