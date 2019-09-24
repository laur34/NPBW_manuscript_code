# Perform Mantel test between two Jaccard distance matrices of the two years.
# 24.9.2019 LH
#install.packages("vegan")
library(vegan)
#install.packages("betapart")
library(betapart)
#setwd("/media/laur/wdhdd1/NPW_manuscript_code/")
data <- read.table("/media/laur/wdhdd1/NPBW_manuscript_code/Reordered_for_pest_R_5_newFebNPBW_VL2.tsv", header=T, sep="\t", stringsAsFactors = F)

data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA
data$Phylum[data$Phylum == ""] <- NA
data$Class[data$Class == ""] <- NA
data$Order[data$Order == ""] <- NA
#Subset to only the tissue powder ones:
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
names(data)
#write.table(data, file="Pest_table.tsv", quote=F, row.names=F, sep="\t")
#Subset to BINs and sample data
bindata <- data[,c(9,14:ncol(data))]

#Combine by BIN to avoid duplicate row names (changes column 1 name to Group.1).
bindata1 <- aggregate(bindata[,2:ncol(bindata)], by=list(bindata[,1]), FUN=sum )

#Transpose it for BiodiversityR/vegan package (first get rid of the column 1 name, then make it the row names).
bindata2 <- bindata1[,-1]
rownames(bindata2) <- bindata1[,1]
bindata2

tbindata2 <- data.frame(t(bindata2))

#Add metadata about the samples (sample names are now row names)
#tbindata2$year = c(rep("2016", 90), rep("2018", 90))
tbindata2$year = rep(c("2016", "2018"),90)
tbindata2$location =c(rep("outside", 30), rep("inside", 60), rep("outside", 30), rep("inside", 60))
tbindata2$trap = c(rep("Igg", 10), rep("Jos", 10), rep("Sal", 10), rep("T102B", 10), rep("T134B", 10), rep("T152B", 10), rep("T163B", 10), rep("T350B", 10), rep("T464B", 10),
                   rep("Igg", 10), rep("Jos", 10), rep("Sal", 10), rep("T102B", 10), rep("T134B", 10), rep("T152B", 10), rep("T163B", 10), rep("T350B", 10), rep("T464B", 10))
#samplenames = read.table("/media/laur/wdhdd1/NPBW_manuscript_code/trapnames", header=F, stringsAsFactors = F)
samplenames = row.names(tbindata2)
tbindata2$sample = samplenames
tbindata2[1:6,c(1:7,4861:4864)]
#Get rid of row names and move metadata columns from last to first.
rownames(tbindata2) <- c()
mydf <- tbindata2[,c(4861:4864, 1:4860)]

#Jaccard distance matrix with vegdist
jac <- vegdist(mydf[,5:4864], method="jaccard" )


d2016 <- as.dist(as.matrix(jac)[mydf$year=="2016", mydf$year=="2016"])
d2016_by_trap <- as.dist(meandist(d2016, mydf$trap))

d2018 <- as.dist(as.matrix(jac)[mydf$year=="2018", mydf$year=="2018"])
d2018_by_trap <- as.dist(meandist(d2018, mydf$trap))

#Mantel test
#install.packages("ade4")
library(ade4)
mantel(d2016_by_trap, d2018_by_trap, method = "pearson", permutations = 999) # using this one. r = 0.4995, significance = 0.015



# Bray-Curtis:
bc <- vegdist(mydf[,5:4864], method="bray")

b2016 <- as.dist(as.matrix(bc)[mydf$year=="2016", mydf$year=="2016"])
b2018 <- as.dist(as.matrix(bc)[mydf$year=="2018", mydf$year=="2018"])

b2016_by_trap <- as.dist(meandist(b2016, mydf$trap))
b2018_by_trap <- as.dist(meandist(b2018, mydf$trap))

mantel.rtest(b2016_by_trap, b2018_by_trap)
mantel(b2016_by_trap, b2018_by_trap, method="pearson", permutations = 999)

