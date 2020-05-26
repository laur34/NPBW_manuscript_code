# Create area plots for species of interest, 2016 and 2018 homognate.
# 17.5.2020 Version to remove color padding

library(ggplot2)

#setwd("/media/laur/wdhdd1/allNPBW/")
setwd("/home/laur/Schreibtisch/NPBW_manuscript_code")
data <- read.table("/home/laur/Schreibtisch/NPBW_raw_data/allNPBW/ForR_5_newFeb.otusNPBW-AllApril2018BINsMegablast_VL.tsv", header=T, sep="\t", stringsAsFactors = F)

data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA
data$Phylum[data$Phylum == ""] <- NA
data$Class[data$Class == ""] <- NA
data$Order[data$Order == ""] <- NA
# Tissue powders only:
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]

names(data)
# Change values greater than 0 to 1, so it will be pres-abs:
data[,c(14:ncol(data))] <- (data[,c(14:ncol(data))] != 0)*1

## Subset to contain columns 1 to (14) and 2016:
data2016 <- cbind.data.frame(data[,1:13], data[,grepl("2016", names(data))] )
## Columns 1 to 14 and 2018:
data2018 <- cbind.data.frame(data[,1:13], data[,grepl("2018", names(data))])


# This will exclude the semilysis, due to different naming scheme
summary2016 <- cbind.data.frame(data2016[1:13],
                                rowSums(data2016[ , which(grepl("1LMai", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("2LMai", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("1LJuni", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("2LJuni", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("1LJuli", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("2LJuli", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("1LAug", names(data2016)))]),
                                rowSums(data2016[ , which(grepl("2LAug", names(data2016)))]), 
                                rowSums(data2016[ , which(grepl("1LSep", names(data2016)))]), 
                                rowSums(data2016[ , which(grepl("2LSep", names(data2016)))])  )

names(summary2016)[14:23] <- c("x2016_May1", "x2016_May2", "x2016_June1", "x2016_June2", "x2016_July1", "x2016_July2", "x2016_Aug1", "x2016_Aug2", "x2016_Sep1", "x2016_Sep2")

summary2018 <- cbind.data.frame(data2018[1:13],
                                rowSums(data2018[ , which(grepl("1LMai", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("2LMai", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("1LJuni", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("2LJuni", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("1LJuli", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("2LJuli", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("1LAug", names(data2018)))]),
                                rowSums(data2018[ , which(grepl("2LAug", names(data2018)))]), 
                                rowSums(data2018[ , which(grepl("1LSep", names(data2018)))]), 
                                rowSums(data2018[ , which(grepl("2LSep", names(data2018)))])  )

names(summary2018)[14:23] <- c("x2018_May1", "x2018_May2", "x2018_June1", "x2018_June2", "x2018_July1", "x2018_July2", "x2018_Aug1", "x2018_Aug2", "x2018_Sep1", "x2018_Sep2")

#Epinotia tedella
et_2016 <- summary2016[which(grepl("Epinotia_tedella", summary2016$Species)),]
et_2016 <- et_2016[14:ncol(et_2016)]
et_2016 <- as.numeric(et_2016)

et_2018 <- summary2018[which(grepl("Epinotia_tedella", summary2018$Species)),]
et_2018 <- et_2018[14:ncol(et_2018)]
et_2018 <- as.numeric(et_2018)

#Lissonota dubia
#By BIN, because this species not listed in our table as species:
ldu_2016 <- summary2016[which(grepl("BOLD:ACG5352",summary2016$BIN)),]
ldu_2016 <- ldu_2016[14:ncol(ldu_2016)]
ldu_2016 <- as.numeric(ldu_2016)

ldu_2018 <- summary2018[which(grepl("BOLD:ACG5352",summary2018$BIN)),]
ldu_2018 <- ldu_2018[14:ncol(ldu_2018)]
ldu_2018 <- as.numeric(ldu_2018)


#Plot:
xvar <- c("May I", "May II", "June I", "June II", "July I", "July II", "Aug. I", "Aug. II", "Sep. I", "Sep. II")

collection <- rep(xvar,2)
#num_coll <- c(ld_2016, ld_2018)
year <- c(rep("2016",10) ,rep("2018",10))
num_coll_et <- c(et_2016, et_2018)
num_coll_ldu <- c(ldu_2016, ldu_2018)

df <- cbind.data.frame(collection,num_coll_et,num_coll_ldu,year)
collection_order = c("May I", "May II",  "June I",  "June II", "July I",  "July II", "Aug. I",  "Aug. II", "Sep. I",  "Sep. II")

p <- ggplot(data=df, aes(x=factor(collection, level=collection_order), y=num_coll_et, group=year, color=year)) +  geom_line(linetype="dotted") +
  geom_point(shape=factor(year), size=3) +
  labs(title="Epinotia tedella and Lymantria dispar", x=NULL, y="Number of detections") +
  theme_classic() +
  theme(plot.title = element_text(face="italic"))

p
