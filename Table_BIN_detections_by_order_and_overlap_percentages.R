# Get counts of BIN detections in 2016, and 2018.
# For 2018, do both homogenized tissue and all sources.
# Also, get the overlap
# "Comparison of total BIN detections within malaise trap surveys in 2016 and 2018. The overlap indicates the number of identical BINs detected in both survey years"

setwd("/home/laur/Schreibtisch/NPBW_raw_data/allNPBW/")

#2016 and 2018 BINs ##################### all methods ########################################################################
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
#Get rid of extra columns
#data <- data[, !grepl("Etoh", names(data))]
#data <- data[, !grepl("semi", names(data))]
#data <- data[, !grepl("filter", names(data))]
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]

names(data)
#2016
data2016 <- data[, !grepl("2018", names(data))]
present2016 <- data2016[which( rowSums(data2016[ , 14:ncol(data2016)]) >0 ),]
diptera2016 <- present2016[present2016$Order=="Diptera", ]
diptera2016 <- diptera2016[which(rowSums(diptera2016[, 14:ncol(diptera2016)]) >0), ]
length(diptera2016$BIN[diptera2016$BIN != ""]) #2385
length(unique(diptera2016$BIN[diptera2016$BIN != ""])) #2119

coleoptera2016 <- present2016[present2016$Order=="Coleoptera", ]
coleoptera2016 <- coleoptera2016[which(rowSums(coleoptera2016[, 14:ncol(coleoptera2016)]) >0), ]
length(coleoptera2016$BIN[coleoptera2016$BIN != ""]) #283
length(unique(coleoptera2016$BIN[coleoptera2016$BIN != ""])) #268

hymenoptera2016 <- present2016[present2016$Order=="Hymenoptera", ]
hymenoptera2016 <- hymenoptera2016[which(rowSums(hymenoptera2016[, 14:ncol(hymenoptera2016)]) >0), ]
length(hymenoptera2016$BIN[hymenoptera2016$BIN != ""]) #783
length(unique(hymenoptera2016$BIN[hymenoptera2016$BIN != ""])) #731

lepidoptera2016 <- present2016[present2016$Order=="Lepidoptera", ]
lepidoptera2016 <- lepidoptera2016[which(rowSums(lepidoptera2016[, 14:ncol(lepidoptera2016)]) >0), ]
length(lepidoptera2016$BIN[lepidoptera2016$BIN != ""]) #340
length(unique(lepidoptera2016$BIN[lepidoptera2016$BIN != ""])) #328


#2018
data2018 <- data[, !grepl("2016", names(data))]
present2018 <- data2018[which( rowSums(data2018[ , 14:ncol(data2018)]) >0 ),]
diptera2018 <- present2018[present2018$Order=="Diptera", ]
diptera2018 <- diptera2018[which(rowSums(diptera2018[, 14:ncol(diptera2018)]) >0), ]
length(diptera2018$BIN[diptera2018$BIN != ""]) #2545
length(unique(diptera2018$BIN[diptera2018$BIN != ""])) #2021

coleoptera2018 <- present2018[present2018$Order=="Coleoptera", ]
coleoptera2018 <- coleoptera2018[which(rowSums(coleoptera2018[, 14:ncol(coleoptera2018)]) >0), ]
length(coleoptera2018$BIN[coleoptera2018$BIN != ""]) #277
length(unique(coleoptera2018$BIN[coleoptera2018$BIN != ""])) #257

hymenoptera2018 <- present2018[present2018$Order=="Hymenoptera", ]
hymenoptera2018 <- hymenoptera2018[which(rowSums(hymenoptera2018[, 14:ncol(hymenoptera2018)]) >0), ]
length(hymenoptera2018$BIN[hymenoptera2018$BIN != ""]) #897
length(unique(hymenoptera2018$BIN[hymenoptera2018$BIN != ""])) #788

lepidoptera2018 <- present2018[present2018$Order=="Lepidoptera", ]
lepidoptera2018 <- lepidoptera2018[which(rowSums(lepidoptera2018[, 14:ncol(lepidoptera2018)]) >0), ]
length(lepidoptera2018$BIN[lepidoptera2018$BIN != ""]) #405
length(unique(lepidoptera2018$BIN[lepidoptera2018$BIN != ""])) #372

#################################################### 2018 homogenized tissue only ###############################################
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
#Get rid of extra columns
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]

data2018 <- data[, !grepl("2016", names(data))]
present2018 <- data2018[which( rowSums(data2018[ , 14:ncol(data2018)]) >0 ),]
#Diptera 2018 tissue
diptera2018 <- present2018[present2018$Order=="Diptera", ]
diptera2018 <- diptera2018[which(rowSums(diptera2018[, 14:ncol(diptera2018)]) >0), ]
length(unique(diptera2016$BIN[diptera2016$BIN != ""]))
length(unique(diptera2018$BIN[diptera2018$BIN != ""]))
l <- length(intersect(diptera2016$BIN[diptera2016$BIN!=""], diptera2018$BIN[diptera2018$BIN != ""] ))
u <- length(union(diptera2018$BIN[diptera2018$BIN != ""], diptera2016$BIN[diptera2016$BIN!=""] ))
o <- l/u
o


coleoptera2018 <- present2018[present2018$Order=="Coleoptera", ]
coleoptera2018 <- coleoptera2018[which(rowSums(coleoptera2018[, 14:ncol(coleoptera2018)]) >0), ]
length(coleoptera2018$BIN[coleoptera2018$BIN != ""]) #250
length(unique(coleoptera2018$BIN[coleoptera2018$BIN != ""])) #234
#overlaps
l <- length(intersect(coleoptera2016$BIN[coleoptera2016$BIN!=""], coleoptera2018$BIN[coleoptera2018$BIN != ""] )) #144

hymenoptera2018 <- present2018[present2018$Order=="Hymenoptera", ]
hymenoptera2018 <- hymenoptera2018[which(rowSums(hymenoptera2018[, 14:ncol(hymenoptera2018)]) >0), ]
length(hymenoptera2018$BIN[hymenoptera2018$BIN != ""]) #774
length(unique(hymenoptera2018$BIN[hymenoptera2018$BIN != ""])) #709
#overlaps
l <- length(intersect(hymenoptera2016$BIN[hymenoptera2016$BIN!=""], hymenoptera2018$BIN[hymenoptera2018$BIN != ""] )) #446

lepidoptera2018 <- present2018[present2018$Order=="Lepidoptera", ]
lepidoptera2018 <- lepidoptera2018[which(rowSums(lepidoptera2018[, 14:ncol(lepidoptera2018)]) >0), ]
length(lepidoptera2018$BIN[lepidoptera2018$BIN != ""]) #369
length(unique(lepidoptera2018$BIN[lepidoptera2018$BIN != ""])) #351
#overlaps
l <- length(intersect(lepidoptera2016$BIN[lepidoptera2016$BIN!=""], lepidoptera2018$BIN[lepidoptera2018$BIN != ""] )) #208

####
#Araneae
araneae2016 <- present2016[present2016$Order=="Araneae", ]
araneae2016 <- araneae2016[which(rowSums(araneae2016[, 14:ncol(araneae2016)]) >0), ]
araneae2018 <- present2018[present2018$Order=="Araneae", ]
araneae2018 <- araneae2018[which(rowSums(araneae2018[, 14:ncol(araneae2018)]) >0), ]
#length(araneae2018$BIN[araneae2018$BIN != ""]) #43
#length(unique(araneae2018$BIN[araneae2018$BIN != ""])) #42
#overlaps
#l <- length(intersect(araneae2016$BIN[araneae2016$BIN!=""], araneae2018$BIN[araneae2018$BIN != ""] )) #32
length(unique(araneae2016$BIN[araneae2016$BIN != ""])) #67
length(unique(araneae2018$BIN[araneae2018$BIN != ""])) #42
l <- length(intersect(araneae2016$BIN[araneae2016$BIN!=""], araneae2018$BIN[araneae2018$BIN != ""] ))
u <- length(union(araneae2018$BIN[araneae2018$BIN != ""], araneae2016$BIN[araneae2016$BIN!=""] ))
o <- l/u
o #0.4155844



#Mesostigmata
mesostigmata2016 <- present2016[present2016$Order=="Mesostigmata", ]
mesostigmata2016 <- mesostigmata2016[which(rowSums(mesostigmata2016[, 14:ncol(mesostigmata2016)]) >0), ]
mesostigmata2018 <- present2018[present2018$Order=="Mesostigmata", ]
mesostigmata2018 <- mesostigmata2018[which(rowSums(mesostigmata2018[, 14:ncol(mesostigmata2018)]) >0), ]
#length(mesostigmata2018$BIN[mesostigmata2018$BIN != ""]) #3
#length(unique(mesostigmata2018$BIN[mesostigmata2018$BIN != ""])) #1
#overlaps
#l <- length(intersect(mesostigmata2016$BIN[mesostigmata2016$BIN!=""], mesostigmata2018$BIN[mesostigmata2018$BIN != ""] )) #1
length(unique(mesostigmata2016$BIN[mesostigmata2016$BIN != ""])) #2
length(unique(mesostigmata2018$BIN[mesostigmata2018$BIN != ""])) #3
l <- length(intersect(mesostigmata2016$BIN[mesostigmata2016$BIN!=""], mesostigmata2018$BIN[mesostigmata2018$BIN != ""] ))
u <- length(union(mesostigmata2018$BIN[mesostigmata2018$BIN != ""], mesostigmata2016$BIN[mesostigmata2016$BIN!=""] ))
o <- l/u
o #0.25



#Opiliones
opiliones2016 <- present2016[present2016$Order=="Opiliones", ]
opiliones2016 <- opiliones2016[which(rowSums(opiliones2016[, 14:ncol(opiliones2016)]) >0), ]
opiliones2018 <- present2018[present2018$Order=="Opiliones", ]
opiliones2018 <- opiliones2018[which(rowSums(opiliones2018[, 14:ncol(opiliones2018)]) >0), ]
#length(opiliones2018$BIN[opiliones2018$BIN != ""]) #6
#length(unique(opiliones2018$BIN[opiliones2018$BIN != ""])) #4
#overlaps
#l <- length(intersect(opiliones2016$BIN[opiliones2016$BIN!=""], opiliones2018$BIN[opiliones2018$BIN != ""] )) #2
length(unique(opiliones2016$BIN[opiliones2016$BIN != ""])) #2
length(unique(opiliones2018$BIN[opiliones2018$BIN != ""])) #4
l <- length(intersect(opiliones2016$BIN[opiliones2016$BIN!=""], opiliones2018$BIN[opiliones2018$BIN != ""] ))
u <- length(union(opiliones2018$BIN[opiliones2018$BIN != ""], opiliones2016$BIN[opiliones2016$BIN!=""] ))
o <- l/u
o #0.5



#Sarcoptiformes
sarcoptiformes2016 <- present2016[present2016$Order=="Sarcoptiformes", ]
sarcoptiformes2016 <- sarcoptiformes2016[which(rowSums(sarcoptiformes2016[, 14:ncol(sarcoptiformes2016)]) >0), ]
sarcoptiformes2018 <- present2018[present2018$Order=="Sarcoptiformes", ]
sarcoptiformes2018 <- sarcoptiformes2018[which(rowSums(sarcoptiformes2018[, 14:ncol(sarcoptiformes2018)]) >0), ]
#length(sarcoptiformes2018$BIN[sarcoptiformes2018$BIN != ""]) #2
#length(unique(sarcoptiformes2018$BIN[sarcoptiformes2018$BIN != ""])) #2
#overlaps
#l <- length(intersect(sarcoptiformes2016$BIN[sarcoptiformes2016$BIN!=""], sarcoptiformes2018$BIN[sarcoptiformes2018$BIN != ""] )) #2
length(unique(sarcoptiformes2016$BIN[sarcoptiformes2016$BIN != ""])) #2
length(unique(sarcoptiformes2018$BIN[sarcoptiformes2018$BIN != ""])) #2
l <- length(intersect(sarcoptiformes2016$BIN[sarcoptiformes2016$BIN!=""], sarcoptiformes2018$BIN[sarcoptiformes2018$BIN != ""] ))
u <- length(union(sarcoptiformes2018$BIN[sarcoptiformes2018$BIN != ""], sarcoptiformes2016$BIN[sarcoptiformes2016$BIN!=""] ))
o <- l/u
o #1


#Diptera (again)
length(unique(diptera2016$BIN[diptera2016$BIN != ""]))
length(unique(diptera2018$BIN[diptera2018$BIN != ""]))
l <- length(intersect(diptera2016$BIN[diptera2016$BIN!=""], diptera2018$BIN[diptera2018$BIN != ""] ))
u <- length(union(diptera2018$BIN[diptera2018$BIN != ""], diptera2016$BIN[diptera2016$BIN!=""] ))
o <- l/u
o


#Entomobryomorpha
entomobryomorpha2016 <- present2016[present2016$Order=="Entomobryomorpha", ]
entomobryomorpha2016 <- entomobryomorpha2016[which(rowSums(entomobryomorpha2016[, 14:ncol(entomobryomorpha2016)]) >0), ]
entomobryomorpha2018 <- present2018[present2018$Order=="Entomobryomorpha", ]
entomobryomorpha2018 <- entomobryomorpha2018[which(rowSums(entomobryomorpha2018[, 14:ncol(entomobryomorpha2018)]) >0), ]
#length(entomobryomorpha2018$BIN[entomobryomorpha2018$BIN != ""]) #12
#length(unique(entomobryomorpha2018$BIN[entomobryomorpha2018$BIN != ""])) #6
#overlaps
#l <- length(intersect(entomobryomorpha2016$BIN[entomobryomorpha2016$BIN!=""], entomobryomorpha2018$BIN[entomobryomorpha2018$BIN != ""] )) #5
length(unique(entomobryomorpha2016$BIN[entomobryomorpha2016$BIN != ""])) #6
length(unique(entomobryomorpha2018$BIN[entomobryomorpha2018$BIN != ""])) #6
l <- length(intersect(entomobryomorpha2016$BIN[entomobryomorpha2016$BIN!=""], entomobryomorpha2018$BIN[entomobryomorpha2018$BIN != ""] ))
u <- length(union(entomobryomorpha2018$BIN[entomobryomorpha2018$BIN != ""], entomobryomorpha2016$BIN[entomobryomorpha2016$BIN!=""] ))
o <- l/u
o #0.7142857



#Symphypleona
symphypleona2016 <- present2016[present2016$Order=="Symphypleona", ]
symphypleona2016 <- symphypleona2016[which(rowSums(symphypleona2016[, 14:ncol(symphypleona2016)]) >0), ]
symphypleona2018 <- present2018[present2018$Order=="Symphypleona", ]
symphypleona2018 <- symphypleona2018[which(rowSums(symphypleona2018[, 14:ncol(symphypleona2018)]) >0), ]
#length(symphypleona2018$BIN[symphypleona2018$BIN != ""]) #2
#length(unique(symphypleona2018$BIN[symphypleona2018$BIN != ""])) #2
#overlaps
#l <- length(intersect(symphypleona2016$BIN[symphypleona2016$BIN!=""], symphypleona2018$BIN[symphypleona2018$BIN != ""] )) #2
length(unique(symphypleona2016$BIN[symphypleona2016$BIN != ""])) #4
length(unique(symphypleona2018$BIN[symphypleona2018$BIN != ""])) #2
l <- length(intersect(symphypleona2016$BIN[symphypleona2016$BIN!=""], symphypleona2018$BIN[symphypleona2018$BIN != ""] ))
u <- length(union(symphypleona2018$BIN[symphypleona2018$BIN != ""], symphypleona2016$BIN[symphypleona2016$BIN!=""] ))
o <- l/u
o #0.5



#Blattodea
blattodea2016 <- present2016[present2016$Order=="Blattodea", ]
blattodea2016 <- blattodea2016[which(rowSums(blattodea2016[, 14:ncol(blattodea2016)]) >0), ]
blattodea2018 <- present2018[present2018$Order=="Blattodea", ]
blattodea2018 <- blattodea2018[which(rowSums(blattodea2018[, 14:ncol(blattodea2018)]) >0), ]
#length(blattodea2018$BIN[blattodea2018$BIN != ""]) #4
#length(unique(blattodea2018$BIN[blattodea2018$BIN != ""])) #3
#overlaps
#l <- length(intersect(blattodea2016$BIN[blattodea2016$BIN!=""], blattodea2018$BIN[blattodea2018$BIN != ""] )) #2
length(unique(blattodea2016$BIN[blattodea2016$BIN != ""])) #2
length(unique(blattodea2018$BIN[blattodea2018$BIN != ""])) #3
l <- length(intersect(blattodea2016$BIN[blattodea2016$BIN!=""], blattodea2018$BIN[blattodea2018$BIN != ""] ))
u <- length(union(blattodea2018$BIN[blattodea2018$BIN != ""], blattodea2016$BIN[blattodea2016$BIN!=""] ))
o <- l/u
o #0.6666667

#Coleoptera
coleoptera2018 <- present2018[present2018$Order=="Coleoptera", ]
coleoptera2018 <- coleoptera2018[which(rowSums(coleoptera2018[, 14:ncol(coleoptera2018)]) >0), ]
length(unique(coleoptera2016$BIN[coleoptera2016$BIN != ""])) #268
length(unique(coleoptera2018$BIN[coleoptera2018$BIN != ""])) #234
l <- length(intersect(coleoptera2016$BIN[coleoptera2016$BIN!=""], coleoptera2018$BIN[coleoptera2018$BIN != ""] ))
u <- length(union(coleoptera2018$BIN[coleoptera2018$BIN != ""], coleoptera2016$BIN[coleoptera2016$BIN!=""] ))
o <- l/u
o #0.4022



#Dermaptera
dermaptera2016 <- present2016[present2016$Order=="Dermaptera", ]
dermaptera2016 <- dermaptera2016[which(rowSums(dermaptera2016[, 14:ncol(dermaptera2016)]) >0), ]
dermaptera2018 <- present2018[present2018$Order=="Dermaptera", ]
dermaptera2018 <- dermaptera2018[which(rowSums(dermaptera2018[, 14:ncol(dermaptera2018)]) >0), ]
#length(dermaptera2018$BIN[dermaptera2018$BIN != ""]) #5
#length(unique(dermaptera2018$BIN[dermaptera2018$BIN != ""])) #3
#overlaps
#l <- length(intersect(dermaptera2016$BIN[dermaptera2016$BIN!=""], dermaptera2018$BIN[dermaptera2018$BIN != ""] )) #3
length(unique(dermaptera2016$BIN[dermaptera2016$BIN != ""])) #3
length(unique(dermaptera2018$BIN[dermaptera2018$BIN != ""])) #3
l <- length(intersect(dermaptera2016$BIN[dermaptera2016$BIN!=""], dermaptera2018$BIN[dermaptera2018$BIN != ""] ))
u <- length(union(dermaptera2018$BIN[dermaptera2018$BIN != ""], dermaptera2016$BIN[dermaptera2016$BIN!=""] ))
o <- l/u
o #1


#Ephemeroptera
ephemeroptera2016 <- present2016[present2016$Order=="Ephemeroptera", ]
ephemeroptera2016 <- ephemeroptera2016[which(rowSums(ephemeroptera2016[, 14:ncol(ephemeroptera2016)]) >0), ]
ephemeroptera2018 <- present2018[present2018$Order=="Ephemeroptera", ]
ephemeroptera2018 <- ephemeroptera2018[which(rowSums(ephemeroptera2018[, 14:ncol(ephemeroptera2018)]) >0), ]
#length(ephemeroptera2018$BIN[ephemeroptera2018$BIN != ""]) #2
#length(unique(ephemeroptera2018$BIN[ephemeroptera2018$BIN != ""])) #2
#overlaps
#l <- length(intersect(ephemeroptera2016$BIN[ephemeroptera2016$BIN!=""], ephemeroptera2018$BIN[ephemeroptera2018$BIN != ""] )) #0
length(unique(ephemeroptera2016$BIN[ephemeroptera2016$BIN != ""])) #2
length(unique(ephemeroptera2018$BIN[ephemeroptera2018$BIN != ""])) #2
l <- length(intersect(ephemeroptera2016$BIN[ephemeroptera2016$BIN!=""], ephemeroptera2018$BIN[ephemeroptera2018$BIN != ""] ))
u <- length(union(ephemeroptera2018$BIN[ephemeroptera2018$BIN != ""], ephemeroptera2016$BIN[ephemeroptera2016$BIN!=""] ))
o <- l/u
o #0


#Hemiptera
hemiptera2016 <- present2016[present2016$Order=="Hemiptera", ]
hemiptera2016 <- hemiptera2016[which(rowSums(hemiptera2016[, 14:ncol(hemiptera2016)]) >0), ]
hemiptera2018 <- present2018[present2018$Order=="Hemiptera", ]
hemiptera2018 <- hemiptera2018[which(rowSums(hemiptera2018[, 14:ncol(hemiptera2018)]) >0), ]
#length(hemiptera2018$BIN[hemiptera2018$BIN != ""]) #103
#length(unique(hemiptera2018$BIN[hemiptera2018$BIN != ""])) #92
#overlaps
#l <- length(intersect(hemiptera2016$BIN[hemiptera2016$BIN!=""], hemiptera2018$BIN[hemiptera2018$BIN != ""] )) #59
length(unique(hemiptera2016$BIN[hemiptera2016$BIN != ""])) #94
length(unique(hemiptera2018$BIN[hemiptera2018$BIN != ""])) #92
l <- length(intersect(hemiptera2016$BIN[hemiptera2016$BIN!=""], hemiptera2018$BIN[hemiptera2018$BIN != ""] ))
u <- length(union(hemiptera2018$BIN[hemiptera2018$BIN != ""], hemiptera2016$BIN[hemiptera2016$BIN!=""] ))
o <- l/u
o #0.4645669

#Hymenoptera
hymenoptera2018 <- present2018[present2018$Order=="Hymenoptera", ]
hymenoptera2018 <- hymenoptera2018[which(rowSums(hymenoptera2018[, 14:ncol(hymenoptera2018)]) >0), ]
length(unique(hymenoptera2016$BIN[hymenoptera2016$BIN != ""])) #731
length(unique(hymenoptera2018$BIN[hymenoptera2018$BIN != ""])) #709
l <- length(intersect(hymenoptera2016$BIN[hymenoptera2016$BIN!=""], hymenoptera2018$BIN[hymenoptera2018$BIN != ""] ))
u <- length(union(hymenoptera2018$BIN[hymenoptera2018$BIN != ""], hymenoptera2016$BIN[hymenoptera2016$BIN!=""] ))
o <- l/u
o #0.4486922


#Lepidoptera
lepidoptera2018 <- present2018[present2018$Order=="Lepidoptera", ]
lepidoptera2018 <- lepidoptera2018[which(rowSums(lepidoptera2018[, 14:ncol(lepidoptera2018)]) >0), ]
length(unique(lepidoptera2016$BIN[lepidoptera2016$BIN != ""])) #328
length(unique(lepidoptera2018$BIN[lepidoptera2018$BIN != ""])) #351
l <- length(intersect(lepidoptera2016$BIN[lepidoptera2016$BIN!=""], lepidoptera2018$BIN[lepidoptera2018$BIN != ""] ))
u <- length(union(lepidoptera2018$BIN[lepidoptera2018$BIN != ""], lepidoptera2016$BIN[lepidoptera2016$BIN!=""] ))
o <- l/u
o #0.4416


#Mecoptera
mecoptera2016 <- present2016[present2016$Order=="Mecoptera", ]
mecoptera2016 <- mecoptera2016[which(rowSums(mecoptera2016[, 14:ncol(mecoptera2016)]) >0), ]
mecoptera2018 <- present2018[present2018$Order=="Mecoptera", ]
mecoptera2018 <- mecoptera2018[which(rowSums(mecoptera2018[, 14:ncol(mecoptera2018)]) >0), ]
#length(mecoptera2018$BIN[mecoptera2018$BIN != ""]) #4
#length(unique(mecoptera2018$BIN[mecoptera2018$BIN != ""])) #3
#overlaps
#l <- length(intersect(mecoptera2016$BIN[mecoptera2016$BIN!=""], mecoptera2018$BIN[mecoptera2018$BIN != ""] )) #3
length(unique(mecoptera2016$BIN[mecoptera2016$BIN != ""])) #3
length(unique(mecoptera2018$BIN[mecoptera2018$BIN != ""])) #3
l <- length(intersect(mecoptera2016$BIN[mecoptera2016$BIN!=""], mecoptera2018$BIN[mecoptera2018$BIN != ""] ))
u <- length(union(mecoptera2018$BIN[mecoptera2018$BIN != ""], mecoptera2016$BIN[mecoptera2016$BIN!=""] ))
o <- l/u
o #1


#Neuroptera
neuroptera2016 <- present2016[present2016$Order=="Neuroptera", ]
neuroptera2016 <- neuroptera2016[which(rowSums(neuroptera2016[, 14:ncol(neuroptera2016)]) >0), ]
neuroptera2018 <- present2018[present2018$Order=="Neuroptera", ]
neuroptera2018 <- neuroptera2018[which(rowSums(neuroptera2018[, 14:ncol(neuroptera2018)]) >0), ]
#length(neuroptera2018$BIN[neuroptera2018$BIN != ""]) #18
#length(unique(neuroptera2018$BIN[neuroptera2018$BIN != ""])) #17
#overlaps
#l <- length(intersect(neuroptera2016$BIN[neuroptera2016$BIN!=""], neuroptera2018$BIN[neuroptera2018$BIN != ""] )) #11
length(unique(neuroptera2016$BIN[neuroptera2016$BIN != ""])) #19
length(unique(neuroptera2018$BIN[neuroptera2018$BIN != ""])) #17
l <- length(intersect(neuroptera2016$BIN[neuroptera2016$BIN!=""], neuroptera2018$BIN[neuroptera2018$BIN != ""] ))
u <- length(union(neuroptera2018$BIN[neuroptera2018$BIN != ""], neuroptera2016$BIN[neuroptera2016$BIN!=""] ))
o <- l/u
o #0.44


#Odonata
odonata2016 <- present2016[present2016$Order=="Odonata", ]
odonata2016 <- odonata2016[which(rowSums(odonata2016[, 14:ncol(odonata2016)]) >0), ]
odonata2018 <- present2018[present2018$Order=="Odonata", ]
odonata2018 <- odonata2018[which(rowSums(odonata2018[, 14:ncol(odonata2018)]) >0), ]
#length(odonata2018$BIN[odonata2018$BIN != ""]) #14
#length(unique(odonata2018$BIN[odonata2018$BIN != ""])) #14
#overlaps
#l <- length(intersect(odonata2016$BIN[odonata2016$BIN!=""], odonata2018$BIN[odonata2018$BIN != ""] )) #0
length(unique(odonata2016$BIN[odonata2016$BIN != ""])) #0
length(unique(odonata2018$BIN[odonata2018$BIN != ""])) #14
l <- length(intersect(odonata2016$BIN[odonata2016$BIN!=""], odonata2018$BIN[odonata2018$BIN != ""] ))
u <- length(union(odonata2018$BIN[odonata2018$BIN != ""], odonata2016$BIN[odonata2016$BIN!=""] ))
o <- l/u
o #0


#Orthoptera
orthoptera2016 <- present2016[present2016$Order=="Orthoptera", ]
orthoptera2016 <- orthoptera2016[which(rowSums(orthoptera2016[, 14:ncol(orthoptera2016)]) >0), ]
orthoptera2018 <- present2018[present2018$Order=="Orthoptera", ]
orthoptera2018 <- orthoptera2018[which(rowSums(orthoptera2018[, 14:ncol(orthoptera2018)]) >0), ]
#length(orthoptera2018$BIN[orthoptera2018$BIN != ""]) #28
#length(unique(orthoptera2018$BIN[orthoptera2018$BIN != ""])) #17
#overlaps
#l <- length(intersect(orthoptera2016$BIN[orthoptera2016$BIN!=""], orthoptera2018$BIN[orthoptera2018$BIN != ""] )) #10
length(unique(orthoptera2016$BIN[orthoptera2016$BIN != ""])) #13
length(unique(orthoptera2018$BIN[orthoptera2018$BIN != ""])) #17
l <- length(intersect(orthoptera2016$BIN[orthoptera2016$BIN!=""], orthoptera2018$BIN[orthoptera2018$BIN != ""] ))
u <- length(union(orthoptera2018$BIN[orthoptera2018$BIN != ""], orthoptera2016$BIN[orthoptera2016$BIN!=""] ))
o <- l/u
o #0.5


#Plecoptera
plecoptera2016 <- present2016[present2016$Order=="Plecoptera", ]
plecoptera2016 <- plecoptera2016[which(rowSums(plecoptera2016[, 14:ncol(plecoptera2016)]) >0), ]
plecoptera2018 <- present2018[present2018$Order=="Plecoptera", ]
plecoptera2018 <- plecoptera2018[which(rowSums(plecoptera2018[, 14:ncol(plecoptera2018)]) >0), ]
#length(plecoptera2018$BIN[plecoptera2018$BIN != ""]) #16
#length(unique(plecoptera2018$BIN[plecoptera2018$BIN != ""])) #10
#overlaps
#l <- length(intersect(plecoptera2016$BIN[plecoptera2016$BIN!=""], plecoptera2018$BIN[plecoptera2018$BIN != ""] )) #9
length(unique(plecoptera2016$BIN[plecoptera2016$BIN != ""])) #16
length(unique(plecoptera2018$BIN[plecoptera2018$BIN != ""])) #10
l <- length(intersect(plecoptera2016$BIN[plecoptera2016$BIN!=""], plecoptera2018$BIN[plecoptera2018$BIN != ""] ))
u <- length(union(plecoptera2018$BIN[plecoptera2018$BIN != ""], plecoptera2016$BIN[plecoptera2016$BIN!=""] ))
o <- l/u
o #0.5294



#Psocodea
psocodea2016 <- present2016[present2016$Order=="Psocodea", ]
psocodea2016 <- psocodea2016[which(rowSums(psocodea2016[, 14:ncol(psocodea2016)]) >0), ]
psocodea2018 <- present2018[present2018$Order=="Psocodea", ]
psocodea2018 <- psocodea2018[which(rowSums(psocodea2018[, 14:ncol(psocodea2018)]) >0), ]
#length(psocodea2018$BIN[psocodea2018$BIN != ""]) #10
#length(unique(psocodea2018$BIN[psocodea2018$BIN != ""])) #9
#overlaps
#l <- length(intersect(psocodea2016$BIN[psocodea2016$BIN!=""], psocodea2018$BIN[psocodea2018$BIN != ""] )) #9
length(unique(psocodea2016$BIN[psocodea2016$BIN != ""])) #9
length(unique(psocodea2018$BIN[psocodea2018$BIN != ""])) #9
l <- length(intersect(psocodea2016$BIN[psocodea2016$BIN!=""], psocodea2018$BIN[psocodea2018$BIN != ""] ))
u <- length(union(psocodea2018$BIN[psocodea2018$BIN != ""], psocodea2016$BIN[psocodea2016$BIN!=""] ))
o <- l/u
o #1



#Raphidioptera
raphidioptera2016 <- present2016[present2016$Order=="Raphidioptera", ]
raphidioptera2016 <- raphidioptera2016[which(rowSums(raphidioptera2016[, 14:ncol(raphidioptera2016)]) >0), ]
raphidioptera2018 <- present2018[present2018$Order=="Raphidioptera", ]
raphidioptera2018 <- raphidioptera2018[which(rowSums(raphidioptera2018[, 14:ncol(raphidioptera2018)]) >0), ]
#length(raphidioptera2018$BIN[raphidioptera2018$BIN != ""]) #0
#length(unique(raphidioptera2018$BIN[raphidioptera2018$BIN != ""])) #0
#overlaps
#l <- length(intersect(raphidioptera2016$BIN[raphidioptera2016$BIN!=""], raphidioptera2018$BIN[raphidioptera2018$BIN != ""] )) #0
length(unique(raphidioptera2016$BIN[raphidioptera2016$BIN != ""])) #4
length(unique(raphidioptera2018$BIN[raphidioptera2018$BIN != ""])) #3
l <- length(intersect(raphidioptera2016$BIN[raphidioptera2016$BIN!=""], raphidioptera2018$BIN[raphidioptera2018$BIN != ""] ))
u <- length(union(raphidioptera2018$BIN[raphidioptera2018$BIN != ""], raphidioptera2016$BIN[raphidioptera2016$BIN!=""] ))
o <- l/u
o #0.75



#Thysanoptera
thysanoptera2016 <- present2016[present2016$Order=="Thysanoptera", ]
thysanoptera2016 <- thysanoptera2016[which(rowSums(thysanoptera2016[, 14:ncol(thysanoptera2016)]) >0), ]
thysanoptera2018 <- present2018[present2018$Order=="Thysanoptera", ]
thysanoptera2018 <- thysanoptera2018[which(rowSums(thysanoptera2018[, 14:ncol(thysanoptera2018)]) >0), ]
#length(thysanoptera2018$BIN[thysanoptera2018$BIN != ""]) #1
#length(unique(thysanoptera2018$BIN[thysanoptera2018$BIN != ""])) #1
#overlaps
#l <- length(intersect(thysanoptera2016$BIN[thysanoptera2016$BIN!=""], thysanoptera2018$BIN[thysanoptera2018$BIN != ""] )) #0
length(unique(thysanoptera2016$BIN[thysanoptera2016$BIN != ""])) #1
length(unique(thysanoptera2018$BIN[thysanoptera2018$BIN != ""])) #1
l <- length(intersect(thysanoptera2016$BIN[thysanoptera2016$BIN!=""], thysanoptera2018$BIN[thysanoptera2018$BIN != ""] ))
u <- length(union(thysanoptera2018$BIN[thysanoptera2018$BIN != ""], thysanoptera2016$BIN[thysanoptera2016$BIN!=""] ))
o <- l/u
o #0  


#Trichoptera
trichoptera2016 <- present2016[present2016$Order=="Trichoptera", ]
trichoptera2016 <- trichoptera2016[which(rowSums(trichoptera2016[, 14:ncol(trichoptera2016)]) >0), ]
trichoptera2018 <- present2018[present2018$Order=="Trichoptera", ]
trichoptera2018 <- trichoptera2018[which(rowSums(trichoptera2018[, 14:ncol(trichoptera2018)]) >0), ]
#length(trichoptera2018$BIN[trichoptera2018$BIN != ""]) #19
#length(unique(trichoptera2018$BIN[trichoptera2018$BIN != ""])) #19
#overlaps
#l <- length(intersect(trichoptera2016$BIN[trichoptera2016$BIN!=""], trichoptera2018$BIN[trichoptera2018$BIN != ""] )) #16
length(unique(trichoptera2016$BIN[trichoptera2016$BIN != ""])) #24
length(unique(trichoptera2018$BIN[trichoptera2018$BIN != ""])) #19
l <- length(intersect(trichoptera2016$BIN[trichoptera2016$BIN!=""], trichoptera2018$BIN[trichoptera2018$BIN != ""] ))
u <- length(union(trichoptera2018$BIN[trichoptera2018$BIN != ""], trichoptera2016$BIN[trichoptera2016$BIN!=""] ))
o <- l/u
o #0.59259

#Isopoda
isopoda2016 <- present2016[present2016$Order=="Isopoda", ]
isopoda2016 <- isopoda2016[which(rowSums(isopoda2016[, 14:ncol(isopoda2016)]) >0), ]
isopoda2018 <- present2018[present2018$Order=="Isopoda", ]
isopoda2018 <- isopoda2018[which(rowSums(isopoda2018[, 14:ncol(isopoda2018)]) >0), ]
#length(isopoda2018$BIN[isopoda2018$BIN != ""]) #3
#length(unique(isopoda2018$BIN[isopoda2018$BIN != ""])) #3
#overlaps
#l <- length(intersect(isopoda2016$BIN[isopoda2016$BIN!=""], isopoda2018$BIN[isopoda2018$BIN != ""] )) #0
length(unique(isopoda2016$BIN[isopoda2016$BIN != ""])) #0
length(unique(isopoda2018$BIN[isopoda2018$BIN != ""])) #3
l <- length(intersect(isopoda2016$BIN[isopoda2016$BIN!=""], isopoda2018$BIN[isopoda2018$BIN != ""] ))
u <- length(union(isopoda2018$BIN[isopoda2018$BIN != ""], isopoda2016$BIN[isopoda2016$BIN!=""] ))
o <- l/u
o #0


#Stylommatophora
stylommatophora2016 <- present2016[present2016$Order=="Stylommatophora", ]
stylommatophora2016 <- stylommatophora2016[which(rowSums(stylommatophora2016[, 14:ncol(stylommatophora2016)]) >0), ]
stylommatophora2018 <- present2018[present2018$Order=="Stylommatophora", ]
stylommatophora2018 <- stylommatophora2018[which(rowSums(stylommatophora2018[, 14:ncol(stylommatophora2018)]) >0), ]
#length(stylommatophora2018$BIN[stylommatophora2018$BIN != ""]) #3
#length(unique(stylommatophora2018$BIN[stylommatophora2018$BIN != ""])) #3
#overlaps
#l <- length(intersect(stylommatophora2016$BIN[stylommatophora2016$BIN!=""], stylommatophora2018$BIN[stylommatophora2018$BIN != ""] )) #0
length(unique(stylommatophora2016$BIN[stylommatophora2016$BIN != ""])) #1
length(unique(stylommatophora2018$BIN[stylommatophora2018$BIN != ""])) #3
l <- length(intersect(stylommatophora2016$BIN[stylommatophora2016$BIN!=""], stylommatophora2018$BIN[stylommatophora2018$BIN != ""] ))
u <- length(union(stylommatophora2018$BIN[stylommatophora2018$BIN != ""], stylommatophora2016$BIN[stylommatophora2016$BIN!=""] ))
o <- l/u
o #0
