#Create a PCOA or NM plot comparing sites with species.
#install.packages("remotes")
library(remotes)
remotes::install_github("MadsAlbertsen/ampvis2")
library(ampvis2)
#Load data for ampvis functions, like example:
#amp_load(otutable, metadata = NULL, fasta = NULL, tree = NULL)
#Create appropriate OTU table from current version of data:
#setwd("/media/laur/wdhdd11/allNPBW/")
setwd("/home/laur/Schreibtisch/NPBW_manuscript_code/")
data <- read.table("Reordered_for_pest_R_5_newFebNPBW_VL2.tsv", header=T, sep="\t", stringsAsFactors = F)

######################### Clean/prepare data for ampvis
#Replace blanks with NAs.
data$Species[data$Species == ""] <- NA
data$Family[data$Family == ""] <- NA
data$Subfamily[data$Subfamily == ""] <- NA
data$Genus[data$Genus == ""] <- NA
data$BIN[data$BIN == ""] <- NA
data$Phylum[data$Phylum == ""] <- NA
data$Class[data$Class == ""] <- NA
data$Order[data$Order == ""] <- NA
# Tissue powders only for this comparison - remove others:
data <- data[, !grepl("_224", names(data))]
data <- data[, !grepl("_210", names(data))]
data <- data[, !grepl("Etoh", names(data))]
data <- data[, !grepl("semi", names(data))]
data <- data[, !grepl("filter", names(data))]
head(data)
#The rows are OTU IDs, and the cols are samples:
#(see https://madsalbertsen.github.io/ampvis2/reference/amp_load.html)
df <- data[13:ncol(data)]
#The OTU ID's are expected to be in either the rownames of the data frame or in a column called "OTU". 
colnames(df)[1] <- "OTU"
#The last 7 columns are the corresponding taxonomy assigned to the OTUs, named "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species".
df$Kingdom <- rep("Animalia", length(df$OTU))
otutbl <- cbind.data.frame(df, data$Phylum, data$Class, data$Order, data$Family, data$Genus, data$Species)
colnames(otutbl)[183:188] <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")

######################### Analyze 2016 data:
otutbl2016 <- otutbl[,  !grepl("2018", names(otutbl))]
head(otutbl2016)
# Read in created metadata file and combine using amp_load() to make it compatible with ampvis2 functions.
metadata2016 <- read.table("Metadata2016_r5.csv", header=T, sep=",")
head(metadata2016)

d2016 <- amp_load(otutable=otutbl2016, metadata = metadata2016)
d2016

# Create new distance matrix with 7-level taxonomy (not BINs).
# (see https://madsalbertsen.github.io/ampvis2/reference/amp_ordinate.html)
p2016 <- amp_ordinate(d2016, type="PCA", transform="hellinger", sample_color_by = "Trap", sample_colorframe = T)+
  labs(title="PCA of taxonomy by trap, 2016", tag="A")+ annotate(geom="text", x=0.475, y=0.5, label="ANOSIM R: 0.2, Significance: 2e-04")

######################## Analyze 2018 data:
otutbl2018 <- otutbl[, !grepl("2016", names(otutbl))]
head(otutbl2018)
metadata2018 <- read.table("Metadata2018_r5.csv", header=T, sep=",")
head(metadata2018)

d2018 <- amp_load(otutable=otutbl2018, metadata=metadata2018)
d2018
p2018 <- amp_ordinate(d2018, type="PCA", transform="hellinger", sample_color_by = "Trap", sample_colorframe = T)+
  labs(title="PCA of taxonomy by trap, 2018", tag="B") + annotate(geom="text", x=0.475, y=0.75, label="ANOSIM R: 0.2393, Significance: 1e-04")


######################### Set up multiple plot function and plot both years with 2016 on top:
# According to directions at http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##########################

multiplot(p2016,p2018, cols=1)
