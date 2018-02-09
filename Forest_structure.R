#### Forest structure - Real word data set #### 

###################################################
##    Author: Maximilian Hesselbarth             ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
###################################################

#### Install packages ####
toc <- "cde286ffbe355d59b6d9ac4639bdb66d7bdda3ec"

devtools::install_github("mhesselbarth/UtilityFunctions", auth_token=toc, quiet=T)

rm(toc)

#### Import packages and functions ####
library(ggplot2)
library(UtilityFunctions)
library(viridis)

data <- paste0(getwd(), "/Data")
figures <- paste0(getwd(), "/Figures")

#### Import data ####

data_2007 <- readRDS(paste0(data, "/data_2007.rds"))

data_2007$Basal_area <- pi * (data_2007$DBH_07/100)^2/4

data_2007_living <- subset(data_2007, Type!="dead")
data_2007_dead <- subset(data_2007, Type=="dead")

#### Abundance and basal area ####

abundance_2007 <- as.data.frame(table(data_2007_living$Species))
names(abundance_2007) <- c("Species", "Abundance")
abundance_2007$Abundance_rel <- (abundance_2007$Abundance / sum(abundance_2007$Abundance)) * 100

basal_area_2007 <-aggregate(x=list(Basal_area=data_2007_living$Basal_area), by=list(Species=data_2007_living$Species), FUN=sum, na.rm=T)
basal_area_2007$Basal_area_rel <- basal_area_2007$Basal_area / sum(basal_area_2007$Basal_area) * 100

abundance_dbh_2007 <- merge(abundance_2007, basal_area_2007)
abundance_dbh_2007 <- reshape2::melt(abundance_dbh_2007, id.vars="Species", 
                                     variable.name="Measurement", value.name="Value")
abundance_dbh_2007 <- subset(abundance_dbh_2007, Measurement=="Abundance_rel" | Measurement=="Basal_area_rel",
                             drop=T)

plot_abundance_dbh <- ggplot(data=abundance_dbh_2007, aes(x=Species, y=Value, fill=Measurement)) +
  geom_bar(stat="identity", position=position_dodge(width = 0.9), width=0.8) + 
  geom_text(aes(label=paste0(round(Value,1), "%")), position=position_dodge(0.9), vjust=-0.5) +
  scale_fill_viridis(discrete=T, labels=c("Abundance", "Basal area")) +
  scale_x_discrete(labels=c("Fagus \nsylvatica", "Fraxinus \nexcelsior", "Carpinus \nbetulus", 
                            "Acer \npseudoplatanus", "others")) +
  labs(y="Value [%]", fill="") +
  theme_bw()
Save.Function.ggplot(object=plot_abundance_dbh, file=paste0(figures, "/Abundance_dbh.jpeg"), dpi=1000)

#### DBH ####
data_2007_living$DBH_class <- cut(data_2007_living$DBH_07, breaks=seq(0,140,5))
dbh_class_2007 <- as.data.frame(table(data_2007_living$DBH_class))
names(dbh_class_2007) <- c("Class", "Abundance")

plot_dbh <- ggplot(dbh_class_2007) +
  geom_bar(aes(x=Class, y=Abundance/sum(Abundance)),
           position=position_dodge(width = 0.9), stat="identity") +
  scale_x_discrete(name="DHB Class", labels=paste("<",seq(5,140,5), "cm")) +
  ylab(label="Abundance [%]") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), text=element_text(size=20))
Save.Function.ggplot(object=plot_dbh, file=paste0(figures, "/DBH_distribution.jpeg"), dpi=1000)

paste0("<10 cm: ", (3101 + 2803) /sum(dbh_class_2007$Abundance) *100, "%")  

