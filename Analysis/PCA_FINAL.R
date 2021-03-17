library(ggplot2)
library(ggbiplot)
library(pca3d)
library(dplyr) 
library(tibble)
# Choose what group to compare by on the PCA graph
Fire <- "fire.type"
Status <- "status"
#Options are the objects Fire for Fire Promoting/Unmeasured and Status for Native/Invasive
Trait <- Status
# Chose how many traits you want to measure >10 will lead to few datapoints
NTraits <- 10

#You can collapse and just run the next part, no input needed
################################################# 
pca_dat1 <- read.csv("traits_columns.csv")
pca_dat <- column_to_rownames(pca_dat1, var = "X")
speciesfilter <- subset(read.csv("species_filter.csv"), select = -c(X))

Traits_measured <- na.omit(pca_dat[,c(1:NTraits)])
filteredspecies <- subset(as.data.frame(speciesfilter[match(unique(row.names.data.frame(Traits_measured)), 
                                                           speciesfilter$species),]), select = c(species, get(Trait)))

pcaGroups <- as.data.frame(table((filteredspecies[,2])))
subtitle <- c()
for (i in 1:NROW(pcaGroups$Var1)) {
  subtitle <- toString(c(subtitle, gsub(",", "" ,toString(c(as.character(pcaGroups$Var1[i]), "=",pcaGroups$Freq[i] )))))
}
groupvector <- c()
for (i in 1:NROW(pcaGroups$Var1)) {
  groupvector <- c(groupvector,(rep(as.character(pcaGroups$Var1[i]), as.numeric(pcaGroups$Freq[i])) ))
}
grouplevels <- gsub(",", "" ,toString(c((nrow(Traits_measured)), 
                                        "Species Measured Across", toString(c((ncol(Traits_measured)))), "Traits")))


###################################################
traits.pca <- prcomp(Traits_measured, center = TRUE, scale. = TRUE)
summary(traits.pca)
ggbiplot( traits.pca, ellipse=TRUE,  groups=groupvector, obs.scale = 1) + labs(title = grouplevels, subtitle = subtitle)


pca3d(traits.pca, group = groupvector,  show.ellipses=TRUE,
      ellipse.ci=0.75, show.plane=FALSE, legend= "top")    
