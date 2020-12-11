# pca

# setup ==============
library(tidyverse)
library(ramify)
library(ggplot2)
# Reading in the trait data ========================
Traits_1 <- read_csv("data/OriginList.csv")%>%
  dplyr::select(`species name`, "status")%>%
  left_join(x = read_tsv("data/11649.txt"), y = ., by = c("SpeciesName" = "species name") )
Traits_2 <- read_csv("data/OriginList.csv")%>%
  dplyr::select(`species name`, "status")%>%
  left_join(x = read_tsv("data/11650.txt"), y = ., by = c("SpeciesName" = "species name") )
Traits_3 <- read_csv("data/OriginList.csv")%>%
  dplyr::select(`species name`, "status")%>%
  left_join(x = read_tsv("data/11651.txt"), y = ., by = c("SpeciesName" = "species name") )

#Combining the trait data and pairing it with fire prone ==================
Traits_f <- rbind(Traits_1, Traits_2, Traits_3)
glimpse(read.csv("data/FireProne.csv"))
Traits_l <- read_csv("data/FireProne.csv")%>%
  dplyr::select("species_name", "Fire_Type")%>%
  left_join(x = Traits_f,
            y = ., by = c("SpeciesName" = "species_name") ) 
Traits_l$Fire_Type <- Traits_l$Fire_Type %>%
  replace_na("Unmeasured")

# analysis ==============================

pca_df <- Traits_l %>%
  dplyr::select(AccSpeciesName, TraitName,  UnitName,OrigValueStr,
                StdValue ) %>%
  filter(!is.na(UnitName)) %>%
  na.omit() %>%
  mutate(TraitName = str_replace_all(TraitName," ", ""),
         TraitName = str_replace_all(TraitName,"\\\\", ""),
         TraitName = str_replace_all(TraitName,"\\(", ""),
         TraitName = str_replace_all(TraitName,"\\)", ""),
         TraitName = str_replace_all(TraitName,"\\:", ""),
         TraitName = str_replace_all(TraitName,"\\&", ""),
         TraitName = str_replace_all(TraitName,"\\/", ""),
         TraitName = str_replace_all(TraitName,"\\,", ""),
         TraitName = str_replace_all(TraitName,"\\;", ""),
         TraitName = str_replace_all(TraitName,"\\-", ""),
         TraitName = str_sub(TraitName, 1, 10)) %>%
  group_by(AccSpeciesName, TraitName) %>%
  summarise(StdValue = mean(StdValue)) %>%
  ungroup() %>%
  pivot_wider(names_from = TraitName, 
              values_from= StdValue)

mice_df<-mice(pca_df[,2:10],m = 10) %>%
  complete()

rownames(mice_df) <- pca_df$AccSpeciesName

prcomp(mice_df[1:100,]) %>%
  biplot()

metaMDS(mice_df)

library(vegan)
vegan::rda(mice_df[1:100,]) %>% biplot

library(missMDA)
imputePCA(pca_df[1:10,2:ncol(pca_df)], method="EM")

# example cleaning thing
# %>% 
#   filter(UnitName == "year" & is.na(StdValue))

#%>%
 # mutate(StdValue = as.numeric(str_extract(OrigValueStr,"\\d+")))
