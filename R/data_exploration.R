# other data exploration

# setup ==============
library(tidyverse)
library(ramify)
library(ggplot2)
library(vroom)

# Reading in the trait data ========================

traits <- list.files("data", pattern=".txt", full.names = T) %>%
  lapply(read_tsv) %>%
  bind_rows() %>%
  left_join(vroom("data/OriginList.csv")%>%
              dplyr::select(`species name`, "status"), 
            by = c("SpeciesName" = "species name"))%>%
  left_join(vroom("data/FireProne.csv")%>%
              dplyr::select("species_name", "Fire_Type"), 
            by = c("SpeciesName" = "species_name")) %>%
  mutate(Fire_Type = replace_na(Fire_Type,"Unmeasured")) %>%
  mutate(gen = str_split(AccSpeciesName, " ", simplify = T)[,1] %>% str_to_title,
         spec = str_split(AccSpeciesName, " ", simplify = T)[,2] %>% str_to_lower,
         gensp=paste(gen, spec))

# looks like the comment field is sometimes being broken up into two columns
# hence the "...28" column. probably no big deal
glimpse(traits)

# investigating which traits and species are morst prevalent ===================


# making a wider df of only quant traits and removing tricky characters ========

wide_df_fire <- traits%>%
  filter(!is.na(UnitName),
         Fire_Type != "Unmeasured") %>%
  dplyr::select(AccSpeciesName, TraitName,  UnitName,OrigValueStr,
                StdValue ) %>%
  na.omit() %>%
  mutate(TraitName = str_to_title(TraitName),
         TraitName = str_replace_all(TraitName," ", ""),
         TraitName = str_replace_all(TraitName,"\\\\", ""),
         TraitName = str_replace_all(TraitName,"\\(", ""),
         TraitName = str_replace_all(TraitName,"\\)", ""),
         TraitName = str_replace_all(TraitName,"\\:", ""),
         TraitName = str_replace_all(TraitName,"\\&", ""),
         TraitName = str_replace_all(TraitName,"\\/", ""),
         TraitName = str_replace_all(TraitName,"\\,", ""),
         TraitName = str_replace_all(TraitName,"\\;", ""),
         TraitName = str_replace_all(TraitName,"\\-", ""),
         TraitName = str_sub(TraitName, 1, 25)) %>%
  group_by(AccSpeciesName, TraitName) %>%
  dplyr::summarise(StdValue = mean(StdValue)) %>%
  ungroup() %>%
  pivot_wider(names_from = TraitName, 
              values_from= StdValue)

wide_df <- traits%>%
  filter(!is.na(UnitName)) %>%
  dplyr::select(AccSpeciesName, TraitName,  UnitName,OrigValueStr,
                StdValue, gensp ) %>%
  na.omit() %>%
  mutate(TraitName = str_to_title(TraitName),
         TraitName = str_replace_all(TraitName," ", ""),
         TraitName = str_replace_all(TraitName,"\\\\", ""),
         TraitName = str_replace_all(TraitName,"\\(", ""),
         TraitName = str_replace_all(TraitName,"\\)", ""),
         TraitName = str_replace_all(TraitName,"\\:", ""),
         TraitName = str_replace_all(TraitName,"\\&", ""),
         TraitName = str_replace_all(TraitName,"\\/", ""),
         TraitName = str_replace_all(TraitName,"\\,", ""),
         TraitName = str_replace_all(TraitName,"\\;", ""),
         TraitName = str_replace_all(TraitName,"\\-", ""),
         TraitName = str_sub(TraitName, 1, 25)) %>%
  group_by(gensp, TraitName) %>%
  dplyr::summarise(StdValue = mean(StdValue)) %>%
  ungroup() %>%
  pivot_wider(names_from = TraitName, 
              values_from= StdValue)

# =======
# How many species each trait has and how often each trait is used ============================
Speciestraits <- traits%>%
  group_by(gensp)%>%
  summarize(ntraits = length(unique(TraitID)))%>%
  ungroup()%>%
  arrange(desc(ntraits))

TraitsOcurrence <- traits%>%
  group_by(TraitName)%>%
  dplyr::summarize(nspecies = length(unique(gensp)), 
                   Traitvalue = first(UnitName))%>%
  ungroup()%>%
  arrange(desc(nspecies))

Speciestraits%>%
ggplot() +
  geom_histogram(aes(x=ntraits)) +
  ylab("number of species")

TraitsOcurrence%>%
  ggplot() +
  geom_histogram(aes(x=nspecies)) +
  ylab("number of traits")

# correlation matrix =============
library(corrplot)

cormat<- wide_df %>%
  tibble::column_to_rownames("gensp")%>%
  filter(sum(is.na(.))>1) %>%
  cor(method = "pearson",
      use = "pairwise.complete.obs") 

corrplot(cormat[1:40,1:40],
         method = "shade",
         type="upper",
         order = "FPC"
         )
