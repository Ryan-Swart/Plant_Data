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
unique(Traits_l$Fire_Type)

# Looking at certain traits (Best for Text) ==========================
glimpse(Traits_l)
Traits_f$ErrorRisk
unique(GRF$OrigValueStr)
unique(Traits_f$DataName)
unique(Growth_form)
Growth_form <- Traits_f %>%
  filter(TraitName == "Leaf photosynthesis pathway")
GRF <- Growth_form[c("SpeciesName", "OrigValueStr")] %>%
  mutate(OrigValueStr = str_to_upper(OrigValueStr), 
         OrigValueStr = str_replace_all(OrigValueStr, "C4\\?", "C3/C4"),
         OrigValueStr = str_replace_all(OrigValueStr, "C3\\?", "C3/C4"), )
unique(GRF$OrigValueStr)
UNKS <- GRF %>%
  filter(OrigValueStr == "UNKNOWN") %>%
  pull(SpeciesName) 
GRF %>%
  filter(str_to_lower(SpeciesName) %in% str_to_lower(UNKS))
# How many species each trait has and how often each trait is used ============================
Speciestraits <- Traits_f%>%
  group_by(SpeciesName)%>%
  summarize(ntraits = length(unique(TraitID)))%>%
  ungroup()%>%
  arrange(desc(ntraits))

TraitsOcurrence <- Traits_f%>%
  group_by(TraitName)%>%
  dplyr::summarize(nspecies = length(unique(SpeciesName)), 
            Traitvalue = first(UnitName))%>%
  ungroup()%>%
  arrange(desc(nspecies))
view(TraitsOcurrence)
TraitsOcurrence[11:20,]

view(TraitsOcurrence %>% na.omit)
# Filter out invasive species present ==========================
invtraits <- Traits_f%>%
  filter(status == "non-native")%>%
  group_by(SpeciesName)%>%
  summarize(ntraits = length(unique(TraitID)))%>%
  ungroup()%>%
  arrange(desc(ntraits))%>%
  mutate(status = "non-native")
  
glimpse(invtraits)
# Filter out all native species and combine with invasive===================================
nattraits <- Traits_f%>%
  filter(status == "native")%>%
  group_by(SpeciesName)%>%
  summarize(ntraits = length(unique(TraitID)))%>%
  ungroup()%>%
  arrange(desc(ntraits))%>%
  mutate(status = "native")%>%
  rbind(invtraits)
nattraits
write.csv(nattraits, file = "Firstnat")
invtraits <- Traits_f%>%
  filter(status == "invasive")%>%
  group_by(SpeciesName)%>%
  summarize(ntraits = length(unique(TraitID)))%>%
  ungroup()%>%
  arrange(desc(ntraits))%>%
  mutate(status = "invasive")%>%
  rbind(invtraits)
# Specifying the tested variable here: ===============================
x = "Seed dry mass"
#### Storing The Trait For Native Species And Binding It To Dataframe
traits_comp_nat <- Traits_f %>%
    filter(Traits_f$TraitName == x) %>%
    filter(status == "native")

### Binding relevant data only
nat <- traits_comp_nat[c("SpeciesName", "OrigValueStr")]
### Forcing data into numeric
nat$OrigValueStr <- as.numeric(as.character(nat$OrigValueStr))

#Calculating The Average Of Each Species In The Native Group
nat_mean_data <- nat %>%
  group_by(SpeciesName) %>%
  summarize(avg = mean(OrigValueStr))

#Finding All Quartile And Min/Max Of Native Data
quantile(nat_mean_data$avg, na.rm = TRUE)

#Calculating The 1st Quartile Of The Native Data
Q1 <- quantile(nat_mean_data$avg, .25, na.rm = TRUE)
#Calculating The 3rd Quartile Of The Native Data
Q3 <- quantile(nat_mean_data$avg, .75, na.rm = TRUE)
IQR <- IQR(nat_mean_data$avg, na.rm = TRUE)
#removing outliers that are more than 5 times the interquartile range less than Q1 or more than Q3
no_outliers_nat <- subset(nat_mean_data,
                          nat_mean_data$avg> (Q1 - 5*IQR) & nat_mean_data$avg< (Q3 + 5*IQR))

# Storing The Trait For Invasive Species And Binding It To Dataframe ============
traits_comp_inv <- Traits_f %>%
  filter(Traits_f$TraitName == x) %>%
  filter(status == "non-native") 

### Binding relevant data only
inv <- traits_comp_inv[c("SpeciesName", "OrigValueStr")]
### Forcing data into numeric
inv$OrigValueStr <- as.numeric(as.character(inv$OrigValueStr))

#Calculating The Average Of Each Species In The Invasive Group
inv_mean_data <- inv %>%
  group_by(SpeciesName) %>%
  summarize(avg = mean(OrigValueStr))

#finding all quartile and min/max of invasive data
quantile(inv_mean_data$avg, na.rm = TRUE)
     
#Calculating The 1st Quartile Of The Invasive Data
Q1 <- quantile(inv_mean_data$avg, .25, na.rm = TRUE)
#Calculating The 3rd Quartile Of The Invasive Data
Q3 <- quantile(inv_mean_data$avg, .75, na.rm = TRUE)
IQR <- IQR(inv_mean_data$avg, na.rm = TRUE)
#removing outliers that are more than 5 times the interquartile range less than Q1 or more than Q3
no_outliers_inv <- subset(inv_mean_data,
                          inv_mean_data$avg> (Q1 - 5*IQR) & inv_mean_data$avg< (Q3 + 5*IQR))
# Sorting and examining the traits for Fire prone species ==============
Speciestraits <- Traits_l%>%
  filter(Fire_Type == "Invasive/Fire Promoting") %>%
  group_by(SpeciesName)%>%
  summarize(ntraits = length(unique(TraitID)))%>%
  ungroup()%>%
  arrange(desc(ntraits))
TraitsOcurrence <- Traits_l%>%
  filter(Fire_Type == "Invasive/Fire Promoting") %>%
  group_by(TraitName)%>%
  dplyr::summarize(nspecies = length(unique(SpeciesName)), 
                   Traitvalue = first(UnitName))%>%
  ungroup()%>%
  arrange(desc(nspecies))


# Storing the traits for invasive fire prone species ============
traits_comp_inv_f <- Traits_l %>%
  filter(Traits_l$TraitName == x) %>%
  filter(Fire_Type == "Invasive/Fire Promoting") 
### Binding relevant data only
invf <- traits_comp_inv_f[c("SpeciesName", "OrigValueStr")]
### Forcing data into numeric
invf$OrigValueStr <- as.numeric(as.character(invf$OrigValueStr))

#Calculating The Average Of Each Species In The Invasive Group
inv_f_mean_data <- invf %>%
  group_by(SpeciesName) %>%
  summarize(avg = mean(OrigValueStr))

#finding all quartile and min/max of invasive data
quantile(inv_f_mean_data$avg, na.rm = TRUE)

#Calculating The 1st Quartile Of The Invasive Data
Q1 <- quantile(inv_f_mean_data$avg, .25, na.rm = TRUE)
#Calculating The 3rd Quartile Of The Invasive Data
Q3 <- quantile(inv_f_mean_data$avg, .75, na.rm = TRUE)
IQR <- IQR(inv_f_mean_data$avg, na.rm = TRUE)
#removing outliers that are more than 5 times the interquartile range less than Q1 or more than Q3
no_outliers_inv_f <- subset(inv_f_mean_data,
                            inv_f_mean_data$avg> (Q1 - 5*IQR) & inv_f_mean_data$avg< (Q3 + 5*IQR))
#Plotting the Data =================
lab <- TraitsOcurrence$Traitvalue[(which(TraitsOcurrence == x))]
state <- c("Native", "Invasive", "Fire Prone")
boxplot(no_outliers_inv$avg, no_outliers_nat$avg,  no_outliers_inv_f$avg, names = state, horizontal = TRUE, outline = FALSE,
        main = x, col = "orange", border = "brown", xlab = lab)


#running a t-test between native and invasive ======================
t.test(no_outliers_nat$avg, no_outliers_inv$avg)
wilcox.test(no_outliers_nat$avg, no_outliers_inv$avg)
p <- c((t.test(no_outliers_nat$avg, no_outliers_inv$avg)$p.value),
       wilcox.test(no_outliers_nat$avg, no_outliers_inv$avg)$p.value)
p
#running a t-test between native and fire prone ======================
t.test(no_outliers_nat$avg, no_outliers_inv_f$avg)
wilcox.test(no_outliers_nat$avg, no_outliers_inv_f$avg)
p <- c((t.test(no_outliers_nat$avg, no_outliers_inv_f$avg)$p.value),
         wilcox.test(no_outliers_nat$avg, no_outliers_inv_f$avg)$p.value)
p
#Cutting down to only-numeric traits ==========================

Num_Traits <- Traits_l %>%
  
