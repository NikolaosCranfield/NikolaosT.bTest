library(dplyr)
library(BIOMASS)
library(ggplot2)
library(tidyr)



#### READ IN FIELD DATA ####

# these data are available at 

# Waddell, E. H., Fleiss, S., Bala Ola, B., Bin Sailim, A., Jelling, A., Yeong, K. L., ... & Banin, L. F. (2020). 
# Vegetation and habitat data for fragmented and continuous forest sites in Sabah, Malaysian Borneo, 2017.
# NERC Environmental Information Data Centre.
# https://doi.org/10.5285/c67b06b7-c3f6-49a3-baf2-9fc3a72cb98a


# change to correct file path
setwd("D:/carbon stock assessment/Data/upload full dataset")


CWD <- read.csv("CWD.csv", header = T)# deadwood
lianas <- read.csv("lianas.csv", header = T) # lianas
trees_all <- read.csv("trees.csv", header = T) # tree stems
plots <- read.csv("sites_habitat.csv")# site/plot characteristics: need lat long





########### CALCULATING TREE BIOMASS ###########


######## 1. SORTING DATA #########

### remove palms because their biomass will be calculated separately
# remove trees <10cm dbh from biomass calculations

trees <- filter(trees_all,
                ID != "PALM" ,
                Dbh_cm >=10)

summary(trees) # 3120 stems



## On tree names:
# ID is identity given by botanist - always to genus, and to species when known
# note there is one which was omitted and this is 'UNKNOWN'
# Family is family of name given by botanist (ID column), extracted from the Plant List


###### write columns with tree genus and species separately

trees$ID <- as.character(trees$ID)

# only run strsplit on the rows where there are two words to get a list where 
# each element holds the same number of objects

trees$ID <- as.character(trees$ID)

trees$Genus <- as.character(NA)
trees$Specific <- as.character(NA)  


for(i in 1:length(trees$ID)){
  if(grepl(trees$ID[i], pattern = " ") == TRUE){
    names <- unlist(strsplit(trees$ID[i], split = " "))
    trees$Genus[i] <- names[1]
    trees$Specific[i] <- names[2]
  }else if(trees$ID[i] == "PALM" | trees$ID[i] == "UNKNOWN"){
    # do nothing
  }else{
    trees$Genus[i] <- trees$ID[i]
  }
}


######## write columns for names for obtaining wood density #####

# previously checked against the plant list:
# 2 names where an alternative (accepted) name to the ID in the data gives more detailed
# data on wood density from the Global Wood Density Database:
# Pleiocarpidia sandahanica - Urophyllum sandahanicum
#  Fordia gibbsiae - Fordia splendidissima

wd_switch <- data.frame(ID = c("Pleiocarpidia sandahanica",
                               "Fordia gibbsiae"),
                        Genus_acc_name_TPL = c("Urophyllum",
                                               "Fordia"),
                        Specific_acc_name_TPL = c("sandahanicum",
                                                  "splendissima"))
                        
str(wd_switch)
wd_switch$ID <- as.character(wd_switch$ID)
wd_switch$Genus_acc_name_TPL <- as.character(wd_switch$Genus_acc_name_TPL)
wd_switch$Specific_acc_name_TPL <- as.character(wd_switch$Specific_acc_name_TPL)


# write wd columns for family, genus and species using appropriate names
# in all cases but the two above, this is the botanist given names (note family is always as listed)

trees$Genus_wd <- NA
trees$Specific_wd <- NA

for(i in 1:nrow(trees)){
  if(trees$ID[i] %in% wd_switch$ID){
    trees$Genus_wd[i] <- wd_switch[wd_switch$ID == trees$ID[i],]$Genus_acc_name_TPL
    trees$Specific_wd[i] <- wd_switch[wd_switch$ID == trees$ID[i],]$Specific_acc_name_TPL
  }else{
    trees$Genus_wd[i] <- trees$Genus[i]
    trees$Specific_wd[i] <- trees$Specific[i]  
    }
}




GWD <- getWoodDensity(genus = trees$Genus_wd,
                      species = trees$Specific_wd,
                      stand = trees$Site_plot_code,
                      family = trees$Family,
                      region = "World",
                      addWoodDensityData = NULL)



head(GWD)
summary(GWD)



# add this as a column to the trees dataset

trees$wd <- GWD$meanWD

# How many stems assigned each level of wood density?
GWD$levelWD <- as.factor(GWD$levelWD)
table(GWD$levelWD)

1/3120 * 100 # 0.032% stems assigned mean plot-level wd 
39/3120 * 100 # 1.25% stems assigned family level wd
2440/3120 * 100 # 78.2% stems assigned genus level wd
640/3120 * 100 # 20.5% stems assigned species level wd





########## 3. TREE HEIGHT MODELS ##########

# bind the lat and long onto trees dataframe

trees <- left_join(trees,
                    dplyr::select(plots, Latitude, Longitude, Site_plot_code),
                    by = "Site_plot_code")






##### EXCLUDE height estimates for trees which were Multiple stems or leaning

# make list of the Notes which include leaning etc.
filter(trees, !is.na(Notes)) %>%
  distinct(Notes)

Notes_lean <- c("Leaning horizontally", "leaning", "Leaning", "Lying down")


for(i in 1:length(trees$Dbh_cm)){
  if(grepl(trees$Notes[i], pattern = "Multiple stems") == TRUE | trees$Notes[i] %in% Notes_lean){
    trees$Height_m[i] <- NA
  }
}


heights <- filter(trees, !is.na(Height_m))

# how many stems have height estimates?
nrow(heights) #964

# as % of all stems
nrow(heights)*100/nrow(trees) # 30.9%

# what is their dbh range?
min(heights$Dbh_cm)# 10 cm
max(heights$Dbh_cm) # 130 cm






##### CHECKING CLINOMETER VS ESTIMATED HEIGHT 

# some were measured where I was level with the tree (so Degrees.to.base = NA), 
# so I only need to calculate the 
# height to top, then add 1.5m (my eye height). For others, I need to calculate 
# height to top and height to base.
# where the degrees to base is 0, the height is just the calculated distance to top

#length(A) = tan(a) * distance

# R uses radians so I need to convert the degrees: multiply by pi/180

# use absolute values of the degrees to base because I put these as negative

heights$Degrees_to_top <- as.numeric(as.character(heights$Degrees_to_top))
heights$Degrees_to_base <- as.numeric(as.character(heights$Degrees_to_base))

heights$H_to_top <- tan(heights$Degrees_to_top * (pi/180)) * heights$Distance_to_trunk_m

heights$H_to_base <- tan(abs(heights$Degrees_to_base * (pi/180))) * heights$Distance_to_trunk_m



heights <- mutate(heights,
                  H_trig_m = ifelse(!is.na(H_to_top) & !is.na(H_to_base),
                                           H_to_top + H_to_base,
                                           ifelse(!is.na(H_to_top) & is.na(H_to_base),
                                                  H_to_top + 1.5,
                                                  NA)))


# how many stems have a trig height estimate?
nrow(filter(heights, !is.na(H_trig_m)))
# 50

# as % of stems w eye estimate of height
nrow(filter(heights, !is.na(H_trig_m))) * 100/nrow(heights)
#5.2%







##### Comparing height estimates and trig values ##########

#### Check correlation between clinometer heights and eye heights

heights_cor <- cor.test(heights$H_trig_m,
                       heights$Height_m,
                       method = "pearson",
                       use = "pairwise.complete.obs")

heights_cor

#	Pearson's product-moment correlation

#data:  heights$H_trig_m and heights$Height_m
#t = 7.9419, df = 48, p-value = 2.695e-10
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.6013505 0.8529934
#sample estimates:
#  cor 
#0.7535634 

ggplot(heights, aes(x = H_trig_m, y = Height_m)) +
  geom_point() +
  geom_abline(colour = "#0033FF", 
              intercept = 0,
              slope = 1, 
              linetype = "dashed")+
  xlab("Height (trigonometry)/m")+
  ylab("Height (eye)/m")+
  theme_classic() +
  coord_fixed() + # x and y scales are the same
  scale_x_continuous(limits = c(0,50), expand = c(0,0))+
   # sets limits and removes gap between lower limit and the axis
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  theme(panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey")) +
  annotate("text",
           label = "y = x", x = 42, y = 39, colour = "#0033FF")
  








########### COMPARING DIFFERENT MODELS OF HEIGHT-DIAMETER RELATIONSHIP

####### Using eye estimates of height (not incorporating the clinometer measurements)

#### Comparing different models of H~D

height_model1 <- modelHD(D = trees$Dbh_cm, 
                        H = trees$Height_m,
                        drawGraph = TRUE,
                        method = "log1")

height_model2 <- modelHD(D = trees$Dbh_cm, 
                         H = trees$Height_m,
                         drawGraph = TRUE,
                         method = "log2")

height_model3 <- modelHD(D = trees$Dbh_cm, 
                          H = trees$Height_m,
                          drawGraph = TRUE,
                         method = "weibull")

height_model4 <- modelHD(D = trees$Dbh_cm, 
                         H = trees$Height_m,
                         drawGraph = TRUE,
                         method = "michaelis")


height_model1$RSE
# 5.550826

height_model2$RSE
 # 5.380031

height_model3$RSE
# 5.440097

height_model4$RSE
# 5.547968


### get model formulae
height_model1$coefficients
#             Estimate Std. Error   t value      Pr(>|t|)
#(Intercept) 0.5433618 0.05682210  9.562508  9.288372e-21
#I(log(D)^1) 0.6938928 0.01735193 39.989368 9.167360e-207

height_model2$coefficients
#             Estimate Std. Error   t value     Pr(>|t|)
#(Intercept)  2.3428767 0.29360107  7.979796 4.156409e-15
#I(log(D)^1) -0.4266080 0.18029717 -2.366138 1.817230e-02
#I(log(D)^2)  0.1698611 0.02720991  6.242620 6.447494e-10


height_model3$coefficients
#       Estimate   Std. Error      t value     Pr(>|t|)
# a 6.620864e+04 2.308033e+07  0.002868618 9.977118e-01
# b 8.559308e+05 3.756498e+08  0.002278534 9.981825e-01
# c 7.956632e-01 5.587075e-02 14.241140794 6.760521e-42


height_model4$coefficients
#  Estimate Std. Error  t value     Pr(>|t|)
#A 146.6271   14.81020 9.900408 4.512875e-22
#B 203.0342   24.90957 8.150852 1.118651e-15




##### Using the 2nd order log-log (best model) ###
# lowest RSE 

# the predicted heights from this are only for the trees with a height 
# measurement

# use retrieveH to get the predicted heights for all DBHs

# gives a list - H is the predicted heights component
trees$height_log2 <- retrieveH(D = trees$Dbh_cm,
                               model = height_model2)$H




# use the predicted heights for trees without a height estimate;
# use the original estimate where it exists
trees$height_uselog2 <- NA

for(i in 1:length(trees$height_uselog2)){
  if(!is.na(trees$Height_m[i])){
    trees$height_uselog2[i] <- trees$Height_m[i]
  }else{
    trees$height_uselog2[i] <- trees$height_log2[i]
  }
}




####### Using the Chave et al 2014 environmental stress based height model
# this  requires coordinates and dbh

# E ranges from -0.17 to -0.034 across my study sites

height_chave <- retrieveH(D = trees$Dbh_cm,
                          coord = trees[,c("Longitude","Latitude")]) 

height_chave$RSE # from when equation was derived: RSE = 0.243

trees$height_chave <- height_chave$H


####### Using the Feldpausch et al 2012 regional height model
# this only requires dbh

height_feldpausch <- retrieveH(D = trees$Dbh_cm,
                          region = "SEAsia") # Southeast Asia 

height_feldpausch$RSE # from when equation was derived: RSE = 5.691

trees$height_feldpausch <- height_feldpausch$H





########## plotting #########

# height estimates separated for each site, to display each line of Chave E separately

# write column for site
trees$Site_code <- NA

trees$Site_plot_code <- as.character(trees$Site_plot_code)

for(i in 1:nrow(trees)){
  trees$Site_code[i] <- unlist(strsplit(trees$Site_plot_code[i], split = "P"))[1]
}

trees$Site_code <- as.factor(trees$Site_code)


trees.height <- gather(select(trees,
                              Dbh_cm, 
                              height_log2, 
                              height_chave, 
                              height_feldpausch, 
                              Site_code),
                       "model",
                       "height",
                       2:4)

# write variable that is combination of site and model - for chave per site
trees.height <- unite(trees.height,
                      "model.site",
                      model,
                      Site_code,
                      remove = FALSE)

# sort this into one df
trees.height.a <- filter(trees.height, model != "height_chave") %>%
  dplyr::select(-model.site, -Site_code)

trees.height.b <- filter(trees.height, model == "height_chave")%>%
  dplyr::select(-model, -Site_code)

names(trees.height.b)[2] <- "model"

trees_height <- rbind(trees.height.a, trees.height.b)
trees_height$model <- as.factor(trees_height$model)


# create colours and sizes for lines - for legend

height_col <- c(rep("#8b0000", 18),"black", "#589294")
names(height_col) <- levels(trees_height$model)

height_size <- c(rep(0.5,18), 1,1)
names(height_size) <- levels(trees_height$model)

ggplot()+
  geom_point(data = trees, aes(x = Dbh_cm, y = Height_m), 
             alpha = 0.5,
             colour = "cadetblue") +
  geom_line(data = trees_height,
            aes(x = Dbh_cm,
            y = height,
            col = model,
            size = model))+
  scale_colour_manual(values = height_col)+
  scale_size_manual(values = height_size)+
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"))+
  scale_x_continuous(limits = c(0,150), expand = c(0,0))+
  scale_y_continuous(limits = c(0,80), expand = c(0,0)) +
  xlab("Dbh/cm")+
  ylab("Height estimate/m")








############## 4. CALCULATING TREE BIOMASS ###############

#### Calculate biomass using the different height estimates - 


trees$biomass.Mg <- computeAGB(D = trees$Dbh_cm,
                               WD = GWD$meanWD,
                               H = trees$height_uselog2)




# in Mg (ton)
summary(trees$biomass.Mg)

trees$biomass.Mg_chaveE <- computeAGB(D = trees$Dbh_cm,
                                      WD = GWD$meanWD,
                                      H = trees$height_chave)

# in Mg (ton)
summary(trees$biomass.Mg_chaveE)


trees$biomass.Mg_feldpausch <- computeAGB(D = trees$Dbh_cm,
                                      WD = GWD$meanWD,
                                      H = trees$height_feldpausch)

# in Mg (ton)
summary(trees$biomass.Mg_feldpausch)






############ 5. SUMMARISING BY PLOT ################

# create column for the subplot area of each stem
trees <- mutate(trees, Subplot_radius_m = ifelse(Dbh_cm >= 10 & Dbh_cm <25,
                                      20,
                                      30))



# calculate total biomass for each subplot

trees <- trees %>% 
  group_by(Site_plot_code, Subplot_radius_m)

tree_biomass <- summarise(trees,
                          sum(biomass.Mg), 
                          sum(biomass.Mg_chaveE),
                          sum(biomass.Mg_feldpausch),
                          n())

names(tree_biomass) <- c("Site_plot_code", 
                         "Subplot_radius_m", 
                         "Tree_biomass.Mg_subplot", 
                         "Tree_biomass.Mg_chaveE_subplot", 
                         "Tree_biomass.Mg_feldpausch_subplot", 
                         "n_stems")

trees <- ungroup(trees)

# Carbon per subplot 

# carbon conversion factor from biomass is 0.471 for tropical angiosperms

# plot area m^2 = pi * subplot_radius_m^2
# plot area ha = pi * subplot_radius_m^2 / 10000

tree_biomass <- mutate(tree_biomass,
                       tree_carbon_subplot_Mg = Tree_biomass.Mg_subplot * 0.471,
                       tree_carbon_subplot_Mgha = (tree_carbon_subplot_Mg) / (pi * Subplot_radius_m^2 / 10000),
                       tree_carbon_chaveE_subplot_Mg = Tree_biomass.Mg_chaveE_subplot * 0.471,
                       tree_carbon_chaveE_subplot_Mgha = (tree_carbon_chaveE_subplot_Mg) / (pi * Subplot_radius_m^2 / 10000),
                       tree_carbon_feldpausch_subplot_Mg = Tree_biomass.Mg_feldpausch_subplot * 0.471,
                       tree_carbon_feldpausch_subplot_Mgha = (tree_carbon_feldpausch_subplot_Mg) / (pi * Subplot_radius_m^2 / 10000),
                       tree_nha = n_stems/(pi * Subplot_radius_m^2 / 10000))
  
tree_biomass




##### Rearrange this so that I have each row as one plot, and 
# each column as one component of C stocks

# Initially I am aiming for these columns:
# Site_plot_code
# tree_carbon_Mgha for each radius
# tree_nha (number of STEMS) for each radius
# where no trees recorded in a given subplot, the value will be NA

# easiest way is to do this separately for the counts and for the carbon

tree_carbon <- tree_biomass %>% dplyr::select(c(Site_plot_code, Subplot_radius_m, tree_carbon_subplot_Mgha)) %>%
  spread(Subplot_radius_m, tree_carbon_subplot_Mgha) 

names(tree_carbon) <- c("Site_plot_code", 
                       "tree_carbon_20m_Mgha",
                       "tree_carbon_30m_Mgha")

tree_carbon_chaveE <- tree_biomass %>% dplyr::select(c(Site_plot_code, Subplot_radius_m, tree_carbon_chaveE_subplot_Mgha)) %>%
  spread(Subplot_radius_m, tree_carbon_chaveE_subplot_Mgha) 

names(tree_carbon_chaveE) <- c("Site_plot_code", 
                        "tree_carbon_chaveE_20m_Mgha",
                        "tree_carbon_chaveE_30m_Mgha")

tree_carbon_feldpausch <- tree_biomass %>% dplyr::select(c(Site_plot_code, Subplot_radius_m, tree_carbon_feldpausch_subplot_Mgha)) %>%
  spread(Subplot_radius_m, tree_carbon_feldpausch_subplot_Mgha) 

names(tree_carbon_feldpausch) <- c("Site_plot_code", 
                               "tree_carbon_feldpausch_20m_Mgha",
                               "tree_carbon_feldpausch_30m_Mgha")



tree_n <- tree_biomass %>% dplyr::select(c(Site_plot_code, Subplot_radius_m, tree_nha)) %>%
  spread(Subplot_radius_m, tree_nha)

names(tree_n) <- c("Site_plot_code", 
                  "tree_count_20m_nha",
                  "tree_count_30m_nha")

tree_carbon <- left_join(tree_carbon, tree_carbon_chaveE, by = "Site_plot_code") %>%
  left_join(tree_carbon_feldpausch, by = "Site_plot_code")%>%
  left_join(tree_n, by = "Site_plot_code")



#### Comparing height~dbh models #######

# set NAs to 0 (where no trees in a certain subplot)
tree_carbon[is.na(tree_carbon)] <- 0


tree_carbon <- mutate(tree_carbon, 
                      tree_carbon_Mgha = sum(c(tree_carbon_20m_Mgha,
                                               tree_carbon_30m_Mgha)),
                      tree_carbon_chaveE_Mgha = sum(c(tree_carbon_chaveE_20m_Mgha,
                                               tree_carbon_chaveE_30m_Mgha)),
                      tree_carbon_feldpausch_Mgha = sum(c(tree_carbon_feldpausch_20m_Mgha,
                                                      tree_carbon_feldpausch_30m_Mgha)))



#### SEE LATER FOR PLOTS COMPARING CARBON ESTIMATES OF DIFFERENT HEIGHT MODELS







###################### 6. LIANA BIOMASS ###########################

head(lianas)

# Schnitzer et al 2006 - liana allometric equation based on diameter
# AGB = exp(-1.484 + 2.657*ln(D))

# note log() is natural logarithm

lianas <- mutate(lianas, liana.biomass.kg = exp((-1.484 + 2.657*log(Liana_dbh_cm))))

head(lianas)


# assign subplot for the tree each liana is associated with (as for trees above)

lianas <- mutate(lianas, Subplot_radius_m = ifelse(Tree_dbh_cm >= 10 & Tree_dbh_cm <25,
                                                        20,
                                                        30))

# calculate total biomass for each subplot

lianas <- lianas %>% 
  group_by(Site_plot_code, Subplot_radius_m)

liana_biomass <- summarise(lianas, sum(liana.biomass.kg))

names(liana_biomass) <- c("Site_plot_code", "Subplot_radius_m", "Liana_biomass.kg_subplot")

lianas <- ungroup(lianas)

# Carbon per subplot 
# plot area m^2 = pi * subplot_radius_m^2
# plot area ha = pi * subplot_radius_m^2 / 10000

# 1 Mg is 1000kg

liana_biomass <- mutate(liana_biomass, 
                        liana_carbon_subplot_Mg = Liana_biomass.kg_subplot * 0.471 /1000,
                        liana_carbon_subplot_Mgha = (liana_carbon_subplot_Mg) / (pi * Subplot_radius_m^2 / 10000))

liana_biomass


##### Rearrange this so that I have each row as one plot, and 
# each column as one component of C stocks

# Site_plot_code
# liana_carbon_Mgha for each radius
# where no lianas recorded in a given subplot, the value will be NA

liana_carbon <- liana_biomass %>% dplyr::select(c(Site_plot_code, Subplot_radius_m, liana_carbon_subplot_Mgha)) %>%
  spread(Subplot_radius_m, liana_carbon_subplot_Mgha) 

names(liana_carbon) <- c("Site_plot_code", 
                        "liana_carbon_20m_Mgha",
                        "liana_carbon_30m_Mgha")






###################### 7. PALMS #################


palms <- filter(trees_all, ID == "PALM")

# Palms allometric equation: 
# AGB(kg) =	dmf * D(cm)^2 * Hstem(m)

# Dry mass fraction (dmf) as the proportion of 
# dry mass per unit fresh mass (dry mass/fresh mass or 1 – moisture content)
# This was found to be a better predictor than wood density.

# D is diameter (cm)
# Hstem is stem height in m

# This model is only valid for individuals with Hstem > 1m

# From the same reference: mean dmf for mixed species =  0.370

palms <- mutate(palms, biomass_kg = 0.37 * Dbh_cm^2 * Height_m)


# create column for the subplot area of each stem
palms <- mutate(palms, Subplot_radius_m = ifelse(Dbh_cm >= 10 & Dbh_cm <25,
                                                        20,
                                                        30))

# calculate total biomass for each subplot

palms <- palms %>% 
  group_by(Site_plot_code, Subplot_radius_m)

palm_biomass <- summarise(palms, sum(biomass_kg), n())

names(palm_biomass) <- c("Site_plot_code", "Subplot_radius_m", "Palm_biomass.kg_subplot", "n_palms")

palms <- ungroup(palms)

# Carbon per subplot 
# plot area m^2 = pi * subplot_radius_m^2
# plot area ha = pi * subplot_radius_m^2 / 10000

palm_biomass <- mutate(palm_biomass,
                       palm_carbon_subplot_Mg = Palm_biomass.kg_subplot * 0.471 / 1000,
                       palm_carbon_subplot_Mgha = (palm_carbon_subplot_Mg) / (pi * Subplot_radius_m^2 / 10000),
                       palm_nha = n_palms/(pi * Subplot_radius_m^2 / 10000))

palm_biomass



##### Rearrange this so that I have each row as one plot, and 
# each column as one component of C stocks

# where no palms recorded in a given subplot, the value will be NA

palm_carbon <- palm_biomass %>% dplyr::select(c(Site_plot_code, Subplot_radius_m, palm_carbon_subplot_Mgha)) %>%
  spread(Subplot_radius_m, palm_carbon_subplot_Mgha) 

names(palm_carbon) <- c("Site_plot_code", 
                        "palm_carbon_20m_Mgha",
                        "palm_carbon_30m_Mgha")

palm_n <- palm_biomass %>% dplyr::select(c(Site_plot_code, Subplot_radius_m, palm_nha)) %>%
  spread(Subplot_radius_m, palm_nha)

names(palm_n) <- c("Site_plot_code", 
                   "palm_count_20m_nha",
                   "palm_count_30m_nha")

palm_carbon <- left_join(palm_carbon, palm_n, by = "Site_plot_code")







###################### 8. CWD BIOMASS ###########################

head(CWD)
# Type is standing, fallen, hanging

# wood density classes, based on decay - see Pfeifer et al 2015 
# 1		0.40 ± 0.03	
# 2		0.58 ± 0.08	
# 3		0.37 ± 0.03	
# 4		0.26 ± 0.02	
# 5		0.16 ± 0.06	
# in g/cm^3


# Fallen: calculate the volume of each piece of CWD as a truncated cone 'frustrum of a cone'
# V = h * pi / 3 * ( R² + Rr + r² )
# where R is base radius and r is top radius
# so V (cm^3) = Length_m*100 * pi/3 * 
#              ((Dbh_1_cm/2)^2 + (Dbh_1_cm/2) * (Dbh_2_cm/2) + (Dbh_2_cm/2)^2)


## Standing/hanging
# volume is frustrum of a cone as above
# I need to estimate (i) base diameter and (ii) top diameter

# Where height >130cm, estimate top diameter (at max height) 
# AND base diameter (at 1cm height - chosen because negative exponential curves go towards infinity below 1) using 
# Taper function from Chambers et al 2000 
# dh=1.59DBH(h^–0.091)
# diameter at height h is a function of the dbh (at 130cm) and height h
# So diameter height h  = 1.59*dbh*(h^(-0.091)) ALL IN cm
# and conversely dbh at 130cm = diam at height h / (1.59*h^(-0.091))

# Where height <130cm, so I couldn't measure 'true' dbh,
# recorded dbh is top diameter;
# calculate base diameter (1cm from ground) using the taper function
# first find the dbh as dbh at 130cm = diam at height h / (1.59*h^(-0.091))
# in order to find the base diameter 1cm from ground


# for dead palm calculate biomass as for living palms
# because palm wood density fluctuates as it decays 




####### put in wood density for everything 



# create lookup table of wd values for the decay classes

CWD_wd <- data.frame(Decay = c(1:5),
                     wd_gcm3 = c(0.4, 0.58, 0.37, 0.26, 0.16))

# set wd column based on decay classes
CWD <- CWD %>% left_join(CWD_wd, by = "Decay") 

# for palms, set this to NA
CWD[grep("PALM", CWD$Notes),"wd_gcm3"] <- NA


###### For H/S, write column for dbh at 130cm

# for H/S where length <130cm, I need to estimate the dbh using taper function
# otherwise it is simply the Dbh_1 value

CWD$Dbh130cm <- NA

for(i in 1:nrow(CWD)){
  if((CWD$Type[i] == "H" | CWD$Type[i] == "S") & CWD$Length_m[i] < 1.3){
    CWD$Dbh130cm[i] <- CWD$Dbh_1_cm[i] / (1.59*(CWD$Length_m[i]*100)^(-0.091))
    # dbh at 130cm = diam at height h / (1.59*h^(-0.091))
    # note height is in m so I have *100
  }else if((CWD$Type[i] == "H" | CWD$Type[i] == "S") & CWD$Length_m[i] >= 1.3){
    CWD$Dbh130cm[i] <- CWD$Dbh_1_cm[i]
  }else{
    CWD$Dbh130cm[i] <- NA
  }
}



##### for H/S, estimate diameter at base, 1cm height

CWD$D1cm <- NA

for(i in 1:nrow(CWD)){
  if(CWD$Type[i] == "H" | CWD$Type[i] == "S"){
    CWD$D1cm[i] <- 1.59 * CWD$Dbh130cm[i] * 1^(-0.091)
    # diameter at height h = 1.59*dbh*(h^(-0.091))
  }else{
    CWD$D1cm[i] <- NA
  }
}




##### for H/S, estimate diameter at top, max height

CWD$Dmax <- NA

for(i in 1:nrow(CWD)){
  if((CWD$Type[i] == "H" | CWD$Type[i] == "S") & CWD$Length_m[i] > 1.3){
    CWD$Dmax[i] <- 1.59 * CWD$Dbh130cm[i] * (CWD$Length_m[i]*100)^(-0.091)
    # diameter at height h = 1.59*dbh*(h^(-0.091))
    # note height is in m so I have *100
  }else if((CWD$Type[i] == "H" | CWD$Type[i] == "S") & CWD$Length_m[i] <= 1.3){
    CWD$Dmax[i] <- CWD$Dbh_1_cm[i]
  }else{
    CWD$Dmax[i] <- NA
  }
}




###### calculate volume for each piece


CWD$volumecm3 <- NA

for(i in 1:nrow(CWD)){
  if(grepl("PALM", CWD$Notes[i]) == TRUE){
    CWD$volumecm3[i] <- NA
  }else if(CWD$Type[i] == "F"){
    CWD$volumecm3[i] <- CWD$Length_m[i]*100 * pi/3 * ((CWD$Dbh_1_cm[i]/2)^2 + (CWD$Dbh_1_cm[i]/2) * (CWD$Dbh_2_cm[i]/2) + (CWD$Dbh_2_cm[i]/2)^2)
   # V = h * pi / 3 * ( R² + Rr + r² )
  }else{ # i.e. for all fallen and hanging
    CWD$volumecm3[i] <- CWD$Length_m[i]*100 * pi/3 * ((CWD$D1cm[i]/2)^2 + (CWD$D1cm[i]/2) * (CWD$Dmax[i]/2) + (CWD$Dmax[i]/2)^2)
  }
}
  
    


##### calculate biomass for each piece

CWD <- mutate(CWD, biomass.Mg = ifelse(grepl("PALM", Notes),
                     0.37 * Dbh_1_cm^2 * Length_m/1000, # note need to convert from kg to Mg
                     wd_gcm3 * volumecm3/1000000))




###### bring together in summary table #######

# assign subplot for the tree each CWD piece is associated with (as for trees & lianas)

# create column for the subplot area of each stem
CWD <- mutate(CWD, Subplot_radius_m = ifelse((Dbh_1_cm < 25 & Dbh_2_cm < 25) | (Dbh_1_cm < 25 & is.na(Dbh_2_cm)), 
                                            20, 
                                            30))


# calculate total biomass for each subplot

CWD <- CWD %>% 
  group_by(Site_plot_code, Subplot_radius_m, Type)


CWD_biomass <- summarise(CWD, sum(biomass.Mg), n())

names(CWD_biomass) <- c("Site_plot_code", "Subplot_radius_m", "Type", "CWD_biomass.Mg_subplot", "n")

CWD <- ungroup(CWD)

# Carbon per subplot 
# plot area m^2 = pi * subplot_radius_m^2
# plot area ha = pi * subplot_radius_m^2 / 10000

CWD_biomass <- mutate(CWD_biomass, 
                      CWD_carbon_subplot_Mg = CWD_biomass.Mg_subplot * 0.471,
                      CWD_carbon_subplot_Mgha = (CWD_carbon_subplot_Mg) / (pi * Subplot_radius_m^2 / 10000))


##### Rearrange this so that I have each row as one plot, and 
# each column as one component of C stocks
# separated by fallen, standing, hanging
# CWD_carbon_Mgha for each type and radius: F20, S20, H20, F30, S30, H30
# where no CWD of a given type was recorded in a given subplot, the value will be NA


filter(CWD, Type == "H")
# note that all H was <20cm dbh so within the 20m radius - no columns for 20m and H


CWD_carbon <- CWD_biomass %>% dplyr::select(c(1:3, 7)) %>%
                unite(Subplot_type, Subplot_radius_m, Type) %>%
                spread(Subplot_type, CWD_carbon_subplot_Mgha) 
# uses unite to bind the subplot and type column

names(CWD_carbon) <- c("Site_plot_code", 
                       "CWD_carbon_F_20m_Mgha", 
                       "CWD_carbon_H_20m_Mgha", 
                       "CWD_carbon_S_20m_Mgha", 
                       "CWD_carbon_F_30m_Mgha", 
                       "CWD_carbon_S_30m_Mgha")










##################### 9. COMBINE ALL THESE CARBON STOCKS ###################

tree_carbon
liana_carbon
CWD_carbon
palm_carbon

# set the Site_plot_code column to factor with same levels available

tree_carbon$Site_plot_code <- as.character(tree_carbon$Site_plot_code)
liana_carbon$Site_plot_code <- as.character(liana_carbon$Site_plot_code)
CWD_carbon$Site_plot_code <- as.character(CWD_carbon$Site_plot_code)
palm_carbon$Site_plot_code <- as.character(palm_carbon$Site_plot_code)

plot_carbon <- left_join(tree_carbon, liana_carbon, by = "Site_plot_code") %>%
  left_join(CWD_carbon, by = "Site_plot_code") %>%
  left_join(palm_carbon, by = "Site_plot_code")

plot_carbon

# Set NAs to 0

plot_carbon[is.na(plot_carbon)] <- 0


names(plot_carbon)

# totalling and creating different sums:
plot_carbon <- mutate(plot_carbon,
                      liana_carbon_Mgha = sum(c(liana_carbon_20m_Mgha,
                                                liana_carbon_30m_Mgha)),
                      CWD_carbon_Mgha = sum(c(CWD_carbon_F_20m_Mgha,
                                              CWD_carbon_H_20m_Mgha,
                                              CWD_carbon_S_20m_Mgha,
                                              CWD_carbon_F_30m_Mgha,
                                              CWD_carbon_S_30m_Mgha)),
                      palm_carbon_Mgha = sum(c(palm_carbon_20m_Mgha,
                                               palm_carbon_30m_Mgha)),
                      total_carbon_Mgha = sum(c(tree_carbon_20m_Mgha,
                                                tree_carbon_30m_Mgha,
                                                liana_carbon_20m_Mgha,
                                                liana_carbon_30m_Mgha,
                                                CWD_carbon_F_20m_Mgha,
                                                CWD_carbon_H_20m_Mgha,
                                                CWD_carbon_S_20m_Mgha,
                                                CWD_carbon_F_30m_Mgha,
                                                CWD_carbon_S_30m_Mgha,
                                                palm_carbon_20m_Mgha,
                                                palm_carbon_30m_Mgha)),
                      tree_palm_carbon_Mgha = sum(c(tree_carbon_20m_Mgha,
                                                    tree_carbon_30m_Mgha,
                                                    palm_carbon_20m_Mgha,
                                                    palm_carbon_30m_Mgha)),
                      tree_palm_carbon_chaveE_Mgha = sum(c(tree_carbon_chaveE_20m_Mgha,
                                                           tree_carbon_chaveE_30m_Mgha,
                                                           palm_carbon_20m_Mgha,
                                                           palm_carbon_30m_Mgha)),
                      tree_palm_carbon_feldpausch_Mgha = sum(c(tree_carbon_feldpausch_20m_Mgha,
                                                               tree_carbon_feldpausch_30m_Mgha,
                                                               palm_carbon_20m_Mgha,
                                                               palm_carbon_30m_Mgha)),
                      palm_count_nha = sum(c(palm_count_20m_nha,
                                             palm_count_30m_nha)),
                      CWD_liana_carbon_Mgha = sum(c(liana_carbon_Mgha,
                                                    CWD_carbon_Mgha)))




######### C STOCKS OF OIL PALM ############


######## equations for estimating oil palm carbon

# functions for OP carbon from Carlson et al 
# mean 5.97*x^0.62
# low -2.7+2.35*x+(-0.04)*x^2
# high 13.57+1.83*x+(-0.003)*x^2

# note that in the paper low and high are the other way round, in order to get maximum and minimum
# values of C flux when converting forest to OP

# getting average height of curve between values - mean value theorem for definite integrals
# (1/(b-a)) * integral of curve
# gives the mean height of the curve between a and b

f_mean <- function(x){5.97*x^0.62}
f_low <- function(x){-2.7+2.35*x+(-0.04)*x^2}
f_high <- function(x){13.57+1.83*x+(-0.003)*x^2}



# for 0-30 year time average AG carbon stock

f_mean_int_30 <- integrate(f_mean, lower = 0, upper = 30, rel.tol = 100000)
OP_C_mean <- f_mean_int_30$value/(30-0) # 30.35818

f_low_int_30 <- integrate(f_low, lower = 0, upper = 30, rel.tol = 100000)
OP_C_low <- f_low_int_30$value/(30-0) # 20.55

f_high_int_30 <- integrate(f_high, lower = 0, upper = 30)
OP_C_high <- f_high_int_30$value/(30-0) # 40.12


# calculating SE for 0-30 years

# using the mean of upper and lower as the mean for this calculation, so that it is 
# exactly central
OP_C_mean <- mean(c(OP_C_high, OP_C_low)) # 30.335

OP_C_SE <- (OP_C_high - OP_C_mean)/1.96 # SE = 4.99

# SE = SD/sqrt(n)
# so SD = SE * sqrt(n)
OP_C_SD <- ((40.12-30.335)/1.96) * sqrt(15)
# 19.33528


####### Generate oil palm carbon data to use in lme ##########

# generate n = 15 data points (number used to derive relationships in orignal study)
# with mean and SD to match those of OP above

library(MASS)

OP_C <- mvrnorm(n = 15,
                mu = OP_C_mean,
                Sigma = OP_C_SD^2,
                empirical = TRUE)

mean(OP_C) # 30.335 - as specified
sd(OP_C) # 19.335 - as specified
sd(OP_C)/sqrt(length(OP_C)) # 4.99 - matches with original


plot(OP_C)


