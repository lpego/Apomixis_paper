# ##### Path definition #####
# path <- ("/home/luca/Dropbox/R/Apomixis_paper")
# 
# ### Setting working directory
# setwd(path)
# getwd()

##### Load data ##### 
### Extended dataset, summarized data with missing values, mean
DATA_mean_red
str(DATA_mean_red)
ToBeFactors <- c("ID_FloraAlpina", "ID", "Repr_mode", "Repr_mode_summ",
                 "Ploidy", "Ploidy_summ", "PloidyEvenOdd",
                 "Longevity", "BiologicalForm", "Endemic", "Indigenous", "Distribution",
                 "Collinaire", "Montane", "Subalpine", "Alpine", "Nival",
                 "Phytosociology", "Habitat",
                 "Ca", "Ca.Si", "Si", "pH", "N", "Water", "w")
DATA_mean_red[ToBeFactors] <- lapply(DATA_mean_red[ToBeFactors], factor)
str(DATA_mean_red)
DATA_mean_red$Repr_mode
DATA_mean_red$Repr_mode_summ
DATA_mean_red$Ploidy
DATA_mean_red$Ploidy_summ
DATA_mean_red[!complete.cases(DATA_mean_red), ] # Sigesbeckia is not in Flora Alpina

### Extended dataset, summarized data complete no missing values, mean
DATA_CC_mean_red
str(DATA_CC_mean_red)
DATA_CC_mean_red[ToBeFactors] <- lapply(DATA_CC_mean_red[ToBeFactors], factor)
str(DATA_CC_mean_red)
DATA_CC_mean_red[!complete.cases(DATA_CC_mean_red), ] # no missing values

### Strictly alpine dataset, summarized data, mean (there ano missing values in strictly alpine dataset)
DATA_StrictlyAlps_mean_red
str(DATA_StrictlyAlps_mean_red)
DATA_StrictlyAlps_mean_red[ToBeFactors] <- lapply(DATA_StrictlyAlps_mean_red[ToBeFactors], factor)
str(DATA_StrictlyAlps_mean_red)
DATA_StrictlyAlps_mean_red[!complete.cases(DATA_StrictlyAlps_mean_red), ] # no missing values



##### Phylogenetic tree #####
### Load the tree
library(ape)
library(geiger)
library(phytools)

### Extended dataset, missing values
JanTree4
plot(JanTree4, cex = .5, no.margin = T)
str(JanTree4)
is.rooted(JanTree4)
is.ultrametric(JanTree4)
is.binary(JanTree4)
JanTree4$edge.length[JanTree4$edge.length == 0]

name.check(JanTree4, DATA_mean_red)

### Extended dataset, no missing values
JanTree4_CC
plot(JanTree5_CC, cex = .5, no.margin = T)
str(JanTree4_CC)
is.rooted(JanTree4_CC)
is.ultrametric(JanTree4_CC)
is.binary(JanTree4_CC)
JanTree4_CC$edge.length[JanTree4_CC$edge.length == 0]

name.check(JanTree4_CC, DATA_mean_red)




##### ~ ##### 



##### DATA ANALYTICS ##### 

##### How many genera and species? ######
unique(gsub('_.*', '', DATA_red$SpeciesName)) # list of unique genera
length(unique(gsub('_.*', '', DATA_red$SpeciesName))) # number of unique genera

unique(gsub('_subsp.*', '', DATA_red$SpeciesName)) # list of unique species (no subspecies)
length(unique(gsub('_subsp.*', '', DATA_red$SpeciesName))) # number of unique species (no subspecies)

DATA_red$SpeciesName
length(unique(DATA_red$SpeciesName)) # number of unique taxa (also subspecies)

table(DATA_red$Repr_mode_summ) # number of sexual and apomictic taxa

max(DATA_red$Altitude, na.rm = T)
min(DATA_red$Altitude, na.rm = T)
boxplot(DATA_red$Altitude)

DATA_red[((DATA_red[,"Altitude"]<1000) & (DATA_red[,"Repr_mode_summ"]=="Apomictic")), ] # taxa collected below 1000m and apomictic
DATA_red[((DATA_red[,"Altitude"]<1000) & (DATA_red[,"Repr_mode_summ"]=="Mixed")), ] # taxa collected below 1000m and mixed

# max(DATA_red$GS, na.rm = T)
# min(DATA_red$GS, na.rm = T)
# boxplot(DATA_red$GS)

##### How many "unexpected" reproductive modes in the accessions? #####
nrow(DATA[DATA$Ploidy == "2x", 1:3]) # 258 total diploids
DATA[DATA$Ploidy == "2x" & DATA$Repr_mode_summ == "Apomictic", 1:3] # no diploids are apomictic
DATA[DATA$Ploidy == "2x" & DATA$Repr_mode_summ == "Mixed", 1:3] # 3 species are diploids and apomictic

nrow(DATA[DATA$Ploidy == "3x", 1:3]) # 31 total triploids
DATA[DATA$Ploidy == "3x" & DATA$Repr_modesumm == "Sexual", 1:3] # no triploids are sexual

nrow(DATA[DATA$Ploidy == "4x", 1:3]) # 114 total tetraploid
DATA[DATA$Ploidy == "4x" & DATA$Repr_mode == "Apomictic", 1:3] # 6 tetraploid accessions are apomictic

DATA[DATA$Repr_mode_summ == "Mixed", c(1:3,8)] # mixed reproductive mode is found in 1 tetraploid, 1 triploid and 4 diploid species

##### Are there taxa with multiple ploidy levels? #####
table <- with(DATA, table(SpeciesName, Ploidy))
rownames(table)
table

x = 1
y = 1
sum = 0
MultiplePloidy <- data.frame(Name = as.character(rownames(table)), MultiplePloidy = factor(rep("~", length(rownames(table))), levels = c("~", "YES", "NO")))
while (x %in% 1:length(rownames(table))) {
  while (y <= 6) {
    sum <- sum + as.numeric(table[x, y] >= 1)
    y = y + 1}
  if (sum > 1) {MultiplePloidy[x, "MultiplePloidy"] <- "YES"} else {MultiplePloidy[x, "MultiplePloidy"] <- "NO"}
  x = x + 1
  sum = 0
}
MultiplePloidy$MultiplePloidy

##### Checking taxa numbers across different versions of the dataset ##### 
### Extended dataset
vapply(strsplit(as.character(unique(DATA$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))
genera_all <- unique(vapply(strsplit(as.character(unique(DATA$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
genera_all
### Srictly Alps
vapply(strsplit(as.character(unique(DATA_StrictlyAlps$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))
genera_alps <- unique(vapply(strsplit(as.character(unique(DATA_StrictlyAlps$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
genera_alps
### Apomixis paper Supplementary Table v5
table_v5 <- c("Achillea clavennae", "Achillea erba-rotta", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", 
              "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", "Achillea nana", "Achillea nana", "Achillea nana", 
              "Achillea nana", "Achillea nobilis", "Achillea oxyloba", "Adenostyles alliariae", "Adenostyles alliariae", "Adenostyles alliariae", "Adenostyles alpina", 
              "Adenostyles leucophylla", "Adenostyles leucophylla", "Adenostyles leucophylla", "Adenostyles leucophylla", "Andryala integrifolia", 
              "Antennaria carpatica", "Antennaria carpatica", "Antennaria carpatica", "Antennaria dioica", "Antennaria dioica", "Antennaria dioica", 
              "Antennaria dioica", "Antennaria dioica", "Antennaria dioica", "Aposeris foetida", "Arctium lappa", "Arctium lappa", "Arctium lappa", "Arctium lappa", 
              "Arctium minus", "Arctium minus", "Arctium tomentosum", "Arnica montana", "Arnica montana", "Arnica montana", "Arnica montana", "Arnica montana", 
              "Arnica montana", "Arnica montana", "Artemisia absinthium", "Artemisia absinthium", "Artemisia genipi", "Artemisia glacialis", "Artemisia nitida", 
              "Artemisia nitida", "Artemisia umbelliformis subsp. eriantha", "Artemisia umbelliformis subsp. umbelliformis", "Artemisia umbelliformis subsp. umbelliformis", 
              "Artemisia vulgaris", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", 
              "Bellidiastrum michelii", "Bellis perennis", "Berardia lanuginosa", "Berardia lanuginosa", "Bidens bipinnatus", "Bidens frondosus", 
              "Buphthalmum salicifolium", "Buphthalmum salicifolium", "Buphthalmum salicifolium", "Buphthalmum salicifolium", "Buphthalmum salicifolium", 
              "Calendula arvensis", "Calendula arvensis", "Calendula tripterocarpa", "Carduus acanthoides", "Carduus crispus", "Carduus defloratus", 
              "Carduus defloratus subsp. carlinifolius", "Carduus defloratus subsp. summanus", "Carduus personata", "Carduus personata", "Carlina acanthifolia", 
              "Carlina acaulis", "Carlina acaulis", "Carlina corymbosa", "Carlina vulgaris", "Catananche caerulea", "Catananche caerulea", "Catananche caerulea", 
              "Catananche caerulea", "Centaurea benedicta", "Centaurea jacea", "Centaurea jacea", "Centaurea jacea", "Centaurea jacea subsp. gaudinii", 
              "Centaurea jacea subsp. gaudinii", "Centaurea leucophaea", "Centaurea margaritacea", "Centaurea nervosa", "Centaurea nervosa", "Centaurea nigra", 
              "Centaurea pectinata", "Centaurea rhaetica", "Centaurea rupestris", "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. alpestris", 
              "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. grinensis", 
              "Centaurea uniflora", "Centaurea uniflora", "Centaurea uniflora", "Centaurea valesiaca", "Chondrilla juncea", "Chondrilla juncea", "Cirsium acaulon", 
              "Cirsium acaulon", "Cirsium acaulon", "Cirsium acaulon", "Cirsium acaulon", "Cirsium alsophilum", "Cirsium alsophilum", "Cirsium alsophilum", 
              "Cirsium arvense", "Cirsium arvense", "Cirsium carniolicum", "Cirsium carniolicum", "Cirsium eriophorum", "Cirsium erisithales", "Cirsium erisithales", 
              "Cirsium heterophyllum", "Cirsium monspessulanum", "Cirsium monspessulanum", "Cirsium monspessulanum", "Cirsium oleraceum", "Cirsium oleraceum", 
              "Cirsium pannonicum", "Cirsium rivulare", "Cirsium spinosissimum", "Cirsium spinosissimum", "Cirsium tuberosum", "Cirsium vulgare", "Cirsium vulgare", 
              "Cota tinctoria", "Cota tinctoria", "Cota triumfettii", "Crepis aurea", "Crepis aurea", "Crepis capillaris", "Crepis conyzifolia", "Crepis conyzifolia", 
              "Crepis jacquinii subsp. kerneri", "Crepis paludosa", "Crepis paludosa", "Crepis pontana", "Crepis pontana", "Crepis pygmaea", "Crepis pygmaea", 
              "Crepis pyrenaica", "Crepis pyrenaica", "Crepis rhaetica", "Crepis sancta", "Crepis tectorum", "Crepis terglouensis", "Crepis vesicaria", "Crepis vesicaria", 
              "Crepis vesicaria", "Crepis vesicaria", "Crupina vulgaris", "Cyanus montanus", "Cyanus montanus", "Cyanus segetum", "Cyanus segetum", 
              "Cyanus triumfettii", "Dittrichia graveolens", "Doronicum austriacum", "Doronicum clusii", "Doronicum grandiflorum", "Doronicum grandiflorum", 
              "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", 
              "Doronicum grandiflorum", "Doronicum pardalianches", "Echinops exaltatus", "Echinops ritro", "Echinops sphaerocephalus", "Echinops sphaerocephalus", 
              "Erigeron acris subsp. acris", "Erigeron acris subsp. acris", "Erigeron alpinus", "Erigeron alpinus", "Erigeron alpinus", "Erigeron alpinus", 
              "Erigeron annuus", "Erigeron atticus", "Erigeron bonariensis", "Erigeron canadensis", "Erigeron canadensis", "Erigeron glabratus", "Erigeron karvinskianus", 
              "Erigeron schleicheri", "Erigeron schleicheri", "Erigeron uniflorus", "Erigeron uniflorus", "Eupatorium cannabinum", "Galinsoga parviflora", 
              "Galinsoga quadriradiata", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", "Gnaphalium supinum", 
              "Gnaphalium supinum", "Gnaphalium supinum", "Gnaphalium sylvaticum", "Helianthus tuberosus", "Helichrysum italicum", "Helichrysum italicum", 
              "Helichrysum italicum", "Hieracium alpinum", "Hieracium amplexicaule", "Hieracium amplexicaule", "Hieracium bifidum", "Hieracium cydoniifolium", 
              "Hieracium froelichianum", "Hieracium glaucopsis", "Hieracium glaucum", "Hieracium humile", "Hieracium lawsonii", "Hieracium murorum", 
              "Hieracium murorum", "Hieracium murorum", "Hieracium murorum", "Hieracium murorum", "Hieracium piliferum", "Hieracium piliferum", "Hieracium prenanthoides", 
              "Hieracium ramosissimum subsp. lactucifolium", "Hieracium tomentosum", "Hieracium valdepilosum", "Hieracium villosum", "Hieracium villosum", 
              "Hieracium villosum", "Homogyne alpina", "Homogyne alpina", "Homogyne alpina", "Homogyne discolor", "Homogyne sylvestris", "Hypochaeris maculata", 
              "Hypochaeris maculata", "Hypochaeris maculata", "Hypochaeris radicata", "Hypochaeris radicata", "Hypochaeris uniflora", "Hypochaeris uniflora", 
              "Hypochaeris uniflora", "Inula bifrons", "Inula conyzae", "Inula conyzae", "Inula helenium", "Inula helvetica", "Inula montana", "Inula oculus-christi", 
              "Inula salicina", "Inula spiraefolia", "Jacobaea abrotanifolia subsp. abrotanifolia", "Jacobaea abrotanifolia subsp. abrotanifolia", 
              "Jacobaea abrotanifolia subsp. abrotanifolia", "Jacobaea alpina subsp. alpina", "Jacobaea alpina subsp. alpina", "Jacobaea aquatica", "Jacobaea carniolica", 
              "Jacobaea carniolica", "Jacobaea erucifolia", "Jacobaea erucifolia", "Jacobaea erucifolia", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", 
              "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea subalpina", "Jurinea mollis", "Klasea lycopifolia", 
              "Lactuca alpina ", "Lactuca alpina ", "Lactuca muralis", "Lactuca muralis", "Lactuca perennis", "Lactuca perennis", "Lactuca perennis", "Lactuca serriola", 
              "Lactuca serriola", "Laphangium luteoalbum", "Lapsana communis", "Lapsana communis", "Leontodon crispus", "Leontodon hirtus", "Leontodon hispidus", 
              "Leontodon hispidus", "Leontodon tenuiflorus ", "Leontopodium nivale subsp. alpinum", "Leontopodium nivale subsp. alpinum", 
              "Leontopodium nivale subsp. alpinum", "Leontopodium nivale subsp. alpinum", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", 
              "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemum adustum", 
              "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", 
              "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum atratum", "Leucanthemum atratum", "Leucanthemum coronopifolium", 
              "Leucanthemum coronopifolium", "Leucanthemum coronopifolium", "Leucanthemum halleri", "Leucanthemum pallens", "Leucanthemum platylepis", 
              "Matricaria chamomilla", "Onopordum acanthium", "Onopordum acanthium", "Pallenis spinosa", "Petasites albus", "Petasites paradoxus", "Phagnalon saxatile", 
              "Picris hieracioides", "Picris hieracioides", "Picris hieracioides", "Pilosella aurantiaca", "Pilosella aurantiaca", "Pilosella cymosa", 
              "Pilosella glacialis", "Pilosella hoppeana", "Pilosella lactucella", "Pilosella officinarum", "Pilosella peleteriana", "Pilosella peleteriana", 
              "Pilosella piloselloides", "Podospermum laciniatum", "Podospermum purpureum", "Prenanthes purpurea", "Pulicaria dysenterica", "Pulicaria dysenterica", 
              "Rhaponticum heleniifolium subsp. bicknellii", "Rhaponticum heleniifolium subsp. heleniifolium", "Rhaponticum scariosum", "Saussurea alpina", 
              "Saussurea alpina", "Schlagintweitia huteri subsp. lantoscana", "Schlagintweitia intybacea", "Scorzonera humilis", "Scorzoneroides autumnalis", 
              "Scorzoneroides crocea", "Scorzoneroides crocea", "Scorzoneroides montana", "Scorzoneroides montana", "Scorzoneroides montana", "Senecio doria", 
              "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", 
              "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", 
              "Senecio doronicum", "Senecio inaequidens", "Senecio nemorensis subsp. jacquinianus", "Senecio ovatus", "Senecio squalidus subsp. rupestris", 
              "Senecio squalidus subsp. rupestris", "Senecio viscosus", "Senecio viscosus", "Senecio vulgaris", "Senecio vulgaris", "Senecio vulgaris", "Senecio vulgaris", 
              "Serratula tinctoria", "Sigesbeckia orientalis", "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea", 
              "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea subsp. minuta", "Sonchus oleraceus", "Sonchus oleraceus", "Sonchus tenerrimus", 
              "Staehelina dubia", "Staehelina dubia", "Symphyotrichum squamatum", "Tanacetum corymbosum", "Tanacetum macrophyllum", "Tanacetum parthenium", 
              "Taraxacum officinale", "Taraxacum officinale", "Taraxacum officinale", "Taraxacum officinale", "Telekia speciosa", "Telekia speciosa", 
              "Tephroseris integrifolia", "Tephroseris integrifolia subsp. capitata", "Tephroseris longifolia subsp. gaudinii", "Tolpis staticifolia", 
              "Tolpis staticifolia", "Tragopogon crocifolius", "Tragopogon dubius", "Tragopogon pratensis", "Tragopogon pratensis", "Tragopogon pratensis", 
              "Tragopogon pratensis", "Tragopogon pratensis", "Tragopogon pratensis subsp. orientalis", "Tripleurospermum inodorum", "Tripleurospermum inodorum", 
              "Tripleurospermum inodorum", "Tussilago farfara", "Urospermum dalechampii", "Urospermum dalechampii", "Urospermum picroides", 
              "Xanthium orientale subsp. italicum ", "Xanthium orientale subsp. italicum", "Xeranthemum annuum", "Xerolekia speciosissima"
)
table_v5
vapply(strsplit(table_v5, '(?<=[a-z])\\s+(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))
genera_table_v5 <- unique(vapply(strsplit(table_v5, '(?<=[a-z])\\s+(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
genera_table_v5

setdiff(genera_table_v5, genera_all)
setdiff(genera_all, genera_table_v5)

setdiff(genera_table_v5, genera_alps)
setdiff(genera_alps, genera_table_v5)

#### How many apomictic taxa? ####
### Extended dataset
DATA_CC_mean_red
table(DATA_CC_mean_red$Repr_mode_summ)

DATA_CC_mean_red[DATA_CC_mean_red$Repr_mode_summ == "Apomictic", 
                 "SpeciesName"]

DATA_CC_mean_red[DATA_CC_mean_red$Repr_mode_summ == "Mixed", 
                 "SpeciesName"]

### Strictly Alps
DATA_StrictlyAlps_mean_red
table(DATA_StrictlyAlps_mean_red$Repr_mode_summ)

DATA_StrictlyAlps_mean_red[DATA_StrictlyAlps_mean_red$Repr_mode_summ == "Apomictic", 
                           "SpeciesName"]

DATA_StrictlyAlps_mean_red[DATA_StrictlyAlps_mean_red$Repr_mode_summ == "Mixed", 
                           "SpeciesName"]

##### How many apomictic taxa per ploidy level? ##### 
### Extended dataset
with(DATA, table(Ploidy, Repr_mode))
sum(with(DATA, table(Ploidy, Repr_mode))) == nrow(DATA) # sanity check

### Strictly Alps
with(DATA_StrictlyAlps, table(Ploidy, Repr_mode))
sum(with(DATA_StrictlyAlps, table(Ploidy, Repr_mode))) == nrow(DATA_StrictlyAlps) # sanity check

##### Elevation range? #### 
### Extended dataset
DATA$Altitude

min(DATA$Altitude, na.rm = T)
max(DATA$Altitude, na.rm = T)
mean(DATA$Altitude, na.rm = T)
boxplot(DATA$Altitude)



##### Exploratory analysis #####
TraitsNames <- colnames(DATA_red[,-c(1:3, 12)])

boxplot(DATA_red[, "Altitude"])
# boxplot(DATA_red[, "Phytosociology"])

plot(GS ~ PloidyEvenOdd, DATA_red)
plot(GS ~ Chr.number, data = DATA_red)
plot(GS ~ Altitude, data = DATA_red)
abline(lm(GS ~ Altitude, data = DATA_red), col = "red")
plot(GS ~ Endemic, data = DATA_red)
plot(GS ~ Distribution, data = DATA_red)
plot(GS ~ Water, data = DATA_red)
plot(GS ~ w, data = DATA_red)
plot(GS ~ pH, data = DATA_red)
plot(GS ~ N, data = DATA_red)
plot(GS ~ Ca, data = DATA_red)
plot(GS ~ Si, data = DATA_red)
plot(GS ~ Ca.Si, data = DATA_red)

par(mfrow=c(1,5))
plot(GS ~ Collinaire, data = DATA_red)
plot(GS ~ Montane, data = DATA_red)
plot(GS ~ Subalpine, data = DATA_red)
plot(GS ~ Alpine, data = DATA_red)
plot(GS ~ Nival, data = DATA_red)
par(mfrow=c(1,1))

##### Is flowering time different between Sexual and Apomictic species? #####
class(DATA_red)
test1 <- as.vector(DATA_red[DATA_red$Repr_mode_summ == "Sexual" & !is.na(DATA_red$Init.month), "Init.month"])
class(test1) <- "data.frame"
test2 <- as.vector(DATA_red[DATA_red$Repr_mode_summ == "Apomictic" | DATA_red$Repr_mode_summ == "Mixed" & !is.na(DATA_red$Init.month), "Init.month"])
class(test2) <- "data.frame"

wilcox.test(as.numeric(test1$Init.month), as.numeric(test2$Init.month)) # not significantly different



##### ~ #####



##### VISUALIZATION #####

### Phytools: plotting reproductive mode onto tree 
library(phytools)
plotTree(JanTree5, fsize = 0.25, lwd = 1, type = "fan")

Repr_mode_factor <- gsub('Sexual', '0', DATA_red$Repr_mode_summ)
Repr_mode_factor <- as.factor(gsub('Apomictic', '1', Repr_mode_factor))
Repr_mode_factor <- as.factor(gsub('Mixed', '2', Repr_mode_factor))
Repr_mode_factor <- setNames(Repr_mode_factor, rownames(DATA_red))
Repr_mode_colours <- gsub('0', 'black', Repr_mode_factor)
Repr_mode_colours <- gsub('1', 'red', Repr_mode_colours)
Repr_mode_colours <- gsub('2', 'orange', Repr_mode_colours)
Repr_mode_colours <- setNames(Repr_mode_colours, rownames(DATA_red))
length(Repr_mode_colours) == length(JanTree5$tip.label)
match <- match(JanTree5$tip.label, names(Repr_mode_colours)) # need to reorder vecotr to match tree's labels
Repr_mode_colours <- Repr_mode_colours[match]

### Regular plotting w/ character states at tips
png(filename = "ApomixisTree_BlackBranches.png", width = 5000, height = 5000, units = "px", res = 600)
plot(JanTree5, cex = .5, type = "fan", show.tip.label = F)
tiplabels(pch = 19, cex = .5, col = Repr_mode_colours, offset = 1) 
dev.off()

### Plotting ancestral character state reconstruction
JanTree5.simMap <- make.simmap(JanTree5, Repr_mode_factor, model = "ER", nsim = 10)
JanTree5.simMap_summ <- summary(JanTree5.simMap, plots = F)
plot(JanTree5.simMap_summ, type = "fan", fsize = .35, cex=c(0.3,0.3))
plotSimmap(JanTree5.simMap, type = "fan", fsize = .25)



##### ggtree plotting #####
library(ape)
library(ggplot2)
library(ggtree)
library(treeio)
library(ggplotify)
library(RColorBrewer)
library(ggstance)

# ggtree is in active development. Run this to install from author's GitHub
# library(devtools)
# install_github("GuangchuangYu/ggtree")

ape::Ntip(JanTree5)

### IMPORTANT: data attached to the tree must have taxa name in first column! Order is irrelevant
DATA_red_ggtree <- DATA_CC_mean_red
rownames(DATA_red_ggtree) <- NULL
DATA_red_ggtree <- DATA_red_ggtree[, c(2,1,3:ncol(DATA_red_ggtree))]
# DATA_red_ggtree$SpeciesName <- as.character(DATA_red_ggtree$SpeciesName)
# DATA_red_ggtree <- DATA_red_ggtree[match(JanTree5$tip.label, DATA_red_ggtree$SpeciesName), ]
# JanTree5$tip.label == as.character(DATA_red_ggtree$SpeciesName)
DATA_red_ggtree$Ploidy <- factor(DATA_red_ggtree$Ploidy, levels = c("2x","3x","4x","6x","8x","12x"))
DATA_red_ggtree$Repr_mode_summ <- factor(DATA_red_ggtree$Repr_mode_summ, levels = c("Sexual", "Mixed", "Apomictic"))

ggJanTree5 <- ggtree(JanTree5) %<+% DATA_red_ggtree +
  geom_tiplab(size = 1, offset = .05) +
  xlim(0 , 37.5) + # this is to give enough withespace to print longer taxa names
  geom_hilight(mrca(JanTree5)["Achillea_clavennae", "Artemisia_vulgaris"], fill = brewer.pal(10, "Set3")[1]) +
  geom_hilight(mrca(JanTree5)["Doronicum_grandiflorum", "Doronicum_austriacum"], fill = brewer.pal(10, "Set3")[2]) +
  geom_hilight(mrca(JanTree5)["Aster_squamatus", "Bellis_perennis"], fill = brewer.pal(10, "Set3")[3]) +
  geom_hilight(mrca(JanTree5)["Calendula_arvensis", "Calendula_tripterocarpa"], fill = brewer.pal(10, "Set3")[4]) +
  geom_hilight(mrca(JanTree5)["Senecio_vulgaris", "Tephroseris_integrifolia"], fill = brewer.pal(10, "Set3")[2]) +
  geom_hilight(mrca(JanTree5)["Gnaphalium_hoppeanum", "Phagnalon_saxatile"], fill = brewer.pal(10, "Set3")[5]) +
  geom_hilight(mrca(JanTree5)["Galinsoga_quadriradiata", "Bidens_bipinnata"], fill = brewer.pal(10, "Set3")[6]) +
  geom_hilight(mrca(JanTree5)["Inula_conyzae", "Buphthalmum_salicifolium"], fill = brewer.pal(10, "Set3")[7]) +
  geom_hilight(mrca(JanTree5)["Crepis_pygmaea", "Catananche_caerulea"], fill = brewer.pal(10, "Set3")[8]) +
  geom_hilight(mrca(JanTree5)["Cirsium_oleraceum", "Echinops_exaltatus"], fill = brewer.pal(10, "Set3")[9]) +
  # ### Tribe labels as clade labels
  # geom_cladelabel(node = c(295), label = "Anthemidae", offset = 3.5, color = brewer.pal(10, "Set3")[1]) +
  # geom_cladelabel(node = c(321), label = "Senecioneae", offset = 3.5, color = brewer.pal(10, "Set3")[2]) +
  # geom_cladelabel(node = c(279), label = "Astereae", offset = 3.5, color = brewer.pal(10, "Set3")[3]) +
  # geom_cladelabel(node = c(324), label = "Calenduleae", offset = 3.5, color = brewer.pal(10, "Set3")[4]) +
  # geom_cladelabel(node = c(325), label = "Senecioneae", offset = 3.5, color = brewer.pal(10, "Set3")[2]) +
  # geom_cladelabel(node = c(255), label = "Gnaphalieae", offset = 3.5, color = brewer.pal(10, "Set3")[5]) +
  # geom_cladelabel(node = c(247), label = "Heliantheae", offset = 3.5, color = brewer.pal(10, "Set3")[6]) +
  # geom_cladelabel(node = c(263), label = "Inuleae", offset = 3.5, color = brewer.pal(10, "Set3")[7]) +
  # geom_cladelabel(node = c(351), label = "Cichorieae", offset = 3.5, color = brewer.pal(10, "Set3")[8]) +
  # geom_cladelabel(node = c(424), label = "Cardueae", offset = 3.5, color = brewer.pal(10, "Set3")[9]) +
  ### Tribe labels on MRCA node
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Achillea_clavennae", "Artemisia_vulgaris"], label = "Anthemidae"), hjust = 1.1, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Doronicum_grandiflorum", "Doronicum_austriacum"], label = "Senecioneae"), hjust = 1.1, nudge_y = 2.5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Aster_squamatus", "Bellis_perennis"], label = "Astereae"), hjust = 1.1, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Calendula_arvensis", "Calendula_tripterocarpa"], label = "Calenduleae"), hjust = 2.75, nudge_y = -2.5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Senecio_vulgaris", "Tephroseris_integrifolia"], label = "Senecioneae"), hjust = 1.1, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Gnaphalium_hoppeanum", "Phagnalon_saxatile"], label = "Gnaphalieae"), hjust = 1.1, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Galinsoga_quadriradiata", "Bidens_bipinnata"], label = "Heliantheae"), hjust = .75, nudge_y = -5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Inula_conyzae", "Buphthalmum_salicifolium"], label = "Inuleae"), hjust = 1.1, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Crepis_pygmaea", "Catananche_caerulea"], label = "Cichorieae"), hjust = 1.1, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Cirsium_oleraceum", "Echinops_exaltatus"], label = "Cardueae"), hjust = 1.1, nudge_y = 5) +
  geom_tippoint(aes(color = Repr_mode_summ, x = x + .1), size = .3, hjust = 0.025) +
  scale_color_manual(values = c("black", "orange", "red")) +
  # ### Adding points for discretized variables Altitude and Ploidy
  # geom_tippoint(aes(color = Ploidy, x = x + .3), size = .3) +
  # scale_color_manual(values = rev(brewer.pal(6, "Dark2"))) +
  # geom_tippoint(aes(color = Altitude_cat, x = x + .6), size = .3, alpha = 1) +
  # scale_color_manual(values = brewer.pal(11, "PuOr")) +
  # ggplot2::xlim(0, 42.5) +
  # theme_tree2()
  geom_treescale(x = 2.5, y = -.5, offset = 1.5, fontsize = 3) +
  theme(legend.position = c(0.15,.85)) +
  guides(colour = guide_legend(override.aes = list(size = 3)))

ggJanTree5
ggsave(filename = "ggJanTree5.png", device = "png", dpi = 300)

### Plotting traits on tree
sum(is.na(DATA_red_ggtree$Altitude)) # no missing values
DATA_red_ggtree[, c("SpeciesName", "GS")]
DATA_red_ggtree[, c("SpeciesName", "Altitude")]

### Simple ggplot versions work properly
ggplot(data = DATA_red_ggtree,
       aes(x = 0, xend = Altitude,
           y = 1:nrow(DATA_red_ggtree), yend = 1:nrow(DATA_red_ggtree)),
       fill = Altitude) +
  geom_segment()

ggplot(data = DATA_red_ggtree,
       aes(x = Altitude, y = SpeciesName),
       fill = Altitude) +
  geom_barh(stat = "identity")

### with colour also working properly (using ggstance::geom_barh)
ggplot(DATA_red_ggtree, aes(x = SpeciesName, y = Altitude, color = Altitude)) +
  geom_bar(stat = "identity") +
  scale_color_continuous(low = "darkgreen", high = "saddlebrown") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 3), legend.position = "bottom")

ggplot(DATA_red_ggtree, aes(y = SpeciesName, x = Altitude, color = Altitude)) +
  geom_barh(stat = "identity") +
  scale_color_continuous(low = "darkgreen", high = "saddlebrown") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 3), legend.position = "bottom")

ggplot(DATA_red_ggtree, aes(x = SpeciesName, y = GS, fill = Ploidy)) +
  geom_bar(stat = "identity") + scale_colour_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 3), legend.position = "bottom")

ggplot(DATA_red_ggtree, aes(y = SpeciesName, x = GS, fill = Ploidy)) +
  geom_barh(stat = "identity") + scale_colour_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 3), legend.position = "bottom")

### IN DEBUGGING: Adding facetplot, one bargraph of continuous variable, no colour
### The issue here is that the scale for the plots is not correctly estimated, and cuts off most datapoints
### Moreover, xlim_tree and xlim_expand don't seem to work... 
# ggJanTree5_panel <- facet_plot(ggJanTree5, panel = 'Altitude', data = DATA_red_ggtree,
#                                geom = geom_segment, # geom_segment variant is more complicated
#                                mapping = aes(x = 0, xend = DATA_red_ggtree$Altitude, y = y, yend = y),
#                                stat = "identity", size = .5) + xlim_expand(c(0,3000), panel = "Altitude")
# ggJanTree5_panel + theme(legend.position = "bottom") + theme_tree2()

ggJanTree5_panel <- facet_plot(ggJanTree5, panel = 'Altitude', data = DATA_red_ggtree,
                               geom = geom_barh, # geom_barh variant is straightforward
                               mapping = aes(x = DATA_red_ggtree$Altitude),
                               stat = "identity", size = .5)
ggJanTree5_panel + theme(legend.position = "bottom") + theme_tree2() + xlim_expand(c(0,3000), panel = 'Altitude')

ggJanTree5_panel2 <- facet_plot(ggJanTree5, panel = 'GS', data = DATA_red_ggtree,
                                geom = geom_barh, 
                                aes(x =DATA_red_ggtree$GS), 
                                stat = "identity", size = .5)
ggJanTree5_panel2 + theme(legend.position = "bottom") + theme_tree2()

ggJanTree5_panel_facet2 <- facet_widths(ggJanTree5_panel2, 8) # this applies the multiplier to the first facet of the specified object
ggJanTree5_panel_facet2 + theme(legend.position = "bottom")

### IN DEBUGGING: Coloured by categories, using geom_barh
### Complains about discrete value supplied to continuous scale? 
ggJanTree5_panel_colours <- facet_plot(ggJanTree5_panel_colours, panel = 'Altitude', data = DATA_red_ggtree,
                                       geom = geom_barh, 
                                       mapping = aes(x = DATA_red_ggtree$Altitude, fill = DATA_red_ggtree$Altitude),
                                       stat = "identity", size = .5) + 
  scale_color_continuous(low = "darkgreen", high = "saddlebrown") 
ggJanTree5_panel_colours + theme_tree2() + theme(legend.position = "bottom")

ggJanTree5_panel_colours2 <- facet_plot(ggJanTree5, panel = 'GS', data = DATA_red_ggtree,
                                        geom = geom_barh, mapping = aes(x = DATA_red_ggtree$GS, fill = DATA_red_ggtree$Ploidy),
                                        stat = "identity", size = 1) + scale_fill_brewer(palette = "Dark2") +
  theme_tree2() + theme(legend.position = "bottom")
ggJanTree5_panel_colours2

ggJanTree5_panel_facet_ploidycolours <- facet_widths(ggJanTree5_panel_colours, 8) # this applies the multiplier to the first facet of the specified object
ggJanTree5_panel_facet_ploidycolours + theme(legend.position = "bottom")
ggJanTree5_panel_facet_ploidycolours2 <- facet_widths(ggJanTree5_panel_colours2, 8) # this applies the multiplier to the first facet of the specified object
ggJanTree5_panel_facet_ploidycolours2 + theme(legend.position = "bottom")



##### ggTree circular #####
library(ape)
library(ggplot2)
library(ggtree)
# library(treeio)
# library(ggplotify)
library(RColorBrewer)

ggJanTree5_circular <- ggtree(JanTree5, layout = "circular") %<+% DATA_red_ggtree +
  geom_tiplab2(size = 1.5, offset = 2) +
  geom_hilight(mrca(JanTree5)["Achillea_clavennae", "Artemisia_vulgaris"], fill = brewer.pal(10, "Set3")[1]) +
  geom_hilight(mrca(JanTree5)["Doronicum_grandiflorum", "Doronicum_austriacum"], fill = brewer.pal(10, "Set3")[2]) +
  geom_hilight(mrca(JanTree5)["Aster_squamatus", "Bellis_perennis"], fill = brewer.pal(10, "Set3")[3]) +
  geom_hilight(mrca(JanTree5)["Calendula_arvensis", "Calendula_tripterocarpa"], fill = brewer.pal(10, "Set3")[4]) +
  geom_hilight(mrca(JanTree5)["Senecio_vulgaris", "Tephroseris_integrifolia"], fill = brewer.pal(10, "Set3")[2]) +
  geom_hilight(mrca(JanTree5)["Gnaphalium_hoppeanum", "Phagnalon_saxatile"], fill = brewer.pal(10, "Set3")[5]) +
  geom_hilight(mrca(JanTree5)["Galinsoga_quadriradiata", "Bidens_bipinnata"], fill = brewer.pal(10, "Set3")[6]) +
  geom_hilight(mrca(JanTree5)["Inula_conyzae", "Buphthalmum_salicifolium"], fill = brewer.pal(10, "Set3")[7]) +
  geom_hilight(mrca(JanTree5)["Crepis_pygmaea", "Catananche_caerulea"], fill = brewer.pal(10, "Set3")[8]) +
  geom_hilight(mrca(JanTree5)["Cirsium_oleraceum", "Echinops_exaltatus"], fill = brewer.pal(10, "Set3")[9]) +
  geom_tippoint(aes(color = Repr_mode_summ, x = x + 1), size = .5) +
  # scale_color_manual(values = c("red", "black", "orange")) +
  # scale_colour_brewer(palette = "Set1") +
  scale_colour_manual(values = c(brewer.pal(name = "Set1", 3)[2],
                               brewer.pal(name = "Set1", 3)[3],
                               brewer.pal(name = "Set1", 3)[1])) +
  ### Tribe labels on MRCA nodes
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Achillea_clavennae", "Artemisia_vulgaris"], label = "Anthemidae"), hjust = 1, nudge_x = -2.5, nudge_y = 10) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Doronicum_grandiflorum", "Doronicum_austriacum"], label = "Senecioneae"), hjust = 1, nudge_x = 0, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Aster_squamatus", "Bellis_perennis"], label = "Astereae"), hjust = 1, nudge_x = 0, nudge_y = 10) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Calendula_arvensis", "Calendula_tripterocarpa"], label = "Calenduleae"), hjust = 1, nudge_x = 0, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Senecio_vulgaris", "Tephroseris_integrifolia"], label = "Senecioneae"), hjust = .5, nudge_x = -2.5, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Gnaphalium_hoppeanum", "Phagnalon_saxatile"], label = "Gnaphalieae"), hjust = 1, nudge_x = -6, nudge_y = 20) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Galinsoga_quadriradiata", "Bidens_bipinnata"], label = "Heliantheae"), hjust = .75, nudge_x = 0, nudge_y = -5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Inula_conyzae", "Buphthalmum_salicifolium"], label = "Inuleae"), hjust = 1, nudge_x = -4, nudge_y = 9) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Crepis_pygmaea", "Catananche_caerulea"], label = "Cichorieae"), hjust = 1, nudge_x = -2.5, nudge_y = 5) +
  geom_text2(aes(subset = node %in% mrca(JanTree5)["Cirsium_oleraceum", "Echinops_exaltatus"], label = "Cardueae"), hjust = 1, nudge_x = -1, nudge_y = 3) +
  # geom_treescale(x = 5, y = 0, offset = 1.5, fontsize = 3) +
  theme(legend.position = c(-.2,0.1)) +
  guides(colour = guide_legend(override.aes = list(size = 3)))

ggJanTree5_circular
ggsave(ggJanTree5_circular, filename = "ggJanTree5_circular.png", 
       device = "png", dpi = 300, scale = 1.5)



##### Other plots: extended dataset #####
### Data prep
library(dplyr)
Asteraceae_barplot <- DATA_CC_mean_red %>%
  group_by(Ploidy, Repr_mode_summ) %>%
  count()
str(Asteraceae_barplot)

table(DATA_red$Ploidy)
table(DATA_CC_mean_red$Ploidy)

table(DATA_red[, c("Repr_mode_summ", "Init.month")])
table(DATA_red$Repr_mode_summ)
nrow(DATA_red)
nrow(DATA_red[DATA_red$Repr_mode_summ != "Mixed", ])

nrow(DATA_red) - sum(table(DATA_red$Init.month)) # 20 species missing Init.month

### Barplot
library(ggplot2)
library(RColorBrewer)
bargraph <- ggplot(Asteraceae_barplot,
                   aes(factor(Ploidy),
                       n,
                       fill = Repr_mode_summ)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  # theme_bw(base_size = 14) +
  labs(x = "Ploidy level") +
  labs(y = "Number of species") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, colour = "gray50"),
        plot.title = element_text(size = 24, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        axis.title.x = element_blank()
        ) +
  # theme(axis.title = element_text(size=18),
  #       axis.text = element_text(size=18, colour = "gray50"),
  #       plot.title = element_text(size = 24, hjust = 0.5),
  #       legend.text = element_text(size = 16),
  #       legend.title = element_blank()
  #       ) +
  theme(aspect.ratio = 3/2) +
  # scale_fill_brewer(palette = "Set1", name = "Reproduction mode", direction = 0) +
  scale_fill_manual(values = c(brewer.pal(name = "Set1", 3)[2],
                               brewer.pal(name = "Set1", 3)[3],
                               brewer.pal(name = "Set1", 3)[1])) +
  theme(legend.position = c(.7425,.80))
  # ggtitle("Apomixis and ploidy")
bargraph
ggsave(plot = bargraph, filename = "ApomixisVSPloidy_ggsave.pdf", dpi = 150, device = "pdf", scale = 1)

### Boxplot
boxplot <- ggplot(data = DATA_CC_mean_red,
                  aes(x = Repr_mode,
                      y = as.numeric(Init.month),
                      fill = Repr_mode)) +
  stat_boxplot(geom = "errorbar", width = 0.4, lwd = 0.5) +
  geom_boxplot(lwd = 0.1, fatten = 10, outlier.size = 1.5) +
  scale_fill_brewer(palette = "Set1", name = "Reproduction mode", direction = -1) +
  # labs(x = "Reproduction mode") +
  labs(y = "Flowering initiation (month)") +
  scale_y_continuous(limits = c(1, 10), breaks = c(1, 3, 5, 7, 9)) +
  # scale_y_discrete(name = "Flowering initiation (month)",
  #                    labels = c("January", "February", "March", "April",
  #                               "May", "June", "July", "August",
  #                               "September", "October", "November", "December"),
  #                  limits = c("Februrary", "November")) + # can't get it to work...
  # ggtitle("Apomixis and phenology") +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, colour = "gray50"),
        plot.title = element_text(size = 24, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        axis.title.x = element_blank()
  ) +
  theme(aspect.ratio = 3/2) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = "dashed") +
  stat_summary(fun.y = median, fun.ymax = length, geom = "text", aes(label = paste("n = ", ..ymax..)), vjust = -12.5)
  # geom_jitter(width = 0.05, show.legend = F, aes(colour = Repr_mode_summ), alpha = 0.5, position = "dodge") +
  # scale_colour_brewer(palette = "Set1", direction = -1)
boxplot
ggsave(plot = boxplot, filename= "ApomixisVSPhenology_ggsave.pdf", dpi = 150, device = "pdf", scale = 1)

ggpubr::ggarrange(bargraph, boxplot, ncol = 2, nrow = 1)
ggsave(ggpubr::ggarrange(bargraph, boxplot, ncol = 2, nrow = 1), 
       filename = "Barplot_Boxplot_ggarrange.pdf", device = "pdf", dpi = 150)

dev.off() # reset graphics device

##### Other plots: strictly Alps #####
library(dplyr)
Asteraceae_barplot_strictlyAlps <- DATA_StrictlyAlps_mean_red %>%
  group_by(Ploidy, Repr_mode_summ) %>%
  count()
str(Asteraceae_barplot_strictlyAlps)

table(DATA_StrictlyAlps_mean_red$Ploidy)

table(DATA_StrictlyAlps_mean_red[, c("Repr_mode_summ", "Init.month")])
table(DATA_StrictlyAlps_mean_red$Repr_mode_summ)
nrow(DATA_StrictlyAlps_mean_red)
nrow(DATA_StrictlyAlps_mean_red[DATA_StrictlyAlps_mean_red$Repr_mode_summ != "Mixed", ])

nrow(DATA_StrictlyAlps_mean_red) - sum(table(DATA_StrictlyAlps_mean_red$Init.month)) # 0 species missing Init.month

library(ggplot2)
library(RColorBrewer)
### Barplot striclty Alps
bargraph_strictlyAlps <- ggplot(Asteraceae_barplot_strictlyAlps,
                                aes(factor(Ploidy),
                                    n,
                                    fill = Repr_mode_summ)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  # theme_bw(base_size = 14) +
  labs(x = "Ploidy level") +
  labs(y = "Number of species") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, colour = "gray50"),
        plot.title = element_text(size = 24, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        axis.title.x = element_blank()
  ) +
  # theme(axis.title = element_text(size=18),
  #       axis.text = element_text(size=18, colour = "gray50"),
  #       plot.title = element_text(size = 24, hjust = 0.5),
  #       legend.text = element_text(size = 16),
  #       legend.title = element_blank()
  #       ) +
  theme(aspect.ratio = 3/2) +
  # scale_fill_brewer(palette = "Set1", name = "Reproduction mode", direction = 0) +
  scale_fill_manual(values = c(brewer.pal(name = "Set1", 3)[2],
                               brewer.pal(name = "Set1", 3)[3],
                               brewer.pal(name = "Set1", 3)[1])) +
  theme(legend.position = c(.7425,.80))
# ggtitle("Apomixis and ploidy")
bargraph_strictlyAlps
ggsave(plot = bargraph_strictlyAlps, filename = "ApomixisVSPloidy_ggsave_strictlyAlps.pdf", dpi = 150, device = "pdf", scale = 1)

### Boxplot strictly Alps
boxplot_strictlyAlps <- ggplot(data = DATA_StrictlyAlps_mean_red,
                               aes(x = Repr_mode,
                                   y = as.numeric(Init.month),
                                   fill = Repr_mode)) +
  stat_boxplot(geom = "errorbar", width = 0.4, lwd = 0.5) +
  geom_boxplot(lwd = 0.1, fatten = 10, outlier.size = 1.5) +
  scale_fill_brewer(palette = "Set1", name = "Reproduction mode", direction = -1) +
  # labs(x = "Reproduction mode") +
  labs(y = "Flowering initiation (month)") +
  scale_y_continuous(limits = c(1, 10), breaks = c(1, 3, 5, 7, 9)) +
  # scale_y_discrete(name = "Flowering initiation (month)",
  #                    labels = c("January", "February", "March", "April",
  #                               "May", "June", "July", "August",
  #                               "September", "October", "November", "December"),
  #                  limits = c("Februrary", "November")) + # can't get it to work...
  # ggtitle("Apomixis and phenology") +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, colour = "gray50"),
        plot.title = element_text(size = 24, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        axis.title.x = element_blank()
  ) +
  theme(aspect.ratio = 3/2) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = "dashed") +
  stat_summary(fun.y = median, fun.ymax = length, geom = "text", aes(label = paste("n = ", ..ymax..)), vjust = -10)
# geom_jitter(width = 0.05, show.legend = F, aes(colour = Repr_mode_summ), alpha = 0.5, position = "dodge") +
# scale_colour_brewer(palette = "Set1", direction = -1)
boxplot_strictlyAlps

ggsave(plot = boxplot_strictlyAlps, filename= "ApomixisVSPhenology_ggsave_strictlyAlps.pdf", dpi = 150, device = "pdf", scale = 1)

ggpubr::ggarrange(bargraph_strictlyAlps, boxplot_strictlyAlps, ncol = 2, nrow = 1)
ggsave(ggpubr::ggarrange(bargraph_strictlyAlps, boxplot_strictlyAlps, ncol = 2, nrow = 1), 
       filename = "Barplot_Boxplot_ggarrange_strictlyAlps.pdf", device = "pdf", dpi = 150)

dev.off() # reset graphics device



##### STATISTICAL ANALYSIS #####
### Linear modeling: Apomixis VS altitude
### We can use the unsummarized dataset here since we don't take into account phylogeny
plot(GS ~ Altitude, data = DATA)
abline(lm(GS ~ Altitude, data = DATA), col="red")
m1 <- lm(GS ~ Altitude, data = DATA)
summary(m1) # big genomes have massive leverage (ie: influential observations)
plot(m1)

plot(density(DATA$Altitude, na.rm=T)) # data also looks bimodal...
plot(density(DATA$GS, na.rm=T))

plot(log(GS) ~ Altitude, data = DATA)
abline(lm(log(GS) ~ Altitude, data = DATA), col="red")
m2 <- lm(log(GS) ~ Altitude, data = DATA_red)
summary(m2)
plot(m2) # log transformation mitigates but doesn't solve the issue

plot(`1C` ~ Altitude, data = DATA)
abline(lm(`1C` ~ Altitude, data = DATA), col = "red")
m3 <- lm(`1C` ~ Altitude, data = DATA)
summary(m3)
plot(m3) # using 1C instead doesn't change much

plot(GS_per_chrm ~ Altitude, data = DATA)
abline(lm(GS_per_chrm ~ Altitude, data = DATA), col = "red")
m4 <- lm(GS_per_chrm ~ Altitude, data = DATA)
summary(m4)
plot(m4) # using GS/Chromosome number also doesn't affect much

plot(GS ~ Altitude, data = DATA[which(DATA$Ploidy == "2x"), ])
abline(lm(GS ~ Altitude, data = DATA[which(DATA$Ploidy == "2x"), ]), col = "red")
m5 <- lm(GS ~ Altitude, data = DATA[which(DATA$Ploidy == "2x"), ])
summary(m5)
plot(m5) # using only diploids



##### GLMs #####
### Need to use complete cases for GLMs, no missing values allowed
glm1 <- glm(Repr_mode_summ ~ Altitude + Ploidy + GS, 
            data = DATA_CC_mean_red, family = "binomial")
summary(glm1)
plot(glm1)

glm1.1 <- glm(Repr_mode_summ ~ Altitude + Ploidy + GS + Init.month + Tot.months,
            data = DATA_CC_mean_red, family = "binomial")
summary(glm1.1)
plot(glm1.1)

glm1.2 <- glm(Repr_mode_summ ~ Altitude + Init.month + Tot.months,
            data = DATA_CC_mean_red, family = "binomial")
summary(glm1.2)
plot(glm1.2)

### Using only an apomictic clade, Hieracium + Pilosella: 
caper::clade.members(mrca(JanTree4)["Pilosella_officinarum", "Hieracium_tomentosum"], JanTree4, tip.labels = T)
with(DATA_CC_mean_red[DATA_CC_mean_red$SpeciesName %in% clade.members(mrca(JanTree4)["Pilosella_officinarum", "Hieracium_tomentosum"], JanTree4, tip.labels = T), ], table(Repr_mode))

glm2 <- glm(Repr_mode ~ Altitude + Ploidy + Init.month + Tot.months,
            data = DATA_CC_mean_red[DATA_CC_mean_red$SpeciesName %in% clade.members(mrca(JanTree4)["Pilosella_officinarum", "Hieracium_tomentosum"], JanTree4, tip.labels = T), ], 
            family = "binomial") # too few datapoints probably...  
summary(glm2)
plot(glm2)

### Using only an apomictic clade, extended: Cichorieae tribe
caper::clade.members(mrca(JanTree4)["Tolpis_staticifolia", "Crepis_pygmaea"], JanTree4, tip.labels = T)
with(DATA_CC_mean_red[DATA_CC_mean_red$SpeciesName %in% clade.members(mrca(JanTree4)["Tolpis_staticifolia", "Crepis_pygmaea"], JanTree4, tip.labels = T), ], table(Repr_mode))

glm3 <- glm(Repr_mode_summ ~ Altitude + Ploidy + Init.month + Tot.months,
            data = DATA_CC_mean_red[DATA_CC_mean_red$SpeciesName %in% clade.members(mrca(JanTree4)["Tolpis_staticifolia", "Crepis_pygmaea"], JanTree4, tip.labels = T), ], 
            family = "binomial")
summary(glm3)
plot(glm3)



##### MCMC models #####
library(MCMCglmm)
library(caper)
### MCMCglmm on subsets of species, no phylogeny included
mHieraciumSS <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + Init.month + Tot.months,
                         data = DATA_CC_mean_red[DATA_CC_mean_red$SpeciesName %in% clade.members(mrca(JanTree4_CC)["Hieracium_froelichianum", "Hieracium_tomentosum"], JanTree4_CC, tip.labels = T), ],
                         family = "threshold", trunc = T,
                         prior = list(R = list(V = diag(1), nu = 2)),
                         nitt = 10^6, thin = 500, burnin = 2500, verbose = T
)
summary(mHieraciumSS)
plot(mHieraciumSS)

mHieracium <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + Init.month + Tot.months,
                       data = DATA_CC_mean_red[DATA_CC_mean_red$SpeciesName %in% clade.members(mrca(JanTree4_CC)["Pilosella_officinarum", "Hieracium_tomentosum"], JanTree4_CC, tip.labels = T), ],
                       family = "threshold", trunc = T,
                       prior = list(R = list(V = diag(1), nu = 2)),
                       nitt = 10^6, thin = 500, burnin = 2500, verbose = T
)
summary(mHieracium)
plot(mHieracium)

mCichorieae <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + Init.month + Tot.months,
                        data = DATA_CC_mean_red[DATA_CC_mean_red$SpeciesName %in% clade.members(mrca(JanTree4_CC)["Tolpis_staticifolia", "Crepis_pygmaea"], JanTree4_CC, tip.labels = T), ],
                        family = "threshold", trunc = T,
                        prior = list(R = list(V = diag(1), nu = 2)),
                        nitt = 10^6, thin = 500, burnin = 2500, verbose = T
)
summary(mCichorieae)
plot(mCichorieae)

### There's no evidence that apomictic species flower earlier in the year than sexual ones. Not even in subsets of species with high proportions of apomicts.


#### I'VE STOPPED DEBUGGING HERE! 14 Nov ####
### need to clean up data and tree prep for MCMCglmm, and then clean up separate scripts for the cluster
### Don't forget aobout Phylosig at the bottom! 

### Data prep
head(DATA_CC_mean_red)

str(JanTree4)
str(JanTree5) # JanTree4 but fully dichotomous tree

MCMC_data <- DATA_CC_mean_red[match(JanTree4$tip.label, DATA_CC_mean_red$SpeciesName), ]
rownames(MCMC_data) <- JanTree4$tip.label
rownames(MCMC_data)
colnames(MCMC_data)[2] <- "animal"
head(MCMC_data)

### Missing values are not permitted...
nrow(MCMC_data)
# data_red <- 
  MCMC_data[!complete.cases(MCMC_data), 1:12]
nrow(data_red)
rownames(data_red)
setdiff(data$animal, data_red$animal) # missing accessions other than those taxa not in Flora Alpina are due to missing Altitude datum...
data_red <- data_red[match(JanTree4_red$tip.label, data_red$animal), ]

### Handle tree
setdiff(JanTree4$tip.label, data_red$animal)
setdiff(data_red$animal, JanTree4$tip.label)
JanTree4_red <- ape::drop.tip(JanTree4, setdiff(JanTree4$tip.label, data_red$animal))
setdiff(JanTree4_red$tip.label, data_red$animal)
rownames(data_red) <- data_red$animal
geiger::name.check(JanTree4_red, data_red) # name.check is not to be trusted lately...
setdiff(JanTree4_red$tip.label, data_red$animal)
setdiff(data_red$animal, JanTree4_red$tip.label)
nrow(data_red) == length(JanTree4_red$tip.label)
# View(cbind(JanTree4_red$tip.label, data_red$animal, rownames(data_red))) # sanity check
plot(JanTree4_red, cex = .25)
is.binary(JanTree4_red)
JanTree4_red <- multi2di(JanTree4_red) # randomly resolve polytomies
is.binary(JanTree4_red)
plot(JanTree4_red, cex = .25)
is.ultrametric(JanTree4_red)
JanTree4_red <- compute.brlen(JanTree4_red, method = "Grafen") # compute branch lengths
is.ultrametric(JanTree4_red)
JanTree4_red$edge.length[JanTree4_red$edge.length == 0] # no branch lengths have value zero
plot(JanTree4_red, cex = .3)
JanTree4_red

### Handle data
str(data_red)
data_red$animal <- as.factor(data_red$animal)
data_red$Repr_mode <- data_red$Repr_mode_summ
levels(data_red$Repr_mode) <- list(Sexual = "Sexual", Apomictic = c("Apomicitc", "Mixed")) # 2 levels
data_red$Repr_mode_summ <- factor(data_red$Repr_mode_summ, levels = c("Sexual", "Mixed", "Apomictic" )) # 3 levels
data_red$PloidyEvenOdd <- as.factor(data_red$PloidyEvenOdd)
data_red$Ploidy_summ <- gsub('6x|8x|12x', 'Poly', data_red$Ploidy)
data_red$Ploidy_summ <- factor(data_red$Ploidy_summ, levels = c("2x", "3x", "4x", "Poly"))
table(data_red$Ploidy_summ)
data_red$Phytosociology <- as.factor(data_red$Phytosociology)
data_red$Ca <- as.factor(data_red$Ca)
data_red$Ca.Si <- as.factor(data_red$Ca.Si)
data_red$Si <- as.factor(data_red$Si)
data_red$pH <- as.factor(data_red$pH)
data_red$N <- as.factor(data_red$N)
data_red$Water <- as.factor(data_red$Water)
data_red$w <- as.factor(data_red$w)
class(data_red) <- "data.frame"
levels(data_red$animal)
# test <- sapply(data_red, as.factor) # or use sapply y'know...

### Make sure factor levels are correct
data_red$Repr_mode_summ <- factor(data_red$Repr_mode_summ, levels = c("Sexual", "Mixed", "Apomictic"))
data_red$Ploidy <- factor(data_red$Ploidy, levels = c("2x", "3x", "4x", "6x", "8x", "12x"))

##### Binomial (categorical) distribution - weak priors #####
library(MCMCglmm)
# prior <- list(R = list(V = 1, nu = 0.002)) # Maite's prior, weak
# nitt= 1000000, thin=500, burnin=25000 # Maite's run parameters
# prior <- list(R = list(V = 1, nu = 0.002, fix = 1))
#
# prior <- list(R = list(V = 1, nu = 0.002),
#               G = list(G1 = list(V = 1, nu = 0.002))
#               )
#
# a = 1000
# prior <- list(R = list(V = diag(3), nu = 0.002),
#               G = list(G1 = list(V = diag(3), nu = 1, alpha.mu = 0, alpha.V = diag(3)*a))) # strong prior
#
# prior <- list(R = list(V = diag(3), nu = 0.002),
#                G = list(G1 = list(V = diag(3), nu = 1, alpha.mu = 0, alpha.V = diag(3)*a),
#                         G2 = list(V = diag(3), nu = 1, alpha.mu = 0, alpha.V = diag(3)*a),
#                         G3 = list(V = diag(3), nu = 1, alpha.mu = 0, alpha.V = diag(3)*a)))
#
# prior = list(R = list(V = diag(2), fix = 1),
#              G = list(G1 = list(V = diag(2), nu = 1, alpha.mu = c(0, 0), alpha.V = diag(2) * 100))
#              )
#
prior <- list(R = list(V = diag(2), nu = 2))
#
# a = 1000
# prior <- list(V = diag(2), nu = 2, alpha.mu = c(0,0), alpha.V = diag(2)*a)
#
# prior <- list(R = list(fix = 1, V = diag(2), nu = 0.002),
#               B = list(mu = rep(0, 5), V = diag(5)*1e4))

set.seed(111)
mBin1 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy + GS + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
                 family = "categorical", rcov = ~us(trait):units, prior = prior, nitt=1000000, thin=500, burnin=25000)
plot(mBin1)
summary(mBin1)

set.seed(111)
mBin1_noGS <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
                  family = "categorical", rcov = ~us(trait):units, prior = prior, nitt=1000000, thin=500, burnin=25000)
plot(mBin1_noGS)
summary(mBin1_noGS)

# mBin1_evenodd <- MCMCglmm(Repr_mode_summ ~ Altitude + PloidyEvenOdd + GS + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
#                   family = "categorical", rcov = ~us(trait):units, prior = prior, nitt=1000000, thin=500, burnin=25000)
# plot(mBin1_evenodd)
# summary(mBin1_evenodd)

# want eff. samp to be close to the sample size given, if big difference = bad
plot(mBin1$VCV) # If you can see one particularly high peak at the start increase burnin so sampling will start after trace has evened out
plot(mBin1$Sol) # Want traces to be relatively even and compacted

mean(mBin1$Sol[,"Altitude"]) # this gives the coefficient printed by summary(mBin1)
mean(exp(mBin1$Sol[,"Altitude"])) # this gives the odds ratio

mean(exp(mBin1$Sol[,"Altitude"]))
mean(exp(mBin1$Sol[,"Ploidy3x"]))
mean(exp(mBin1$Sol[,"Ploidy4x"]))
mean(exp(mBin1$Sol[,"Ploidy6x"]))
mean(exp(mBin1$Sol[,"Ploidy8x"]))
mean(exp(mBin1$Sol[,"GS"]))
mean(exp(mBin1$Sol[,"Init.month"]))

(mean(exp(mBin1$Sol[,"Altitude"]))-1)*100 # % change in the probability of being sexual per unit of Altitude
(mean(exp(mBin1$Sol[,"Ploidy3x"]))-1)*100
(mean(exp(mBin1$Sol[,"Ploidy4x"]))-1)*100
(mean(exp(mBin1$Sol[,"Ploidy6x"]))-1)*100
(mean(exp(mBin1$Sol[,"Ploidy8x"]))-1)*100
(mean(exp(mBin1$Sol[,"GS"]))-1)*100
(mean(exp(mBin1$Sol[,"Init.month"]))-1)*100

mean(exp(mBin1$Sol[,"Altitude"]))/(1+mean(exp(mBin1$Sol[,"Altitude"]))) # probability of being sexual having Alitutde at its mean (?)
mean(exp(mBin1$Sol[,"Ploidy3x"]))/(1+mean(exp(mBin1$Sol[,"Ploidy3x"]))) # probability of being sexual being 3x
mean(exp(mBin1$Sol[,"Ploidy4x"]))/(1+mean(exp(mBin1$Sol[,"Ploidy4x"])))
mean(exp(mBin1$Sol[,"Init.month"]))/(1+mean(exp(mBin1$Sol[,"Init.month"]))) # probability of being sexual having Init.month at its mean (?)

# ### from Jerrod Hadfield's course notes on MCMCglmm:
# IC.1 <- mBin1$VCV[, 1]/(rowSums(mBin1$VCV) + pi^2/3)
# IC.2 <- mBin2$VCV[, 1]/(rowSums(mBin2$VCV) + pi^2/3)
# plot(mcmc.list(IC.1, IC.2))
#
# c2 <- ((16 * sqrt(3))/(15 * pi))^2
# Int.1 <- mBin1$Sol/sqrt(1 + c2 * mBin1$VCV[, 2])
# Int.2 <- mBin2$Sol/sqrt(1 + c2 * mBin2$VCV[, 2])
# plot(mcmc.list(as.mcmc(Int.1), as.mcmc(Int.2)))

###
set.seed(569)
mBin2 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + GS + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
                  family = "categorical", rcov = ~us(trait):units, prior = prior, nitt=1000000, thin=500, burnin=25000)
summary(mBin2)
plot(mBin2)

###
set.seed(935)
mBin3 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + GS + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
                           family = "categorical", rcov = ~us(trait):units, prior = prior, nitt=1000000, thin=500, burnin=25000)
summary(mBin3)
plot(mBin3)

### Check convergence of multiple chains
chainListBin1 <- mcmc.list(mBin1$Sol, mBin2$Sol, mBin3$Sol)
chainListBin2 <- mcmc.list(mBin1$VCV, mBin2$VCV, mBin3$VCV)
### Gelman rubin diagnostic: should be close to 1.
gelman.diag(chainListBin1)
gelman.diag(chainListBin2)

heidel.diag(mBin1$VCV)
heidel.diag(mBin2$VCV)
heidel.diag(mBin3$VCV)
heidel.diag(mBin1$Sol)
heidel.diag(mBin2$Sol)
heidel.diag(mBin3$Sol)

geweke.plot(mBin1$Sol)
geweke.plot(mBin2$Sol)
geweke.plot(mBin3$Sol)

##### Binomial (categorical) distribution - strong priors #####
prior_strong <- list(R = list(fix = 2, V = diag(2)*a, nu = 2)) # strong prior
# nitt= 1000000, thin=500, burnin=25000 # same run parameters as wek prior's models
set.seed(111)
mBin4 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + GS + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
                  family = "categorical", rcov=~us(trait):units, prior = prior_strong, nitt=1000000, thin=500, burnin=25000)
summary(mBin4)
plot(mBin4)

# set.seed(111)
# mBin4_evenodd <- MCMCglmm(Repr_mode ~ Altitude + PloidyEvenOdd + GS + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
#                   family = "categorical", rcov=~us(trait):units, prior = prior_strong, nitt=1000000, thin=500, burnin=25000)
# summary(mBin4_evenodd)
# plot(mBin4_evenodd)

###
set.seed(569)
mBin5 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + GS + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
                  family = "categorical", rcov=~us(trait):units, prior = prior_strong, nitt=1000000, thin=500, burnin=25000)
summary(mBin5)
plot(mBin5)

###
set.seed(935)
mBin6 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + GS + Init.month + Tot.months, pedigree = JanTree4_red, verbose = T, data = data_red,
                  family = "categorical", rcov=~us(trait):units, prior = prior_strong, nitt=1000000, thin=500, burnin=25000)
summary(mBin6)
plot(mBin6)

### Check convergence of multiple chains
chainListBin3 <- mcmc.list(mBin4$Sol, mBin5$Sol, mBin6$Sol)
chainListBin4 <- mcmc.list(mBin4$VCV, mBin5$VCV, mBin6$VCV)

### Gelman rubin diagnostic: should be close to 1.
gelman.diag(chainListBin3)
gelman.diag(chainListBin4)

heidel.diag(mBin4$VCV)
heidel.diag(mBin4_evenodd$VCV)
heidel.diag(mBin5$VCV)
heidel.diag(mBin6$VCV)
heidel.diag(mBin4$Sol)
heidel.diag(mBin4_evenodd$Sol)
heidel.diag(mBin5$Sol)
heidel.diag(mBin6$Sol)

geweke.plot(mBin4$Sol)
geweke.plot(mBin5$Sol)
geweke.plot(mBin6$Sol)



##### Maite's model #####
library(MCMCglmm)

data_red$Repr_mode # 2 levels
data_red$Repr_mode_summ # 3 levels

### Phylogeny black magic...
invJanTree4_red <- inverseA(JanTree4_red, nodes = "ALL", scale = TRUE) # ALL is better for large phylogenies
invJanTree4_red_tips <- inverseA(JanTree4_red, nodes = "TIPS", scale = TRUE)

#########
prior_nu1000_1 <- list(R = list(V = 1, fix = 1),
                       G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1))
                       )

set.seed(111)
mTre1 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy + GS + Init.month + Tot.months,
                  ginverse = list(animal = invJanTree4_red_tips$Ainv),
                  random = ~animal,
                  verbose = T,
                  data = data_red,
                  family = "threshold",
                  trunc = T,
                  prior = prior_nu1000_1,
                  nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre1)
plot(mTre1)

heidel.diag(mTre1$VCV)
heidel.diag(mTre1$Sol)
geweke.plot(mTre1$Sol)
autocorr.diag(mTre1$Sol)

mTre1_noPhy <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy + GS + Init.month + Tot.months,
                        verbose = T, data = data_red,
                        family = "threshold",
                        trunc = T,
                        prior = prior_nu1000_1,
                        nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre1_noPhy)
plot(mTre1_noPhy)

### Repr_mode with only 2 levels
mTre1.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + GS + Init.month + Tot.months,
                    ginverse = list(animal = invJanTree4_red_tips$Ainv),
                    random = ~animal,
                    verbose = T,
                    data = data_red,
                    family = "threshold",
                    trunc = T,
                    prior = prior_nu1000_1,
                    nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre1.1)
# plot(mTre1.1)

heidel.diag(mTre1.1$VCV)
heidel.diag(mTre1.1$Sol)
geweke.plot(mTre1.1$Sol)
autocorr.diag(mTre1.1$Sol)

##### Second batch: set.seed(534) #####
print("Second batch: set.seed(534)")
set.seed(534)
### Repr_mode with only 3 levels
mTre2 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy + GS + Init.month + Tot.months,
                  ginverse = list(animal = invJanTree4_red_tips$Ainv),
                  random = ~ animal,
                  verbose = T,
                  data = data_red,
                  family = "threshold",
                  trunc = T,
                  prior = prior_nu1000_1,
                  nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre2)
print("DiagPlots_mTre2.1%03d.png")
png('DiagPlots_mTre2.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre2, ask = F)

dev.off()

heidel.diag(mTre2$VCV)
heidel.diag(mTre2$Sol)
print("Geweke_mTre2%03d.png")
png('Geweke_mTre2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(mTre2$Sol, ask = F)

dev.off()
autocorr.diag(mTre2$Sol)

### Repr_mode with only 2 levels
mTre2.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + GS + Init.month + Tot.months,
                    ginverse = list(animal = invJanTree4_red_tips$Ainv),
                    random = ~ animal,
                    verbose = T,
                    data = data_red,
                    family = "threshold",
                    trunc = T,
                    prior = prior_nu1000_1,
                    nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre2.1)
print("DiagPlots_mTre2.1%03d.png")
png('DiagPlots_mTre2.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre2.1, ask = F)

dev.off()

heidel.diag(mTre2.1$VCV)
heidel.diag(mTre2.1$Sol)
print("Geweke_mTre2.1.png")
png('Geweke_mTre2.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(mTre2.1$Sol, ask = F)

dev.off()
autocorr.diag(mTre2.1$Sol)

##### Third batch: set.seed(386) #####
print("Third batch: set.seed(386)")
set.seed(386)
### Repr_mode with only 3 levels
mTre3 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy + GS + Init.month + Tot.months,
                  ginverse = list(animal = invJanTree4_red_tips$Ainv),
                  random = ~ animal,
                  verbose = T,
                  data = data_red,
                  family = "threshold",
                  trunc = T,
                  prior = prior_nu1000_1,
                  nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre3)
print("DiagPlots_mTre3.1%03d.png")
png('DiagPlots_mTre3.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre3, ask = F)

dev.off()

heidel.diag(mTre3$VCV)
heidel.diag(mTre3$Sol)
print("Geweke_mTre3%03d.png")
png('Geweke_mTre3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(mTre3$Sol, ask = F)

dev.off()
autocorr.diag(mTre3$Sol)

### Repr_mode with only 2 levels
mTre3.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy + GS + Init.month + Tot.months,
                    ginverse = list(animal = invJanTree4_red_tips$Ainv),
                    random = ~ animal,
                    verbose = T,
                    data = data_red,
                    family = "threshold",
                    trunc = T,
                    prior = prior_nu1000_1,
                    nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre3.1)
print("DiagPlots_mTre3.1%03d.png")
png('DiagPlots_mTre3.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre3.1, ask = F)

dev.off()

heidel.diag(mTre3.1$VCV)
heidel.diag(mTre3.1$Sol)
print("Geweke_mTre3.1.png")
png('Geweke_mTre3.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(mTre3.1$Sol, ask = F)

dev.off()
autocorr.diag(mTre3.1$Sol)

##### Multiple chains convergence diagnostics #####
chainListTre1_Sol <- mcmc.list(mTre1$Sol, mTre2$Sol, mTre3$Sol)
chainListTre1_VCV <- mcmc.list(mTre1$VCV, mTre2$VCV, mTre3$VCV)

chainListTre2_Sol <- mcmc.list(mTre1.1$Sol, mTre2.1$Sol, mTre3.1$Sol)
chainListTre2_VCV <- mcmc.list(mTre1.1$VCV, mTre2.1$VCV, mTre3.1$VCV)

### Gelman rubin diagnostic: should be close to 1
gelman.diag(chainListTre1_Sol)
gelman.diag(chainListTre1_Sol)

gelman.diag(chainListTre2_Sol)
gelman.diag(chainListTre2_Sol)



##### DIFFERENT prior #####
library(MCMCglmm)
prior_G.V1000_nu.0002_1 <- list(R = list(V = 1000, fix = 1000),
                       G = list(G1 = list(V = 1000, nu = 200, alpha.mu = 1000, alpha.V = 10000))
                       )

set.seed(111)
mTre4 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy + GS + Init.month + Tot.months,
                  ginverse = list(animal = invJanTree4_red_tips$Ainv),
                  random = ~animal,
                  verbose = T,
                  data = data_red,
                  family = "threshold",
                  trunc = T,
                  prior = prior_nu.0002_1,
                  nitt = 10000, thin = 50, burnin = 100)
summary(mTre4)
plot(mTre4)



##### Phylosig #####
### Must give a named numerical vector to the function; use SetNames to set the names
### Factors in R are always encoded numerically, you can get that index via as.numeric(myfactor)
library(phytools)

data_lambda_k <- DATA_CC_mean_red
class(data_lambda_k) <- "data.frame"
str(data_lambda_k)
ToBeFactors <- c("ID_FloraAlpina", "ID", "Repr_mode_summ", "Chr.number", "PloidyEvenOdd",
                 "Endemic", "Indigenous", "Distribution", "CompleteName",
                 "Collinaire", "Montane", "Subalpine", "Alpine", "Nival",
                 "Init.month", "Tot.months", "Phytosociology", "Habitat",
                 "Ca", "Ca.Si", "Si", "pH", "N", "Water", "w", "Sect_Occ", "Repr_mode")
data_lambda_k[ToBeFactors] <- lapply(data_lambda_k[ToBeFactors], factor)
str(data_lambda_k)
rownames(data_lambda_k) <- data_lambda_k$SpeciesName

geiger::name.check(JanTree4, DATA_CC_mean_red)

i = 5
K_lambda_table <- data.frame("Variable" = character(),
                             "K" = double(), "p_K" = double(), "p_k-stars" = character(),
                             "lambda" = double(), "p_lambda" = double(), "p_lambda-stars" = character(),
                             stringsAsFactors = F)
while (i %in% 5:ncol(data_lambda_k)) {
  obj1 <- phylosig(tree = JanTree4, x = setNames(as.numeric(unlist(data_lambda_k[,i])), rownames(data_lambda_k)), method = "K", test = T)
  obj2 <- phylosig(tree = JanTree4, x = setNames(as.numeric(unlist(data_lambda_k[,i])), rownames(data_lambda_k)), method = "lambda", test = T)
  K_lambda_table[i-4,] <- cbind(colnames(data_lambda_k[i]),
                                obj1$K, obj1$P, symnum(obj1$P, corr = FALSE, cutpoints = c(0,  .001,.01,.05, .1, 1), symbols = c("***","**","*","."," ")),
                                obj2$lambda, obj2$P, symnum(obj2$P, corr = FALSE, cutpoints = c(0,  .001,.01,.05, .1, 1), symbols = c("***","**","*","."," "))
  )
  i = i + 1
}
K_lambda_table

### lambda tells about the distribution of the traits in relation to evolution by brownian motion;
### lambda close to 0: completely uncorrelated species; lambda close to 1: brownian motion
### K tells about the ripartition of variance between clades;
### K < 1: variance mainly within clades; K > 1: variance mainly among clades.

write.csv(K_lambda_table, file = "Phylosig_K_lambda.csv")

##### THE END #####
