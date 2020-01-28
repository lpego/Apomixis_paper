##### Read in the table ##### 
Online_v7 <- read.csv(file = "/home/luca/Dropbox/Royal Botanic Gardens of Kew/ApomixisPaper/Apomixis_v7/Pegoraro_et_al_Apomixis-Online_Resource_1-v7 -JP_LP_OH2_LP.csv")
str(Online_v7)

Online_v7$SpeciesName <- gsub('\\s$', '', Online_v7$SpeciesName)
Online_v7$Reproductive.mode <- factor(Online_v7$Reproductive.mode, levels = c("Sexual", "Apomictic"))
Online_v7$Embryo.Ploidy <- factor(Online_v7$Embryo.Ploidy, levels = c("2x", "3x", "4x", "6x", "8x", "12x"))
Online_v7$Embryo.Ploidy.summ <- gsub('6x|8x|12x', 'Poly', Online_v7$Embryo.Ploidy)
Online_v7$Embryo.Ploidy.summ <- factor(Online_v7$Embryo.Ploidy.summ, levels = c("2x", "3x", "4x", "Poly"))
Online_v7$Average.elevation
Online_v7$Flowering.time..initiation.month.

str(Online_v7)

### Adding a level for "mixed" reproductive mode
library(dplyr)
i = 1
Repr_mode_summ <- factor(rep(1, length(unique(Online_v7$SpeciesName))))
levels(Repr_mode_summ) <- c("Sexual", "Mixed", "Apomictic")
Repr_mode_summ <- data.frame(Repr_mode_summ, "SpeciesName" = unique(Online_v7$SpeciesName))
while (i <= length(unique(Online_v7$SpeciesName))) {
  if (length(levels(droplevels(Online_v7[which(unique(Online_v7$SpeciesName)[i] == Online_v7$SpeciesName), "Reproductive.mode"]))) >= 2) {
    Repr_mode_summ$Repr_mode_summ[i] <- "Mixed"} else {
      Repr_mode_summ$Repr_mode_summ[i] <- as.character(first(Online_v7[which(unique(Online_v7$SpeciesName)[i] == Online_v7$SpeciesName), "Reproductive.mode"]))
    }
  i = i + 1
}
Repr_mode_summ

Online_v7 <- merge(Online_v7, Repr_mode_summ, by = "SpeciesName", all.x = T)

##### Adding botanical authors names to species names ##### 
library(taxize)
Online_v7_mean$SpeciesName
gsub('_', ' ', Online_v7_mean$SpeciesName)

View(gnr_datasources()) # IPNI is id 167
t <- gnr_resolve("Eupatorium cannabinum", data_source_ids = 167, canonical = F)
t
t$user_supplied_name
t$submitted_name
t$data_source_title
t$score
t$matched_name
t$matched_name2

ipni_match <- as.data.frame(gnr_resolve(gsub('_', ' ', Online_v7$SpeciesName), data_source_ids = 167, canonical = F, with_context = T))
ipni_match
ipni_match$matched_name
ipni_match$score

ipni_match[, c(1,3,5)]
ipni_match$matched_name
### not so easy to fugure out which name is accepted... 

##### Matching with GSheet for herbarium #####
library(googlesheets4)

### work in progress


# tnrs("Eupatorium cannabinum", source = "ipni")
# tax_name("Eupatorium cannabinum", get = "species", db = "both")
# get_uid("Eupatorium cannabinum")
# get_ids(names = "Eupatorium cannabinum", db = "itis")

##### How many genera and species? - Extended ######
unique(gsub('\\s.*', '', Online_v7$SpeciesName)) # list of unique genera
length(unique(gsub('\\s.*', '', Online_v7$SpeciesName))) # number of unique genera
unique(Online_v7$SpeciesName) # number of taxa (included subspecies)

unique(gsub('subsp.*', '', Online_v7$SpeciesName)) # list of unique species (no subspecies)
length(unique(gsub('subsp.*', '', Online_v7$SpeciesName))) # number of unique species (no subspecies)

table(Online_v7$Repr_mode_summ) # number of sexual and apomictic taxa

##### Average elevation ##### 
as.numeric(gsub('—', 'NA', Online_v7$Elevation.in.strict.alpine.arc.from.wild.collection))

min(as.numeric(gsub('—', 'NA', Online_v7$Elevation.in.strict.alpine.arc.from.wild.collection)), na.rm = T)
mean(as.numeric(gsub('—', 'NA', Online_v7$Elevation.in.strict.alpine.arc.from.wild.collection)), na.rm = T)
max(as.numeric(gsub('—', 'NA', Online_v7$Elevation.in.strict.alpine.arc.from.wild.collection)), na.rm = T)

##### Changing names of species back to those used on the tree ##### 
sort(JanTree4$tip.label) == levels(DATA$SpeciesName)

Online_v7$SpeciesName <- gsub('\\s$', '', Online_v7$SpeciesName)
Online_v7$SpeciesName <- gsub('\\s', '_', Online_v7$SpeciesName)
Online_v7$SpeciesName

setdiff(Online_v7$SpeciesName, DATA$SpeciesName)
setdiff(DATA$SpeciesName, Online_v7$SpeciesName)

unique(Online_v7[Online_v7$SpeciesName %in% setdiff(Online_v7$SpeciesName, DATA$SpeciesName), 
                 c("ID_FloraAlpina", "SpeciesName")])
nrow(Online_v7[Online_v7$SpeciesName %in% setdiff(Online_v7$SpeciesName, DATA$SpeciesName), c("ID_FloraAlpina", "SpeciesName")])

unique(DATA[DATA$SpeciesName %in% setdiff(DATA$SpeciesName, Online_v7$SpeciesName), 
            c("ID_FloraAlpina", "SpeciesName")])
nrow(DATA[DATA$SpeciesName %in% setdiff(DATA$SpeciesName, Online_v7$SpeciesName), c("ID_FloraAlpina", "SpeciesName")])

name.changes <- merge(Online_v7[Online_v7$SpeciesName %in% setdiff(Online_v7$SpeciesName, DATA$SpeciesName), c("ID_FloraAlpina", "SpeciesName", "Collection_ID")], 
                      DATA[DATA$SpeciesName %in% setdiff(DATA$SpeciesName, Online_v7$SpeciesName), c("ID_FloraAlpina", "SpeciesName", "ID")], 
                      by = "ID_FloraAlpina")
name.changes

name.changes[, c(2, 4)] # checks out

sum(Online_v7$SpeciesName %in% name.changes$SpeciesName.x) # this grabs th rows that needs to change
Online_v7[Online_v7$SpeciesName %in% name.changes$SpeciesName.x, "SpeciesName"]
name.changes$SpeciesName.x

sum(DATA$SpeciesName %in% name.changes$SpeciesName.y) # Siegesbeckia is dropped from Online compared to DATA
DATA[DATA$SpeciesName %in% name.changes$SpeciesName.y, "SpeciesName"]
name.changes$SpeciesName.y

### Substitute names
# x = 1
# while (x <= nrow(Online_v7)) {
#   if (Online_v7$SpeciesName[x] %in% name.changes$SpeciesName.x) {
#     Online_v7$SpeciesName[x] <- as.character(name.changes[match(Online_v7$SpeciesName[x], name.changes$SpeciesName.x), "SpeciesName.y"])
#   }
#   x = x + 1
# }
# Online_v7$SpeciesName

JanTree4_CC_online <- JanTree4

x = 1
while (x <= length(JanTree4_CC_online$tip.label)) {
  if (JanTree4_CC_online$tip.label[x] %in% name.changes$SpeciesName.y) {
    JanTree4_CC_online$tip.label[x] <- as.character(name.changes[match(JanTree4_CC_online$tip.label[x], name.changes$SpeciesName.y), "SpeciesName.x"])
  }
  x = x + 1
}
JanTree4_CC_online$tip.label
  
setdiff(Online_v7$SpeciesName, item)
setdiff(item, Online_v7$SpeciesName)
### all looks good, going to drop the two species later



##### ~ #####



##### Summarizing extended #####
library(dplyr) 
Online_v7_ext_mean1 <- Online_v7 %>% 
  select(ID_FloraAlpina, SpeciesName, 
         Average.elevation, Flowering.time..initiation.month.) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(Average.elevation, Flowering.time..initiation.month.), 
               list(mean), na.rm = T)

Online_v7_ext_mean2 <- Online_v7 %>% 
  select(ID_FloraAlpina, SpeciesName, 
         Embryo.Ploidy.summ, Repr_mode_summ, Reproductive.mode) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(Embryo.Ploidy.summ, Reproductive.mode, Repr_mode_summ),
               list(first))

Online_v7_mean <- merge(Online_v7_ext_mean1, Online_v7_ext_mean2, by = c("ID_FloraAlpina", "SpeciesName"), all.x = T)
colnames(Online_v7_mean)
nrow(Online_v7_mean)
rm(Online_v7_ext_mean1)
rm(Online_v7_ext_mean2)

setdiff(Online_v7_mean$SpeciesName, DATA_CC_mean_red$SpeciesName)
nrow(DATA_CC_mean_red)

write.csv(Online_v7_mean, file = "Online_v7_mean.csv")

table(Online_v7_mean$Repr_mode_summ)
Online_v7_mean[Online_v7_mean$Repr_mode_summ == "Mixed", ]
with(Online_v7, table(Reproductive.mode, Embryo.Ploidy.summ))

### Checking tree
library(ape)

setdiff(Online_v7_mean$SpeciesName, JanTree4_CC_online$tip.label)
setdiff(JanTree4_CC_online$tip.label, Online_v7_mean$SpeciesName)

JanTree4_CC_online <- drop.tip(JanTree4_CC_online, setdiff(JanTree4_CC_online$tip.label, Online_v7_mean$SpeciesName))
JanTree4_CC_online

setdiff(Online_v7_mean$SpeciesName, JanTree4_CC_online$tip.label)
setdiff(JanTree4_CC_online$tip.label, Online_v7_mean$SpeciesName)

is.binary(JanTree4_CC_online)
is.ultrametric(JanTree4_CC_online)
JanTree4_CC_online$edge.length[JanTree4_CC_online$edge.length == 0] 
### all good

write.tree(JanTree4_CC_online, file = "JanTree4_CC_online.tre")



##### Strictly alpine #####
##### Could be doing with the grab method, pasted below #####
# ### Stuff from HB Lautaret
# Online_v7[grep('Laut', Online_v7$Collection_ID), c(1:3, 7)]
# NotWild <- as.character(Online_v7[grep('Laut', Online_v7$Collection_ID), c(1:3, 8)][, "Collection_ID"])
# ### Stuff from Rolland
# Online_v7[grep('Douzet', Online_v7$Collection_ID), c(1:3, 7)][3,] # only this needs removing
# NotWild <- c(NotWild, as.character(Online_v7[grep('Douzet', Online_v7$Collection_ID), c(1:3, 8)][3, "Collection_ID"]))
# # ### Stuff from Juri
# # Online_v7[grep('Vos', Online_v7$Collection_ID), c(1:3, 7)]
# # NotWild <- c(NotWild, as.character(Online_v7[grep('Vos', Online_v7$Collection_ID), c(1:3, 8)][, "Collection_ID"]))
# ### Stuff from Jaume
# Online_v7[grep('Pellicer', Online_v7$Collection_ID), c(1:3, 7)]
# NotWild <- c(NotWild, as.character(Online_v7[grep('Pellicer', Online_v7$Collection_ID), c(1:3, 8)][, "Collection_ID"]))
# ### Stuff from Teresa
# Online_v7[grep('Garnatje', Online_v7$Collection_ID), c(1:3, 7)]
# NotWild <- c(NotWild, as.character(Online_v7[grep('Garnatje', Online_v7$Collection_ID), c(1:3, 8)][, "Collection_ID"]))
# ### Stuff from Kew
# Online_v7[grep('Kew', Online_v7$Collection_ID), c(1:3, 7)]
# NotWild <- c(NotWild, as.character(Online_v7[grep('Kew', Online_v7$Collection_ID), c(1:3, 8)][, "Collection_ID"]))
# ### Stuff from Oriane (except the ones that are wild w/ altitude)
# Online_v7[grep('Hidalgo', Online_v7$Collection_ID), c(1:3, 7)]
# NotWild <- c(NotWild, as.character(DATA[grep('Hidalgo', DATA$ID), c(1:3, 8)][, "ID"]))
# 
# ### Stuff outside of the Alps (using QGIS, I selected the features outside of the alpine arc)
# OutsideAlps <- c("FR42","FR60","FR61","FR62","FR63","FR64","FR65","FR66","FR67","FR67b",
#                  "FR68","FR69","FR70","FR71","FR72","FR73","FR74","FR75a","FR75b","FR75c",
#                  "FR300","FRR1","FRR10","FRR11","FRR2","FRR3","FRR4","FRR6","FRR7","FRR8",
#                  "FRR9","FR411","FR477","FR487","FR488","FR544","FR560","FR561","FR562",
#                  "FR563","FR564","FR565","FR566", "FR582","FR612","FR619","FR623","FR641","FR656",
#                  "FR675","FR676","FR683","IT18","IT19","IT20","IT21","IT22","IT37","IT38",
#                  "IT39","IT40","IT41","IT42","IT43","IT72","IT73","IT76","IT77","FR710",
#                  "A1","A2","A3","A4","A30","A31","A32","A41","A42","A43","A44","A45","A46",
#                  "CH159","IT87","IT88","IT99","MB2","MB7","MB8","MB9","MB10","MB11",
#                  "MB12","MB13","MB15","MB16","MB18","MB19","MB20","MB21","MB22","MB23",
#                  "MB24","MB25","MB26","MB27","MB28","MB29","MB30","MB31","MB32","MB33",
#                  "MB34","MB35","MB36","MB37","MB38","MB39","MB43","MB48","MB49",
#                  "MB50","MB51","MB52","MB53","MB54","MB55","MB56","MB57","MB58","MB90",
#                  "MB91","MB92","MB95","MB101","MB105","MB106","MB107","MB108","MB109",
#                  "MB110","MB111","MB112","MB113","MB114","MB115","MB116","MB118","MB119",
#                  "MB120","MB121","MB122","MB123","MB124","MB125","MB126","MB127","MB128",
#                  "MB129","MB131","MB132","MB133","MB134","OH476","OH488")
# 
# OutsideAlps <- c(OutsideAlps, gsub('FR', '', OutsideAlps[grep('FRR.', OutsideAlps)])) # misformatting of IDs with "FR R" fix
# OutsideAlps <- unique(OutsideAlps)
# 
# grep('\\<FR69\\>', Online_v7$Collection_ID) # this grabs only whole words within \\< \\>
# 
# paste("\\<", OutsideAlps[1], "\\>", sep = "")
# 
# length(grep(paste("\\<", OutsideAlps[12], "\\>", sep = ""), Online_v7$Collection_ID, value = T))
# 
# i = 1
# OutsideAlps_online <- data.frame(short = OutsideAlps, long = "x", stringsAsFactors = F)
# while (i <= length(OutsideAlps)) {
#   if (length(grep(paste("\\<", OutsideAlps[i], "\\>", sep = ""), Online_v7$Collection_ID, value = T)) > 0) {
#     OutsideAlps_online[i, "long"] <- grep(paste("\\<", OutsideAlps[i], "\\>", sep = ""), Online_v7$Collection_ID, value = T)
#   } else {OutsideAlps_online[i, "long"] <- "nada"}
#   i = i + 1 
# }
# OutsideAlps_online
# 
# OutsideAlps_online[OutsideAlps_online$long != "nada", "long"]
# 
# 
# NotWild <- c(NotWild, OutsideAlps_online[OutsideAlps_online$long != "nada", "long"])
# NotWild <- unique(NotWild)
# 
# Online_v7_StrictlyAlps <- Online_v7[!Online_v7$Collection_ID %in% NotWild, ]
# nrow(Online_v7_StrictlyAlps)
# 
# Online_v7[!Online_v7$Collection_ID %in% NotWild, ]

##### Easier to just exclude accessions with "—" #####
Online_v7_StrictlyAlps <- Online_v7[Online_v7$Elevation.in.strict.alpine.arc.from.wild.collection != "—" & !is.na(Online_v7$Elevation.in.strict.alpine.arc.from.wild.collection), ]
Online_v7_StrictlyAlps[Online_v7_StrictlyAlps$Lat_N == "—", ]
nrow(Online_v7_StrictlyAlps)

str(Online_v7_StrictlyAlps)
Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection
Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection <- as.integer(as.character(Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection))

##### How many genera and species? - Strictly Alps ######
unique(gsub('\\s.*', '', Online_v7_StrictlyAlps$SpeciesName)) # list of unique genera
length(unique(gsub('\\s.*', '', Online_v7_StrictlyAlps$SpeciesName))) # number of unique genera
unique(Online_v7_StrictlyAlps$SpeciesName) # number of taxa (included subspecies)

unique(gsub('subsp.*', '', Online_v7_StrictlyAlps$SpeciesName)) # list of unique species (no subspecies)
length(unique(gsub('subsp.*', '', Online_v7_StrictlyAlps$SpeciesName))) # number of unique species (no subspecies)

table(Online_v7_StrictlyAlps$Repr_mode_summ) # number of sexual and apomictic taxa

##### Summarizing strictly alpine #####
library(dplyr) 
Online_v7_str_mean1 <- Online_v7_StrictlyAlps %>% 
  select(ID_FloraAlpina, SpeciesName, 
         Elevation.in.strict.alpine.arc.from.wild.collection, Flowering.time..initiation.month.) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(Elevation.in.strict.alpine.arc.from.wild.collection, Flowering.time..initiation.month.), 
               list(mean), na.rm = T)

Online_v7_str_mean2 <- Online_v7_StrictlyAlps %>% 
  select(ID_FloraAlpina, SpeciesName, 
         Embryo.Ploidy.summ, Reproductive.mode, Repr_mode_summ) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(Embryo.Ploidy.summ, Reproductive.mode, Repr_mode_summ),
               list(first))

Online_v7_StrictlyAlps_mean <- merge(Online_v7_str_mean1, Online_v7_str_mean2, by = c("ID_FloraAlpina", "SpeciesName"), all.x = T)
colnames(Online_v7_StrictlyAlps_mean)
nrow(Online_v7_StrictlyAlps_mean)
rm(Online_v7_str_mean1)
rm(Online_v7_str_mean2)

setdiff(DATA_StrictlyAlps_mean_red$SpeciesName, Online_v7_StrictlyAlps_mean$SpeciesName)
nrow(DATA_StrictlyAlps_mean_red)

### Checking tree for summarized alpine: 
setdiff(Online_v7_StrictlyAlps_mean$SpeciesName, JanTree4_StrictlyAlps$tip.label)
setdiff(JanTree4_StrictlyAlps$tip.label, Online_v7_StrictlyAlps_mean$SpeciesName)
### strange that Homogyne discolor was not in the tree before... 

write.csv(Online_v7_StrictlyAlps_mean, file = "Online_v7_StrictlyAlps_mean.csv")

##### Prepare the tree for strictly Alps ##### 
library(ape)
JanTree4_StrictlyAlps_online <- drop.tip(JanTree4, setdiff(JanTree4$tip.label, Online_v7_StrictlyAlps_mean$SpeciesName))

setdiff(Online_v7_StrictlyAlps_mean$SpeciesName, JanTree4_StrictlyAlps_online$tip.label)
setdiff(JanTree4_StrictlyAlps_online$tip.label, Online_v7_StrictlyAlps_mean$SpeciesName)
# length(Online_v7_StrictlyAlps_mean$SpeciesName) == length(JanTree4_StrictlyAlps_online$tip.label)
which(duplicated(Online_v7_StrictlyAlps_mean$SpeciesName))
which(duplicated(JanTree4_StrictlyAlps_online$tip.label))

is.binary(JanTree4_StrictlyAlps_online)
is.ultrametric(JanTree4_StrictlyAlps_online)
JanTree4_StrictlyAlps_online$edge.length[JanTree4_StrictlyAlps_online$edge.length == 0] # are there branch lengths with value zero
### all good

write.tree(JanTree4_StrictlyAlps_online, file = "JanTree4_StrictlyAlps_online.tre")


##### ~ ##### 



##### Phylosig ##### 
### Extended
library(phytools)
Online_v7_mean
str(Online_v7_mean)
colnames(Online_v7_mean)
rownames(Online_v7_mean)

i = 3
K_lambda_table_ext_online <- data.frame("Variable" = character(),
                                        "K" = double(), "p_K" = double(), "p_k-stars" = character(),
                                        "lambda" = double(), "p_lambda" = double(), "p_lambda-stars" = character(),
                                        stringsAsFactors = F)
while (i %in% 3:ncol(Online_v7_mean)) {
  obj1 <- phylosig(tree = JanTree4_CC_online, x = setNames(as.numeric(unlist(Online_v7_mean[,i])), Online_v7_mean$SpeciesName), method = "K", test = T)
  obj2 <- phylosig(tree = JanTree4_CC_online, x = setNames(as.numeric(unlist(Online_v7_mean[,i])), Online_v7_mean$SpeciesName), method = "lambda", test = T)
  K_lambda_table_ext_online[i-2,] <- cbind(colnames(Online_v7_mean[i]),
                                    obj1$K, obj1$P, symnum(obj1$P, corr = FALSE, cutpoints = c(0,  .001,.01,.05, .1, 1), symbols = c("***","**","*","."," ")),
                                    obj2$lambda, obj2$P, symnum(obj2$P, corr = FALSE, cutpoints = c(0,  .001,.01,.05, .1, 1), symbols = c("***","**","*","."," "))
  )
  i = i + 1
}
rm(obj1)
rm(obj2)
K_lambda_table_ext_online

write.table(K_lambda_table_ext_online, file = "K_lambda_table_ext_online.txt")

### Strictly Alpine
library(phytools)
Online_v7_StrictlyAlps_mean
str(Online_v7_StrictlyAlps_mean)
colnames(Online_v7_StrictlyAlps_mean)
rownames(Online_v7_StrictlyAlps_mean)

i = 3
K_lambda_table_str_online <- data.frame("Variable" = character(),
                                        "K" = double(), "p_K" = double(), "p_k-stars" = character(),
                                        "lambda" = double(), "p_lambda" = double(), "p_lambda-stars" = character(),
                                        stringsAsFactors = F)
while (i %in% 3:ncol(Online_v7_StrictlyAlps_mean)) {
  obj1 <- phylosig(tree = JanTree4_StrictlyAlps_online, x = setNames(as.numeric(unlist(Online_v7_StrictlyAlps_mean[,i])), Online_v7_StrictlyAlps_mean$SpeciesName), method = "K", test = T)
  obj2 <- phylosig(tree = JanTree4_StrictlyAlps_online, x = setNames(as.numeric(unlist(Online_v7_StrictlyAlps_mean[,i])), Online_v7_StrictlyAlps_mean$SpeciesName), method = "lambda", test = T)
  K_lambda_table_str_online[i-2,] <- cbind(colnames(Online_v7_StrictlyAlps_mean[i]),
                                           obj1$K, obj1$P, symnum(obj1$P, corr = FALSE, cutpoints = c(0,  .001,.01,.05, .1, 1), symbols = c("***","**","*","."," ")),
                                           obj2$lambda, obj2$P, symnum(obj2$P, corr = FALSE, cutpoints = c(0,  .001,.01,.05, .1, 1), symbols = c("***","**","*","."," "))
  )
  i = i + 1
}
rm(obj1)
rm(obj2)
K_lambda_table_str_online

write.table(K_lambda_table_str_online, file = "K_lambda_table_str_online.txt")


##### ~ ##### 



##### Extended database taxa counts #####
unique(gsub('_.*', '', Online_v7$SpeciesName)) # list of unique genera
length(unique(gsub('_.*', '', Online_v7$SpeciesName))) # number of unique genera

unique(gsub('_subsp.*', '', Online_v7$SpeciesName)) # list of unique species (no subspecies)
length(unique(gsub('_subsp.*', '', Online_v7$SpeciesName))) # number of unique species (no subspecies)

Online_v7$SpeciesName
length(unique(Online_v7$SpeciesName)) # number of unique taxa (also subspecies)

table(Online_v7$Reproductive.mode) # number of sexual and apomictic taxa
with(Online_v7, table(Reproductive.mode, Embryo.Ploidy.summ)) # per ploidy

max(Online_v7$Average.elevation, na.rm = T)
min(Online_v7$Average.elevation, na.rm = T)
mean(Online_v7$Average.elevation, na.rm = T)

##### Strictly Alps database taxa counts #####
unique(gsub('_.*', '', Online_v7_StrictlyAlps$SpeciesName)) # list of unique genera
length(unique(gsub('_.*', '', Online_v7_StrictlyAlps$SpeciesName))) # number of unique genera

unique(gsub('_subsp.*', '', Online_v7_StrictlyAlps$SpeciesName)) # list of unique species (no subspecies)
length(unique(gsub('_subsp.*', '', Online_v7_StrictlyAlps$SpeciesName))) # number of unique species (no subspecies)

Online_v7_StrictlyAlps$SpeciesName
length(unique(Online_v7_StrictlyAlps$SpeciesName)) # number of unique taxa (also subspecies)

table(Online_v7_StrictlyAlps$Reproductive.mode) # number of sexual and apomictic taxa
with(Online_v7_StrictlyAlps, table(Reproductive.mode, Embryo.Ploidy.summ)) # per ploidy

max(Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection, na.rm = T)
min(Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection, na.rm = T)
mean(Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection, na.rm = T)


##### ~ ##### 



##### Plots: extended dataset #####
### Data prep
library(dplyr)
Asteraceae_barplot <- Online_v7_mean %>%
  group_by(Embryo.Ploidy.summ, Repr_mode_summ) %>%
  count()
str(Asteraceae_barplot)

with(Online_v7, table(Repr_mode_summ, Embryo.Ploidy.summ)) # per ploidy
Online_v7_mean[Online_v7_mean$Repr_mode_summ == "Mixed", c("SpeciesName", "Embryo.Ploidy.summ", "Repr_mode_summ")]

### Barplot
library(ggplot2)
library(RColorBrewer)
bargraph_ext <- ggplot(Asteraceae_barplot,
                       aes(factor(Embryo.Ploidy.summ),
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
  scale_fill_manual(values = c(brewer.pal(name = "Set1", 5)[2],
                               brewer.pal(name = "Set1", 5)[5],
                               brewer.pal(name = "Set1", 5)[1])) +
  theme(legend.position = c(.7,.8))
# ggtitle("Apomixis and ploidy")
bargraph_ext
ggsave(plot = bargraph_ext, filename = "ApomixisVSPloidy_ext_online.pdf", dpi = 150, device = "pdf", scale = 1)

### Boxplot
boxplot_ext <- ggplot(data = Online_v7_mean,
                      aes(x = Reproductive.mode,
                          y = as.numeric(Flowering.time..initiation.month.),
                          fill = Reproductive.mode)) +
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
boxplot_ext
ggsave(plot = boxplot_ext, filename= "ApomixisVSPhenology_ext_online.pdf", dpi = 150, device = "pdf", scale = 1)

ggpubr::ggarrange(bargraph_ext, boxplot_ext, ncol = 2, nrow = 1)
ggsave(ggpubr::ggarrange(bargraph_ext, boxplot_ext, ncol = 2, nrow = 1), 
       filename = "Barplot_Boxplot_ext_online.pdf", device = "pdf", dpi = 150)

dev.off() # reset graphics device

#####  ggTree circular #####
library(ape)
library(ggplot2)
library(ggtree)
library(RColorBrewer)

library(geiger)
library(phytools)

JanTree4_CC_online_ggtree <- JanTree4_CC_online
JanTree4_CC_online_ggtree$tip.label <- gsub('_', ' ', JanTree4_CC_online_ggtree$tip.label)

plotTree(JanTree4_CC_online_ggtree, fsize = .35)
# JanTree4_CC_online_ggtree$edge.length <- NULL
# JanTree4_CC_online_ggtree$edge.length <- JanTree4_CC_online_ggtree$edge.length*2
# JanTree4_CC_online_ggtree <- rescale(JanTree4_CC_online_ggtree, model = "BM")
# plotTree(JanTree4_CC_online_ggtree, fsize = .35)

### Make ad-hoc table for Repr_mode to be plotted: 
Online_v7_mean[match(JanTree4_CC_online$tip.label, Online_v7_mean$SpeciesName), "SpeciesName"] == JanTree4_CC_online$tip.label
Online_v7_ggplot <- Online_v7_mean[match(JanTree4_CC_online$tip.label, Online_v7_mean$SpeciesName), c("SpeciesName", "Repr_mode_summ")]
Online_v7_ggplot$SpeciesName <- gsub('_', ' ', Online_v7_ggplot$SpeciesName)
Online_v7_ggplot$Repr_mode_summ

setdiff(Online_v7_ggplot$SpeciesName, JanTree4_CC_online_ggtree$tip.label)
setdiff(JanTree4_CC_online_ggtree$tip.label, Online_v7_ggplot$SpeciesName)

# use ggtree(..., branch.length = "none")
ggJanTree4_CC_online_circular <- ggtree(JanTree4_CC_online_ggtree, layout = "circular", size = 1) %<+% Online_v7_ggplot +
  geom_tiplab2(size = 2, offset = 0.025, fontface = "italic") +
  geom_hilight(mrca(JanTree4_CC_online)["Achillea_clavennae", "Artemisia_vulgaris"], fill = brewer.pal(10, "Set3")[1]) +
  geom_hilight(mrca(JanTree4_CC_online)["Doronicum_grandiflorum", "Doronicum_austriacum"], fill = brewer.pal(10, "Set3")[2]) +
  geom_hilight(mrca(JanTree4_CC_online)["Symphyotrichum_squamatum", "Bellis_perennis"], fill = brewer.pal(10, "Set3")[3]) +
  geom_hilight(mrca(JanTree4_CC_online)["Calendula_arvensis", "Calendula_tripterocarpa"], fill = brewer.pal(10, "Set3")[4]) +
  geom_hilight(mrca(JanTree4_CC_online)["Senecio_vulgaris", "Tephroseris_integrifolia"], fill = brewer.pal(10, "Set3")[2]) +
  geom_hilight(mrca(JanTree4_CC_online)["Gnaphalium_hoppeanum", "Phagnalon_saxatile"], fill = brewer.pal(10, "Set3")[5]) +
  geom_hilight(mrca(JanTree4_CC_online)["Galinsoga_quadriradiata", "Bidens_bipinnatus"], fill = brewer.pal(10, "Set3")[6]) +
  geom_hilight(mrca(JanTree4_CC_online)["Inula_conyzae", "Buphthalmum_salicifolium"], fill = brewer.pal(10, "Set3")[7]) +
  geom_hilight(mrca(JanTree4_CC_online)["Crepis_pygmaea", "Catananche_caerulea"], fill = brewer.pal(10, "Set3")[8]) +
  geom_hilight(mrca(JanTree4_CC_online)["Cirsium_oleraceum", "Echinops_exaltatus"], fill = brewer.pal(10, "Set3")[9]) +
  geom_tippoint(aes(color = Repr_mode_summ, x = x + .0125), size = 3) +
  scale_colour_manual(name = "Reproductive mode", values = c(brewer.pal(name = "Set1", 5)[2],
                                 brewer.pal(name = "Set1", 5)[5],
                                 brewer.pal(name = "Set1", 5)[1]),) +
  # ### Tribe labels on MRCA nodes
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Achillea_clavennae", "Artemisia_vulgaris"], label = "Anthemidae"), hjust = 1, nudge_x = -0, nudge_y = 0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Doronicum_grandiflorum", "Doronicum_austriacum"], label = "Senecioneae"), hjust = 1, nudge_x = 0, nudge_y = 0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Symphyotrichum_squamatum", "Bellis_perennis"], label = "Astereae"), hjust = 1, nudge_x = 1, nudge_y = -0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Calendula_arvensis", "Calendula_tripterocarpa"], label = "Calenduleae"), hjust = 1, nudge_x = 0, nudge_y = 0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Senecio_vulgaris", "Tephroseris_integrifolia"], label = "Senecioneae"), hjust = 1, nudge_x = -0, nudge_y = 0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Gnaphalium_hoppeanum", "Phagnalon_saxatile"], label = "Gnaphalieae"), hjust = 1, nudge_x = -0, nudge_y = 0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Galinsoga_quadriradiata", "Bidens_bipinnatus"], label = "Heliantheae"), hjust = 1, nudge_x = 0, nudge_y = -0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Inula_conyzae", "Buphthalmum_salicifolium"], label = "Inuleae"), hjust = 1, nudge_x = -0, nudge_y = 0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Crepis_pygmaea", "Catananche_caerulea"], label = "Cichorieae"), hjust = 1, nudge_x = -0, nudge_y = 0) +
  # geom_text2(aes(subset = node %in% mrca(JanTree4_CC_online)["Cirsium_oleraceum", "Echinops_exaltatus"], label = "Cardueae"), hjust = 1, nudge_x = -0, nudge_y = 0) +
  # geom_treescale(x = .5, y = 0, offset = 0, fontsize = 3) +
  theme(legend.position = c(0.05, 0.05)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +  
  xlim(0, 1.1)

ggJanTree4_CC_online_circular

ggsave(ggJanTree4_CC_online_circular, filename = "ggJanTree4_CC_online_circular.png", 
       device = "png", dpi = 300, scale = 1, 
       width = 30, height = 30, units = "cm")

ggsave(ggJanTree4_CC_online_circular, filename = "ggJanTree4_CC_online_circular.pdf", 
       device = "pdf", dpi = 300, scale = 1, 
       width = 30, height = 30, units = "cm")


##### Other plots: strictly Alps #####
### Data prep
library(dplyr)
Asteraceae_barplot <- Online_v7_StrictlyAlps %>%
  group_by(Embryo.Ploidy.summ, Repr_mode_summ) %>%
  count()
str(Asteraceae_barplot)

with(Online_v7_StrictlyAlps, table(Repr_mode_summ, Embryo.Ploidy.summ)) # per ploidy

### Barplot
library(ggplot2)
library(RColorBrewer)
bargraph_str <- ggplot(Asteraceae_barplot,
                       aes(factor(Embryo.Ploidy.summ),
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
bargraph_str
ggsave(plot = bargraph, filename = "ApomixisVSPloidy_str_online.pdf", dpi = 150, device = "pdf", scale = 1)

### Boxplot
boxplot_str <- ggplot(data = Online_v7_StrictlyAlps,
                      aes(x = Reproductive.mode,
                          y = as.numeric(Flowering.time..initiation.month.),
                          fill = Reproductive.mode)) +
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
boxplot_str
ggsave(plot = boxplot_str, filename= "ApomixisVSPhenology_str_online.pdf", dpi = 150, device = "pdf", scale = 1)

ggpubr::ggarrange(bargraph, boxplot, ncol = 2, nrow = 1)
ggsave(ggpubr::ggarrange(bargraph_str, boxplot_str, ncol = 2, nrow = 1), 
       filename = "Barplot_Boxplot_str_online.pdf", device = "pdf", dpi = 150)

dev.off() # reset graphics device 


###



