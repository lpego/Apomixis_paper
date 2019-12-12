##### Read in the table ##### 
Online_v7 <- read.csv(file = "/home/luca/Dropbox/Royal Botanic Gardens of Kew/ApomixisPaper/Apomixis_v7/Pegoraro_et_al_Apomixis-Online_Resource_1-v7 -JP_LP_OH2_LP.csv")
str(Online_v7)

Online_v7$SpeciesName <- as.character(Online_v7$SpeciesName)
Online_v7$Reproductive.mode <- factor(Online_v7$Reproductive.mode, levels = c("Sexual", "Apomictic"))
Online_v7$Embryo.Ploidy <- factor(Online_v7$Embryo.Ploidy, levels = c("2x", "3x", "4x", "6x", "8x", "12x"))
Online_v7$Embryo.Ploidy.summ <- gsub('6x|8x|12x', 'Poly', Online_v7$Embryo.Ploidy)
Online_v7$Embryo.Ploidy.summ <- factor(Online_v7$Embryo.Ploidy.summ, levels = c("2x", "3x", "4x", "Poly"))
Online_v7$Average.elevation
Online_v7$Flowering.time..initiation.month.

str(Online_v7)



##### Changing names of species back to those used on the tree ##### 
JanTree4$tip.label

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
x = 1
while (x <= nrow(Online_v7)) {
  if (Online_v7$SpeciesName[x] %in% name.changes$SpeciesName.x) {
    Online_v7$SpeciesName[x] <- as.character(name.changes[match(Online_v7$SpeciesName[x], name.changes$SpeciesName.x), "SpeciesName.y"])
  }
  x = x + 1
}
Online_v7$SpeciesName
  
setdiff(Online_v7$SpeciesName, DATA$SpeciesName)
setdiff(DATA$SpeciesName, Online_v7$SpeciesName)
### all looks good



##### ~ #####



##### Summarizing extended #####
library(dplyr) 
Online_v7_ext_mean1 <- Online_v7 %>% 
  select(ID_FloraAlpina, SpeciesName, 
         Average.elevation, Embryo.Ploidy.summ, Reproductive.mode, Flowering.time..initiation.month.) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(Average.elevation, Flowering.time..initiation.month.), 
               list(mean), na.rm = T)

Online_v7_ext_mean2 <- Online_v7 %>% 
  select(ID_FloraAlpina, SpeciesName, 
         Embryo.Ploidy.summ, Reproductive.mode) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(Embryo.Ploidy.summ, Reproductive.mode),
               list(first))

Online_v7_mean <- merge(Online_v7_ext_mean1, Online_v7_ext_mean2, by = c("ID_FloraAlpina", "SpeciesName"), all.x = T)
colnames(Online_v7_mean)
nrow(Online_v7_mean)
rm(Online_v7_ext_mean1)
rm(Online_v7_ext_mean2)

setdiff(Online_v7_mean$SpeciesName, DATA_CC_mean_red$SpeciesName)
nrow(DATA_CC_mean_red)

write.csv(Online_v7_mean, file = "Online_v7_mean.csv")

### Checking tree
library(ape)

setdiff(Online_v7_mean$SpeciesName, JanTree4$tip.label)
setdiff(JanTree4$tip.label, Online_v7_mean$SpeciesName)

JanTree4_CC_online <- drop.tip(JanTree4_CC, setdiff(JanTree4$tip.label, Online_v7$SpeciesName))
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
Online_v7_StrictlyAlps[Online_v7_StrictlyAlps$Lat_N == "—", "Collection_ID"]
nrow(Online_v7_StrictlyAlps)

str(Online_v7_StrictlyAlps)
Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection
as.integer(as.character(Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection))
Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection <- as.integer(as.character(Online_v7_StrictlyAlps$Elevation.in.strict.alpine.arc.from.wild.collection))

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
         Embryo.Ploidy.summ, Reproductive.mode) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(Embryo.Ploidy.summ, Reproductive.mode),
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




### 