##### Correcting the FloraAlpinaID ####

### Due to the fact that many collections are not identified to subspecies level, 
### I had to introduce an additional taxon for each species with one or more subspecies in Flora Alpina. 
### This is necessary if I want to use FloraALpinaID as a unique key, as otherwise 
### 124.1.1 would match 124.1.1.1 and 124.1.1.2 (if there are subspecies); 
### To indicate undetermined subspecies I use notation 124.1.1.0
### Code below uses the Google Sheet Alpine Asteraceae Checklist as a base

GSheet <- read.csv("Alpine Asteraceae Checklist - DATA.csv") # as dowloaded on 04Feb2019

sub <- as.character(GSheet$Flora.Alpina.ID)
index <- grep('124\\.[0-9]+\\.[0-9]+$', sub) # grab the positions of the values matched by the RegEx
sub[index] # these are the actual values that I want to change

sub[index] <- paste(sub[index], ".0", sep="") # paste DIRECTLY onto the grabbed positions 
sub
length(sub)
length(GSheet$Flora.Alpina.ID)
cbind(sub, as.vector(GSheet[,1])) # sanity check to see if all went well

GSheet[,1] <- sub
write.csv(GSheet, file="GSheet_mod.csv")
# View(GSheet)



##### Append FloraAlpinaID to ApomixisTable2018, from Google Sheet #####
### Matching using species names
NewApomixisData <- read.csv("NewApomixis_Table_2018.csv")

GSheet$SpeciesName <- paste(GSheet$Genus, GSheet$species, sep = " ")
GSheet$SpeciesName <- paste(GSheet$SpeciesName, GSheet$subsp, sep = " subsp. ")
GSheet$SpeciesName <- gsub(' subsp.[ ]$','', GSheet$SpeciesName)
GSheet$SpeciesName <- gsub(' subsp. ind$','', GSheet$SpeciesName)

colnames(NewApomixisData)[2] <- "SpeciesName"

NewApoData1 <- merge(NewApomixisData, unique(GSheet[,c("SpeciesName", "Flora.Alpina.ID")]), by = "SpeciesName")

intersect(NewApoData1$ID, NewApomixisData$ID)
setdiff(NewApomixisData$SpeciesName, NewApoData1$SpeciesName)
nrow(NewApomixisData) - nrow(unique(NewApoData1))

### Matching using collector's ID
GSheet$ID <- gsub('(?i)Luca Pegoraro ', '', GSheet$Collector.s.ID)
NewApoData2 <- merge(NewApomixisData, unique(GSheet[,c("ID", "Flora.Alpina.ID")]), by = "ID")

nrow(NewApomixisData) - nrow(NewApoData2)
setdiff(NewApomixisData$ID, NewApoData2$ID)

setdiff(NewApoData1[,c("ID")], NewApoData2[,c("ID")])

NewApoData <- rbind(NewApoData1, NewApoData2)
NewApoData <- unique(NewApoData)
NewApoData <- NewApoData[, c(2,1,3:10)]

nrow(NewApomixisData) - nrow(NewApoData)

write.csv(NewApoData, file = "NewApoData.csv")

NewApomixisData[,c("Flora.Alpina.ID")] <- 0
length(intersect(NewApomixisData[,"ID"], NewApoData[,"ID"]))

colnames(NewApomixisData)[2] <- "SpeciesName"
NewApomixisData_FloraAlpinaID <- rbind(NewApoData, NewApomixisData)
nrow(NewApomixisData_FloraAlpinaID)
NewApomixisData_FloraAlpinaID$SpeciesName <- gsub(' $','',NewApomixisData_FloraAlpinaID$SpeciesName)
NewApomixisData_FloraAlpinaID <- NewApomixisData_FloraAlpinaID[!duplicated(NewApomixisData_FloraAlpinaID[, c("ID", "SpeciesName","GS","Altitude")]),]
nrow(NewApomixisData_FloraAlpinaID)
nrow(NewApomixisData_FloraAlpinaID) == nrow(NewApomixisData) 
### some accessions of the same ID have two different GS or two Repr_mode... 

write.csv(NewApomixisData_FloraAlpinaID, file ="NewApomixisData_FloraAlpinaID.csv")

### Need to resolve manually double IDs! See file FloraAlpina_Asteraceae_OriginalData.csv 



##### ~ #####



##### PHYLOGENETIC TREE #####
library(ape)
JanTree <- read.tree(file = "Apomixis_alpine_Jan2019.tre")
JanTree
plot(JanTree, cex = .25)
is.binary(JanTree)
is.ultrametric(JanTree)
JanTree$edge.length[JanTree$edge.length == 0]

JanTree2 <- JanTree
JanTree2$tip.label

##### Checking names and tree tip labels #####
names_correspondences <- read.csv(file = "Apomixis_asteraceae_cbind.csv")

is.nothing <- function(x) {
  if (x==""){
    return (TRUE)
  }
  else {
    return (FALSE)
  }
}

pos <- unlist(lapply(names_correspondences$Name.on.original.table, is.nothing))

count=1
while (count <= nrow(names_correspondences)){
  if (pos[count]) {
    names_correspondences$newcolumn[count] <- as.character(names_correspondences$Name.on.tree[count])
  } else {
    names_correspondences$newcolumn[count] <- as.character(names_correspondences$Name.on.original.table[count])
  }
  count<-count+1
}

old_names <- as.character(names_correspondences$Name.on.tree)
old_names <- gsub(' ', '_', old_names)
old_names <- gsub('subs._', 'subsp._', old_names)
new_names <- names_correspondences$newcolumn

setdiff(JanTree$tip.label, old_names)
setdiff(old_names, JanTree$tip.label)
intersect(JanTree$tip.label, old_names)

duplicated(old_names)
old_names[which(duplicated(old_names))]

JanTree2$tip.label[order(JanTree2$tip.label)]
is.ultrametric(JanTree2)
JanTree2 <- phytools::force.ultrametric(JanTree2)
ape::is.binary(JanTree2)

new <- names_correspondences[order(names_correspondences$Name.on.tree),]



##### Adding tips to tree #####
library(phytools)
### make sure that tree is read as ultrametric before modiying tips! Otherwise some functions may toss branch legnths. 

#### Alternative function that could be used to add tips; it doesn't let you decide specifically where inside the genus it'll put the new tip though. 
# par(mfrow=c(1,2))
# plot(JanTree2, cex = .3, no.margin = T)
# JanTree_add <- add.species.to.genus(JanTree2, "Leucanthemum_platylepis")
# plot(JanTree_add, cex = .3, no.margin = T)
# par(mfrow=c(1,1))

mrca(JanTree2)["Centaurea_rhaetica", "Centaurea_nigra"]
JanTree2 <- bind.tip(JanTree2, "Centaurea_uniflora", where = 426)
mrca(JanTree2)["Cirsium_vulgare", "Cirsium_arvense"]
JanTree2 <- bind.tip(JanTree2, "Cirsium_acaule", where = 456)
mrca(JanTree2)["Hieracium_alpinum", "Hieracium_lucidum"]
JanTree2 <- bind.tip(JanTree2, "Hieracium_humile", where = 390)
mrca(JanTree2)["Carduus_defloratus", "Cirsium_bertolonii"]
JanTree2 <- bind.tip(JanTree2, "Carduus_defloratus_subsp._carlinifolius", where = 446)
mrca(JanTree2)["Carduus_defloratus", "Cirsium_bertolonii"]
JanTree2 <- bind.tip(JanTree2, "Carduus_defloratus_subsp._summanus", where = 447)
mrca(JanTree2)["Chamaemelum_nobile", "Leucanthemum_vulgare"]
JanTree2 <- bind.tip(JanTree2, "Leucanthemum_platylepis", where = 314)
mrca(JanTree2)["Leucanthemum_vulgare", "Leucanthemum_platylepis"]
JanTree2 <- bind.tip(JanTree2, "Leucanthemum_pallens", where = 315)
mrca(JanTree2)["Leucanthemum_vulgare", "Leucanthemum_pallens"]
JanTree2 <- bind.tip(JanTree2, "Leucanthemum_atratum", where = 316)
mrca(JanTree2)["Leucanthemum_vulgare", "Leucanthemum_atratum"]
JanTree2 <- bind.tip(JanTree2, "Leucanthemum_coronopifolium", where = 317)
mrca(JanTree2)["Leucanthemum_vulgare", "Leucanthemum_coronopifolium"]
JanTree2 <- bind.tip(JanTree2, "Leucanthemum_halleri", where = 318)
mrca(JanTree2)["Senecio_doronicum", "Senecio_doria"]
JanTree2 <- bind.tip(JanTree2, "Senecio_nemorensis_subsp._jacquinianus", where = 333)

is.ultrametric(JanTree2)
JanTree2$edge.length

JanTree2$tip.label[order(JanTree2$tip.label)]
JanTree2$tip.label[which(duplicated(JanTree2$tip.label))]

plot(JanTree2, cex = .5, no.margin = T)
write.tree(JanTree2, file = "JanTree_AddedTips")

rename.tips <- function(phy, old_names, new_names) {
  mpos <- match(old_names, phy$tip.label)
  phy$tip.label[mpos] <- new_names
  return(phy)
}

JanTree3 <- rename.tips(JanTree2, old_names, new_names)

duplicated(JanTree3$tip.label)
JanTree3$tip.label[which(duplicated(JanTree3$tip.label))]
# View(cbind(JanTree3$tip.label[order(JanTree3$tip.label)],new_names[order(new_names)]))
setdiff(JanTree3$tip.label, new_names)
setdiff(new_names, JanTree3$tip.label)

### Need to resolve manually double IDs! See file FloraAlpina_Asteraceae_OriginalData.csv 



##### Check that names on tree correspond to names on dataset #####
NewApomixisData_FloraAlpinaID_manual <- read.csv(file = "NewApomixisData_FloraAlpinaID_manual_Jaume.csv")
NewApomixisData_FloraAlpinaID_manual$X <- NULL
NewApomixisData_FloraAlpinaID_manual$X.1 <- NULL
NewApomixisData_FloraAlpinaID_manual$SpeciesName <- gsub(' $', '', NewApomixisData_FloraAlpinaID_manual$SpeciesName)
NewApomixisData_FloraAlpinaID_manual$SpeciesName <- gsub(' ', '_', NewApomixisData_FloraAlpinaID_manual$SpeciesName)
NewApomixisData_FloraAlpinaID_manual$SpeciesName <- gsub('__', '_', NewApomixisData_FloraAlpinaID_manual$SpeciesName)
NewApomixisData_FloraAlpinaID_manual$Repr_mode <- gsub('apomictic', 'Apomictic', NewApomixisData_FloraAlpinaID_manual$Repr_mode)
NewApomixisData_FloraAlpinaID_manual$Repr_mode <- gsub('Apomictic ', 'Apomictic', NewApomixisData_FloraAlpinaID_manual$Repr_mode)
NewApomixisData_FloraAlpinaID_manual$Repr_mode <- gsub('sexual', 'Sexual', NewApomixisData_FloraAlpinaID_manual$Repr_mode)

# cbind(JanTree2$tip.label, NewApomixisData_FloraAlpinaID_manual$SpeciesName)
intersect(JanTree3$tip.label, NewApomixisData_FloraAlpinaID_manual$SpeciesName)
setdiff(NewApomixisData_FloraAlpinaID_manual$SpeciesName, JanTree3$tip.label)
setdiff(JanTree3$tip.label, NewApomixisData_FloraAlpinaID_manual$SpeciesName)

### Need to prune out a bunch of species
JanTree4 <- ape::drop.tip(JanTree3, c("Erigeron_borealis", "Erigeron_simplex", "Senecio_germanicus", "Scorzonera_hispanica"))

setdiff(NewApomixisData_FloraAlpinaID_manual$SpeciesName, JanTree4$tip.label)
setdiff(JanTree4$tip.label, NewApomixisData_FloraAlpinaID_manual$SpeciesName)
### All taxa are in both tree and table 

##### Checking tree properties (rooted, ultrametric, binary, etc) ##### 
plot(JanTree4, cex = .5, no.margin = T)
is.rooted(JanTree4)
is.ultrametric(JanTree4)
is.binary(JanTree4)
JanTree4 <- multi2di(JanTree4, random = TRUE)
is.rooted(JanTree4)
is.ultrametric(JanTree4)
is.binary(JanTree4)
JanTree4$edge.length[JanTree4$edge.length == 0] # are there branch lengths with value zero? 
JanTree4 <- compute.brlen(JanTree4, method = "Grafen")
is.rooted(JanTree4)
is.ultrametric(JanTree4)
is.binary(JanTree4)
JanTree4$edge.length[JanTree4$edge.length == 0]
plot(JanTree4, cex = .5, no.margin = T)
plotTree(JanTree4, type = "fan", ftype="i", fsize = 0.25, lwd=1)

### Visual check of differences betweeen trees
library(phytools)
facetoface1 <- cophylo(JanTree, JanTree2) # original VS added tips
plot(facetoface1, fsize = .3, pts = F)
facetoface2 <- cophylo(JanTree2, JanTree4) # added tips VS renamed & dropped tips
plot(facetoface2, fsize = .3, pts = F)

par(mfrow=c(1,2))
plot(JanTree2, cex = .25, tip.color = ifelse(JanTree2$tip.label %in% setdiff(JanTree2$tip.label, JanTree$tip.label), "red","black"))
plot(JanTree4, cex = .25, tip.color = ifelse(JanTree4$tip.label %in% setdiff(JanTree4$tip.label, JanTree2$tip.label), "red","black"))
par(mfrow=c(1,1))

ape::write.tree(JanTree4, file = "JanTree4.tre")

##### ~ ##### 



##### EXTENDED DATASET: alpine collections + botanic gardens and other localities #####
##### Flora Alpina original data prep #####
FloraAlpina <- read.csv("FloraAlpina_Asteraceae_OriginalData.csv", sep = ";")
colnames(NewApomixisData_FloraAlpinaID_manual)[10] <- "ID_FloraAlpina"

### Modify Flora Alpina ID to reflect unidentified subspecies
sub1 <- as.vector(FloraAlpina$ID_FloraAlpina)
index1 <- grep('124\\.[0-9]+\\.[0-9]+$', sub1) # grab the positions of the values matched by the RegEx
sub1[index1] # these are the actual values that I want to change

sub1[index1] <- paste(sub1[index1], ".0", sep="") # paste DIRECTLY onto the grabbed positions 
sub1
length(sub1) == length(FloraAlpina$ID_FloraAlpina)
cbind(sub1, as.vector(FloraAlpina$ID_FloraAlpina)) # sanity check to see if all went well

FloraAlpina$ID_FloraAlpina <- sub1

### Merge NewApomixisData_manual with Flora Alpina original data using FloraAlpina ID as key
colnames(NewApomixisData_FloraAlpinaID_manual)[10] <- "ID_FloraAlpina"
DATA <- merge(NewApomixisData_FloraAlpinaID_manual, FloraAlpina, by = "ID_FloraAlpina", all.x = T)
setdiff(NewApomixisData_FloraAlpinaID_manual$ID_FloraAlpina, FloraAlpina$ID_FloraAlpina)
colnames(DATA)[9] <- "Chr.number"
# View(DATA)

DATA$SpeciesName <- as.factor(DATA$SpeciesName)
DATA$SpeciesName
str(DATA)

##### Correspondence between unspecified subspecies and one of the subspecies #####
### This is to avoid missing values. Subspecies were chosen manually, according to where the samples come from. 
old_IDs <- c("124.2.1.0", 
             "124.6.1.0", 
             "124.31.11.0", 
             "124.31.19.0", 
             "124.31.2.0", 
             "124.35.2.0", 
             "124.39.2.0", 
             "124.39.8.0", 
             "124.46.8.0", 
             "124.48.14.0", 
             "124.96.1.0", 
             "124.96.1.0", 
             "124.86.4.0", 
             "124.68.25.0", 
             "124.67.1.0", 
             "124.65.1.0", 
             "124.56.3.0", 
             "124.61.3.0", 
             "124.60.9.0", 
             "124.60.4.0"
)

new_IDs <- c("124.2.1.2", 
             "124.6.1.1", 
             "124.31.11.1", 
             "124.31.19.1", 
             "124.31.2.1", 
             "124.35.2.1", 
             "124.39.2.1", 
             "124.39.8.3", 
             "124.46.8.2", 
             "124.48.14.1", 
             "124.96.1.1", 
             "124.96.1.1", 
             "124.86.4.3", 
             "124.68.25.1", 
             "124.67.1.1", 
             "124.65.1.2", 
             "124.56.3.1", 
             "124.61.3.1", 
             "124.60.9.3",
             "124.60.4.1"
)

### Initialize temporary objects for cycle
i = 1
collapsed <- data.frame(OLD_SpeciesName = as.character(), 
                        OLD_FloraAlpina.ID = as.character()
)
collapsed1 <- data.frame(NEW_Genus = as.character(),
                         NEW_Species = as.character(),
                         NEW_Subspecies = as.character(),
                         NEW_FloraAlpina.ID = as.character()
)
### Create correspondence table with chosen subspecies for undertemined accessions. 
if (length(old_IDs) == length(new_IDs)) {
  while (i <= length(old_IDs)) {
    collapsed <- rbind(collapsed, DATA[grep(old_IDs[[i]], DATA$ID_FloraAlpina), 
                                          c("SpeciesName", "ID_FloraAlpina", "ID")]) 
    collapsed1 <- rbind(collapsed1, FloraAlpina[grep(new_IDs[[i]], FloraAlpina$ID_FloraAlpina), 
                                                c("Genus", "Species", "Subspecies", "ID_FloraAlpina")]) 
    i = i + 1}
  pos <- match(gsub('.0$', '', collapsed$ID_FloraAlpina), gsub('.[0-9]$', '', collapsed1$ID_FloraAlpina))
  Subspecies_selection_table <- cbind(collapsed, collapsed1[pos,])
  colnames(Subspecies_selection_table) <- c("OLD_SpeciesName", "OLD_ID_FloraAlpina", "ID", 
                                            "NEW_Genus", "NEW_Species", "NEW_Subspecies", "NEW_ID_FloraAlpina")
  # rm(collapsed)
  # rm(collapsed1)
} else {
  collapsed <- "ERROR" 
  collapsed1 <- "ERROR"
}
Subspecies_selection_table
write.csv(Subspecies_selection_table, file = "Subspecies_selection_table.csv")

### Substitute the IDs with the ones referring to the subspecies (as chosen above)

### Try RegEx in isolation
grep('124.2.1.0', DATA$ID_FloraAlpina)
grep('124.2.1.0', gsub(old_IDs[1], new_IDs[1], DATA$ID_FloraAlpina))
grep('124.2.1.2', DATA$ID_FloraAlpina)
grep('124.2.1.2', gsub(old_IDs[1], new_IDs[1], DATA$ID_FloraAlpina))

### Put RegEx in a cycle
nrow(Subspecies_selection_table)
### Initialize objects
i = 1
ids <- DATA$ID_FloraAlpina
while (i <= length(old_IDs)) {
  ids <- gsub(old_IDs[i], new_IDs[i], ids)
  i = i+1
}
cbind(as.character(DATA$ID_FloraAlpina), ids)
setdiff(DATA$ID_FloraAlpina, ids)

length(ids) == length(DATA$ID_FloraAlpina) # sanity check
DATA$ID_FloraAlpina <- ids # substitute modified IDs onto dataset



##### Merge with Flora Alpina original database #####
DATA <- merge(DATA[, c(1:10)], FloraAlpina, by = "ID_FloraAlpina", all.x = T)
### Check that missing values are only due to taxa that are not matchable to others in Flora Alpina
setdiff(DATA$ID_FloraAlpina, FloraAlpina$ID_FloraAlpina)
DATA[grep('124$', DATA$ID_FloraAlpina), 1:4] # Neophyte
DATA[grep('124.51$', DATA$ID_FloraAlpina), 1:4] # Neophyte
DATA[grep('124.88$', DATA$ID_FloraAlpina), 1:4] # Not in the Alps
DATA[grep('124.99$', DATA$ID_FloraAlpina), 1:4] # Hieracium taxa not in Flora Alpina

1+1+1+6 # number of taxa with missing values



##### Pick similar species to fill gaps #####
DATA[DATA$SpeciesName == "Hieracium_valdepilosum", 11:116] <- DATA[DATA$SpeciesName == "Hieracium_villosum", 11:116][1,]
DATA[DATA$SpeciesName == "Hieracium_glaucopsis", 11:116] <- DATA[DATA$SpeciesName == "Hieracium_villosum", 11:116][1,]
DATA[DATA$SpeciesName == "Hieracium_cydoniifolium", 11:116] <- DATA[DATA$SpeciesName == "Hieracium_villosum", 11:116][1,]
DATA[DATA$SpeciesName == "Hieracium_froelichianum", 11:116] <- FloraAlpina[grep('Hieracium bifidum', FloraAlpina$CompleteName), 2:107]
DATA[DATA$SpeciesName == "Hieracium_ramosissimums_subsp._lactucifolium", 11:116] <- DATA[DATA$SpeciesName == "Hieracium_amplexicaule", 11:116][1,]
DATA[DATA$SpeciesName == "Schlagintweitia_huteri_subsp._lantoscana", 11:116] <- DATA[DATA$SpeciesName == "Schlagintweitia_intybacea", 11:116][1,]
DATA[DATA$SpeciesName == "Aremisia_nitida", 11:116] <- DATA[DATA$SpeciesName == "Artemisia_glacialis", 11:116][1,] # doesn't have altitude in any case
DATA[DATA$SpeciesName == "Sonchus_tenerrimus", 11:116] <- DATA[DATA$SpeciesName == "Sonchus_oleraceus", 11:116][1,]
DATA[DATA$SpeciesName == "Calendula_tripterocarpa", 11:116] <- FloraAlpina[grep('Calendula arvensis', FloraAlpina$CompleteName), 2:107]



##### Factorization of variables ##### 
### Phytosociology 
DATA$Phytosociology <- gsub("\\(", "", DATA$Phytosociology)
DATA$Phytosociology <- gsub("\\)", "", DATA$Phytosociology)
DATA$Phytosociology <- gsub("\\(", "", DATA$Phytosociology)
DATA$Phytosociology <- gsub(" - ", "", DATA$Phytosociology)
DATA$Phytosociology <- gsub("\\[", "NEO_", DATA$Phytosociology)
DATA$Phytosociology <- gsub("\\]", "", DATA$Phytosociology)
DATA$Phytosociology

### Phenology
library(dplyr)
Phen <- DATA[, c(1:3, 25:36)]
Phen[,4:15] <- apply(Phen[,4:15], 2, function(y) as.numeric(gsub('x', '1', y)))
Phen$Tot.months <- apply(Phen[,4:15], 1, function(y) sum(y, na.rm=T))
Phen$Init.month <- apply(Phen[,4:15], 1, function (y) first(which(y == '1')))
Phen$End.month <- apply(Phen[,4:15], 1, function (y) last(which(y == '1')))

DATA$Tot.months <- Phen$Tot.months
DATA$Init.month <- Phen$Init.month
DATA$End.month <- Phen$End.month 

### pH and N
factorize_pH <- function(data, y) {
pH_bas <- sum(data[y, c("f1", "f2", "f3")])
pH_neu <- sum(data[y, c("f4", "f5", "f6")])
pH_aci <- sum(data[y, c("f7", "f8", "f9")])
 if (is.na(pH_aci) == T || is.na(pH_neu) == T || is.na(pH_bas) == T) {pH <- NA} else {
  if (pH_bas > pH_neu) {pH <- "bas"} else {
   if (pH_neu > pH_aci) {pH <- "neu"} else {
     if (pH_aci > pH_neu) {pH <- "aci"} else {
       if (pH_bas == pH_neu) {pH <- "neu/bas"} else {
         if (pH_neu == pH_aci) {pH <- "neu/aci"} else {pH <- "ERROR"}
         }
       }
     }
   }
 }
return(pH)
}

factorize_pH(DATA, 1)
factorize_pH(DATA, 2)
factorize_pH(DATA, 40)

pH <- NULL
i = 1
while (i<=nrow(DATA)) {
  pH[i] <- factorize_pH(DATA, i)
  i <- i+1
}

DATA$pH <- pH
# obj <- apply(DATA, 1, factorize_pH) # why don't you work... 

factorize_N <- function(data, y) {
  N_low <- sum(data[y, c("f1", "f4", "f7")])
  N_med <- sum(data[y, c("f5", "f5", "f8")])
  N_hig <- sum(data[y, c("f3", "f6", "f9")])
  if (is.na(N_hig) == T || is.na(N_med) == T || is.na(N_low) == T) {N <- NA} else {
    if (N_low > N_med) {N <- "low"} else {
      if (N_med > N_hig) {N <- "med"} else {
        if (N_hig > N_med) {N <- "hig"} else {
          if (N_low == N_med) {N <- "med/low"} else {
            if (N_med == N_hig) {N <- "med/hig"} else {N <- "ERROR"}
          }
        }
      }
    }
  }
  return(N)
}

factorize_N(DATA, 1)
factorize_N(DATA, 2)
factorize_N(DATA, 20)

N <- NULL
i = 1
while (i<=nrow(DATA)) {
  N[i] <- factorize_N(DATA, i)
  i <- i+1
}

# apply(DATA, 1, factorize_N) # ofc you don't work either... 
DATA$N <- N

### Water availability 

factorize_Water <- function(data, y) {
  W_veryDry <- data[y, "VeryDry"]
  W_Dry <- data[y, "Dry"]
  W_Average <- data[y, "Average"]
  W_Wet <- data[y, "Wet"]
  W_Aquatic <- data[y, "Aquatic"]
  if (is.na(W_veryDry) == T || is.na(W_Dry) == T || is.na(W_Average) == T || is.na(W_Wet) == T || is.na(W_Aquatic) == T) {W <- NA} else {
    if (W_veryDry > W_Dry) {W <- "veryDry"} else {
      if ((W_veryDry == W_Dry) && (sum(W_veryDry, W_Dry) != 0)) {W <- "veryDry/Dry"} else {
       if (W_Dry > W_Average) {W <- "Dry"} else {
         if ((W_Dry == W_Average) && (sum(W_Dry, W_Average) != 0)) {W <- "Dry/Average"} else {
           if (W_Average > W_Wet) {W <- "Average"} else {
             if ((W_Average == W_Wet) && (sum(W_Average, W_Wet) != 0)) {W <- "Average/Wet"} else {
               if (W_Wet > W_Aquatic) {W <- "Wet"} else {
                 if ((W_Wet == W_Aquatic) && (sum(W_Wet, W_Aquatic) != 0)) {W <- "Wet/Aquatic"} else {
                   if (W_aquatic > W_Wet) {W <- "Aquatic"} else {W <- "ERROR"}
                 }
               }
             }
           }
         }
       }
      }
    }
  }
  return(W)
}

W_veryDry <- DATA[9, "VeryDry"]
W_Dry <- DATA[9, "Dry"]
W_Average <- DATA[9, "Average"]
W_Wet <- DATA[9, "Wet"]
W_Aquatic <- DATA[9, "Aquatic"]

factorize_Water(DATA, 1)
factorize_Water(DATA, 2)
factorize_Water(DATA, 20)

W <- NULL
i = 1
while (i<=nrow(DATA)) {
  W[i] <- factorize_Water(DATA, i)
  i <- i+1
}

DATA$Water <- W

### Geographic occurrences 
sum(DATA[3, 42:96]=="+")

Occ <- NULL
i = 1
while (i<=nrow(DATA)) {
  Occ[i] <- sum(DATA[i, 42:96]=="+")
  i <- i+1
}
Occ
### the apply alternative:
DATA$Sect_Occ <- apply(DATA[, 42:96]=="+", 1, sum)
### sanity check:
Occ == apply(DATA[, 42:96]=="+", 1, sum)
rbind(Occ, apply(DATA[, 42:96]=="+", 1, sum))

### Odd VS Even ploidy levels
factorize_Ploidy <- function(y) {
  if ((is.na(y)) == T) {P <- NA} else {
    if (y == "2x") {P <- "Even"} else {
      if (y == "3x") {P <- "Odd"} else {
        if (y == "4x") {P <- "Even"} else {
          if (y == "5x") {P <- "Odd"} else {
            if (y == "6x") {P <- "Even"} else {
              if (y == "7x") {P <- "Odd"} else {
                if (y == "8x") {P <- "Even"} else {
                  if (y == "9x") {P <- "Odd"} else {
                    if (y == "10x") {P <- "Even"} else {
                      if (y == "11x") {P <- "Odd"} else {
                        if (y == "12x") {P <- "Even"} else {P <- "ERROR"}
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
 return(P)
}

DATA$PloidyEvenOdd <- sapply(DATA$Ploidy, factorize_Ploidy)
# DATA[which(DATA$PloidyEvenOdd == "ERROR"), "Ploidy"]

##### Calculating new variables #####
DATA[, "GS_per_chrm"] <- DATA$GS / DATA$Chr.number
range(DATA$GS_per_chrm)
mean(DATA$GS_per_chrm)
plot(DATA$GS_per_chrm)

DATA[, "1C"] <- DATA$GS / as.numeric(gsub('x', '', DATA$Ploidy))
range(DATA$'1C')
mean(DATA$'1C')
plot(DATA$'1C')

### Create a new variable with higher ploidy levels collapsed together
DATA$Ploidy_summ <- gsub('6x|8x|12x', 'Poly', DATA$Ploidy)
DATA$Ploidy_summ <- factor(DATA$Ploidy_summ, levels = c("2x", "3x", "4x", "Poly"))
table(DATA$Ploidy_summ)

### Correcting mistakes in spelling
levels(as.factor(DATA$Repr_mode))
DATA$Repr_mode <- gsub('Apomictic ', 'Apomictic', DATA$Repr_mode)
DATA$Repr_mode <- gsub('sexual', 'Sexual', DATA$Repr_mode)
levels(as.factor(DATA$Repr_mode))
DATA$Repr_mode <- factor(DATA$Repr_mode, levels = c("Sexual", "Apomictic"))

DATA$Ploidy <- factor(DATA$Ploidy, levels = c("2x", "3x", "4x", "6x", "8x", "12x"))

### Adding a level for "mixed" reproductive mode
i = 1
Repr_mode_summ <- factor(rep(1, length(unique(DATA$SpeciesName))))
levels(Repr_mode_summ) <- c("Sexual", "Mixed", "Apomictic")
Repr_mode_summ <- data.frame(Repr_mode_summ, "SpeciesName" = unique(DATA$SpeciesName))
while (i <= length(unique(DATA$SpeciesName))) {
  if (length(levels(droplevels(DATA[which(unique(DATA$SpeciesName)[i] == DATA$SpeciesName), "Repr_mode"]))) >= 2) {
    Repr_mode_summ$Repr_mode_summ[i] <- "Mixed"} else {
      Repr_mode_summ$Repr_mode_summ[i] <- as.character(first(DATA[which(unique(DATA$SpeciesName)[i] == DATA$SpeciesName), "Repr_mode"]))
    }
  i = i + 1
}
Repr_mode_summ

Repr_mode_summ <- Repr_mode_summ[order(Repr_mode_summ[, "SpeciesName"]), ]
Repr_mode_summ_ext <- merge(DATA, Repr_mode_summ, by = "SpeciesName", all.x = T)[c("SpeciesName", "Repr_mode_summ")]

# View(cbind(Repr_mode_summ_ext[order(Repr_mode_summ_ext$SpeciesName),], DATA[order(DATA$SpeciesName), c(3,5)]))
# View(cbind(Repr_mode_summ_ext, DATA[, c(3,5)]))

### Make sure both dataset are in the same order before pasting the modified Repr_mode
DATA <- DATA[order(DATA$SpeciesName),]

DATA$Repr_mode_summ <- Repr_mode_summ_ext$Repr_mode_summ
str(DATA)
colnames(DATA)

##### Calculate mean elevation from Flora Alpina data and Ozenda 1985 #####
### From Ozenda 1985: cutpoints between vegtation belts for the Alps
mean(c(600,800)) # collineen
mean(c(1300,1500)) # montagnarde
mean(c(2000,2200)) # subalpin
mean(c(2000,2200)) # alpin
mean(c(2700,2900)) # nival
### Mean elevation of vegetation belts
mean(c(0, 
       mean(c(600,800))
)) # collineen
mean(c(mean(c(600,800)),
       mean(c(1300,1500))
)) # montagnard
mean(c(mean(c(1300,1500)), 
       mean(c(2000,2200))
)) # subalpin
mean(c(mean(c(2000,2200)), 
       mean(c(2700,2900))
)) # alpin
mean(c(mean(c(2700,2900)), 
       4500
)) # nival

elevation_Ozenda <- DATA[, c("SpeciesName", "Collinaire", "Montane", "Subalpine", "Alpine", "Nival")]
nrow(elevation_Ozenda)
elevation_Ozenda <- elevation_Ozenda[!duplicated(elevation_Ozenda$SpeciesName), ]
nrow(elevation_Ozenda)
colnames(elevation_Ozenda)
elevation_Ozenda$sum <- rowSums(elevation_Ozenda[, 2:6])
elevation_Ozenda[, 2] <- elevation_Ozenda[, 2]*350 # collineen
elevation_Ozenda[, 3] <- elevation_Ozenda[, 3]*1050 # montagnard
elevation_Ozenda[, 4] <- elevation_Ozenda[, 4]*1750 # subalpin
elevation_Ozenda[, 5] <- elevation_Ozenda[, 5]*2450 # alpin
elevation_Ozenda[, 6] <- elevation_Ozenda[, 6]*3650 # nival

elevation_Ozenda$avg <- sapply(1:nrow(elevation_Ozenda), function(x) sum(elevation_Ozenda[x,2:6])/elevation_Ozenda[x,"sum"])

DATA <- merge(DATA, elevation_Ozenda[, c("SpeciesName", "avg")], by = "SpeciesName", all.x = T)
colnames(DATA)[129] <- "elevation_Ozenda"

write.csv(DATA, file = "DATA_elevation_Ozenda.csv")

### How many unique genera?
unique(vapply(strsplit(as.character(unique(DATA$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))



##### Summarizing data: all accessions, missing values #####
to_keep <- c("ID_FloraAlpina", "ID", "SpeciesName", "CompleteName", 
             "Repr_mode", "Repr_mode_summ", "GS", "1C", 
             "Ploidy", "Ploidy_summ", "PloidyEvenOdd", "Chr.number", "GS_per_chrm",  
             "Altitude", "elevation_Ozenda", "Longevity", "BiologicalForm", 
             "Endemic", "Indigenous", "Distribution", "Sect_Occ", 
             "Collinaire", "Montane", "Subalpine", "Alpine", "Nival", 
             "Init.month", "Tot.months", "End.month", 
             "Phytosociology", "Habitat", "Ca", "Ca.Si", "Si", "pH", "N", "Water", "w")

library(dplyr)
DATA_red <- DATA %>% 
  select(ID_FloraAlpina, ID, SpeciesName, CompleteName, 
         Repr_mode, Repr_mode_summ, GS, `1C`, 
         Ploidy, Ploidy_summ, PloidyEvenOdd, Chr.number, GS_per_chrm, 
         Altitude, elevation_Ozenda, Longevity, BiologicalForm, 
         Endemic, Indigenous, Distribution, Sect_Occ, 
         Collinaire, Montane, Subalpine, Alpine, Nival, 
         Init.month, Tot.months, End.month, 
         Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w) %>%
  group_by(ID_FloraAlpina, SpeciesName) %>%
  summarize_all(list(first))

str(DATA_red)

setdiff(DATA_red$SpeciesName, JanTree4$tip.label)
setdiff(JanTree4$tip.label, DATA_red$SpeciesName)
length(DATA_red$SpeciesName) == length(JanTree4$tip.label)
which(duplicated(DATA_red$SpeciesName))
which(duplicated(JanTree4$tip.label))

rownames(DATA_red) <- DATA_red$SpeciesName
geiger::name.check(JanTree4, DATA_red)

DATA_red <- DATA_red[match(DATA_red$SpeciesName, JanTree4$tip.label), ]
# class(DATA_red) <- "data.frame"

write.csv(DATA_red, file = "DATA_red.csv")



##### Summarizing data: all accessions, missing values, mean ##### 
library(dplyr) 
Temp1 <- DATA %>% 
  select(ID_FloraAlpina, SpeciesName, 
         GS, `1C`, Chr.number, GS_per_chrm, Altitude, elevation_Ozenda, Sect_Occ, 
         Init.month, Tot.months, End.month) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(GS, `1C`, Chr.number, GS_per_chrm, 
                    Altitude, elevation_Ozenda, Sect_Occ, Init.month, Tot.months, End.month), 
               list(mean), na.rm = T)

Temp2 <- DATA %>% 
  select(ID_FloraAlpina, ID, SpeciesName, CompleteName, 
         Repr_mode, Repr_mode_summ, Ploidy, Ploidy_summ, PloidyEvenOdd, 
         Longevity, BiologicalForm, 
         Endemic, Indigenous, Distribution, 
         Collinaire, Montane, Subalpine, Alpine, Nival, 
         Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(ID, CompleteName,
                    Repr_mode, Repr_mode_summ, Ploidy, Ploidy_summ, PloidyEvenOdd,
                    Longevity, BiologicalForm,
                    Endemic, Indigenous, Distribution, 
                    Collinaire, Montane, Subalpine, Alpine, Nival,
                    Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w),
               list(first))

DATA_mean_red <- merge(Temp1, Temp2, by = c("ID_FloraAlpina", "SpeciesName"), all.x = T)
colnames(DATA_mean_red)
nrow(DATA_mean_red)

DATA_mean_red <- DATA_mean_red[, to_keep] # reorder columns
colnames(DATA_mean_red)

DATA_mean_red[!complete.cases(DATA_mean_red[, c(1:2, 5, 13, 25)]), ] # Sigesbeckia is not in Flora Alpina

rownames(DATA_mean_red) <- DATA_mean_red$SpeciesName
geiger::name.check(JanTree4, DATA_mean_red)

write.csv(DATA_mean_red, file = "DATA_mean_red.csv")



##### Complete cases ##### 
DATA[!complete.cases(DATA[, to_keep]), ] # these are the 6 accessions that will be dropped
DATA_CC <- DATA[complete.cases(DATA[, to_keep]), ]
nrow(DATA) - nrow(DATA_CC) # 6 accessions dropped

##### Preparing tree for complete cases ##### 
# Sigesbeckia orientalis has no data in Flora Alpina, therefore needs to be dropped from the tree
JanTree4_CC <- ape::drop.tip(JanTree4, setdiff(JanTree4$tip.label, DATA_CC$SpeciesName))

plot(JanTree4_CC, cex = .5, no.margin = T)
is.rooted(JanTree4_CC)
is.ultrametric(JanTree4_CC)
is.binary(JanTree4_CC)
JanTree4_CC <- multi2di(JanTree4_CC, random = TRUE)
is.rooted(JanTree4_CC)
is.ultrametric(JanTree4_CC)
is.binary(JanTree4_CC)
JanTree4_CC$edge.length[JanTree4_CC$edge.length == 0] # are there branch lengths with value zero? 
JanTree4_CC <- compute.brlen(JanTree4_CC, method = "Grafen")
is.rooted(JanTree4_CC)
is.ultrametric(JanTree4_CC)
is.binary(JanTree4_CC)
JanTree4_CC$edge.length[JanTree4_CC$edge.length == 0]

ape::write.tree(JanTree4_CC, file = "JanTree4_CC.tre")



##### Summarizing data: complete cases only #####
library(dplyr)
DATA_CC_red <- DATA_CC %>% 
  select(ID_FloraAlpina, ID, SpeciesName, CompleteName, 
         Repr_mode, Repr_mode_summ, GS, `1C`, 
         Ploidy, Ploidy_summ, PloidyEvenOdd, Chr.number, GS_per_chrm, 
         Altitude, elevation_Ozenda, Longevity, BiologicalForm, 
         Endemic, Indigenous, Distribution, Sect_Occ, 
         Collinaire, Montane, Subalpine, Alpine, Nival, 
         Init.month, Tot.months, End.month, 
         Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w) %>%
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_all(list(first))

nrow(DATA_CC_red)

setdiff(DATA_CC_red$SpeciesName, JanTree4$tip.label)
setdiff(JanTree4$tip.label, DATA_CC_red$SpeciesName)
which(duplicated(DATA_CC_red$SpeciesName))
which(duplicated(JanTree4$tip.label))

rownames(DATA_CC_red) <- DATA_CC_red$SpeciesName
geiger::name.check(JanTree4_CC, DATA_CC_red)
length(DATA_CC_red$SpeciesName) == length(JanTree4_CC$tip.label)

DATA_CC_red <- DATA_CC_red[match(DATA_CC_red$SpeciesName, JanTree4_CC$tip.label), ]

colnames(DATA_CC_red)
DATA_CC_red[!complete.cases(DATA_CC_red[, c(1:2, 5, 13, 25)]), ] # no missing values

write.csv(DATA_CC_red, file = "DATA_CC_red.csv")



##### Summarizing data: complete cases, mean ##### 
Temp1 <- DATA_CC %>% 
  select(ID_FloraAlpina, SpeciesName, 
         GS, `1C`, Chr.number, GS_per_chrm, Altitude, elevation_Ozenda, Sect_Occ, 
         Init.month, Tot.months, End.month) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(GS, `1C`, Chr.number, GS_per_chrm, 
                    Altitude, elevation_Ozenda, Sect_Occ, Init.month, Tot.months, End.month), 
               list(mean), na.rm = T)

Temp2 <- DATA_CC %>% 
  select(ID_FloraAlpina, ID, SpeciesName, CompleteName, 
         Repr_mode, Repr_mode_summ, Ploidy, Ploidy_summ, PloidyEvenOdd, 
         Longevity, BiologicalForm, 
         Endemic, Indigenous, Distribution, 
         Collinaire, Montane, Subalpine, Alpine, Nival, 
         Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(ID, CompleteName,
                    Repr_mode, Repr_mode_summ, Ploidy, Ploidy_summ, PloidyEvenOdd,
                    Longevity, BiologicalForm,
                    Endemic, Indigenous, Distribution, 
                    Collinaire, Montane, Subalpine, Alpine, Nival,
                    Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w),
               list(first))

DATA_CC_mean_red <- merge(Temp1, Temp2, by = c("ID_FloraAlpina", "SpeciesName"), all.x = T)
colnames(DATA_CC_mean_red)
nrow(DATA_CC_mean_red)

DATA_CC_mean_red <- DATA_CC_mean_red[, to_keep] # reorder columns
colnames(DATA_CC_mean_red)

DATA_CC_mean_red[!complete.cases(DATA_CC_mean_red[, c(1:2, 5, 13, 25)]), ] # no missing values

rownames(DATA_CC_mean_red) <- DATA_CC_mean_red$SpeciesName
geiger::name.check(JanTree4_CC, DATA_CC_mean_red)

DATA_CC_mean_red <- DATA_CC_mean_red[match(JanTree4_CC$tip.label, DATA_CC_mean_red$SpeciesName), ]

write.csv(DATA_CC_mean_red, file = "DATA_CC_mean_red.csv")



##### Adding locality information to the data ##### 
AA_MasterSheet <- read.csv(file = "AlpineAsteraceae_MasterSheet.csv")

AA_MasterSheet$ID

write.csv(merge(DATA_red, AA_MasterSheet[, c(2, 9:12)], by = "ID", all.x = T), file = "TotalDATA_red.csv")

write.csv(merge(DATA, AA_MasterSheet[, c(2, 9:12)], by = "ID", all.x = T), file = "Total_DATA.csv")





##### STRICTLY ALPS DATASET: removing botanic gardens and other accessions ##### 
### Making a vector with IDs of accessions to drop

### Stuff from HB Lautaret
DATA[grep('Laut', DATA$ID), c(1:3, 8)]
NotWild <- as.character(DATA[grep('Laut', DATA$ID), c(1:3, 8)][, "ID"])
DATA[grep('210.', DATA$Altitude), c(1:3, 8)][-c(7, 10), ]
NotWild <- c(NotWild, as.character(DATA[grep('210.', DATA$Altitude), c(1:3, 8)][-c(7, 10), ][, "ID"]))
### Stuff from Rolland
DATA[grep('RD', DATA$ID), c(1:3, 8)]
NotWild <- c(NotWild, as.character(DATA[grep('RD', DATA$ID), c(1:3, 8)][, "ID"]))
### Stuff from Juri
DATA[grep('Vos', DATA$ID), c(1:3, 8)]
NotWild <- c(NotWild, as.character(DATA[grep('Vos', DATA$ID), c(1:3, 8)][, "ID"]))
### Stuff from Jaume
DATA[grep('JP', DATA$ID), c(1:3, 8)]
NotWild <- c(NotWild, as.character(DATA[grep('JP', DATA$ID), c(1:3, 8)][, "ID"]))
### Stuff from Teresa
DATA[grep('GR', DATA$ID), c(1:3, 8)]
NotWild <- c(NotWild, as.character(DATA[grep('GR', DATA$ID), c(1:3, 8)][, "ID"]))
### Stuff from Kew
DATA[grep('RBGK', DATA$ID), c(1:3, 8)]
NotWild <- c(NotWild, as.character(DATA[grep('RBGK', DATA$ID), c(1:3, 8)][, "ID"]))
### Stuff from Oriane (except the ones that are wild w/ altitude)
DATA[grep('OH', DATA$ID), c(1:3, 8)][,-c(13, # Lautaret wild
                                         16, # Ville-Vieille
                                         18, # Briançon
                                         19 # Col d'Izoard
                                         )] 
NotWild <- c(NotWild, as.character(DATA[grep('OH', DATA$ID), c(1:3, 8)][ ,-c(13, 16, 18, 19)][, "ID"]))
### Stuff outside of the Alps (using QGIS, I selected the features outside of the alpine arc)
OutsideAlps <- c("FR42","FR60","FR61","FR62","FR63","FR64","FR65","FR66","FR67","FR67b",
                  "FR68","FR69","FR70","FR71","FR72","FR73","FR74","FR75a","FR75b","FR75c",
                  "FR300","FRR1","FRR10","FRR11","FRR2","FRR3","FRR4","FRR6","FRR7","FRR8",
                  "FRR9","FR411","FR477","FR487","FR488","FR544","FR560","FR561","FR562",
                  "FR563","FR564","FR565","FR566","FR612","FR619","FR623","FR641","FR656",
                  "FR675","FR676","FR683","IT18","IT19","IT20","IT21","IT22","IT37","IT38",
                  "IT39","IT40","IT41","IT42","IT43","IT72","IT73","IT76","IT77","FR710",
                  "A1","A2","A3","A4","A30","A31","A32","A41","A42","A43","A44","A45","A46",
                  "CH159","IT87","IT88","IT99","MB2","MB7","MB8","MB9","MB10","MB11",
                  "MB12","MB13","MB15","MB16","MB18","MB19","MB20","MB21","MB22","MB23",
                  "MB24","MB25","MB26","MB27","MB28","MB29","MB30","MB31","MB32","MB33",
                  "MB34","MB35","MB36","MB37","MB38","MB39","MB43","MB48","MB49",
                  "MB50","MB51","MB52","MB53","MB54","MB55","MB56","MB57","MB58","MB90",
                  "MB91","MB92","MB95","MB101","MB105","MB106","MB107","MB108","MB109",
                  "MB110","MB111","MB112","MB113","MB114","MB115","MB116","MB118","MB119",
                  "MB120","MB121","MB122","MB123","MB124","MB125","MB126","MB127","MB128",
                  "MB129","MB131","MB132","MB133","MB134","OH476","OH488")

OutsideAlps <- c(OutsideAlps, gsub('FR', '', OutsideAlps[grep('FRR.', OutsideAlps)])) # misformatting of IDs with "FR R" fix
OutsideAlps <- unique(OutsideAlps)

NotWild <- c(NotWild, as.character(OutsideAlps))
NotWild <- unique(NotWild)

nrow(DATA)
 
### accessions outside of the Alps
DATA_NotStrictlyAlps <- DATA[DATA$ID %in% NotWild, ]
nrow(DATA_NotStrictlyAlps)
write.csv(DATA_NotStrictlyAlps[, c(1:3, 8, 10)], file = "DATA_NotStrictlyAlps.csv")

DATA_StrictlyAlps <- DATA[!DATA$ID %in% NotWild, ]
nrow(DATA_StrictlyAlps)
write.csv(DATA_StrictlyAlps[, c(1:3, 8, 10)], file = "DATA_StrictlyAlps.csv")

setdiff(as.character(unique(DATA$SpeciesName)), as.character(unique(DATA_StrictlyAlps$SpeciesName)))

# View(DATA_StrictlyAlps[, c("SpeciesName", "Init.month")])

is.na(DATA_StrictlyAlps$Altitude)
is.na(DATA_StrictlyAlps$Init.month)

### Double checking 
nrow(DATA_StrictlyAlps)
unique(DATA_StrictlyAlps$SpeciesName)
DATA_StrictlyAlps[is.na(DATA_StrictlyAlps$Init.month), ]
DATA_StrictlyAlps[is.na(DATA_StrictlyAlps$Altitude), ]
DATA_StrictlyAlps[is.na(DATA_StrictlyAlps$Repr_mode), ]
DATA_StrictlyAlps[is.na(DATA_StrictlyAlps$Repr_mode_summ), ]

colnames(DATA_StrictlyAlps)
DATA_StrictlyAlps[!complete.cases(DATA_StrictlyAlps[, c(3,5,6,8,118)]), ]
nrow(DATA_StrictlyAlps)
DATA_StrictlyAlps <- DATA_StrictlyAlps[complete.cases(DATA_StrictlyAlps[, c(3,5,6,8,118)]), ]
nrow(DATA_StrictlyAlps) # no missing values

### How many unique genera in the striclty alpne dataset? 
unique(vapply(strsplit(as.character(unique(DATA_StrictlyAlps$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))

##### Prepare the tree for strictly Alps ##### 
library(ape)
JanTree4_StrictlyAlps <- drop.tip(JanTree4, setdiff(JanTree4$tip.label, DATA_StrictlyAlps$SpeciesName))
plot(JanTree4_StrictlyAlps, cex = .25, no.margin = T)

setdiff(DATA_StrictlyAlps$SpeciesName, JanTree4_StrictlyAlps$tip.label)
setdiff(JanTree4_StrictlyAlps$tip.label, DATA_StrictlyAlps$SpeciesName)
# length(DATA_StrictlyAlps$SpeciesName) == length(JanTree4_StrictlyAlps$tip.label)
which(duplicated(DATA_StrictlyAlps$SpeciesName))
which(duplicated(JanTree4_StrictlyAlps$tip.label))

is.binary(JanTree4_StrictlyAlps)
is.binary(JanTree4_StrictlyAlps)
JanTree4_StrictlyAlps <- multi2di(JanTree4_StrictlyAlps, tol = 1)
is.binary(JanTree4_StrictlyAlps)
is.ultrametric(JanTree4_StrictlyAlps)
JanTree4_StrictlyAlps$edge.length[JanTree4_StrictlyAlps$edge.length == 0] # are there branch lengths with value zero
JanTree4_StrictlyAlps <- compute.brlen(JanTree4_StrictlyAlps, method = "Grafen")
is.binary(JanTree4_StrictlyAlps)
is.ultrametric(JanTree4_StrictlyAlps)
JanTree4_StrictlyAlps$edge.length[JanTree4_StrictlyAlps$edge.length == 0] 



##### Summarize strictly Alps data #####
library(dplyr)
DATA_StrictlyAlps_red <- DATA_StrictlyAlps %>%
  select(ID_FloraAlpina, ID, SpeciesName, CompleteName, 
         Repr_mode, Repr_mode_summ, GS, `1C`, 
         Ploidy, Ploidy_summ, PloidyEvenOdd, Chr.number, GS_per_chrm, 
         Altitude, elevation_Ozenda, Longevity, BiologicalForm, 
         Endemic, Indigenous, Distribution, Sect_Occ, 
         Collinaire, Montane, Subalpine, Alpine, Nival, 
         Init.month, Tot.months, End.month, 
         Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w) %>%
  group_by(ID_FloraAlpina, SpeciesName) %>%
  summarize_all(list(first))

colnames(DATA_StrictlyAlps_red)
nrow(DATA_StrictlyAlps_red)

# DATA_StrictlyAlps_red <- DATA_StrictlyAlps_red[order(DATA_StrictlyAlps_red$SpeciesName),]
nrow(DATA_StrictlyAlps_red)
colnames(DATA_StrictlyAlps_red)
DATA_StrictlyAlps_red[!complete.cases(DATA_StrictlyAlps_red[, c(2,5:6,9:11,14,28)]), ]
# View(DATA_StrictlyAlps_red[!complete.cases(DATA_StrictlyAlps_red[, c(2,4,6:8,19:20)]), ])

setdiff(DATA_StrictlyAlps_red$SpeciesName, JanTree4$tip.label)
setdiff(JanTree4$tip.label, DATA_StrictlyAlps_red$SpeciesName) # these taxa need to be dropped from the tree

DATA_StrictlyAlps_red <- DATA_StrictlyAlps_red[order(match(DATA_StrictlyAlps_red$SpeciesName, JanTree4_StrictlyAlps$tip.label)), ]

# View(cbind(as.character(DATA_StrictlyAlps_red$SpeciesName), JanTree4_StrictlyAlps$tip.label))

rownames(DATA_StrictlyAlps_red) <- DATA_StrictlyAlps_red$SpeciesName

geiger::name.check(JanTree4_StrictlyAlps, DATA_StrictlyAlps_red)

# write.csv(DATA_CC_StrictlyAlps, file = "DATA_CC_StrictlyAlps.csv")
write.csv(DATA_StrictlyAlps_red, file = "DATA_StrictlyAlps_red.csv")
write.tree(JanTree4_StrictlyAlps, file = "JanTree4_StrictlyAlps.tre")

##### Summarize strictly Alps data, using mean values ##### 
Temp1 <- DATA_StrictlyAlps %>% 
  select(ID_FloraAlpina, SpeciesName, 
         GS, `1C`, Chr.number, GS_per_chrm, Altitude, elevation_Ozenda, Sect_Occ, 
         Init.month, Tot.months, End.month) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(GS, `1C`, Chr.number, GS_per_chrm, 
                    Altitude, elevation_Ozenda, Sect_Occ, Init.month, Tot.months, End.month), 
               list(mean), na.rm = T)

Temp2 <- DATA_StrictlyAlps %>% 
  select(ID_FloraAlpina, ID, SpeciesName, CompleteName, 
         Repr_mode, Repr_mode_summ, Ploidy, Ploidy_summ, PloidyEvenOdd, 
         Longevity, BiologicalForm, 
         Endemic, Indigenous, Distribution, 
         Collinaire, Montane, Subalpine, Alpine, Nival, 
         Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w) %>% 
  group_by(ID_FloraAlpina, SpeciesName) %>% 
  summarize_at(vars(ID, CompleteName,
                    Repr_mode, Repr_mode_summ, Ploidy, Ploidy_summ, PloidyEvenOdd,
                    Longevity, BiologicalForm,
                    Endemic, Indigenous, Distribution, 
                    Collinaire, Montane, Subalpine, Alpine, Nival,
                    Phytosociology, Habitat, Ca, Ca.Si, Si, pH, N, Water, w),
               list(first))

DATA_StrictlyAlps_mean_red <- merge(Temp1, Temp2, by = c("ID_FloraAlpina", "SpeciesName"), all.x = T)
colnames(DATA_StrictlyAlps_mean_red)
nrow(DATA_StrictlyAlps_mean_red)

DATA_StrictlyAlps_mean_red <- DATA_StrictlyAlps_mean_red[, to_keep] # reoder columns

rownames(DATA_StrictlyAlps_mean_red) <- DATA_StrictlyAlps_mean_red$SpeciesName

DATA_StrictlyAlps_mean_red[!complete.cases(DATA_StrictlyAlps_mean_red[, c(2, 5, 27)]), ]
DATA_StrictlyAlps_mean_red[!complete.cases(DATA_StrictlyAlps_mean_red[, c(2, 5, 27, 7)]), ]

geiger::name.check(JanTree4_StrictlyAlps, DATA_StrictlyAlps_mean_red)
JanTree4_StrictlyAlps$tip.label == as.character(DATA_StrictlyAlps_mean_red$SpeciesName)
DATA_StrictlyAlps_mean_red <- DATA_StrictlyAlps_mean_red[c(match(DATA_StrictlyAlps_mean_red$SpeciesName, JanTree4_StrictlyAlps$tip.label)), ]
geiger::name.check(JanTree4_StrictlyAlps, DATA_StrictlyAlps_mean_red)
JanTree4_StrictlyAlps$tip.label == as.character(DATA_StrictlyAlps_mean_red$SpeciesName)

DATA_StrictlyAlps_mean_red[!complete.cases(DATA_StrictlyAlps_mean_red), ]

write.csv(DATA_StrictlyAlps_mean_red, file = "DATA_StrictlyAlps_mean_red.csv")



##### END #####
