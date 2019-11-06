sink(file = "Apomixis_MCMCglmm_chains_summ_mean_Ozenda_Echo.txt")

##### HELLO! This is the beginning of the Apomixis_MCMCglmm_chains_reduced.R script! ##### 
path <- getwd()
print(getwd())

##### Load some data #####
data_red <- read.csv(file = "DATA_CC_mean_red.csv", header = T)
colnames(data_red)
colnames(data_red)[3] <- "animal"

### Make sure factor levels are correct
data_red$Repr_mode_summ <- factor(data_red$Repr_mode_summ, levels = c("Sexual", "Mixed", "Apomictic"))
data_red$Repr_mode_summ # 3 levels
data_red$Repr_mode <- gsub('Mixed', 'Apomictic', data_red$Repr_mode_summ)
data_red$Repr_mode <- factor(data_red$Repr_mode, levels = c("Sexual", "Apomictic"))
data_red$Repr_mode # 2 levels
data_red$Ploidy <- factor(data_red$Ploidy, levels = c("2x", "3x", "4x", "6x", "8x", "12x"))
data_red$Ploidy
table(data_red$Ploidy)
data_red$Ploidy_summ <- gsub('6x|8x|12x', 'Poly', data_red$Ploidy)
data_red$Ploidy_summ <- factor(data_red$Ploidy_summ, levels = c("2x", "3x", "4x", "Poly"))
table(data_red$Ploidy_summ)

str(data_red)
nrow(data_red)
ncol(data_red)
colnames(data_red)

data_red[!complete.cases(data_red), c(3,5,7,8,20)]

##### Grab data from similar species to fill missing values #####
### Hieracium glaucopsis and H. valdepilosum are in sect Villosa like H. villosum
data_red[data_red$animal == "Hieracium_valdepilosum", 11:31] <- data_red[data_red$animal == "Hieracium_villosum", 11:31]
data_red[data_red$animal == "Hieracium_glaucopsis", 11:31] <- data_red[data_red$animal == "Hieracium_villosum", 11:31]
data_red[data_red$animal == "Hieracium_cydoniifolium", 11:31] <- data_red[data_red$animal == "Hieracium_villosum", 11:31]
data_red[data_red$animal == "Hieracium_ramosissimums_subsp._lactucifolium", 11:31] <- data_red[data_red$animal == "Hieracium_amplexicaule", 11:31]
data_red[data_red$animal == "Schlagintweitia_huteri_subsp._lantoscana", 11:31] <- data_red[data_red$animal == "Schlagintweitia_intybacea", 11:31]
# data_red[data_red$animal == "Aremisia_nitida", 11:31] <- data_red[data_red$animal == "Artemisia_glacialis", 11:31] # doesn't have altitude in any case
data_red[data_red$animal == "Sonchus_tenerrimus", 11:31] <- data_red[data_red$animal == "Sonchus_oleraceus", 11:31]
data_red[data_red$animal == "Calendula_tripterocarpa", 11:31] <- data_red[data_red$animal == "Calendula_arvensis", 11:31]

data_red[!complete.cases(data_red), c(3,5,7,8,20)]

### Add elevation data calculated from Ozenda (1985)
elevation_Ozenda <- data_red[, c(3, 15:19)]
nrow(elevation_Ozenda)
colnames(elevation_Ozenda)
elevation_Ozenda$sum <- rowSums(elevation_Ozenda[, 2:6])
elevation_Ozenda[, 2] <- elevation_Ozenda[, 2]*350 # collineen
elevation_Ozenda[, 3] <- elevation_Ozenda[, 3]*1050 # montagnard
elevation_Ozenda[, 4] <- elevation_Ozenda[, 4]*1750 # subalpin
elevation_Ozenda[, 5] <- elevation_Ozenda[, 5]*2450 # alpin
elevation_Ozenda[, 6] <- elevation_Ozenda[, 6]*3650 # nival

elevation_Ozenda$avg <- sapply(1:nrow(elevation_Ozenda), function(x) sum(elevation_Ozenda[x,2:6])/elevation_Ozenda[x,"sum"])

data_red$animal == elevation_Ozenda$animal # are they in the same order still? 
data_red$elevation <- elevation_Ozenda$avg

colnames(data_red[, c(3,5,7:8,20:21,32,33,34)]) # columns necessary for analysis
data_red[!complete.cases(data_red[,c(3,5,7:8,20:21,32,33,34)]), ]
data_red <- data_red[complete.cases(data_red[,c(3,5,7:8,20:21,32,33,34)]), ]
complete.cases(data_red[,c(3,5,7:8,20:21,32,33,34)])
nrow(data_red)



##### Load the tree #####
library(ape)
JanTree4_red <- read.tree(file = "JanTree4.tre")

setdiff(JanTree4_red$tip.label, data_red$animal)
setdiff(data_red$animal, JanTree4_red$tip.label)

JanTree4_red <- drop.tip(JanTree4_red, setdiff(JanTree4_red$tip.label, data_red$animal))
setdiff(JanTree4_red$tip.label, data_red$animal)
setdiff(data_red$animal, JanTree4_red$tip.label)

print("is.binary")
is.binary(JanTree4_red)
JanTree4_red <- multi2di(JanTree4_red, tol = 1)
is.binary(JanTree4_red)
print("is.ultrametric")
is.ultrametric(JanTree4_red)
print("zero branch length?")
JanTree4_red$edge.length[JanTree4_red$edge.length == 0] # are there branch lengths with value zero
JanTree4_red <- compute.brlen(JanTree4_red, method = "Grafen")
is.binary(JanTree4_red)
is.ultrametric(JanTree4_red)
JanTree4_red$edge.length[JanTree4_red$edge.length == 0] # are there branch lengths with value zero

print("Tree.png")
png('Tree.png', width = 20, height = 30, units = 'cm', res=300)
plot(JanTree4_red, cex = .3)
dev.off()



##### MCMC chains #####
library(MCMCglmm)

### Phylogeny black magic...
invJanTree4_red <- inverseA(JanTree4_red, nodes = "ALL", scale = TRUE) # ALL is better for large phylogenies
invJanTree4_red_tips <- inverseA(JanTree4_red, nodes = "TIPS", scale = TRUE)

### No phylogeny included, response with 3 levels
prior_V1fix1 <- list(R = list(V = 1, fix = 1))
prior_V1fix1

set.seed(111)
# mTre1_noPhy <- MCMCglmm(Repr_mode_summ ~ elevation + Ploidy_summ + Init.month,
#                         verbose = T, data = data_red,
#                         family = "threshold",
#                         trunc = T,
#                         prior = prior_V1fix1,
#                         nitt = 10^6, thin = 500, burnin = 25000)
# summary(mTre1_noPhy)
# print("DiagPlots_mTre1_noPhy%03d.png")
# png('DiagPlots_mTre1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(mTre1_noPhy, ask = F)
# 
# dev.off()

### No phylogeny included, response with 2 levels
mTre1.1_noPhy <- MCMCglmm(Repr_mode ~ elevation + Ploidy_summ + Init.month,
                          verbose = T, data = data_red,
                          family = "threshold",
                          trunc = T,
                          prior = prior_V1fix1,
                          nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre1.1_noPhy)
print("DiagPlots_mTre1_noPhy%03d.png")
png('DiagPlots_mTre1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre1.1_noPhy, ask = F)

dev.off()

##### First batch: set.seed(111) #####
print("First batch: set.seed(111)")
prior_nu1000_1 <- list(R = list(V = 1, fix = 1),
                       G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1))
)
prior_nu1000_1

set.seed(111)
# ### Repr_mode with only 3 levels
# levels(data_red$Repr_mode_summ) # 3 levels
# mTre1 <- MCMCglmm(Repr_mode_summ ~ elevation + Ploidy_summ + Init.month,
#                   ginverse = list(animal = invJanTree4_red_tips$Ainv),
#                   random = ~ animal,
#                   verbose = T,
#                   data = data_red,
#                   family = "threshold",
#                   trunc = T,
#                   prior = prior_nu1000_1,
#                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(mTre1)
# print("DiagPlots_mTre1.1%03d.png")
# png('DiagPlots_mTre1.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   plot(mTre1, ask = F)
# 
# dev.off()
# 
# heidel.diag(mTre1$VCV)
# heidel.diag(mTre1$Sol)
# print("Geweke_mTre1%03d.png")
# png('Geweke_mTre1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   geweke.plot(mTre1$Sol, ask = F)
# 
# dev.off()
# autocorr.diag(mTre1$Sol)

### Repr_mode with only 2 levels
levels(data_red$Repr_mode) # 2 levels
mTre1.1 <- MCMCglmm(Repr_mode ~ elevation + Ploidy_summ + Init.month,
                    ginverse = list(animal = invJanTree4_red_tips$Ainv),
                    random = ~ animal,
                    verbose = T,
                    data = data_red,
                    family = "threshold",
                    trunc = T,
                    prior = prior_nu1000_1,
                    nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre1.1)
print("DiagPlots_mTre1.1%03d.png")
png('DiagPlots_mTre1.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre1.1, ask = F)

dev.off()

heidel.diag(mTre1.1$VCV)
heidel.diag(mTre1.1$Sol)
print("Geweke_mTre1.1.png")
png('Geweke_mTre1.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(mTre1.1$Sol, ask = F)

dev.off()
autocorr.diag(mTre1.1$Sol)

##### Second batch: set.seed(534) #####
print("Second batch: set.seed(534)")
set.seed(534)
# ### Repr_mode with only 3 levels
# mTre2 <- MCMCglmm(Repr_mode_summ ~ elevation + Ploidy_summ + Init.month,
#                   ginverse = list(animal = invJanTree4_red_tips$Ainv),
#                   random = ~ animal,
#                   verbose = T,
#                   data = data_red,
#                   family = "threshold",
#                   trunc = T,
#                   prior = prior_nu1000_1,
#                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(mTre2)
# print("DiagPlots_mTre2.1%03d.png")
# png('DiagPlots_mTre2.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(mTre2, ask = F)
# 
# dev.off()
# 
# heidel.diag(mTre2$VCV)
# heidel.diag(mTre2$Sol)
# print("Geweke_mTre2%03d.png")
# png('Geweke_mTre2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   geweke.plot(mTre2$Sol, ask = F)
# 
# dev.off()
# autocorr.diag(mTre2$Sol)

### Repr_mode with only 2 levels
mTre2.1 <- MCMCglmm(Repr_mode ~ elevation + Ploidy_summ + Init.month,
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
# ### Repr_mode with only 3 levels
# mTre3 <- MCMCglmm(Repr_mode_summ ~ elevation + Ploidy_summ + Init.month,
#                   ginverse = list(animal = invJanTree4_red_tips$Ainv),
#                   random = ~ animal,
#                   verbose = T,
#                   data = data_red,
#                   family = "threshold",
#                   trunc = T,
#                   prior = prior_nu1000_1,
#                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(mTre3)
# print("DiagPlots_mTre3.1%03d.png")
# png('DiagPlots_mTre3.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   plot(mTre3, ask = F)
# 
# dev.off()
# 
# heidel.diag(mTre3$VCV)
# heidel.diag(mTre3$Sol)
# print("Geweke_mTre3%03d.png")
# png('Geweke_mTre3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   geweke.plot(mTre3$Sol, ask = F)
# 
# dev.off()
# autocorr.diag(mTre3$Sol)

### Repr_mode with only 2 levels
mTre3.1 <- MCMCglmm(Repr_mode ~ elevation + Ploidy_summ + Init.month,
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
# chainListTre1_Sol <- mcmc.list(mTre1$Sol, mTre2$Sol, mTre3$Sol)
# chainListTre1_VCV <- mcmc.list(mTre1$VCV, mTre2$VCV, mTre3$VCV)

chainListTre2_Sol <- mcmc.list(mTre1.1$Sol, mTre2.1$Sol, mTre3.1$Sol)
chainListTre2_VCV <- mcmc.list(mTre1.1$VCV, mTre2.1$VCV, mTre3.1$VCV)

### Gelman rubin diagnostic: should be close to 1
# gelman.diag(chainListTre1_Sol)
# gelman.diag(chainListTre1_Sol)

gelman.diag(chainListTre2_Sol)
gelman.diag(chainListTre2_Sol)

##### GOODBYE! This is the end of the Apomixis_MCMCglmm_chains.R script! ##### 

sink() 
