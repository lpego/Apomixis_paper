sink(file = "Apomixis_MCMCglmm_chains_StrictlyAlpine_mean.txt")

##### HELLO! This is the beginning of the Apomixis_MCMCglmm_chains_reduced.R script! ##### 
path <- getwd()
print(getwd())

##### Load some data #####
data_red <- read.csv(file = "DATA_StrictlyAlps_mean_red.csv", header = T)[, -1]
data_red$Ploidy_summ <- gsub('6x|8x|12x', 'Poly', data_red$Ploidy)
colnames(data_red)[2] <- "animal"
colnames(data_red)
# data_red <- cbind(data_red[, -c(5, 7:8, 11:13, 14:32)], 
#                   sapply(data_red[, c(5, 7:8, 11:13, 14:32)], as.factor)
#                   )
# colnames(data_red[, c(7:9, 1:6, 10:31)]) # reorder columns

### Make sure factor levels are correct
data_red$Repr_mode_summ <- factor(data_red$Repr_mode_summ, levels = c("Sexual", "Mixed", "Apomictic"))
data_red$Repr_mode_summ # 3 levels
data_red$Repr_mode <- factor(gsub('Mixed', 'Apomictic', data_red$Repr_mode), levels = c("Sexual", "Apomictic"))
data_red$Repr_mode # 2 levels
data_red$Ploidy <- factor(data_red$Ploidy, levels = c("2x", "3x", "4x", "6x", "8x", "12x"))
table(data_red$Ploidy)
data_red$Ploidy_summ <- gsub("6x|8x|12x", "Poly", data_red$Ploidy)
data_red$Ploidy_summ <- factor(data_red$Ploidy_summ, levels = c("2x", "3x", "4x", "Poly"))
table(data_red$Ploidy_summ)

nrow(data_red)
nrow(data_red[complete.cases(data_red),])

##### Load the tree #####
library(ape)
JanTree4_red <- read.tree(file = "JanTree4_StrictlyAlps.tre")

setdiff(JanTree4_red$tip.label, data_red$animal)
setdiff(data_red$animal, JanTree4_red$tip.label)
# JanTree4_red <- drop.tip(JanTree4_red, setdiff(JanTree4_red$tip.label, data_red$animal))

print("is.binary")
is.binary(JanTree4_red)
# JanTree4_red <- multi2di(JanTree4_red, tol = 1)
# is.binary(JanTree4_red)
print("is.ultrametric")
is.ultrametric(JanTree4_red)
print("zero branch length?")
JanTree4_red$edge.length[JanTree4_red$edge.length == 0] # are there branch lengths with value zero
# JanTree4_red <- compute.brlen(JanTree4_red, method = "Grafen")
# is.binary(JanTree4_red)
# is.ultrametric(JanTree4_red)
# JanTrece4_red$edge.length[JanTree4_red$edge.length == 0] # are there branch lengths with value zero
print("Tree.png")
png('Tree.png', width = 20, height = 30, units = 'cm', res=300)
plot(JanTree4_red, cex = .3)
dev.off()

cbind(JanTree4_red$tip.label, as.character(data_red$animal))

##### MCMC chains #####
library(MCMCglmm)

### Phylogeny black magic...
invJanTree4_red <- inverseA(JanTree4_red, nodes = "ALL", scale = TRUE) # ALL is better for large phylogenies
invJanTree4_red_tips <- inverseA(JanTree4_red, nodes = "TIPS", scale = TRUE)

##### No phylogeny #####
# ### No phylogeny included, response with 3 levels
prior_V1fix1 <- list(R = list(V = 1, fix = 1))
prior_V1fix1

set.seed(111)
# mTre1_noPhy <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy_summ + Init.month,
#                         verbose = T, data = data_red,
#                         family = "threshold",
#                         trunc = T,
#                         prior = prior_V1fix1,
#                         nitt = 10^6, thin = 500, burnin = 25000)
# summary(mTre1_noPhy)
# print("DiagPlots_mTre1_noPhy_3lvl%03d.png")
# png('DiagPlots_mTre1_noPhy_3lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(mTre1_noPhy, ask = F)
# 
# dev.off()

### No phylogeny included, response with 2 levels
mTre1.1_noPhy <- MCMCglmm(Repr_mode ~ Altitude + Ploidy_summ + Init.month,
                          verbose = T, data = data_red,
                          family = "threshold",
                          trunc = T,
                          prior = prior_V1fix1,
                          nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre1.1_noPhy)
print("DiagPlots_mTre1_noPhy_2lvl%03d.png")
png('DiagPlots_mTre1_noPhy_2lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
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
# mTre1 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy_summ + Init.month,
#                   ginverse = list(animal = invJanTree4_red_tips$Ainv),
#                   random = ~ animal,
#                   verbose = T,
#                   data = data_red,
#                   family = "threshold",
#                   trunc = T,
#                   prior = prior_nu1000_1,
#                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(mTre1)
# print("DiagPlots_mTre1.1_3lvl%03d.png")
# png('DiagPlots_mTre1.1_3lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   plot(mTre1, ask = F)
# 
# dev.off()
# 
# heidel.diag(mTre1$VCV)
# heidel.diag(mTre1$Sol)
# print("Geweke_mTre1_3lvl%03d.png")
# png('Geweke_mTre1_3lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   geweke.plot(mTre1$Sol, ask = F)
# 
# dev.off()
# autocorr.diag(mTre1$Sol)

### Repr_mode with only 2 levels
levels(data_red$Repr_mode) # 2 levels
mTre1.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy_summ + Init.month,
                    ginverse = list(animal = invJanTree4_red_tips$Ainv),
                    random = ~ animal,
                    verbose = T,
                    data = data_red,
                    family = "threshold",
                    trunc = T,
                    prior = prior_nu1000_1,
                    nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre1.1)
print("DiagPlots_mTre1.1_2lvl%03d.png")
png('DiagPlots_mTre1.1_2lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre1.1, ask = F)

dev.off()

heidel.diag(mTre1.1$VCV)
heidel.diag(mTre1.1$Sol)
print("Geweke_mTre1.1_2lvl.png")
png('Geweke_mTre1.1_2lvl.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(mTre1.1$Sol, ask = F)

dev.off()
autocorr.diag(mTre1.1$Sol)

##### Second batch: set.seed(534) #####
print("Second batch: set.seed(534)")
set.seed(534)
# ### Repr_mode with only 3 levels
# mTre2 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy_summ + Init.month,
#                   ginverse = list(animal = invJanTree4_red_tips$Ainv),
#                   random = ~ animal,
#                   verbose = T,
#                   data = data_red,
#                   family = "threshold",
#                   trunc = T,
#                   prior = prior_nu1000_1,
#                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(mTre2)
# print("DiagPlots_mTre2.1_3lvl%03d.png")
# png('DiagPlots_mTre2.1_3lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(mTre2, ask = F)
# 
# dev.off()
# 
# heidel.diag(mTre2$VCV)
# heidel.diag(mTre2$Sol)
# print("Geweke_mTre2_3lvl%03d.png")
# png('Geweke_mTre2_3lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   geweke.plot(mTre2$Sol, ask = F)
# 
# dev.off()
# autocorr.diag(mTre2$Sol)

### Repr_mode with only 2 levels
mTre2.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy_summ + Init.month,
                    ginverse = list(animal = invJanTree4_red_tips$Ainv),
                    random = ~ animal,
                    verbose = T,
                    data = data_red,
                    family = "threshold",
                    trunc = T,
                    prior = prior_nu1000_1,
                    nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre2.1)
print("DiagPlots_mTre2.1_2lvl%03d.png")
png('DiagPlots_mTre2.1_2lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre2.1, ask = F)

dev.off()

heidel.diag(mTre2.1$VCV)
heidel.diag(mTre2.1$Sol)
print("Geweke_mTre2.1_2lvl.png")
png('Geweke_mTre2.1_2lvl.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(mTre2.1$Sol, ask = F)

dev.off()
autocorr.diag(mTre2.1$Sol)

##### Third batch: set.seed(386) #####
print("Third batch: set.seed(386)")
set.seed(386)
# ### Repr_mode with only 3 levels
# mTre3 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy_summ + Init.month,
#                   ginverse = list(animal = invJanTree4_red_tips$Ainv),
#                   random = ~ animal,
#                   verbose = T,
#                   data = data_red,
#                   family = "threshold",
#                   trunc = T,
#                   prior = prior_nu1000_1,
#                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(mTre3)
# print("DiagPlots_mTre3.1_3lvl%03d.png")
# png('DiagPlots_mTre3.1_3lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   plot(mTre3, ask = F)
# 
# dev.off()
# 
# heidel.diag(mTre3$VCV)
# heidel.diag(mTre3$Sol)
# print("Geweke_mTre3_3lvl%03d.png")
# png('Geweke_mTre3_3lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
#   geweke.plot(mTre3$Sol, ask = F)
# 
# dev.off()
# autocorr.diag(mTre3$Sol)

### Repr_mode with only 2 levels
mTre3.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy_summ + Init.month,
                    ginverse = list(animal = invJanTree4_red_tips$Ainv),
                    random = ~ animal,
                    verbose = T,
                    data = data_red,
                    family = "threshold",
                    trunc = T,
                    prior = prior_nu1000_1,
                    nitt = 10^6, thin = 500, burnin = 25000)
summary(mTre3.1)
print("DiagPlots_mTre3.1_2lvl%03d.png")
png('DiagPlots_mTre3.1_2lvl%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(mTre3.1, ask = F)

dev.off()

heidel.diag(mTre3.1$VCV)
heidel.diag(mTre3.1$Sol)
print("Geweke_mTre3.1_2lvl.png")
png('Geweke_mTre3.1_2lvl.png', width = 15, height = 15, units = 'cm', res = 300)
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
