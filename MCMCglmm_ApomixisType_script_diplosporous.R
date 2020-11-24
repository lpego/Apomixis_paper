sink(file = "MCMCglmm_ApomixisType_script_diplosporous.txt")

##### HELLO! This is the beginning of the MCMCglmm_ApomixisType_script.R ! ##### 
setwd("C:/Users/lucap/Documents/R/Apomixis_MCMCglmm_ApomixisType")
print(getwd())
start_time_overall <- Sys.time()

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### THIS IS THE SCRIPT FOR THE EXTENDED DATASET, DIPLOSPOROUS ONLY  ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##### Load data #####
Online_v7_mean_ApomixisType <- read.csv(file = "Online_v7_mean_ApomixisType.csv", header = T)[,-1]
str(Online_v7_mean_ApomixisType)
Online_v7_mean_ApomixisType$Repr_mode <- factor(Online_v7_mean_ApomixisType$Repr_mode, levels = c("Sexual", "Apomictic"))
Online_v7_mean_ApomixisType$Repr_mode_summ <- factor(Online_v7_mean_ApomixisType$Repr_mode_summ, levels = c("Sexual", "Mixed", "Apomictic"))
Online_v7_mean_ApomixisType$Ploidy_summ <- factor(Online_v7_mean_ApomixisType$Embryo.Ploidy.summ, levels = c("2x", "3x", "4x", "Poly"))
Online_v7_mean_ApomixisType$Apomixis_type <- factor(as.character(Online_v7_mean_ApomixisType$Apomixis_type), levels = c("Aposporous", "Diplosporous", "Apo-diplosporous", "Uncertain", "Sexual"))
Online_v7_mean_ApomixisType[!complete.cases(Online_v7_mean_ApomixisType), ] # no missing values
nrow(Online_v7_mean_ApomixisType)

colnames(Online_v7_mean_ApomixisType)[2] <- "animal" # this is needed because of a MCMCglmm quirkiness

##### Load the tree #####
library(ape)
JanTree4_CC_online <- read.tree(file = "JanTree4_CC_online.tre")

setdiff(JanTree4_CC_online$tip.label, Online_v7_mean_ApomixisType$animal)
setdiff(Online_v7_mean_ApomixisType$animal, JanTree4_CC_online$tip.label)

print("is rooted?")
is.rooted(JanTree4_CC_online)
print("is binary?")
is.binary(JanTree4_CC_online)
print("is ultrametric?")
is.ultrametric(JanTree4_CC_online)
print("are branches of length zero?")
JanTree4_CC_online$edge.length[JanTree4_CC_online$edge.length == 0] 
print("JanTree4_CC_online.png")
# png('JanTree4_CC_online.png', width = 20, height = 30, units = 'cm', res=300)
# plot(JanTree4_CC_online, cex = .3)
# dev.off()



##### ~ ####



# ##### Splitting the database: Aposporous ##### 
# library(ape)
# library(phytools)
# setdiff(JanTree4_CC_online$tip.label, unique(Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Aposporous", "Sexual"), "animal"]))
# drop.tip(JanTree4_CC_online, 
#          setdiff(JanTree4_CC_online$tip.label, 
#                  unique(Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Aposporous", "Sexual"), "animal"])
#          )
# )
# 
# facetoface_apo <- cophylo(JanTree4_CC_online, 
#                           drop.tip(JanTree4_CC_online, 
#                                    setdiff(JanTree4_CC_online$tip.label, 
#                                            unique(Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Aposporous", "Sexual"), "animal"])
#                                    )
#                           )
# )
# plot(facetoface_apo, fsize = .3)
# ###
# Online_v7_mean_ApomixisType_aposporous <- Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Aposporous", "Sexual"), ]
# str(Online_v7_mean_ApomixisType_aposporous)
# 
# JanTree4_CC_online_aposporous <- drop.tip(JanTree4_CC_online, 
#                                           setdiff(JanTree4_CC_online$tip.label, 
#                                                   unique(Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Aposporous", "Sexual"), "animal"])))
# JanTree4_CC_online_aposporous
# 
# ##### MCMC chains #####
# library(MCMCglmm)
# 
# ### Phylogeny black magic
# invJanTree_CC_aposporous <- inverseA(JanTree4_CC_online_aposporous, nodes = "ALL", scale = TRUE)
# invJanTree_CC_tips_aposporous <- inverseA(JanTree4_CC_online_aposporous, nodes = "TIPS", scale = TRUE)
# 
# ### No phylogeny included, response with 3 levels
# prior_V1fix1 <- list(R = list(V = 1, fix = 1))
# prior_V1fix1
# 
# set.seed(111)
# ext_mThre_aposporous1_noPhy <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
#                                         verbose = F, data = Online_v7_mean_ApomixisType_aposporous,
#                                         family = "threshold", trunc = T,
#                                         prior = prior_V1fix1,
#                                         nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre_aposporous1_noPhy)
# print("DiagPlots_ext_mThre_aposporous1_noPhy%03d.png")
# png('DiagPlots_ext_mThre_aposporous1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre_aposporous1_noPhy, ask = F)
# dev.off()
# 
# ### No phylogeny included, response with 2 levels
# ext_mThre_aposporous1.1_noPhy <- MCMCglmm(Repr_mode ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
#                                           verbose = F, data = Online_v7_mean_ApomixisType_aposporous,
#                                           family = "threshold", trunc = T,
#                                           prior = prior_V1fix1,
#                                           nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre_aposporous1.1_noPhy)
# print("DiagPlots_ext_mThre_aposporous1.1_noPhy%03d.png")
# png('DiagPlots_ext_mThre_aposporous1.1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre_aposporous1.1_noPhy, ask = F)
# dev.off()
# 
# ##### First batch: set.seed(111) #####
# print("First batch: set.seed(111)")
# prior_nu1000_1 <- list(R = list(V = 1, fix = 1),
#                        G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1))
# )
# prior_nu1000_1
# 
# set.seed(111)
# ### Repr_mode with 3 levels
# ext_mThre_aposporous1 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
#                                   ginverse = list(animal = invJanTree_CC_tips_aposporous$Ainv),
#                                   random = ~ animal, verbose = F,
#                                   data = Online_v7_mean_ApomixisType_aposporous,
#                                   family = "threshold", trunc = T,
#                                   prior = prior_nu1000_1,
#                                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre_aposporous1)
# print("DiagPlots_ext_mThre_aposporous1%03d.png")
# png('DiagPlots_ext_mThre_aposporous1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre_aposporous1, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre_aposporous1$VCV)
# heidel.diag(ext_mThre_aposporous1$Sol)
# print("Geweke_ext_mThre_aposporous1%03d.png")
# png('Geweke_ext_mThre_aposporous1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre_aposporous1$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre_aposporous1$Sol)
# 
# ### Repr_mode with only 2 levels
# ext_mThre_aposporous1.1 <- MCMCglmm(Repr_mode ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
#                                     ginverse = list(animal = invJanTree_CC_tips_aposporous$Ainv),
#                                     random = ~ animal, verbose = F,
#                                     data = Online_v7_mean_ApomixisType_aposporous,
#                                     family = "threshold", trunc = T,
#                                     prior = prior_nu1000_1,
#                                     nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre_aposporous1.1)
# print("DiagPlots_ext_mThre_aposporous1.1%03d.png")
# png('DiagPlots_ext_mThre_aposporous1.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre_aposporous1.1, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre_aposporous1.1$VCV)
# heidel.diag(ext_mThre_aposporous1.1$Sol)
# print("Geweke_ext_mThre_aposporous1.1.png")
# png('Geweke_ext_mThre_aposporous1.1.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre_aposporous1.1$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre_aposporous1.1$Sol)
# 
# ##### Second batch: set.seed(534) #####
# print("Second batch: set.seed(534)")
# set.seed(534)
# ### Repr_mode with 3 levels
# ext_mThre_aposporous2 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
#                                   ginverse = list(animal = invJanTree_CC_tips_aposporous$Ainv),
#                                   random = ~ animal, verbose = F,
#                                   data = Online_v7_mean_ApomixisType_aposporous,
#                                   family = "threshold", trunc = T,
#                                   prior = prior_nu1000_1,
#                                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre_aposporous2)
# print("DiagPlots_ext_mThre_aposporous2%03d.png")
# png('DiagPlots_ext_mThre_aposporous2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre_aposporous2, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre_aposporous2$VCV)
# heidel.diag(ext_mThre_aposporous2$Sol)
# print("Geweke_ext_mThre_aposporous2%03d.png")
# png('Geweke_ext_mThre_aposporous2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre_aposporous2$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre_aposporous2$Sol)
# 
# ### Repr_mode with only 2 levels
# ext_mThre_aposporous2.1 <- MCMCglmm(Repr_mode ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
#                                     ginverse = list(animal = invJanTree_CC_tips_aposporous$Ainv),
#                                     random = ~ animal, verbose = F,
#                                     data = Online_v7_mean_ApomixisType_aposporous,
#                                     family = "threshold", trunc = T,
#                                     prior = prior_nu1000_1,
#                                     nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre_aposporous2.1)
# print("DiagPlots_ext_mThre_aposporous2.1%03d.png")
# png('DiagPlots_ext_mThre_aposporous2.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre_aposporous2.1, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre_aposporous2.1$VCV)
# heidel.diag(ext_mThre_aposporous2.1$Sol)
# print("Geweke_ext_mThre_aposporous2.1.png")
# png('Geweke_ext_mThre_aposporous2.1.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre_aposporous2.1$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre_aposporous2.1$Sol)
# 
# ##### Third batch: set.seed(386) #####
# print("Third batch: set.seed(386)")
# set.seed(386)
# ### Repr_mode with 3 levels
# ext_mThre_aposporous3 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
#                                   ginverse = list(animal = invJanTree_CC_tips_aposporous$Ainv),
#                                   random = ~ animal, verbose = F,
#                                   data = Online_v7_mean_ApomixisType_aposporous,
#                                   family = "threshold", trunc = T,
#                                   prior = prior_nu1000_1,
#                                   nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre_aposporous3)
# print("DiagPlots_ext_mThre_aposporous3%03d.png")
# png('DiagPlots_ext_mThre_aposporous3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre_aposporous3, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre_aposporous3$VCV)
# heidel.diag(ext_mThre_aposporous3$Sol)
# print("Geweke_ext_mThre_aposporous3%03d.png")
# png('Geweke_ext_mThre_aposporous3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre_aposporous3$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre_aposporous3$Sol)
# 
# ### Repr_mode with only 2 levels
# ext_mThre_aposporous3.1 <- MCMCglmm(Repr_mode ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
#                                     ginverse = list(animal = invJanTree_CC_tips_aposporous$Ainv),
#                                     random = ~ animal, verbose = F,
#                                     data = Online_v7_mean_ApomixisType_aposporous,
#                                     family = "threshold", trunc = T,
#                                     prior = prior_nu1000_1,
#                                     nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre_aposporous3.1)
# print("DiagPlots_ext_mThre_aposporous3.1%03d.png")
# png('DiagPlots_ext_mThre_aposporous3.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre_aposporous3.1, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre_aposporous3.1$VCV)
# heidel.diag(ext_mThre_aposporous3.1$Sol)
# print("Geweke_ext_mThre_aposporous3.1.png")
# png('Geweke_ext_mThre_aposporous3.1.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre_aposporous3.1$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre_aposporous3.1$Sol)
# 
# ##### Multiple chains convergence diagnostics #####
# ChainListTre_aposporous1_Sol <- mcmc.list(ext_mThre_aposporous1$Sol, ext_mThre_aposporous2$Sol, ext_mThre_aposporous3$Sol)
# ChainListTre_aposporous1_VCV <- mcmc.list(ext_mThre_aposporous1$VCV, ext_mThre_aposporous2$VCV, ext_mThre_aposporous3$VCV)
# 
# ChainListTre_aposporous2_Sol <- mcmc.list(ext_mThre_aposporous1.1$Sol, ext_mThre_aposporous2.1$Sol, ext_mThre_aposporous3.1$Sol)
# ChainListTre_aposporous2_VCV <- mcmc.list(ext_mThre_aposporous1.1$VCV, ext_mThre_aposporous2.1$VCV, ext_mThre_aposporous3.1$VCV)
# 
# ### Gelman rubin diagnostic: should be close to 1
# gelman.diag(ChainListTre_aposporous1_Sol)
# gelman.diag(ChainListTre_aposporous1_Sol)
# 
# gelman.diag(ChainListTre_aposporous2_Sol)
# gelman.diag(ChainListTre_aposporous2_Sol)



##### ~ #####



##### Splitting the database: Diplosporous #####
library(phytools)
setdiff(JanTree4_CC_online$tip.label, unique(Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Diplosporous", "Sexual"), "animal"]))
ape::drop.tip(JanTree4_CC_online,
              setdiff(JanTree4_CC_online$tip.label,
                      unique(Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Diplosporous", "Sexual"), "animal"])
              )
)

facetoface_diplo <- cophylo(JanTree4_CC_online,
                            drop.tip(JanTree4_CC_online,
                                     setdiff(JanTree4_CC_online$tip.label,
                                             unique(Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Diplosporous", "Sexual"), "animal"])
                                     )
                            )
)
plot(facetoface_diplo, fsize = .5)
###
Online_v7_mean_ApomixisType_diplosporous <- Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Diplosporous", "Sexual"), ]
str(Online_v7_mean_ApomixisType_diplosporous)

JanTree4_CC_online_diplosporous <- drop.tip(JanTree4_CC_online,
                                            setdiff(JanTree4_CC_online$tip.label,
                                                    unique(Online_v7_mean_ApomixisType[Online_v7_mean_ApomixisType$Apomixis_type %in% c("Diplosporous", "Sexual"), "animal"])))
JanTree4_CC_online_diplosporous

##### MCMC chains #####
library(MCMCglmm)

### Phylogeny black magic
invJanTree_CC_diplosporous <- inverseA(JanTree4_CC_online_diplosporous, nodes = "ALL", scale = TRUE)
invJanTree_CC_tips_diplosporous <- inverseA(JanTree4_CC_online_diplosporous, nodes = "TIPS", scale = TRUE)

### No phylogeny included, response with 3 levels
prior_V1fix1 <- list(R = list(V = 1, fix = 1))
prior_V1fix1

set.seed(111)
ext_mThre_diplosporous1_noPhy <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                                          verbose = F, data = Online_v7_mean_ApomixisType_diplosporous,
                                          family = "threshold", trunc = T,
                                          prior = prior_V1fix1,
                                          nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre_diplosporous1_noPhy)
print("DiagPlots_ext_mThre_diplosporous1_noPhy%03d.png")
png('DiagPlots_ext_mThre_diplosporous1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre_diplosporous1_noPhy, ask = F)
dev.off()

### No phylogeny included, response with 2 levels
ext_mThre_diplosporous1.1_noPhy <- MCMCglmm(Repr_mode ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                                            verbose = F, data = Online_v7_mean_ApomixisType_diplosporous,
                                            family = "threshold", trunc = T,
                                            prior = prior_V1fix1,
                                            nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre_diplosporous1.1_noPhy)
print("DiagPlots_ext_mThre_diplosporous1.1_noPhy%03d.png")
png('DiagPlots_ext_mThre_diplosporous1.1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre_diplosporous1.1_noPhy, ask = F)
dev.off()

##### First batch: set.seed(111) #####
print("First batch: set.seed(111)")
prior_nu1000_1 <- list(R = list(V = 1, fix = 1),
                       G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1))
)
prior_nu1000_1

set.seed(111)
### Repr_mode with 3 levels
ext_mThre_diplosporous1 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                                    ginverse = list(animal = invJanTree_CC_tips_diplosporous$Ainv),
                                    random = ~ animal, verbose = F,
                                    data = Online_v7_mean_ApomixisType_diplosporous,
                                    family = "threshold", trunc = T,
                                    prior = prior_nu1000_1,
                                    nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre_diplosporous1)
print("DiagPlots_ext_mThre_diplosporous1%03d.png")
png('DiagPlots_ext_mThre_diplosporous1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre_diplosporous1, ask = F)
dev.off()

heidel.diag(ext_mThre_diplosporous1$VCV)
heidel.diag(ext_mThre_diplosporous1$Sol)
print("Geweke_ext_mThre_diplosporous1%03d.png")
png('Geweke_ext_mThre_diplosporous1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre_diplosporous1$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre_diplosporous1$Sol)

### Repr_mode with only 2 levels
ext_mThre_diplosporous1.1 <- MCMCglmm(Repr_mode ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                                      ginverse = list(animal = invJanTree_CC_tips_diplosporous$Ainv),
                                      random = ~ animal, verbose = F,
                                      data = Online_v7_mean_ApomixisType_diplosporous,
                                      family = "threshold", trunc = T,
                                      prior = prior_nu1000_1,
                                      nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre_diplosporous1.1)
print("DiagPlots_ext_mThre_diplosporous1.1%03d.png")
png('DiagPlots_ext_mThre_diplosporous1.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre_diplosporous1.1, ask = F)
dev.off()

heidel.diag(ext_mThre_diplosporous1.1$VCV)
heidel.diag(ext_mThre_diplosporous1.1$Sol)
print("Geweke_ext_mThre_diplosporous1.1.png")
png('Geweke_ext_mThre_diplosporous1.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre_diplosporous1.1$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre_diplosporous1.1$Sol)

##### Second batch: set.seed(534) #####
print("Second batch: set.seed(534)")
set.seed(534)
### Repr_mode with 3 levels
ext_mThre_diplosporous2 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                                    ginverse = list(animal = invJanTree_CC_tips_diplosporous$Ainv),
                                    random = ~ animal, verbose = F,
                                    data = Online_v7_mean_ApomixisType_diplosporous,
                                    family = "threshold", trunc = T,
                                    prior = prior_nu1000_1,
                                    nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre_diplosporous2)
print("DiagPlots_ext_mThre_diplosporous2%03d.png")
png('DiagPlots_ext_mThre_diplosporous2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre_diplosporous2, ask = F)
dev.off()

heidel.diag(ext_mThre_diplosporous2$VCV)
heidel.diag(ext_mThre_diplosporous2$Sol)
print("Geweke_ext_mThre_diplosporous2%03d.png")
png('Geweke_ext_mThre_diplosporous2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre_diplosporous2$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre_diplosporous2$Sol)

### Repr_mode with only 2 levels
ext_mThre_diplosporous2.1 <- MCMCglmm(Repr_mode ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                                      ginverse = list(animal = invJanTree_CC_tips_diplosporous$Ainv),
                                      random = ~ animal, verbose = F,
                                      data = Online_v7_mean_ApomixisType_diplosporous,
                                      family = "threshold", trunc = T,
                                      prior = prior_nu1000_1,
                                      nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre_diplosporous2.1)
print("DiagPlots_ext_mThre_diplosporous2.1%03d.png")
png('DiagPlots_ext_mThre_diplosporous2.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre_diplosporous2.1, ask = F)
dev.off()

heidel.diag(ext_mThre_diplosporous2.1$VCV)
heidel.diag(ext_mThre_diplosporous2.1$Sol)
print("Geweke_ext_mThre_diplosporous2.1.png")
png('Geweke_ext_mThre_diplosporous2.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre_diplosporous2.1$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre_diplosporous2.1$Sol)

##### Third batch: set.seed(386) #####
print("Third batch: set.seed(386)")
set.seed(386)
### Repr_mode with 3 levels
ext_mThre_diplosporous3 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                                    ginverse = list(animal = invJanTree_CC_tips_diplosporous$Ainv),
                                    random = ~ animal, verbose = F,
                                    data = Online_v7_mean_ApomixisType_diplosporous,
                                    family = "threshold", trunc = T,
                                    prior = prior_nu1000_1,
                                    nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre_diplosporous3)
print("DiagPlots_ext_mThre_diplosporous3%03d.png")
png('DiagPlots_ext_mThre_diplosporous3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre_diplosporous3, ask = F)
dev.off()

heidel.diag(ext_mThre_diplosporous3$VCV)
heidel.diag(ext_mThre_diplosporous3$Sol)
print("Geweke_ext_mThre_diplosporous3%03d.png")
png('Geweke_ext_mThre_diplosporous3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre_diplosporous3$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre_diplosporous3$Sol)

### Repr_mode with only 2 levels
ext_mThre_diplosporous3.1 <- MCMCglmm(Repr_mode ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                                      ginverse = list(animal = invJanTree_CC_tips_diplosporous$Ainv),
                                      random = ~ animal, verbose = F,
                                      data = Online_v7_mean_ApomixisType_diplosporous,
                                      family = "threshold", trunc = T,
                                      prior = prior_nu1000_1,
                                      nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre_diplosporous3.1)
print("DiagPlots_ext_mThre_diplosporous3.1%03d.png")
png('DiagPlots_ext_mThre_diplosporous3.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre_diplosporous3.1, ask = F)
dev.off()

heidel.diag(ext_mThre_diplosporous3.1$VCV)
heidel.diag(ext_mThre_diplosporous3.1$Sol)
print("Geweke_ext_mThre_diplosporous3.1.png")
png('Geweke_ext_mThre_diplosporous3.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre_diplosporous3.1$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre_diplosporous3.1$Sol)

##### Multiple chains convergence diagnostics #####
ChainListTre_diplosporous1_Sol <- mcmc.list(ext_mThre_diplosporous1$Sol, ext_mThre_diplosporous2$Sol, ext_mThre_diplosporous3$Sol)
ChainListTre_diplosporous1_VCV <- mcmc.list(ext_mThre_diplosporous1$VCV, ext_mThre_diplosporous2$VCV, ext_mThre_diplosporous3$VCV)

ChainListTre_diplosporous2_Sol <- mcmc.list(ext_mThre_diplosporous1.1$Sol, ext_mThre_diplosporous2.1$Sol, ext_mThre_diplosporous3.1$Sol)
ChainListTre_diplosporous2_VCV <- mcmc.list(ext_mThre_diplosporous1.1$VCV, ext_mThre_diplosporous2.1$VCV, ext_mThre_diplosporous3.1$VCV)

### Gelman rubin diagnostic: should be close to 1
gelman.diag(ChainListTre_diplosporous1_Sol)
gelman.diag(ChainListTre_diplosporous1_Sol)

gelman.diag(ChainListTre_diplosporous2_Sol)
gelman.diag(ChainListTre_diplosporous2_Sol)


### Elapsed time 
print("Total elapsed time since beginning of script: ")
print(Sys.time() - start_time_overall)


##### GOODBYE! This is the end of the MCMCglmm_extended_script.R ! ##### 

sink() 
