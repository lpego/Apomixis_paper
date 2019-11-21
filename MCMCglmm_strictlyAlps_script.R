sink(file = "MCMCglmm_strictlyAlps_script_Echo.txt")

##### HELLO! This is the beginning of MCMCglmm_strictlyAlps_script.R ! ##### 
path <- getwd()
print(getwd())

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### THIS IS THE SCRIPT FOR THE STRICLY ALPS DATASET ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##### Load data #####
MCMC_DATA_StrictlyAlps_mean_red <- read.csv(file = "DATA_StrictlyAlps_mean_red.csv", header = T)[,-1]
str(MCMC_DATA_StrictlyAlps_mean_red)
ToBeFactors <- c("ID_FloraAlpina", "ID", "Repr_mode", "Repr_mode_summ",
                 "Ploidy", "Ploidy_summ", "PloidyEvenOdd",
                 "Longevity", "BiologicalForm", "Endemic", "Indigenous", "Distribution",
                 "Collinaire", "Montane", "Subalpine", "Alpine", "Nival",
                 "Phytosociology", "Habitat",
                 "Ca", "Ca.Si", "Si", "pH", "N", "Water", "w")
MCMC_DATA_StrictlyAlps_mean_red[ToBeFactors] <- lapply(MCMC_DATA_StrictlyAlps_mean_red[ToBeFactors], factor)
str(MCMC_DATA_StrictlyAlps_mean_red)
MCMC_DATA_StrictlyAlps_mean_red$Repr_mode <- factor(MCMC_DATA_StrictlyAlps_mean_red$Repr_mode, levels = c("Sexual", "Apomictic"))
MCMC_DATA_StrictlyAlps_mean_red$Repr_mode_summ <- factor(MCMC_DATA_StrictlyAlps_mean_red$Repr_mode_summ, levels = c("Sexual", "Mixed", "Apomictic"))
MCMC_DATA_StrictlyAlps_mean_red$Ploidy <- factor(MCMC_DATA_StrictlyAlps_mean_red$Ploidy, levels = c("2x", "3x", "4x", "6x", "8x", "12x"))
MCMC_DATA_StrictlyAlps_mean_red$Ploidy_summ <- factor(MCMC_DATA_StrictlyAlps_mean_red$Ploidy_summ, levels = c("2x", "3x", "4x", "Poly"))
MCMC_DATA_StrictlyAlps_mean_red[!complete.cases(MCMC_DATA_StrictlyAlps_mean_red), ] # no missing values
nrow(MCMC_DATA_StrictlyAlps_mean_red)

colnames(MCMC_DATA_StrictlyAlps_mean_red)[3] <- "animal" # this is needed because of a MCMCglmm quirkiness

##### Load the tree #####
library(ape)
JanTree4_StrictlyAlps <- read.tree(file = "JanTree4_StrictlyAlps.tre")

setdiff(JanTree4_StrictlyAlps$tip.label, MCMC_DATA_StrictlyAlps_mean_red$animal)
setdiff(MCMC_DATA_StrictlyAlps_mean_red$animal, JanTree4_StrictlyAlps$tip.label)
print("is rooted?")
is.rooted(JanTree4_StrictlyAlps)
print("is binary?")
is.binary(JanTree4_StrictlyAlps)
print("is ultrametric?")
is.ultrametric(JanTree4_StrictlyAlps)
print("are branches of length zero?")
JanTree4_StrictlyAlps$edge.length[JanTree4_StrictlyAlps$edge.length == 0] 
print("JanTree4_StrictlyAlps.png")
png('JanTree4_StrictlyAlps.png', width = 20, height = 30, units = 'cm', res=300)
  plot(JanTree4_StrictlyAlps, cex = .3)
dev.off()

##### MCMC chains #####
library(MCMCglmm)

### Phylogeny black magic
invJanTree4_StrictlyAlps <- inverseA(JanTree4_StrictlyAlps, nodes = "ALL", scale = TRUE)
invJanTree4_StrictlyAlps_tips <- inverseA(JanTree4_StrictlyAlps, nodes = "TIPS", scale = TRUE)

### No phylogeny included, response with 3 levels
prior_V1fix1 <- list(R = list(V = 1, fix = 1))
prior_V1fix1

set.seed(111)
str_mThre1_noPhy <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy_summ + Init.month,
                                 verbose = F, data = MCMC_DATA_StrictlyAlps_mean_red,
                                 family = "threshold", trunc = T,
                                 prior = prior_V1fix1,
                                 nitt = 10^6, thin = 500, burnin = 25000)
summary(str_mThre1_noPhy)
print("DiagPlots_str_mThre1_noPhy%03d.png")
png('DiagPlots_str_mThre1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  plot(str_mThre1_noPhy, ask = F)
dev.off()

### No phylogeny included, response with 2 levels
str_mThre1.1_noPhy <- MCMCglmm(Repr_mode ~ Altitude + Ploidy_summ + Init.month,
                               verbose = F, data = MCMC_DATA_StrictlyAlps_mean_red,
                               family = "threshold", trunc = T,
                               prior = prior_V1fix1,
                               nitt = 10^6, thin = 500, burnin = 25000)
summary(str_mThre1.1_noPhy)
print("DiagPlots_str_mThre1_noPhy%03d.png")
png('DiagPlots_str_mThre1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  plot(str_mThre1.1_noPhy, ask = F)
dev.off()

##### First batch: set.seed(111) #####
print("First batch: set.seed(111)")
prior_nu1000_1 <- list(R = list(V = 1, fix = 1),
                       G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1))
                       )
prior_nu1000_1

set.seed(111)
### Repr_mode with 3 levels
str_mThre1 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy_summ + Init.month,
                       ginverse = list(animal = invJanTree4_StrictlyAlps_tips$Ainv),
                       random = ~ animal, verbose = F,
                       data = MCMC_DATA_StrictlyAlps_mean_red,
                       family = "threshold", trunc = T,
                       prior = prior_nu1000_1,
                       nitt = 10^6, thin = 500, burnin = 25000)
summary(str_mThre1)
print("DiagPlots_str_mThre1%03d.png")
png('DiagPlots_str_mThre1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  plot(str_mThre1, ask = F)
dev.off()

heidel.diag(str_mThre1$VCV)
heidel.diag(str_mThre1$Sol)
print("Geweke_str_mThre1%03d.png")
png('Geweke_str_mThre1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  geweke.plot(str_mThre1$Sol, ask = F)
dev.off()
autocorr.diag(str_mThre1$Sol)

### Repr_mode with only 2 levels
str_mThre1.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy_summ + Init.month,
                         ginverse = list(animal = invJanTree4_StrictlyAlps_tips$Ainv),
                         random = ~ animal, verbose = F,
                         data = MCMC_DATA_StrictlyAlps_mean_red,
                         family = "threshold", trunc = T,
                         prior = prior_nu1000_1,
                         nitt = 10^6, thin = 500, burnin = 25000)
summary(str_mThre1.1)
print("DiagPlots_str_mThre1.1%03d.png")
png('DiagPlots_str_mThre1.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  plot(str_mThre1.1, ask = F)
dev.off()

heidel.diag(str_mThre1.1$VCV)
heidel.diag(str_mThre1.1$Sol)
print("Geweke_str_mThre1.1.png")
png('Geweke_str_mThre1.1.png', width = 15, height = 15, units = 'cm', res = 300)
  geweke.plot(str_mThre1.1$Sol, ask = F)
dev.off()
autocorr.diag(str_mThre1.1$Sol)

##### Second batch: set.seed(534) #####
print("Second batch: set.seed(534)")
set.seed(534)
### Repr_mode with 3 levels
str_mThre2 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy_summ + Init.month,
                       ginverse = list(animal = invJanTree4_StrictlyAlps_tips$Ainv),
                       random = ~ animal, verbose = F,
                       data = MCMC_DATA_StrictlyAlps_mean_red,
                       family = "threshold", trunc = T,
                       prior = prior_nu1000_1,
                       nitt = 10^6, thin = 500, burnin = 25000)
summary(str_mThre2)
print("DiagPlots_str_mThre2%03d.png")
png('DiagPlots_str_mThre2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  plot(str_mThre2, ask = F)
dev.off()

heidel.diag(str_mThre2$VCV)
heidel.diag(str_mThre2$Sol)
print("Geweke_str_mThre2%03d.png")
png('Geweke_str_mThre2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  geweke.plot(str_mThre2$Sol, ask = F)
dev.off()
autocorr.diag(str_mThre2$Sol)

### Repr_mode with only 2 levels
str_mThre2.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy_summ + Init.month,
                         ginverse = list(animal = invJanTree4_StrictlyAlps_tips$Ainv),
                         random = ~ animal, verbose = F,
                         data = MCMC_DATA_StrictlyAlps_mean_red,
                         family = "threshold", trunc = T,
                         prior = prior_nu1000_1,
                         nitt = 10^6, thin = 500, burnin = 25000)
summary(str_mThre2.1)
print("DiagPlots_str_mThre2.1%03d.png")
png('DiagPlots_str_mThre2.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  plot(str_mThre2.1, ask = F)
dev.off()

heidel.diag(str_mThre2.1$VCV)
heidel.diag(str_mThre2.1$Sol)
print("Geweke_str_mThre2.1.png")
png('Geweke_str_mThre2.1.png', width = 15, height = 15, units = 'cm', res = 300)
  geweke.plot(str_mThre2.1$Sol, ask = F)
dev.off()
autocorr.diag(str_mThre2.1$Sol)

##### Third batch: set.seed(386) #####
print("Third batch: set.seed(386)")
set.seed(386)
### Repr_mode with 3 levels
str_mThre3 <- MCMCglmm(Repr_mode_summ ~ Altitude + Ploidy_summ + Init.month,
                       ginverse = list(animal = invJanTree4_StrictlyAlps_tips$Ainv),
                       random = ~ animal, verbose = F,
                       data = MCMC_DATA_StrictlyAlps_mean_red,
                       family = "threshold", trunc = T,
                       prior = prior_nu1000_1,
                       nitt = 10^6, thin = 500, burnin = 25000)
summary(str_mThre3)
print("DiagPlots_str_mThre3%03d.png")
png('DiagPlots_str_mThre3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  plot(str_mThre3, ask = F)
dev.off()

heidel.diag(str_mThre3$VCV)
heidel.diag(str_mThre3$Sol)
print("Geweke_str_mThre3%03d.png")
png('Geweke_str_mThre3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  geweke.plot(str_mThre3$Sol, ask = F)
dev.off()
autocorr.diag(str_mThre3$Sol)

### Repr_mode with only 2 levels
str_mThre3.1 <- MCMCglmm(Repr_mode ~ Altitude + Ploidy_summ + Init.month,
                         ginverse = list(animal = invJanTree4_StrictlyAlps_tips$Ainv),
                         random = ~ animal, verbose = F,
                         data = MCMC_DATA_StrictlyAlps_mean_red,
                         family = "threshold", trunc = T,
                         prior = prior_nu1000_1,
                         nitt = 10^6, thin = 500, burnin = 25000)
summary(str_mThre3.1)
print("DiagPlots_str_mThre3.1%03d.png")
png('DiagPlots_str_mThre3.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
  plot(str_mThre3.1, ask = F)
dev.off()

heidel.diag(str_mThre3.1$VCV)
heidel.diag(str_mThre3.1$Sol)
print("Geweke_str_mThre3.1.png")
png('Geweke_str_mThre3.1.png', width = 15, height = 15, units = 'cm', res = 300)
  geweke.plot(str_mThre3.1$Sol, ask = F)
dev.off()
autocorr.diag(str_mThre3.1$Sol)

##### Multiple chains convergence diagnostics #####
chainListTre1_Sol <- mcmc.list(str_mThre1$Sol, str_mThre2$Sol, str_mThre3$Sol)
chainListTre1_VCV <- mcmc.list(str_mThre1$VCV, str_mThre2$VCV, str_mThre3$VCV)

chainListTre2_Sol <- mcmc.list(str_mThre1.1$Sol, str_mThre2.1$Sol, str_mThre3.1$Sol)
chainListTre2_VCV <- mcmc.list(str_mThre1.1$VCV, str_mThre2.1$VCV, str_mThre3.1$VCV)

### Gelman rubin diagnostic: should be close to 1
gelman.diag(chainListTre1_Sol)
gelman.diag(chainListTre1_Sol)

gelman.diag(chainListTre2_Sol)
gelman.diag(chainListTre2_Sol)

##### GOODBYE! This is the end of the MCMCglmm_strictlyAlps_script.R ! ##### 

sink() 
