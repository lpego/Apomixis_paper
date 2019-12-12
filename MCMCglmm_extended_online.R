sink(file = "MCMCglmm_extended_online_Echo.txt")

##### HELLO! This is the beginning of the MCMCglmm_extended_script.R ! ##### 
path <- getwd()
print(getwd())

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
### THIS IS THE SCRIPT FOR THE EXTENDED DATASET ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##### Load data #####
Online_v7_mean <- read.csv(file = "Online_v7_mean.csv", header = T)[,-1]
str(Online_v7_mean)
Online_v7_mean$Reproductive.mode <- factor(Online_v7_mean$Reproductive.mode, levels = c("Sexual", "Apomictic"))
Online_v7_mean$Embryo.Ploidy.summ <- factor(Online_v7_mean$Embryo.Ploidy.summ, levels = c("2x", "3x", "4x", "Poly"))
Online_v7_mean[!complete.cases(Online_v7_mean), ] # no missing values
nrow(Online_v7_mean)

colnames(Online_v7_mean)[2] <- "animal" # this is needed because of a MCMCglmm quirkiness

##### Load the tree #####
library(ape)
JanTree4_CC_online <- read.tree(file = "JanTree4_CC_online.tre")

setdiff(JanTree4_CC_online$tip.label, Online_v7_mean$animal)
setdiff(Online_v7_mean$animal, JanTree4_CC_online$tip.label)

print("is rooted?")
is.rooted(JanTree4_CC_online)
print("is binary?")
is.binary(JanTree4_CC_online)
print("is ultrametric?")
is.ultrametric(JanTree4_CC_online)
print("are branches of length zero?")
JanTree4_CC_online$edge.length[JanTree4_CC_online$edge.length == 0] 
print("JanTree4_CC_online.png")
png('JanTree4_CC_online.png', width = 20, height = 30, units = 'cm', res=300)
plot(JanTree4_CC_online, cex = .3)
dev.off()

##### MCMC chains #####
library(MCMCglmm)

### Phylogeny black magic
invJanTree4_CC_online <- inverseA(JanTree4_CC_online, nodes = "ALL", scale = TRUE)
invJanTree4_CC_online_tips <- inverseA(JanTree4_CC_online, nodes = "TIPS", scale = TRUE)

### No phylogeny included, response with 3 levels
prior_V1fix1 <- list(R = list(V = 1, fix = 1))
prior_V1fix1

set.seed(111)
# ext_mThre1_noPhy <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Embryo.Ploidy.summ + Flowering.time..initiation.month.,
#                              verbose = F, data = Online_v7_mean,
#                              family = "threshold", trunc = T,
#                              prior = prior_V1fix1,
#                              nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre1_noPhy)
# print("DiagPlots_ext_mThre1_noPhy%03d.png")
# png('DiagPlots_ext_mThre1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre1_noPhy, ask = F)
# dev.off()

### No phylogeny included, response with 2 levels
ext_mThre1.1_noPhy <- MCMCglmm(Reproductive.mode ~ Average.elevation + Embryo.Ploidy.summ + Flowering.time..initiation.month.,
                               verbose = F, data = Online_v7_mean,
                               family = "threshold", trunc = T,
                               prior = prior_V1fix1,
                               nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre1.1_noPhy)
print("DiagPlots_ext_mThre1.1_noPhy%03d.png")
png('DiagPlots_ext_mThre1.1_noPhy%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre1.1_noPhy, ask = F)
dev.off()

##### First batch: set.seed(111) #####
print("First batch: set.seed(111)")
prior_nu1000_1 <- list(R = list(V = 1, fix = 1),
                       G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1))
)
prior_nu1000_1

set.seed(111)
### Reproductive.mode with 3 levels
# ext_mThre1 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Embryo.Ploidy.summ + Flowering.time..initiation.month.,
#                        ginverse = list(animal = invJanTree4_CC_online_tips$Ainv),
#                        random = ~ animal, verbose = F,
#                        data = Online_v7_mean,
#                        family = "threshold", trunc = T,
#                        prior = prior_nu1000_1,
#                        nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre1)
# print("DiagPlots_ext_mThre1%03d.png")
# png('DiagPlots_ext_mThre1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre1, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre1$VCV)
# heidel.diag(ext_mThre1$Sol)
# print("Geweke_ext_mThre1%03d.png")
# png('Geweke_ext_mThre1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre1$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre1$Sol)

### Reproductive.mode with only 2 levels
ext_mThre1.1 <- MCMCglmm(Reproductive.mode ~ Average.elevation + Embryo.Ploidy.summ + Flowering.time..initiation.month.,
                         ginverse = list(animal = invJanTree4_CC_online_tips$Ainv),
                         random = ~ animal, verbose = F,
                         data = Online_v7_mean,
                         family = "threshold", trunc = T,
                         prior = prior_nu1000_1,
                         nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre1.1)
print("DiagPlots_ext_mThre1.1%03d.png")
png('DiagPlots_ext_mThre1.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre1.1, ask = F)
dev.off()

heidel.diag(ext_mThre1.1$VCV)
heidel.diag(ext_mThre1.1$Sol)
print("Geweke_ext_mThre1.1.png")
png('Geweke_ext_mThre1.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre1.1$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre1.1$Sol)

##### Second batch: set.seed(534) #####
print("Second batch: set.seed(534)")
set.seed(534)
### Reproductive.mode with 3 levels
# ext_mThre2 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Embryo.Ploidy.summ + Flowering.time..initiation.month.,
#                        ginverse = list(animal = invJanTree4_CC_online_tips$Ainv),
#                        random = ~ animal, verbose = F,
#                        data = Online_v7_mean,
#                        family = "threshold", trunc = T,
#                        prior = prior_nu1000_1,
#                        nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre2)
# print("DiagPlots_ext_mThre2%03d.png")
# png('DiagPlots_ext_mThre2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre2, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre2$VCV)
# heidel.diag(ext_mThre2$Sol)
# print("Geweke_ext_mThre2%03d.png")
# png('Geweke_ext_mThre2%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre2$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre2$Sol)

### Reproductive.mode with only 2 levels
ext_mThre2.1 <- MCMCglmm(Reproductive.mode ~ Average.elevation + Embryo.Ploidy.summ + Flowering.time..initiation.month.,
                         ginverse = list(animal = invJanTree4_CC_online_tips$Ainv),
                         random = ~ animal, verbose = F,
                         data = Online_v7_mean,
                         family = "threshold", trunc = T,
                         prior = prior_nu1000_1,
                         nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre2.1)
print("DiagPlots_ext_mThre2.1%03d.png")
png('DiagPlots_ext_mThre2.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre2.1, ask = F)
dev.off()

heidel.diag(ext_mThre2.1$VCV)
heidel.diag(ext_mThre2.1$Sol)
print("Geweke_ext_mThre2.1.png")
png('Geweke_ext_mThre2.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre2.1$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre2.1$Sol)

##### Third batch: set.seed(386) #####
print("Third batch: set.seed(386)")
set.seed(386)
### Reproductive.mode with 3 levels
# ext_mThre3 <- MCMCglmm(Repr_mode_summ ~ Average.elevation + Embryo.Ploidy.summ + Flowering.time..initiation.month.,
#                        ginverse = list(animal = invJanTree4_CC_online_tips$Ainv),
#                        random = ~ animal, verbose = F,
#                        data = Online_v7_mean,
#                        family = "threshold", trunc = T,
#                        prior = prior_nu1000_1,
#                        nitt = 10^6, thin = 500, burnin = 25000)
# summary(ext_mThre3)
# print("DiagPlots_ext_mThre3%03d.png")
# png('DiagPlots_ext_mThre3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# plot(ext_mThre3, ask = F)
# dev.off()
# 
# heidel.diag(ext_mThre3$VCV)
# heidel.diag(ext_mThre3$Sol)
# print("Geweke_ext_mThre3%03d.png")
# png('Geweke_ext_mThre3%03d.png', width = 15, height = 15, units = 'cm', res = 300)
# geweke.plot(ext_mThre3$Sol, ask = F)
# dev.off()
# autocorr.diag(ext_mThre3$Sol)

### Reproductive.mode with only 2 levels
ext_mThre3.1 <- MCMCglmm(Reproductive.mode ~ Average.elevation + Embryo.Ploidy.summ + Flowering.time..initiation.month.,
                         ginverse = list(animal = invJanTree4_CC_online_tips$Ainv),
                         random = ~ animal, verbose = F,
                         data = Online_v7_mean,
                         family = "threshold", trunc = T,
                         prior = prior_nu1000_1,
                         nitt = 10^6, thin = 500, burnin = 25000)
summary(ext_mThre3.1)
print("DiagPlots_ext_mThre3.1%03d.png")
png('DiagPlots_ext_mThre3.1%03d.png', width = 15, height = 15, units = 'cm', res = 300)
plot(ext_mThre3.1, ask = F)
dev.off()

heidel.diag(ext_mThre3.1$VCV)
heidel.diag(ext_mThre3.1$Sol)
print("Geweke_ext_mThre3.1.png")
png('Geweke_ext_mThre3.1.png', width = 15, height = 15, units = 'cm', res = 300)
geweke.plot(ext_mThre3.1$Sol, ask = F)
dev.off()
autocorr.diag(ext_mThre3.1$Sol)

##### Multiple chains convergence diagnostics #####
# chainListTre1_Sol <- mcmc.list(ext_mThre1$Sol, ext_mThre2$Sol, ext_mThre3$Sol)
# chainListTre1_VCV <- mcmc.list(ext_mThre1$VCV, ext_mThre2$VCV, ext_mThre3$VCV)

chainListTre2_Sol <- mcmc.list(ext_mThre1.1$Sol, ext_mThre2.1$Sol, ext_mThre3.1$Sol)
chainListTre2_VCV <- mcmc.list(ext_mThre1.1$VCV, ext_mThre2.1$VCV, ext_mThre3.1$VCV)

### Gelman rubin diagnostic: should be close to 1
# gelman.diag(chainListTre1_Sol)
# gelman.diag(chainListTre1_Sol)

gelman.diag(chainListTre2_Sol)
gelman.diag(chainListTre2_Sol)

##### GOODBYE! This is the end of the MCMCglmm_extended_online.R ! ##### 

sink() 
