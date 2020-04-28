# 
# library(MCMCglmm)
# 
# alp <- read.csv("~/Dropbox/alpineAster/alpinedat.csv", h=T)
# nrow(alp)  # 460
# 
# str(alp)
# 
# alp2$y <- alp2$Repr_mode_MCMCglmm
# levels(alp2$y)
# #[1] "Apomictic" "Mixed"     "Sexual"   
# 
# ##################
# 
# range(alp$Altitude)  # has NAs
# 
# alp2 <- alp[!is.na(alp$Altitude), ]
# nrow(alp2)  # 455
# range(alp2$Altitude)  # 8 2951
# 
# head(alp)
# tail(alp)
# 
# ##################
# ## Jarrod Hadfield's prior, p 97 course notes 2019
# 
# IJ <- (1/3) * (diag(2) + matrix(1, 2, 2))
# prior = list(R = list(V = IJ, fix = 1))
# 
# ##################
# ## predict probability for each reproductive type
# 
# set.seed(4222)
# 
# mtest <- MCMCglmm(y ~ trait , rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior)
# 
# mtest <- MCMCglmm(y ~ trait , rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior)
# 
# 
# 
# summary(mtest)
# post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# (Intercept)    -0.52998 -1.13868  0.01637    18.18  0.072 .  
# traity.Sexual   3.29182  2.84358  3.80066    28.82 <0.001 ***
#   
#   
#   ################## with -1, as per Hadfield course notes
#   set.seed(4222)
# 
# mtest1 <- MCMCglmm(y ~ trait  - 1, rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior)
# 
# summary(mtest1)
# 
# post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# traity.Mixed   -0.52998 -1.13868  0.01637    18.18  0.072 .  
# traity.Sexual   2.76184  2.45460  3.15160    42.46 <0.001 ***
#   
#   ## probability is relative to base level  (apomicitic)
#   
#   # Hadfield rescales (?) intercepts and exponentiates:
#   # (p 97 course notes)
#   # obtains a probability value for each level of the dependent variable
#   # probabilities sum to 1
#   
#   Delta <- cbind(c(-1, 1, 0), c(-1, 0, 1))
# c2 <- (16 * sqrt(3)/(15 * pi))^2
# D <- ginv(Delta %*% t(Delta)) %*% Delta
# Int <- t(apply(mtest1$Sol, 1, function(x) {
#   D %*% (x/sqrt(1 + c2 * diag(IJ)))
# }))
# 
# summary(mcmc(exp(Int)/rowSums(exp(Int))))
# 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean       SD  Naive SE Time-series SE
# [1,] 0.07384 0.011445 0.0003619       0.001787 # apomictic
# [2,] 0.04632 0.009725 0.0003075       0.001965 # mixed
# [3,] 0.87984 0.013907 0.0004398       0.001828 # sexual # high prob, makes sense with p-value in summary above
# 
# 2. Quantiles for each variable:
#   
#   2.5%     25%     50%     75%   97.5%
# var1 0.05316 0.06588 0.07303 0.08152 0.09697
# var2 0.02799 0.03848 0.04716 0.05333 0.06480
# var3 0.85172 0.86998 0.88106 0.89027 0.90438
# 
# 
# ######################################################
# ################## with a continuous independent variable
# set.seed(4222)
# 
# mtest2 <- MCMCglmm(y ~ trait + Altitude - 1, rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior)
# 
# summary(mtest2)
# post.mean   l-95% CI   u-95% CI eff.samp  pMCMC    
# traity.Mixed  -5.943e-01 -1.411e+00  3.915e-01    33.30  0.222    
# traity.Sexual  2.702e+00  1.950e+00  3.510e+00    40.96 <0.001 ***
#   Altitude       3.608e-05 -4.477e-04  4.281e-04    47.84  0.838    
# 
# # baseline level= apomictic
# # "Altitude" is dif between altitude in apomixic spp and altitude for mixed+sex spp jointly
# 
# ######### with "trait:"    
# set.seed(4222)
# 
# mtest3 <- MCMCglmm(y ~ Altitude:trait - 1, rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior)
# 
# summary(mtest3)
# post.mean   l-95% CI   u-95% CI eff.samp  pMCMC    
# traity.Mixed:Altitude  -2.295e-04 -4.757e-04  9.832e-05    19.34   0.15    
# traity.Sexual:Altitude  1.565e-03  1.332e-03  1.786e-03    38.31 <0.001 ***
#   
#   str(mtest3$Sol)
# Int <- t(apply(mtest3$Sol, 1, function(x) {
#   D %*% (x/sqrt(1 + c2 * diag(IJ)))
# }))
# 
# summary(mcmc(exp(Int)/rowSums(exp(Int))))
# 
# Mean        SD  Naive SE Time-series SE
# [1,] 0.3332 2.440e-05 7.717e-07      5.042e-06
# [2,] 0.3331 2.479e-05 7.840e-07      5.003e-06
# [3,] 0.3337 2.004e-05 6.338e-07      2.796e-06  
# 
# # all equal probability??? 
# # contradicts pMCMC vlaue for sex:alt
# 
# 
# #################################################
# ######### with ploidy
# 
# levels(alp2$Embryo.Ploidy)
# # "12x" "2x"  "3x"  "4x"  "6x"  "8x" 
# 
# alp2$ploidy <- alp2$Embryo.Ploidy
# table(alp2$ploidy)
# 
# levels(alp2$ploidy) <- c("poly", "dip", "tri", "tetra", "poly", "poly")
# table(alp2$ploidy)
# 
# ##################
# ## continency table
# ctable <- table(alp2$y, alp2$ploidy)
# poly dip tri tetra
# Apomictic    0   0  29     4
# Mixed        0  13   1     6
# Sexual      55 243   0   104
# 
# 
# # without interaction = estimating the difference between ploidal levels in reproductive types between apomictic and mixed+sexual (ie reference level vs jointly the other two levels)
# 
# set.seed(4222)
# mtest4 <- MCMCglmm(y ~ trait + ploidy -1, rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior)
# 
# summary(mtest4)
# post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# traity.Mixed     17.953   11.853   22.325    3.601 <0.001 ***
#   traity.Sexual    21.236   15.000   25.509    3.825 <0.001 ***
#   ploidydip        -9.533  -14.038   -5.819    2.685 <0.001 ***
#   ploidytri       -25.182  -30.060  -18.642    6.190 <0.001 ***
#   ploidytetra     -17.515  -21.736  -11.308    5.363 <0.001 ***
#   
#   ## ? very high post.means ...
#   
#   
#   ################################################
# ########  ploidy on each level of rep mode, with "trait:"
# 
# set.seed(4222)
# 
# mtest5 <- MCMCglmm(y ~ at.level(ploidy, "dip"):trait - 1 + at.level(ploidy, "tri"):trait + at.level(ploidy, "tetra"):trait + at.level(ploidy, "poly"):trait, rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior)
# 
# summary(mtest5)
# 
# post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# at.level(ploidy, "dip"):traity.Mixed       4.1162   2.6021   5.3803    3.376 <0.001 ***
#   at.level(ploidy, "dip"):traity.Sexual      7.4122   5.7713   8.5495    5.815 <0.001 ***
#   traity.Mixed:at.level(ploidy, "tri")      -4.9164  -9.4429  -2.3036    4.187 <0.001 ***
#   traity.Sexual:at.level(ploidy, "tri")    -12.6027 -23.5292  -4.7696    1.431 <0.001 ***
#   traity.Mixed:at.level(ploidy, "tetra")     0.3428  -0.9578   1.5173   10.960  0.648    
# traity.Sexual:at.level(ploidy, "tetra")    3.4730   2.5672   4.3578   36.736 <0.001 ***
#   traity.Mixed:at.level(ploidy, "poly")      0.2398  -6.5432   5.0670    1.769  0.954    
# traity.Sexual:at.level(ploidy, "poly")     8.2908   3.6346  13.6640    2.343 <0.001 ***
#   
#   ###################################
# ###### same model as above, different way of specifying levels in factor:
# 
# levels(alp2$ploidy)
# [1] "poly"  "dip"   "tri"   "tetra"  # note levels are not in alpha order
# 
# set.seed(4222)
# 
# mtest5b <- MCMCglmm(y ~ at.level(ploidy, 2):trait - 1 + at.level(ploidy, 3):trait + at.level(ploidy, 4):trait + at.level(ploidy, 1):trait, rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior)
# 
# summary(mtest5b)
# post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# at.level(ploidy, 2):traity.Mixed     4.1162   2.6021   5.3803    3.376 <0.001 ***
#   at.level(ploidy, 2):traity.Sexual    7.4122   5.7713   8.5495    5.815 <0.001 ***
#   traity.Mixed:at.level(ploidy, 3)    -4.9164  -9.4429  -2.3036    4.187 <0.001 ***
#   traity.Sexual:at.level(ploidy, 3)  -12.6027 -23.5292  -4.7696    1.431 <0.001 ***
#   traity.Mixed:at.level(ploidy, 4)     0.3428  -0.9578   1.5173   10.960  0.648    
# traity.Sexual:at.level(ploidy, 4)    3.4730   2.5672   4.3578   36.736 <0.001 ***
#   traity.Mixed:at.level(ploidy, 1)     0.2398  -6.5432   5.0670    1.769  0.954    
# traity.Sexual:at.level(ploidy, 1)    8.2908   3.6346  13.6640    2.343 <0.001 ***
#   
#   
#   #######################################################################
# ## with Jarrod's "close to being flat" prior (p 99 of course notes 2019)
# ## this prior is more appropriate when some categories have zero values
# 
# # rename prior
# prior2 = list(R = list(V = IJ, fix = 1))
# prior2$B = list(mu = rep(0, 8), V = kronecker(IJ, diag(4)) * (1.7 + pi^2/3))
# 
# 
# set.seed(4222)
# mtest5c <- MCMCglmm(y ~ at.level(ploidy, "dip"):trait - 1 + at.level(ploidy, "tri"):trait + at.level(ploidy, "tetra"):trait + at.level(ploidy, "poly"):trait, rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior2)
# 
# summary(mtest5c)
# 
# post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# at.level(ploidy, "dip"):traity.Mixed       2.0771   1.0938   3.9048    5.038 <0.001 ***
#   at.level(ploidy, "dip"):traity.Sexual      5.3777   4.3629   6.9337    8.751 <0.001 ***
#   traity.Mixed:at.level(ploidy, "tri")      -3.2926  -4.6744  -1.9190   48.203 <0.001 ***
#   traity.Sexual:at.level(ploidy, "tri")     -2.9920  -4.3444  -1.6657   59.999 <0.001 ***
#   traity.Mixed:at.level(ploidy, "tetra")     0.4320  -1.1147   1.6201   13.943   0.55    
# traity.Sexual:at.level(ploidy, "tetra")    3.6213   2.6938   4.6748   31.268 <0.001 ***
#   traity.Mixed:at.level(ploidy, "poly")     -3.3983  -5.7477  -0.9174    8.347 <0.001 ***
#   traity.Sexual:at.level(ploidy, "poly")     3.7837   2.5689   5.1394   38.193 <0.001 ***
#   ---
#   
#   ## post.means seem more reasonable (smaller range)
#   
#   ## applying Jarrod's code (p 99), get probabilities for diploids: 
#   Int <- t(apply(mtest5c $Sol[, 1:2], 1, function(x) {
#     D %*% (x/sqrt(1 + c2 * diag(IJ)))
#   }))
# summary(mcmc(exp(Int)/rowSums(exp(Int))))
# 
# [1,] 0.008595 0.004384 0.0001386       0.001079
# [2,] 0.049524 0.012095 0.0003825       0.001907
# [3,] 0.941882 0.012765 0.0004037       0.001857
# 
# ## for 3x
# Int <- t(apply(mtest5c $Sol[, 3:4], 1, function(x) {
#   D %*% (x/sqrt(1 + c2 * diag(IJ)))
# }))
# summary(mcmc(exp(Int)/rowSums(exp(Int))))
# 
# Mean      SD Naive SE Time-series SE
# [1,] 0.87669 0.05033 0.001592       0.005172
# [2,] 0.05330 0.03236 0.001023       0.005060
# [3,] 0.07001 0.03971 0.001256       0.004518
# 
# ## for 4x
# Int <- t(apply(mtest5c $Sol[, 5:6], 1, function(x) {
#   D %*% (x/sqrt(1 + c2 * diag(IJ)))
# }))
# summary(mcmc(exp(Int)/rowSums(exp(Int))))
# Mean      SD  Naive SE Time-series SE
# [1,] 0.03816 0.01636 0.0005174       0.002671
# [2,] 0.05542 0.02099 0.0006638       0.003678
# [3,] 0.90642 0.02539 0.0008028       0.003690
# 
# 
# ## for > 4x
# Int <- t(apply(mtest5c $Sol[, 7:8], 1, function(x) {
#   D %*% (x/sqrt(1 + c2 * diag(IJ)))
# }))
# summary(mcmc(exp(Int)/rowSums(exp(Int))))
# 
# Mean       SD  Naive SE Time-series SE
# [1,] 0.037680 0.022957 0.0007260       0.003283
# [2,] 0.002842 0.003422 0.0001082       0.001157
# [3,] 0.959478 0.023864 0.0007546       0.003393
# 
# 
# #####################################################################
# ####################### include altitude
# 
# prior2c = list(R = list(V = IJ, fix = 1))
# prior2c$B = list(mu = rep(0, 10), V = kronecker(IJ, diag(5)) * (1.7 + pi^2/3))
# 
# set.seed(4222)
# mtest6 <- MCMCglmm(y ~ at.level(ploidy, 2):trait - 1 + 
#                      at.level(ploidy, 3):trait + 
#                      at.level(ploidy, 4):trait + 
#                      at.level(ploidy, 1):trait + 
#                      Altitude:trait, 
#                    rcov = ~us(trait):units, family = "categorical", data= alp2, prior=prior2c)
# 
# summary(mtest6)
# 
# post.mean   l-95% CI   u-95% CI eff.samp  pMCMC    
# at.level(ploidy, 2):traity.Mixed   1.3354469  0.1963658  2.5235329    23.83  0.012 *  
#   at.level(ploidy, 2):traity.Sexual  4.4315102  3.2137177  5.9111202    35.41 <0.001 ***
#   traity.Mixed:at.level(ploidy, 3)  -3.2634004 -4.8954621 -1.6716440    42.38 <0.001 ***
#   traity.Sexual:at.level(ploidy, 3) -4.1657302 -5.4730539 -2.6730172    64.84 <0.001 ***
#   traity.Mixed:at.level(ploidy, 4)  -0.2124809 -1.6093425  1.2001235    24.15  0.794    
# traity.Sexual:at.level(ploidy, 4)  2.8656745  1.7393717  4.0349819    54.44 <0.001 ***
#   traity.Mixed:at.level(ploidy, 1)  -0.5726953 -2.6092567  2.1538750    15.36  0.582    
# traity.Sexual:at.level(ploidy, 1)  3.8335673  2.2001049  5.8637685    36.80 <0.001 ***
#   traity.Mixed:Altitude              0.0004960 -0.0004015  0.0012197    15.64  0.266    
# traity.Sexual:Altitude             0.0005913 -0.0001663  0.0013471    16.34  0.148    
# 
# #####################################################################
# 
# str(Online_v7_mean)
# mtest6.1 <- MCMCglmm(Repr_mode_summ ~ at.level(Embryo.Ploidy.summ, 2):trait - 1 + at.level(Embryo.Ploidy.summ, 3):trait + at.level(Embryo.Ploidy.summ, 4):trait + at.level(Embryo.Ploidy.summ, 1):trait + Average.elevation:trait, rcov = ~us(trait):units, family = "categorical", data= Online_v7_mean, prior=prior2c)
# 
# summary(mtest6.1)



##### ~ #####

library(ape)
library(MCMCglmm)

setdiff(Online_v7_mean_ApomixisType$animal, JanTree4_CC_online$tip.label)
setdiff(JanTree4_CC_online$tip.label, Online_v7_mean_ApomixisType$animal)

### Phylogeny black magic
invJanTree_CC <- inverseA(JanTree4_CC_online, nodes = "ALL", scale = TRUE)
invJanTree_CC_tips <- inverseA(JanTree4_CC_online, nodes = "TIPS", scale = TRUE)

prior_nu1000_1 <- list(R = list(V = 1, fix = 1),
                       G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1))
)
prior_nu1000_1

### 

mThre_levels <- MCMCglmm(Apomixis_type ~ Average.elevation + Ploidy_summ + Flowering.time..initiation.month.,
                         ginverse = list(animal = invJanTree_CC$Ainv),
                         random = ~ animal, verbose = F,
                         data = Online_v7_mean_ApomixisType,
                         family = "threshold", trunc = T,
                         prior = prior_nu1000_1,
                         nitt = 10^4, thin = 500, burnin = 1000)
summary(mThre_levels)

###

str(Online_v7_mean_ApomixisType) # let's say response has 5 levels

IJ <- (1/5) * (diag(4) + matrix(1, 4, 4))
prior3 <- list(R = list(V = IJ, fix = 1)#,
               # G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)), 
               # B = list(mu = rep(0, 5), V = kronecker((1/5) * (diag(5) + matrix(1, 5, 5)), diag(5)) * (1.7 + pi^2/3))
               )
prior3

# mThre_levels <- MCMCglmm(Apomixis_type ~ at.level(Ploidy_summ, 1):trait + at.level(Ploidy_summ, 2):trait + at.level(Ploidy_summ, 3):trait + at.level(Ploidy_summ, 4):trait - 1,
#                          # ginverse = list(animal = invJanTree_CC$Ainv), random = ~ animal,
#                          rcov = ~us(trait):units,
#                          verbose = T,
#                          data = Online_v7_mean_ApomixisType,
#                          family = "categorical",
#                          # trunc = T,
#                          prior = prior3,
#                          nitt = 10^4, thin = 500, burnin = 10^3)
# mThre_levels <- MCMCglmm(Apomixis_type ~ Ploidy_summ + Average.elevation + Flowering.time..initiation.month. - 1,
#                          # ginverse = list(animal = invJanTree_CC$Ainv), random = ~animal,
#                          rcov = ~us(trait):units,
#                          verbose = T,
#                          data = Online_v7_mean_ApomixisType,
#                          family = "categorical",
#                          # trunc = T,
#                          prior = prior3,
#                          nitt = 10^4, thin = 500, burnin = 10^3)
mThre_levels <- MCMCglmm(Apomixis_type ~ trait:Average.elevation - 1,
                         # ginverse = list(animal = invJanTree_CC$Ainv), random = ~animal,
                         rcov = ~us(trait):units,
                         verbose = T,
                         data = Online_v7_mean_ApomixisType,
                         family = "categorical",
                         # trunc = T,
                         prior = prior3,
                         nitt = 10^5, thin = 500, burnin = 10^3)
summary(mThre_levels)
plot(mThre_levels)
heidel.diag(mThre_levels$Sol)
heidel.diag(mThre_levels$VCV)
geweke.plot(mThre_levels$Sol)
autocorr.diag(mThre_levels$Sol)

j = 4 # dimensionality of the matrix
Delta <- rbind(rep(-1, j), diag(1, j, j))
c2 <- (16 * sqrt(3)/(15 * pi))^2
D <- MASS::ginv(Delta %*% t(Delta)) %*% Delta
Int <- t(apply(mThre_levels$Sol, 1, function(x) {
  D %*% (x/sqrt(1 + c2 * diag(IJ)))
  }))
summary(mcmc(exp(Int)/rowSums(exp(Int))))

table(Online_v7_mean_ApomixisType$Apomixis_type)
### I have no idea why they are all so similar in post mean... 

### let's try to add phylogeny
j = 4
IJ <- (1/5) * (diag(j) + matrix(1, j, j))
prior4 <- list(R = list(V = IJ, fix = 1),
                    G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1))
)

mThre_levels_phylo <- MCMCglmm(Apomixis_type ~ trait:Average.elevation + trait:Flowering.time..initiation.month. - 1,
                         ginverse = list(animal = invJanTree_CC$Ainv), random = ~animal,
                         rcov = ~us(trait):units,
                         verbose = T,
                         data = Online_v7_mean_ApomixisType,
                         family = "categorical",
                         # trunc = T,
                         prior = prior4,
                         nitt = 10^4, thin = 500, burnin = 10^2)
summary(mThre_levels_phylo)
plot(mThre_levels_phylo_phylo)
heidel.diag(mThre_levels_phylo$Sol)
heidel.diag(mThre_levels_phylo$VCV)
geweke.plot(mThre_levels_phylo$Sol)
autocorr.diag(mThre_levels_phylo$Sol)

###

# Jarrod has J = 3, I = 2 (?)
# IJ <- (1/3) * (diag(2) + matrix(1, 2, 2))
# prior = list(R = list(V = IJ, fix = 1))
# prior$B = list(mu = rep(0, 4), V = kronecker(IJ, diag(2)) * (1.7 + pi^2/3))

j = 4 # dimensionality of the matrix
IJ <- (1/5) * (diag(j) + matrix(1, j, j))
prior4_flat <- list(R = list(V = IJ, fix = 1),
                    G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)),
                    B = list(mu = rep(0, 8), V = kronecker(IJ, diag(2)) * (1.7 + pi^2/3))
)

mThre_levels_phylo_flat <- MCMCglmm(Apomixis_type ~ trait:Average.elevation + trait:Flowering.time..initiation.month. - 1,
                               ginverse = list(animal = invJanTree_CC$Ainv), random = ~animal,
                               rcov = ~us(trait):units,
                               verbose = T,
                               data = Online_v7_mean_ApomixisType,
                               family = "categorical",
                               # trunc = T,
                               prior = prior4_flat,
                               nitt = 10^6, thin = 500, burnin = 2500)
summary(mThre_levels_phylo_flat)
plot(mThre_levels_phylo_flat)
heidel.diag(mThre_levels_phylo_flat$Sol)
heidel.diag(mThre_levels_phylo_flat$VCV)
geweke.plot(mThre_levels_phylo_flat$Sol)
autocorr.diag(mThre_levels_phylo_flat$Sol)

j = 8 # dimensionality of the matrix
Delta <- rbind(rep(-1, j), diag(1, j, j))
c2 <- (16 * sqrt(3)/(15 * pi))^2
D <- MASS::ginv(Delta %*% t(Delta)) %*% Delta
Int <- t(apply(mThre_levels_phylo_flat$Sol, 1, function(x) {
  D %*% (x/sqrt(1 + c2 * diag(IJ)))
}))
summary(mcmc(exp(Int)/rowSums(exp(Int))))

###

str(Online_v7_mean_ApomixisType)
# Online_v7_mean_ApomixisType$animal <- Online_v7_mean_ApomixisType$SpeciesName

with(Online_v7_mean_ApomixisType, table(Apomixis_type, Flowering.time..initiation.month.))
with(Online_v7_mean_ApomixisType, table(Apomixis_type, Average.elevation))
