### R code from vignette source 'BMP_submission_0.Rnw'
zz <- file("script_output.Rout", open="wb")
sink(zz)
sink(zz, type = "message")

###################################################
### code chunk number 14: BMP_submission_0.Rnw:265-267
###################################################
library(sf)
used.cars <- st_read("Data and Shapes/hanna66.gpkg", quiet=TRUE)


###################################################
### code chunk number 15: BMP_submission_0.Rnw:271-275
###################################################
library(spdep)
nb <- poly2nb(used.cars)
centroids <- st_centroid(st_geometry(used.cars))
nbl <- nb2lines(nb, coords=centroids, as_sf=TRUE)


###################################################
### code chunk number 16: BMP_submission_0.Rnw:279-290
###################################################
used.cars_laea <- st_transform(used.cars, "EPSG:2163")
nbl_laea <- st_transform(nbl, "EPSG:2163")
library(tmap)
a <- tm_shape(used.cars_laea) + tm_fill("av55_59", title=c("1960 USD"), n=8, palette="Reds") + 
  tm_borders(lwd=0.5) + tm_legend(position=c("RIGHT", "BOTTOM")) + tm_layout(bg="grey90") + tm_shape(nbl) + tm_lines(col="blue")
b <- tm_shape(used.cars_laea) + tm_fill(c("transp", "salesTax"), title=c("1960 USD"), n=8, palette="YlOrBr") + 
  tm_legend(position=c("right", "bottom")) + tm_facets(free.scales=FALSE) + tm_borders(lwd=0.5) + 
  tm_layout(panel.labels=c("Transport costs", "Sales tax"), bg="grey90")
tmap_arrange(a, b, nrow=2)


###################################################
### code chunk number 18: BMP_submission_0.Rnw:314-316
###################################################
nb
lw <- nb2listw(nb, style="W")


###################################################
### code chunk number 19: BMP_submission_0.Rnw:329-333
###################################################
av55_59_APLE <- aple(c(scale(used.cars$av55_59, scale=FALSE)), lw)
transp_APLE <- aple(c(scale(used.cars$transp, scale=FALSE)), lw)
salesTax_APLE <- aple(c(scale(used.cars$salesTax, scale=FALSE)), lw)
both_APLE <- aple(c(scale(with(used.cars, transp + salesTax), scale=FALSE)), lw)


###################################################
### code chunk number 20: data cross section dui
###################################################
shape2 <- st_read("Data and Shapes/ccountyR.gpkg", quiet=TRUE)


###################################################
### code chunk number 21: BMP_submission_0.Rnw:382-383
###################################################
nblist_dui <- readRDS("Data and Shapes/nblist_dui.rds")


###################################################
### code chunk number 22: BMP_submission_0.Rnw:385-386 (eval = FALSE)
###################################################
## nblist_dui <- poly2nb(shape2)


###################################################
### code chunk number 23: BMP_submission_0.Rnw:388-392
###################################################
listw_dui <- nb2listw(nblist_dui, style="W")
fm2 <- dui ~ nondui + vehicles + dry
endog2 <- ~ police
instruments2 <- ~ elect 


###################################################
### code chunk number 24: ricedata
###################################################
data(RiceFarms, package="splm")
data(riceww, package="splm")
ricelw <- mat2listw(riceww)


###################################################
### code chunk number 25: ricefm
###################################################
ricefm  <- log(goutput) ~ log(seed) + log(urea) + phosphate +
    log(totlabor) + log(size) + I(pesticide>0) +
    I(varieties=="high") + I(varieties=="mixed") + # as.factor(region) +
    I(as.numeric(time) %in% c(1,3,5))


###################################################
### code chunk number 26: splm data 4
###################################################
library("splm")
data_cor <- read.csv("./Data and Shapes/cornwell.csv")
shape4 <- st_read("Data and Shapes/north_carol.gpkg", quiet=TRUE)
coord_NC <- st_centroid(st_geometry(shape4), of_largest_polygon=TRUE)
nbnc_NC <- knn2nb(knearneigh(coord_NC, k = 5))
nc_listw <- nb2listw(nbnc_NC, style="W")
fm4 <- lcrmrte ~ lprbconv + lprbpris + lavgsen + 
  ldensity + lwcon + lwtuc + 
  lwtrd + lwfir + lwser + lwmfg + 
  lwfed+ lwsta + lwloc + lpctymle + lpctmin + 
  west + central + urban+ d82 + d83 + d84 + d85 + d86 + d87
endog4 = ~ lprbarr + lpolpc   
instruments4 = ~ ltaxpc + lmix


###################################################
### code chunk number 27: BMP_submission_0.Rnw:656-661
###################################################
# n <- nrow(used.cars)
# rho <- c(0.0, 0.2, 0.5, 0.8)
# IrWs <- lapply(rho, function(r) invIrW(lw, r))
# b_0 <- mu_x <- sig_x <- sig_y <- 1
# mat <- matrix(NA, length(rho), length(rho))
# nsims <- 10000
# res <- integer(nsims)
# tlim <- qt(0.975, n-2)
# for (i in seq_along(IrWs)) {
#   for (j in seq_along(IrWs)) {
#     set.seed(123456)
#     for (k in seq_along(res)) {
#       y <- b_0 + IrWs[[i]] %*% rnorm(n, 0, sig_y)
#       x <- mu_x + IrWs[[j]] %*% rnorm(n, 0, sig_x)
#       lm_obj <- summary(lm(y ~ x))
#       res[k] <- ifelse(abs(lm_obj$coefficients[2,1]/lm_obj$coefficients[2,2]) > tlim, 1L, 0L)
#     }
#     mat[i, j] <- sum(res)/nsims
#   }
# }
# rownames(mat) <- paste("$\\rho_y$", rho)
# colnames(mat) <- paste("$\\rho_x$", rho)
# saveRDS(mat, file="smith_lee_extract.rds")
mat <- readRDS("Data and Shapes/smith_lee_extract.rds")
suppressPackageStartupMessages(library(xtable))
print(xtable(mat, align=c("l","r","r","r","r"), digits=c(NA, 4, 4, 4, 4), 
display=c("s", "f", "f", "f", "f")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,4))


###################################################
### code chunk number 28: BMP_submission_0.Rnw:681-684
###################################################
lm_obj1 <- lm(av55_59 ~ I(transp + salesTax), used.cars)
lm_obj2 <- lm(av55_59 ~ transp + salesTax, used.cars)
lm_obj3 <- lm(av55_59 ~ offset(transp) + salesTax, used.cars)


###################################################
### code chunk number 29: BMP_submission_0.Rnw:702-715
###################################################
cmat <- matrix("", ncol=3, nrow=9)
cmat[1:4, 1] <- formatC(c(t(summary(lm_obj1)$coefficients[,1:2])), format="f", digits=3)
cmat[c(1:2, 5:8), 2] <- formatC(c(t(summary(lm_obj2)$coefficients[,1:2])), format="f", digits=3)
cmat[c(1:2, 7:8), 3] <- formatC(c(t(summary(lm_obj3)$coefficients[,1:2])), format="f", digits=3)
cmat[9,1] <- formatC(summary(lm_obj1)$sigma^2, format="f", digits=3)
cmat[9,2] <- formatC(summary(lm_obj2)$sigma^2, format="f", digits=3)
cmat[9,3] <- formatC(summary(lm_obj3)$sigma^2, format="f", digits=3)
even <- (1:nrow(cmat) %% 2) == 0
cmat[even,] <- t(apply(cmat[even,], 1, function(x) { sapply(x, function(y) {ifelse(nzchar(y), paste("(", y, ")", sep=""), y)})}))
rownames(cmat) <- c("(Intercept)", " ", "I(transp + salesTax)", "  ", "transp", "   ", "salesTax", "    ", "$\\sigma^2$")
colnames(cmat) <- c("(1)", "(2)", "(3)")
print(xtable(cmat, align=c("l","r","r","r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,9))


###################################################
### code chunk number 30: BMP_submission_0.Rnw:733-736
###################################################
moran1 <- lm.morantest(lm_obj1, lw, alternative="two.sided")
moran2 <- lm.morantest(lm_obj2, lw, alternative="two.sided")
moran3 <- lm.morantest(lm_obj3, lw, alternative="two.sided")


###################################################
### code chunk number 31: BMP_submission_0.Rnw:752-759
###################################################
mat <- sapply(list(moran1, moran2, moran3), function(x) c(x$estimate, "Standard deviate"=unname(x$statistic), "p.value"=x$p.value))
cmat <- formatC(mat[1:4,], format="f", digits=4)
cmat <- rbind(cmat, format.pval(mat[5,]))
rownames(cmat)[5] <- "Pr(z != 0)"
colnames(cmat) <- paste("(", 1:3, ")", sep="")
print(xtable(cmat, align=c("l","r","r","r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,5))


###################################################
### code chunk number 32: BMP_submission_0.Rnw:775-778
###################################################
LM1 <- lm.LMtests(lm_obj1, lw, test="all")
LM2 <- lm.LMtests(lm_obj2, lw, test="all")
LM3 <- lm.LMtests(lm_obj3, lw, test="all")


###################################################
### code chunk number 33: BMP_submission_0.Rnw:787-792
###################################################
mat <- sapply(list(LM1, LM2, LM3), function(x) sapply(x, "[[", "p.value"))
cmat <- t(apply(mat, 1, format.pval))
colnames(cmat) <- paste("(", 1:3, ")", sep="")
print(xtable(cmat, align=c("l","r","r","r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,5))


###################################################
### code chunk number 34: BMP_submission_0.Rnw:902-904
###################################################
library(spatialreg)
sem_obj2 <- spatialreg::errorsarlm(av55_59 ~ transp + salesTax, used.cars, listw=lw)


###################################################
### code chunk number 35: BMP_submission_0.Rnw:917-919
###################################################
slm_obj2 <- spatialreg::lagsarlm(av55_59 ~ transp + salesTax, used.cars, listw=lw)
sdm_obj2 <- spatialreg::lagsarlm(av55_59 ~ transp + salesTax, used.cars, listw=lw, Durbin=TRUE)


###################################################
### code chunk number 36: BMP_submission_0.Rnw:929-950
###################################################
cmat <- matrix("", ncol=4, 15)
cmat[1:6, 1] <- formatC(c(t(summary(lm_obj2)$coefficients[,1:2])), format="f", digits=3)
cmat[1:6, 2] <- formatC(c(t(summary(sem_obj2)$Coef[,1:2])), format="f", digits=3)
cmat[1:6, 3] <- formatC(c(t(summary(slm_obj2)$Coef[,1:2])), format="f", digits=3)
cmat[1:10, 4] <- formatC(c(t(summary(sdm_obj2)$Coef[,1:2])), format="f", digits=3)
cmat[15, 1] <- formatC(summary(lm_obj2)$sigma^2, format="f", digits=3)
cmat[15, 2] <- formatC(summary(sem_obj2)$s2, format="f", digits=3)
cmat[15, 3] <- formatC(summary(slm_obj2)$s2, format="f", digits=3)
cmat[15, 4] <- formatC(summary(sdm_obj2)$s2, format="f", digits=3)
cmat[11, 2] <- formatC(summary(sem_obj2)$lambda, format="f", digits=3)
cmat[12, 2] <- formatC(summary(sem_obj2)$lambda.se, format="f", digits=3)
cmat[13, 3] <- formatC(summary(slm_obj2)$rho, format="f", digits=3)
cmat[14, 3] <- formatC(summary(slm_obj2)$rho.se, format="f", digits=3)
cmat[13, 4] <- formatC(summary(sdm_obj2)$rho, format="f", digits=3)
cmat[14, 4] <- formatC(summary(sdm_obj2)$rho.se, format="f", digits=3)
even <- (1:nrow(cmat) %% 2) == 0
cmat[even,] <- t(apply(cmat[even,], 1, function(x) { sapply(x, function(y) {ifelse(nzchar(y), paste("(", y, ")", sep=""), y)})}))
rownames(cmat) <- c("(Intercept)", " ", "transp", "  ", "salesTax", "   ", "lag(transp)", "    ", "lag(salesTax)", "     ", "$\\rhoerr$", "      ", "$\\rholag$", "       ", "$\\sigma^2$")
colnames(cmat) <- c("OLS", "SEM", "SLM", "SDM")
print(xtable(cmat, align=c("l","r","r","r","r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,15))


###################################################
### code chunk number 37: lag model spatialreg
###################################################
lag_gmm_sreg <- spatialreg::stsls(dui ~ nondui + vehicles + dry 
                                  + police, data = shape2, 
                                  listw = listw_dui)
err_gmm_sreg <- spatialreg::GMerrorsar(dui ~ nondui + vehicles 
                                       + dry + police, data = shape2, 
                                       listw = listw_dui)
sarar_gmm_sreg <- spatialreg::gstsls(dui ~ nondui + vehicles 
                                     + dry + police, 
                                  data = shape2, listw = listw_dui)


###################################################
### code chunk number 38: BMP_submission_0.Rnw:1038-1052
###################################################
cmat <- matrix("", ncol=3, 14)
cmat[1:10, 2] <- formatC(c(t(summary(lag_gmm_sreg)$Coef[-1,1:2])), format="f", digits=3)
cmat[13:14, 2] <- formatC(c(t(summary(lag_gmm_sreg)$Coef[1,1:2])), format="f", digits=3)
cmat[1:10, 1] <- formatC(c(t(summary(err_gmm_sreg)$Coef[,1:2])), format="f", digits=3)
cmat[11:12, 1] <- formatC(c(t(c(summary(err_gmm_sreg)$lambda, as.numeric(summary(err_gmm_sreg)$lambda.se)))), format="f", digits=3)
cmat[1:10, 3] <- formatC(c(t(summary(sarar_gmm_sreg)$Coef[-1,1:2])), format="f", digits=3)
cmat[13:14, 3] <- formatC(c(t(summary(sarar_gmm_sreg)$Coef[1,1:2])), format="f", digits=3)
cmat[11, 3] <- formatC(c(summary(sarar_gmm_sreg)$lambda), format="f", digits=3)
even <- (1:nrow(cmat) %% 2) == 0
cmat[even,] <- t(apply(cmat[even,], 1, function(x) { sapply(x, function(y) {ifelse(nzchar(y), paste("(", y, ")", sep=""), y)})}))
rownames(cmat) <- c("(Intercept)", " ", "nondui", "  ", "vehicles", "   ", "dry", "    ", "police", "     ", "$\\rhoerr$", "      ", "$\\rholag$", "       ")
colnames(cmat) <- c("SEM", "SLM", "SARAR")
print(xtable(cmat, align=c("l","r","r","r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,14))


###################################################
### code chunk number 39: sarar model sphet
###################################################
het_old <- sphet::gstslshet(dui ~ nondui + vehicles + dry 
                            + police,  data = shape2, 
                            listw = listw_dui)


###################################################
### code chunk number 40: BMP_submission_0.Rnw:1097-1105
###################################################
cmat <- matrix("", ncol=1, 14)
cmat[1:14, 1] <- formatC(c(t(summary(het_old)$Coef[,1:2])), format="f", digits=3)
even <- (1:nrow(cmat) %% 2) == 0
cmat[even,] <-  apply(as.matrix(cmat[even,]),1, function(y) {ifelse(nzchar(y), paste("(", y, ")", sep=""), y)})
rownames(cmat) <- c("(Intercept)", " ", "nondui", "  ", "vehicles", "   ", "dry", "    ", "police", "     ", "$\\rholag$", "      ", "$\\rhoerr$", "       ")
colnames(cmat) <- c("SARAR Het")
print(xtable(cmat, align=c("l","r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,14))


###################################################
### code chunk number 41: hac stslshac
###################################################
crd_dui <- st_coordinates(st_centroid(shape2))
knn_dui <- spdep::knearneigh(crd_dui, k = 10)
nb_dui <- spdep::knn2nb(knn_dui)
dst_dui <- spdep::nbdists(nb_dui, crd_dui)
k10lw <- spdep::nb2listw(nb_dui, glist = dst_dui, style="B")
class(k10lw) <- "distance"
hac_old <- sphet::stslshac(dui ~ nondui + vehicles + dry 
                            + police,  data = shape2, 
                            listw = listw_dui, distance = k10lw,
                type = "Triangular", bandwidth = "variable")


###################################################
### code chunk number 42: BMP_submission_0.Rnw:1156-1167
###################################################
cmat <- matrix("", ncol=1, 18)
seq1 <- seq(1,18,3)
seq2 <- seq(2,18,3)
seq3 <- seq(3,18,3)
cmat[seq1, 1] <- formatC(c(t(summary(lag_gmm_sreg)$Coef[-1,1]), t(summary(lag_gmm_sreg)$Coef[1,1])), format="f", digits=3)
cmat[seq2, 1] <- formatC(c(t(summary(lag_gmm_sreg)$Coef[-1,2]), t(summary(lag_gmm_sreg)$Coef[1,2])), format="f", digits=3)
cmat[seq3, 1] <- formatC(c(t(summary(hac_old)$Coef[-1,2]), t(summary(hac_old)$Coef[1,2])), format="f", digits=3)
cmat[sort(c(seq2, seq3)),] <-  apply(as.matrix(cmat[sort(c(seq2,seq3)),]),1, function(y) {ifelse(nzchar(y), paste("(", y, ")", sep=""), y)})
row.names(cmat) <- c("(Intercept)", "", " ", "nondui", "  ", "   ", "vehicles", "    ", "     ", "dry", "       ","        ", "police", "         ",  "          ", "$\\rholag$", "            ", "              ")
colnames(cmat) <- c("LAG  HAC")
print(xtable(cmat, align=c("l","r")), floating=FALSE, comment=FALSE,sanitize.text.function=function(x){x}, hline.after=c(-1,0,18))


###################################################
### code chunk number 43: BMP_submission_0.Rnw:1210-1268 (eval = FALSE)
###################################################
## err_eigen <- spatialreg::errorsarlm(dui ~ nondui + vehicles + dry + police, 
##                                     data=shape2, listw=listw_dui, method="eigen", 
##                                     control=list(returnHcov=FALSE))
## saveRDS(err_eigen, file="err_eigen.rds")
## err_Cholesky <- spatialreg::errorsarlm(dui ~ nondui + vehicles + dry + police, 
##                                        data=shape2, listw=listw_dui, method="Matrix", 
##                                        control=list(returnHcov=FALSE))
## saveRDS(err_Cholesky, file="err_Cholesky.rds")
## set.seed(1)
## err_MCMC <- spatialreg::spBreg_err(dui ~ nondui + vehicles + dry + police, 
##                                    data=shape2, listw=listw_dui)
## saveRDS(err_MCMC, file="err_MCMC.rds")
## library(INLA)
## W <- as(listw_dui, "CsparseMatrix")
## n <- nrow(shape2)
## shape2$idx <- 1:n
## zero.variance <- list(prec = list(initial = 15, fixed = TRUE))
## rho.max <- err_eigen$interval[2]
## rho.min <- err_eigen$interval[1]
## args.slm <- list(rho.min = rho.min, rho.max = rho.max, 
##                  W = W, X = matrix(0, n, 0), 
##                  Q.beta = matrix(1, 0, 0))
## hyper.slm <- list(prec = list(prior = "loggamma", 
##                               param = c(0.01, 0.01)), 
##                   rho = list(initial = 0, 
##                              prior = "logitbeta", param = c(1, 1)))
## err_INLA_slm <- inla(dui ~ nondui + vehicles + dry + police + 
##                        f(idx, model="slm", args.slm=args.slm, hyper=hyper.slm),  
##                      data=as.data.frame(shape2), family="gaussian", 
##                      control.family = list(hyper = zero.variance), 
##                      control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE))
## saveRDS(err_INLA_slm, file="err_INLA_slm.rds")
## err_eigen <- readRDS("err_eigen.rds")
## err_Chollesky <- readRDS("err_Cholesky.rds")
## err_MCMC <- readRDS("err_MCMC.rds")
## err_INLA_slm <- readRDS("err_INLA_slm.rds")
## ff <- function(z) { z * (rho.max - rho.min) + rho.min }
## semmarg <- inla.tmarginal(ff, err_INLA_slm$marginals.hyperpar[[2]])
## cmat <- matrix("", ncol=5, nrow=12)
## cmat[1:6, 2] <- formatC(coefficients(err_eigen), format="f", digits=4)
## cmat[7, 2] <- formatC(err_eigen$lambda.se, format="f", digits=4)
## cmat[1:6, 1] <- formatC(coefficients(err_gmm_sreg)[c(6, 1:5)], format="f", digits=4)
## cmat[7, 1] <- formatC(err_gmm_sreg$lambda.se, format="f", digits=4)
## cmat[1:6, 3] <- formatC(coefficients(err_Cholesky), format="f", digits=4)
## cmat[7, 3] <- formatC(err_Cholesky$lambda.se, format="f", digits=4)
## cmat[1:6, 4] <- formatC(summary(err_MCMC)$statistics[c(6, 1:5), 1], format="f", digits=4)
## cmat[7, 4] <- formatC(summary(err_MCMC)$statistics[6, 2], format="f", digits=4)
## cmat[2:6, 5] <- formatC(err_INLA_slm$summary.fixed[,1], format="f", digits=4)
## cmat[c(1, 7), 5] <- formatC(unname(unlist(inla.zmarginal(semmarg, TRUE)[c(1, 2)])), format="f", digits=4)
## cmat[c(9, 10, 12), 5] <- paste(formatC(unname(err_INLA_slm$cpu.used)[1:3], format="f", digits=4), "s", sep="")
## cmat[8, 2] <- format.pval(c(unname(LR1.sarlm(err_eigen)$p.value)), digits=4)
## cmat[8, 3] <- format.pval(c(unname(LR1.sarlm(err_Cholesky)$p.value)), digits=4)
## cmat[c(9, 11, 12), 4] <- paste(formatC(aggregate(attr(err_MCMC, "timings")[,3], list(c(1,1,1,2,3)), sum)$x, format="f", digits=4), "s", sep="")
## cmat[c(9, 10, 12), 3] <- paste(formatC(aggregate(err_Cholesky$timings[,2], list(c(1,1,2,3,3)), sum)$x, format="f", digits=4), "s", sep="")
## cmat[c(9, 10, 12), 2] <- paste(formatC(aggregate(err_eigen$timings[,2], list(c(1,1,2,3,3)), sum)$x, format="f", digits=4), "s", sep="")
## colnames(cmat) <- c("GMM", "eigen", "Cholesky", "MCMC", "INLA")
## rownames(cmat) <- c("$\\rhoerr$", names(coefficients(err_Cholesky))[2:6], "$\\rhoerr$ s.e.", "LR test", "Set up", "Fitting", "Sampling", "Completion")
## saveRDS(cmat, "cmat_ML2.rds")


###################################################
### code chunk number 44: BMP_submission_0.Rnw:1272-1281 (eval = FALSE)
###################################################
## err_eigen <- spatialreg::errorsarlm(dui ~ nondui + vehicles + dry + police, 
##                                     data=shape2, listw=listw_dui, method="eigen", 
##                                     control=list(returnHcov=FALSE))
## err_Cholesky <- spatialreg::errorsarlm(dui ~ nondui + vehicles + dry + police, 
##                                        data=shape2, listw=listw_dui, method="Matrix", 
##                                        control=list(returnHcov=FALSE))
## set.seed(1)
## err_MCMC <- spatialreg::spBreg_err(dui ~ nondui + vehicles + dry + police, 
##                                    data=shape2, listw=listw_dui)


###################################################
### code chunk number 45: BMP_submission_0.Rnw:1293-1309 (eval = FALSE)
###################################################
## library(INLA)
## W <- as(listw_dui, "CsparseMatrix")
## n <- nrow(shape2)
## shape2$idx <- 1:n
## zero.variance <- list(prec = list(initial = 15, fixed = TRUE))
## rho.max <- err_eigen$interval[2]
## rho.min <- err_eigen$interval[1]
## args.slm <- list(rho.min = rho.min, rho.max = rho.max,
##                  W = W, X = matrix(0, n, 0), 
##                  Q.beta = matrix(1, 0, 0))
## hyper.slm <- list(prec = list(prior = "loggamma", param = c(0.01, 0.01)), 
##                   rho = list(initial = 0, prior = "logitbeta", param = c(1, 1)))
## err_INLA_slm <- inla(dui ~ nondui + vehicles + dry + police + f(idx, model="slm", 
##                      args.slm=args.slm, hyper=hyper.slm),  data=as.data.frame(shape2), 
##                      family="gaussian", control.family = list(hyper = zero.variance), 
##                      control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE))


###################################################
### code chunk number 46: BMP_submission_0.Rnw:1319-1322
###################################################
cmat <- readRDS("Data and Shapes/cmat_ML2.rds")
print(xtable(cmat, align=c("l","r","r","r","r","r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,8,12))


###################################################
### code chunk number 47: BMP_submission_0.Rnw:1433-1439
###################################################
p0 <- predict(lm_obj2, newdata=used.cars)
transp1 <- used.cars
transp1$transp <- transp1$transp + 1
p1 <- predict(lm_obj2, newdata=transp1)
mean(p1-p0)
coefficients(lm_obj2)["transp"]


###################################################
### code chunk number 48: BMP_submission_0.Rnw:1446-1450
###################################################
p0_slm <- predict(slm_obj2, newdata=used.cars, listw=lw)
p1_slm <- predict(slm_obj2, newdata=transp1, listw=lw)
mean(p1_slm-p0_slm)
coefficients(slm_obj2)["transp"]


###################################################
### code chunk number 49: BMP_submission_0.Rnw:1464-1468
###################################################
invIrW <- invIrW(lw, rho=coef(slm_obj2)[1])
n <- nrow(used.cars)
S_transp <- invIrW %*% (diag(n) * coef(slm_obj2)["transp"])
sum(c(S_transp))/n


###################################################
### code chunk number 50: BMP_submission_0.Rnw:1477-1479
###################################################
sum(diag(S_transp))/n
impacts(slm_obj2, listw=lw)


###################################################
### code chunk number 51: BMP_submission_0.Rnw:1486-1487
###################################################
impacts(slm_obj2, evalues=eigenw(lw))


###################################################
### code chunk number 52: BMP_submission_0.Rnw:1496-1498
###################################################
impacts(slm_obj2, tr=trW(as(lw, "CsparseMatrix"), m=30, type="mult"))
impacts(slm_obj2, tr=trW(as(lw, "CsparseMatrix"), m=100, type="mult"))


###################################################
### code chunk number 53: lag model sphet
###################################################
err_gmm_sphet <- sphet::spreg(formula = dui ~ nondui + vehicles + dry
                                + police, data = shape2, 
                                 listw = listw_dui,
                                 model = "error")
err_gmm_sphet_e <- sphet::spreg(formula = fm2, data = shape2, 
                                 listw = listw_dui, endog = endog2, 
                                 instruments = instruments2,
                                 model = "error")
lag_gmm_sphet <- sphet::spreg(formula = dui ~ nondui + vehicles + dry
                                + police, data = shape2, 
                                 listw = listw_dui,
                                 model = "lag")
lag_gmm_sphet_e <- sphet::spreg(formula = fm2, data = shape2, 
                                 listw = listw_dui, endog = endog2, 
                                 instruments = instruments2,
                                 model = "lag")
sarar_gmm_sphet <- sphet::spreg(formula = dui ~ nondui + vehicles + dry
                                + police, data = shape2, 
                                 listw = listw_dui,
                                 model = "sarar")
sarar_gmm_sphet_e <- sphet::spreg(formula = fm2, data = shape2, 
                                 listw = listw_dui, endog = endog2, 
                                 instruments = instruments2,
                                 model = "sarar")


###################################################
### code chunk number 54: BMP_submission_0.Rnw:1552-1571
###################################################
cmat <- matrix("", ncol=6, 14)
cmat[1:12, 1] <- formatC(c(t(as.matrix(summary(err_gmm_sphet)$Coef[,1:2]))), format="f", digits=3)
cmat[1:12, 2] <- formatC(c(t(as.matrix(summary(err_gmm_sphet_e)$Coef[,1:2]))), format="f", digits=3)
cmat[1:10, 3] <- formatC(c(t(as.matrix(summary(lag_gmm_sphet)$Coef[-6,1:2]))), format="f", digits=3)
cmat[13:14, 3] <- formatC(c(t(as.matrix(summary(lag_gmm_sphet)$Coef[6,1:2]))), format="f", digits=3)
cmat[1:10, 4] <- formatC(c(t(as.matrix(summary(lag_gmm_sphet_e)$Coef[-6,1:2]))), format="f", digits=3)
cmat[13:14, 4] <- formatC(c(t(as.matrix(summary(lag_gmm_sphet_e)$Coef[6,1:2]))), format="f", digits=3)
cmat[1:10, 5] <- formatC(c(t(as.matrix(summary(sarar_gmm_sphet)$Coef[-c(6,7),1:2]))), format="f", digits=3)
cmat[13:14, 5] <- formatC(c(t(as.matrix(summary(sarar_gmm_sphet)$Coef[6,1:2]))), format="f", digits=3)
cmat[11:12, 5] <- formatC(c(t(as.matrix(summary(sarar_gmm_sphet)$Coef[7,1:2]))), format="f", digits=3)
cmat[1:10, 6] <- formatC(c(t(as.matrix(summary(sarar_gmm_sphet_e)$Coef[-c(6,7),1:2]))), format="f", digits=3)
cmat[13:14, 6] <- formatC(c(t(as.matrix(summary(sarar_gmm_sphet_e)$Coef[6,1:2]))), format="f", digits=3)
cmat[11:12, 6] <- formatC(c(t(as.matrix(summary(sarar_gmm_sphet_e)$Coef[7,1:2]))), format="f", digits=)
even <- (1:nrow(cmat) %% 2) == 0
cmat[even,] <- t(apply(cmat[even,], 1, function(x) { sapply(x, function(y) {ifelse(nzchar(y), paste("(", y, ")", sep=""), y)})}))
rownames(cmat) <- c("(Intercept)", " ", "nondui", "  ", "vehicles", "   ", "dry", "    ", "police", "     ", "$\\rhoerr$", "      ", "$\\rholag$", "       ")
colnames(cmat) <- c("SEM","SEM-end", "SLM", "SLM-end", "SARAR", "SARAR-end")
print(xtable(cmat, align=c("l","r","r","r","r", "r", "r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,14))


###################################################
### code chunk number 55: sem
###################################################
library(splm)
sem <- spml(ricefm, data = RiceFarms, listw = ricelw,
              model="pooling", lag = FALSE, spatial.error = "b")


###################################################
### code chunk number 56: bsktests
###################################################
LMH <- bsktest(ricefm, data = RiceFarms, listw = ricelw, test="LMH")
LM1 <- bsktest(ricefm, data = RiceFarms, listw = ricelw, test="LM1")
LM2 <- bsktest(ricefm, data = RiceFarms, listw = ricelw, test="LM2")
CLMm <- bsktest(ricefm, data = RiceFarms, listw = ricelw, test="CLMmu")
CLMl <- bsktest(ricefm, data = RiceFarms, listw = ricelw, test="CLMlambda")
LMtab <- cbind(c(LMH$statistic, LMH$p.value),
               c(LM1$statistic, LM1$p.value),
               c(LM2$statistic, LM2$p.value),
               c(CLMm$statistic, CLMm$p.value),
               c(CLMl$statistic, CLMl$p.value))
dimnames(LMtab) <- list(c("test", "p-value"), c("LM joint", "LM mu", "LM lambda", "CLM mu", "CLM lambda"))
round(LMtab, 5)


###################################################
### code chunk number 57: localCD
###################################################
library(plm)
CDp.pool <- pcdtest(plm(ricefm, data = RiceFarms, model='pooling'),
               w = listw2mat(ricelw), test='cd')
CDp.FE <- pcdtest(plm(ricefm, data = RiceFarms, model='within'),
               w = listw2mat(ricelw), test='cd')
CDp.RE <- pcdtest(plm(ricefm, data = RiceFarms, model='random'),
               w = listw2mat(ricelw), test='cd')
CDtab <- cbind(c(CDp.pool$statistic, CDp.pool$p.value),
               c(CDp.FE$statistic, CDp.FE$p.value),
               c(CDp.RE$statistic, CDp.RE$p.value))
dimnames(CDtab) <- list(c("test", "p-value"), c("Pooled","Fixed","Random"))
round(CDtab, 5)


###################################################
### code chunk number 58: sarre
###################################################
sarre <- spml(ricefm, data = RiceFarms, listw = ricelw,
              model="random", lag = TRUE, spatial.error = "none")


###################################################
### code chunk number 59: semfe
###################################################
library(splm)
semfe <- spml(ricefm, data = RiceFarms, listw = ricelw,
              lag = FALSE, spatial.error = "b")


###################################################
### code chunk number 60: saremfe
###################################################
saremfe <- spml(ricefm, data = RiceFarms, listw = ricelw,
              lag = TRUE, spatial.error = "b")
summary(saremfe)$CoefTable[1:2, ]


###################################################
### code chunk number 61: semre
###################################################
library(splm)
semre <- spml(ricefm, data = RiceFarms, listw = ricelw,
              model="random", lag = FALSE, spatial.error = "b")


###################################################
### code chunk number 62: sem2re
###################################################
sem2re <- spml(ricefm, data = RiceFarms, listw = ricelw,
              model="random", lag = FALSE, spatial.error = "kkp")


###################################################
### code chunk number 63: saremre
###################################################
saremre <- spml(ricefm, data = RiceFarms, listw = ricelw,
              model="random", lag = TRUE, spatial.error = "b")


###################################################
### code chunk number 64: bsjktests
###################################################
J <- bsjktest(ricefm, data = RiceFarms, listw = ricelw, test="J")
C.1 <- bsjktest(ricefm, data = RiceFarms, listw = ricelw, test="C.1")
C.2 <- bsjktest(ricefm, data = RiceFarms, listw = ricelw, test="C.2")
C.3 <- bsjktest(ricefm, data = RiceFarms, listw = ricelw, test="C.3")
LMtab2 <- cbind(c(J$statistic, J$p.value),
               c(C.1$statistic, C.1$p.value),
               c(C.2$statistic, C.2$p.value),
               c(C.3$statistic, C.3$p.value))
dimnames(LMtab2) <- list(c("test", "p-value"), c("LM joint", "CLM lambda", "CLM psi", "CLM mu"))
round(LMtab2, 5)


###################################################
### code chunk number 65: saremsrre
###################################################
saremsrre <- spreml(ricefm, data = RiceFarms, w = riceww,
                    lag = TRUE, errors = "semsrre")


###################################################
### code chunk number 66: sarem2srre
###################################################
sarem2srre <- spreml(ricefm, data = RiceFarms, w = riceww,
                    lag = TRUE, errors = "sem2srre")


###################################################
### code chunk number 67: Baltagi2
###################################################
ec2sls <- spgm(fm4, data = data_cor, listw = nc_listw, 
model = "random", lag = FALSE, spatial.error = FALSE, 
endog = endog4, instruments = instruments4, method = "ec2sls" )
sarar_ec <-spgm(fm4, data = data_cor, lag = TRUE, listw = nc_listw, 
endog = endog4, instruments = instruments4, spatial.error = TRUE, 
model = "random", optim.method = "nlminb", pars = c(0.2, 1), 
method = "ec2sls")


###################################################
### code chunk number 68: BMP_submission_0.Rnw:2123-2135
###################################################
library(xtable)
cmat <- matrix("", ncol=4, nrow=28)
cmat[1:27, 1] <- formatC(c(t(summary(ec2sls)$Coef[,1])), format="f", digits=3)
cmat[1:27, 2] <- paste("(", formatC(c(t(summary(ec2sls)$Coef[,2])), format="f", digits=3), ")", sep="")
cmat[1:27, 3] <- formatC(c(t(summary(sarar_ec)$Coef[-3, 1])), format="f", digits=3)
cmat[1:27, 4] <- paste("(", formatC(c(t(summary(sarar_ec)$Coef[-3, 2])), format="f", digits=3), ")", sep="")
cmat[28, 3] <- formatC(c(t(summary(sarar_ec)$Coef[3,1])), format="f", digits=3)
cmat[28, 4] <- paste("(", formatC(c(t(summary(sarar_ec)$Coef[3,2])), format="f", digits=3), ")", sep="")
rownames(cmat) <- c(rownames(summary(ec2sls)$Coef),"$\\rholag$")
colnames(cmat) <- c("EC2SLS", "(std. err.)", "Spatial EC2SLS", "(std. err.)")
print(xtable(cmat, align=c("l","r","r","r","r")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,28))


###################################################
### code chunk number 69: BMP_submission_0.Rnw:2163-2170
###################################################
used.cars$State <- as.factor(used.cars$State)
names(nb) <- as.character(used.cars$State)
N <- nrow(used.cars)
library(mgcv)
gam_obj2 <- gam(av55_59 ~ transp + salesTax + s(State, bs="mrf", k=N-2, xt=list(nb=nb)), 
                data = used.cars)
printCoefmat(summary(gam_obj2)$p.table)


###################################################
### code chunk number 70: BMP_submission_0.Rnw:2182-2187
###################################################
library(hglm)
hglm_obj2 <- hglm(fixed=av55_59 ~ transp + salesTax, random= ~ 1|State, 
                  data=used.cars, 
                  rand.family=SAR(D=as(lw, "CsparseMatrix")))
printCoefmat(summary(hglm_obj2)$FixCoefMat)


###################################################
### code chunk number 71: BMP_submission_0.Rnw:2193-2199
###################################################
sf0 <- used.cars_laea
sf0$hglm_sar <- unname(summary(hglm_obj2, print.ranef=TRUE)$RandCoefMat)[,1]
sf0$GAM_mrf_re <- predict(gam_obj2, type="terms", se=TRUE)$fit[,3]
tm_shape(sf0) + tm_fill(c("hglm_sar", "GAM_mrf_re"), midpoint=c(0), n=8, palette="BrBG", title="1960 USD RE") + tm_facets(free.scales=FALSE) + tm_borders(lwd=0.5) + tm_layout(panel.labels=c("HGLM SAR RE", "GAM MRF RE"), bg="grey90")


###################################################
### code chunk number 72: BMP_submission_0.Rnw:2258-2262
###################################################
(SF0 <- SpatialFiltering(av55_59 ~ transp + salesTax, data = used.cars, nb=nb, style="W"))
SF_obj2 <- lm(av55_59 ~ transp + salesTax + fitted(SF0), data = used.cars)
summary(SF_obj2)
lm.morantest(SF_obj2, lw)


###################################################
### code chunk number 73: BMP_submission_0.Rnw:2268-2275
###################################################
vecs <- fitted(SF0)
pvecs <- vecs %*% diag(coefficients(SF_obj2)[4:7])
colnames(pvecs) <- colnames(vecs)
sf1 <- cbind(used.cars_laea, pvecs)
tm_shape(sf1) + tm_fill(c("vec2", "vec1", "vec4", "vec6"), n=8, palette="BrBG", midpoint=0, title="1960 USD") + tm_facets(free.scales=FALSE) + tm_borders(lwd=0.5) + tm_layout(panel.labels=c("EV 2", "EV 1", "EV 4", "EV 6"), bg="grey90")


###################################################
### code chunk number 74: BMP_submission_0.Rnw:2295-2302
###################################################
set.seed(22)
ME0 <- ME(av55_59 ~ transp + salesTax, data = used.cars, listw=lw, alpha=0.15, nsim=99)
ME_obj2 <- lm(av55_59 ~ transp + salesTax + fitted(ME0), data = used.cars)
mvecs <- fitted(ME0) %*% diag(coefficients(ME_obj2)[4:6])
sem_obj2a <- spautolm(av55_59 ~ transp + salesTax, used.cars, listw=lw)
W <- as(lw, "CsparseMatrix")
SAR_ssre <- 2*as.vector((sem_obj2a$lambda * W) %*% sem_obj2a$Y - (sem_obj2a$lambda * W) %*% (sem_obj2a$X %*% sem_obj2a$fit$coefficients))


###################################################
### code chunk number 75: BMP_submission_0.Rnw:2316-2322
###################################################
library(spmoran)
centroids_laea <- st_centroid(st_geometry(used.cars_laea))
meig <- meigen(st_coordinates(centroids_laea))
y <- model.response(model.frame(av55_59 ~ transp + salesTax, used.cars))
X <- model.matrix( ~ transp + salesTax, used.cars)
(esf_obj2 <- esf(y, X, meig=meig))


###################################################
### code chunk number 76: BMP_submission_0.Rnw:2328-2334
###################################################
evecs <- meig$sf[,c(2, 4, 1, 6)] %*% diag(esf_obj2$r[1:4,1])
colnames(evecs) <- rownames(esf_obj2$r)[1:4]
sf2 <- cbind(sf1, evecs)
tm_shape(sf2) + tm_fill(c("sf2", "sf4", "sf1", "sf6"), n=8, palette="BrBG", midpoint=0, title="1960 USD") + tm_facets(free.scales=FALSE) + tm_borders(lwd=0.5) + tm_layout(panel.labels=c("EV 2", "EV 4", "EV 1", "EV 6"), bg="grey90")


###################################################
### code chunk number 77: BMP_submission_0.Rnw:2351-2357
###################################################
mat <- cor(cbind(sf0$hglm_sar, sf0$GAM_mrf_re, apply(evecs, 1, sum), apply(pvecs, 1, sum), apply(mvecs, 1, sum), SAR_ssre))
colnames(mat) <- rownames(mat) <- c("HGLM", "GAM", "SF", "ESF", "ME", "SAR")
suppressPackageStartupMessages(library(xtable))
print(xtable(mat, align=c("l","r","r","r","r","r","r"), digits=c(NA, 4, 4, 4, 4, 4, 4), 
display=c("s", "f", "f", "f", "f", "f", "f")), floating=FALSE, comment=FALSE, 
sanitize.text.function=function(x){x}, hline.after=c(-1,0,6))

sessionInfo()
sf_extSoftVersion()
sink(type = "message")
sink()
