# Script to regenerate and check analysis by Qin et al.

require(metafor)
data_Qin_IL6 <- read.csv2("C:/Users/Gustav Nilsonne/Box Sync/Gustavs_arbete/Pek/IL-6 och Parkinsons/IL6_Parkinson/data_Qin_IL6.csv")
data_Qin_IL6[, 2] <- as.numeric(as.character(data_Qin_IL6[, 2]))
data_Qin_IL6[, 3] <- as.numeric(as.character(data_Qin_IL6[, 3]))
data_Qin_IL6[, 4] <- as.numeric(as.character(data_Qin_IL6[, 4]))
data_Qin_IL6[, 5] <- as.numeric(as.character(data_Qin_IL6[, 5]))
data_Qin_IL6[, 6] <- as.numeric(as.character(data_Qin_IL6[, 6]))
data_Qin_IL6[, 7] <- as.numeric(as.character(data_Qin_IL6[, 7]))
data_Qin_IL6[, 8] <- as.numeric(as.character(data_Qin_IL6[, 8]))
data_Qin_IL6$ci <- data_Qin_IL6$upper - data_Qin_IL6$g
data_Qin_IL6$sei <- data_Qin_IL6$ci/1.96
data_Qin_IL6$age_diff <- data_Qin_IL6$age_mean_PD - data_Qin_IL6$age_mean_HC

# Meta-analysis with DerSimonian-Laird estimator, as in original study
res <- rma(yi = g, sei = sei, data = data_Qin_IL6, slab=study, measure = "SMD", method = "DL")
forest(res)

# Meta-analysis with REML estimator, which may be  more appropriate
res2 <- rma(yi = g, sei = sei, data = data_Qin_IL6, slab=study, measure = "SMD", method = "REML")
forest(res2)

# Meta-regression on age difference
res3 <- rma(yi = g, sei = sei, mods = ~age_diff, data = data_Qin_IL6, slab=study, measure = "SMD", method = "REML")
forest(res3)

# Meta-regression scatterplot
preds <- predict(res3, newmods=c(-5:8))
size <- weights(res3)
plot(data_Qin_IL6$age_diff, data_Qin_IL6$g, cex=size/3, 
     xlab="Age difference (years)", ylab="g",
     las=1, bty="l")
lines(-5:8, preds$pred)
lines(-5:8, preds$ci.lb, lty="dashed")
lines(-5:8, preds$ci.ub, lty="dashed")
