# Script to regenerate and check analysis by Qin et al.

# 1. Attempt to regenerate original forest plot
require(metafor)
data_Qin <- read.csv("C:/Users/Gustav Nilsonne/Box Sync/Gustavs_arbete/Pek/IL-6 och Parkinsons/IL6_Parkinson/data_Qin.csv")
data_Qin$ci <- data_Qin$upper - data_Qin$g
data_Qin$sei <- data_Qin$ci/1.96

res <- rma(yi = g, sei = sei, data = data_Qin[data_Qin$study != "Overall", ], slab=study, measure = "SMD")
forest(res)
funnel(res, xlab = "g")
trimfill(res)

# 2. Re-code data from included papers
# In progress

# Brodacki 2008
# http://www.sciencedirect.com/science/article/pii/S0304394008008756
# Can be estimated from plot (fig 1), pooling both PD groups and converting from mol/ml to pg/µl

data_new <- data.frame(study = "Brodacki 2008", m1i = )

# Bu 2015
# http://ac.els-cdn.com/S1353802015002333/1-s2.0-S1353802015002333-main.pdf?_tid=f6c2f0d8-8974-11e6-ac0c-00000aab0f6b&acdnat=1475504793_c365b9a48bde9c2a8ff6b7bc914e8459
# Data given in table 1
# Transformation according to Higgins
m1 <- log(34.56) - 0.5*log(((27.05^2)/(34.56^2))+1)
m2 <- log(44.09) - 0.5*log(((32.7^2)/(44.09^2))+1)
sd1 <- sqrt(log(((27.05^2)/(34.56^2))+1))
sd2 <- sqrt(log(((32.7^2)/(44.09^2))+1))
n1 <- 141
n2 <- 131
es_bu <- escalc(measure = "SMD", m1i = m1, m2i = m2, sd1i = sd1, sd2i = sd2, n1i = n1, n2i = n2)
data_new <- rbind(data_new), data.frame(study = "Bu 2015", )

# 
