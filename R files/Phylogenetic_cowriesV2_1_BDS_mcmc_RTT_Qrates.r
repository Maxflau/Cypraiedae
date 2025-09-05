library(scales)


pdf(file='/Users/max/Desktop/Phylogenetic_cowries/pyrate_mcmc_logs/Phylogenetic_cowriesV2_1_BDS_mcmc_RTT_Qrates.pdf',width=0.6*9, height=0.6*7)

age = c(-147.73155615310438, -145.0)
Q_mean = 0.49482880784903727
Q_hpd_m = 0.46540890595884277
Q_hpd_M = 0.5228402171614409
plot(age,age,type = 'n', ylim = c(0, 0.554988966830221), xlim = c(-147.73155615310438,-0.0), ylab = 'Preservation rate', xlab = 'Ma',main='Preservation rates' )
segments(x0=age[1], y0 = Q_mean, x1 = age[2], y1 = Q_mean, col = "#756bb1", lwd=3)
polygon( c(age, rev(age)), c(Q_hpd_m, Q_hpd_m, Q_hpd_M, Q_hpd_M), col = alpha("#756bb1",0.5), border = NA)
n <- dev.off()