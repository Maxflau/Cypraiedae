
    setwd("/Users/max/Desktop/Phylogenetic_cowries/pyrate_mcmc_logs")
    tbl = read.table(file = "Phylogenetic_cowriesV2_1_Grj_se_est_ltt_ltt.txt",header = T)
    pdf(file='Phylogenetic_cowriesV2_1_Grj_se_est_ltt_ltt.pdf',width=12, height=9)
    time = -tbl$time
    library(scales)
    plot(time,tbl$diversity, type="n",ylab= "Number of lineages (Log10)", xlab="Time (Ma)", main="Range-through diversity through time", ylim=c(-inf,2.663700925389648),xlim=c(min(time),0))
    polygon(c(time, rev(time)), c(tbl$M_div, rev(tbl$m_div)), col = alpha("#504A4B",0.5), border = NA)
    lines(time,tbl$diversity, type="l",lwd = 2)
    n<-dev.off()
    