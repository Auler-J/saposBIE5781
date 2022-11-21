source("R/saposim.R")
source("R/funcoes_auxiliares.R")

#valores defaut:
#vecnmacho= vecarea, cffreq = 1.05, cftama = 0.5, cffoot = 0.8, nsim = 1000

# simulacao com vecarea default: vecarea = round(runif(nsim, 10, 100), 0)
sim_Avar = saposim()
save(sim_Avar, file = "data/sim_Avar.Rdata")

#simulacao mais simples, com area constante em 10
sim_Afix10 = saposim(vecarea = rep(10, times = 1000))
save(sim_Afix10, file = "data/sim_Afix10.Rdata")

#simulacao mais simples, com area constante em 50

sim_Afix50 = saposim(vecarea = rep(50, times = 1000))
save(sim_Afix50, file = "data/sim_Afix50.Rdata")

#simulacao mais simples, com area constante em 100
sim_Afix100 = saposim(vecarea = rep(100, times = 1000))
save(sim_Afix100, file = "data/sim_Afix100.Rdata")