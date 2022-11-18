source("R/saposim.R")
source("R/funcoes_auxiliares.R")

# simulacao com valores default: vecarea = round(runif(nsim, 10, 100), 0), vecnmacho= vecarea, cffreq = 1.05, cftama = 0.5, cffoot = 0.8
# nsim foi colocado como 10000 para ver se o modelo misto converge
sim_Avar = saposim(nsim = 10000)
save(sim_Avar, file = "data/sim_Avar.Rdata")

#simulacao mais simples, com area constante em 10
# nsim foi colocado como 10000 para ver se o modelo misto para comparacao com simulacao anterior
sim_Afix10 = saposim(nsim = 10000, vecarea = rep(10, times = 10000))
save(sim_Afix10, file = "data/sim_Afix10.Rdata")

#simulacao mais simples, com area constante em 50
# nsim foi colocado como 10000 para ver se o modelo misto para comparacao com simulacao anterior
sim_Afix50 = saposim(nsim = 10000, vecarea = rep(50, times = 10000))
save(sim_Afix50, file = "data/sim_Afix50.Rdata")

#simulacao mais simples, com area constante em 100
# nsim foi colocado como 10000 para ver se o modelo misto para comparacao com simulacao anterior
sim_Afix100 = saposim(nsim = 10000, vecarea = rep(100, times = 10000))
save(sim_Afix100, file = "data/sim_Afix100.Rdata")