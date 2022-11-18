source("R/saposim.R")
source("R/funcoes_auxiliares.R")

# simulacao com valores default: vecarea = round(runif(nsim, 10, 100), 0), vecnmacho= vecarea, cffreq = 1.05, cftama = 0.5, cffoot = 0.8
# nsim foi colocado como 10000 para ver se o modelo misto converge
sim_Afix = saposim(nsim = 10000)
save(sim_Afix, file = "data/sim_Afix.Rdata")

#simulacao mais simples, com area constante em 10
# nsim foi colocado como 10000 para ver se o modelo misto para comparacao com simulacao anterior
sim_Avar10 = saposim(nsim = 10000, vecarea = rep(10, times = 10000))
save(sim_Avar10, file = "data/sim_Avar10.Rdata")

#simulacao mais simples, com area constante em 50
# nsim foi colocado como 10000 para ver se o modelo misto para comparacao com simulacao anterior
sim_Avar50 = saposim(nsim = 10000, vecarea = rep(50, times = 10000))
save(sim_Avar, file = "data/sim_Avar50.Rdata")

#simulacao mais simples, com area constante em 100
# nsim foi colocado como 10000 para ver se o modelo misto para comparacao com simulacao anterior
sim_Avar10 = saposim(nsim = 10000, vecarea = rep(100, times = 10000))
save(sim_Avar10, file = "data/sim_Avar10.Rdata")