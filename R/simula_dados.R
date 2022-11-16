source("R/saposim.R")
source("R/funcoes_auxiliares.R")


# simulacao com valores default: vecarea = round(runif(nsim, 10, 100), 0), vecnmacho= vecarea, cffreq = 1.05, cftama = 0.5, cffoot = 0.8
# nsim foi colocado como 10000 para ver se o modelo misto converge
sim1 = saposim(nsim = 10000)
save(sim1, file = "data/sim1.Rdata")

#simulacao mais simples, com area constante em 100
sim2 = saposim(nsim = 1000, vecarea = rep(100, times = 1000))
save(sim2, file = "data/sim2.Rdata")

