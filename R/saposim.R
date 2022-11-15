# Simula processo de selecao sexual de femeas de sapos
# Args:
## nsim - numero de simulacoes
## vecarea = vetor com tamanho de area de cara uma das simulacoes. area se refere ao numero de linhas e colunas da matriz de espaco area x area.
## vecnmacho - numero de machos por simulacao
## cffreq - coeficiente para a frequencia de canto
## cftama - coeficiente para tamanho do macho
## cffoot - coeficiente para taxa de footflag
## todos coeficientes sao usados para o calculo de probabilidade de selecao do macho.


saposim = function(nsim = 1000, vecarea = round(runif(nsim, 10, 100), 0), vecnmacho= vecarea, cffreq = 1.05, cftama = 0.5, cffoot = 0.8){
  
  #checagem de input
  if(length(vecarea) != nsim){stop("vecarea deve ser um vetor de tamanho nsim")}
  if(length(vecnmacho) != nsim){stop("vecnmacho deve ser um vetor de tamanho nsim")}
  
  #Cria objetos que vao ser usados no for
  result = data.frame(machoid = NA, dcach = NA, foot = NA, freq = NA, tama = NA, footpad = NA, freqpad = NA, tamapad = NA, exprlin = NA, prob = NA, distf = NA, ttot = NA, nsim = NA, femea = NA, area = NA)
  posf = data.frame(posx = NA, posy = NA, nfemea = NA) #atualmente nao esta como output
  storepos = list(NA) #atualmente nao esta como output
  
  ###############################################
  ############ INICIO FOR EXTERNO ###############
  ###############################################
  
  for(k in 1:nsim){
    
    #nmachos e area para esse espaco
    nmacho = vecnmacho[k]
    area = vecarea[k]
    
    #### Conformacao inicial do espaco ####
    # Simula espaço contendo areaXarea grids
    espaco = matrix(NA, ncol = area, nrow = area)
    dimnames(espaco) = list(paste("L", 1:area), paste("C", 1:area))
    
    # Simula spawn de 10 machos
    xmacho = sample(1:area, nmacho)
    ymacho = sample(1:area, nmacho)
    
    for(i in 1:nmacho){
      espaco[ymacho[i],xmacho[i]] = paste0("m", i)
    }
    espaco_orig = espaco
    
    #### Probabilidade de escolha dos machos ####
    dados = data.frame(machoid = paste0("m", 1:nmacho), dcach = NA, foot = NA, freq = NA, tama = NA, footpad = NA, freqpad = NA, tamapad = NA, exprlin = NA, prob = NA, distf = NA, ttot = NA, nsim = NA, femea = NA, area = NA)
    
    ##### simula caracteristicas dos machos #####
    dados$dcach = area-xmacho
    
    mfoot = 10 + dados$dcach*(-0.5)
    mfoot[mfoot<0] = 0
    
    sfreq = dados$dcach*(10*(1/area))
    mfreq = 2000 - mean(sfreq) +sfreq
    
    stama = dados$dcach*(0.5*(1/area))
    mtama = 40.4 - mean(stama) + stama
    
    dados$foot = rpois(nmacho, lambda = mfoot)
    dados$freq = rnorm(nmacho, mean = mfreq, sd = 200)
    dados$tama = rnorm(nmacho, mean = mtama, sd = 2)
    
    dados$footpad = padroniza(dados$foot)
    dados$tamapad = padroniza(dados$tama)  
    dados$freqpad = padroniza(dados$freq)
    
    ##### logistica #####
    
    dados$exprlin = cftama*dados$tamapad + cffoot*dados$footpad + cffreq*dados$freqpad
    dados$exprlin = dados$exprlin - mean(dados$exprlin)
    dados$prob = exp(dados$exprlin)/ (1+exp(dados$exprlin))
    dados$prob
    
    mdisp = dados$machoid
    mindisp = NA
    
    ####################################################
    ################ INICIO FOR INTERNO ################
    ####################################################
    
    for(j in 1:nmacho){
      
      # Simula spawn de 1 femea
      xfemea = sample(1:area, 1)
      yfemea = sample(1:area, 1)
      
      
      femeas = paste0("f", c(paste0(0, 1:9), 10:nmacho))
      posf[j,] = c(xfemea, yfemea, femeas[j])
      
      #checa se cedula esta ocupada
      while(!is.na(espaco[yfemea,xfemea])){
        xfemea = sample(1:area, 1)
        yfemea = sample(1:area, 1)
      }
      espaco[yfemea,xfemea] = "f"
      
      #### Sorteando macho escolhido ####
      escolhido = sample(mdisp, 1, prob = dados$prob[dados$machoid %in% mdisp])
      
      #### salvando resultados ####
      distf = dist(posicao("f", espaco), posicao(escolhido, espaco))
      dados$distf[dados$machoid == escolhido] = distf
      dados$ttot[dados$machoid == escolhido] = sum(dados$distf, na.rm=TRUE)
      dados$femea[dados$machoid == escolhido] = femeas[j]
      dados$nsim = paste0("sim", k)
      dados$area = area
      
      dados = dados[order(dados$femea),]
      
      #### Voltando ao espaco inicial sem macho escolhido ####
      espaco[which(espaco == "f")] = NA
      espaco[which(espaco == escolhido)] = NA
      mindisp[j] = escolhido
      mdisp = dados$machoid[!dados$machoid %in% mindisp]
    }
    result = rbind(result, dados)
    storepos[[k]] =list(espaco_orig, posf)
    
    if(k %in% seq(1, nsim, 10)){
      paste("simulacao", k, "de", nsim, "concluida")
    }
    
  }
  result = result[-1,]
  result$femeasim = paste(result$femea, result$nsim, sep = "_")
  row.names(result) = 1:length(result$area)
  return(result)
}