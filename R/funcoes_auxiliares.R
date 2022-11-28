
# posicao da matrix retorna contagem linear por coluna. Essa funcao transforma essa contagem em 2 dimensoes c(linha, coluna)
posicao = function(esc, espaco){
  if(!any(espaco == esc, na.rm = TRUE)){
    stop(paste0("nao existe o sapo <", esc, "> no espaco"))
  }
  area = dim(espaco)[1]
  posesc = which(espaco == esc)
  inicol = seq(1, area^2, by = area)
  fimcol = seq(area, area^2, by = area)
  whcol = posesc >= inicol & posesc <= fimcol
  col = which(whcol == TRUE)
  
  row = posesc - area*(col-1)
  res = c(row, col)
  names(res) = c("y.row", "x.col")
  return(res)
}

#calcula a distancia entre dois sapos
dist = function(posf, posesc){
  xf = posf[2]
  yf = posf[1]
  xm = posesc[2]
  ym = posesc[1]
  d = sqrt((xm - xf)^2 + (ym - yf)^2)
  return(unname(d))
}

#padronizacao das preditoras (escala de desvio padrao)
padroniza = function(x){
  (x - mean(x))/sd(x)
}

#transforma logit em probabilidade
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#plots para analise exploratoria
AED = function(dados, binw){

  # 1. Scatter Plots
  
  ## 1.1 Tempo ate amplexo vs DF
  p1 = ggplot(dados, aes(freq, ttot, color=freqpad)) +
    geom_point(shape=16, size=.05, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nFrequência dominante (Hz)") +
    ylab("Tempo até início da cópula\n")
  
  ## 1.2 Tempo ate amplexo vs SVL
  p2 = ggplot(dados, aes(tama, ttot, color=tamapad)) +
    geom_point(shape=16, size=.05, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nTamanho corporal (mm)") +
    ylab("Tempo até início da cópula\n")
  
  ## 1.3 Tempo ate amplexo vs taxa de foot-flagging
  p3 = ggplot(dados, aes(foot, ttot, color=footpad)) +
    geom_point(shape=16, size=.2, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nTaxa de foot-flagging (n eventos/min)") +
    ylab("Tempo até início da cópula\n")
  
  ## 1.4 Tempo ate amplexo vs Prob. do macho ser escolhido
  p4 = ggplot(dados, aes(prob, ttot, color=prob)) +
    geom_point(shape=16, size = .2, show.legend = F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nProb. do macho ser escolhido") +
    ylab("Tempo até início da cópula\n")
  
  # 2. Histograma de tempo ate inicio da copula
  hist <- ggplot(dados, aes(x=ttot)) +
    geom_histogram(binwidth = binw, fill="#55a370", color = "darkslategrey", alpha=0.9) +
    xlab("\nTempo até início da cópula") +
    ylab("Número de machos\n") +
    theme_minimal()
  
  grid.arrange(arrangeGrob(p1, p2, p3, p4, ncol=2, nrow=2), arrangeGrob(hist, ncol=1, nrow=1))
}


#plots para linha de tendencia dos modelos
plot_tendencia = function(dados, modelo){
  
  coef = c(modelo@beta[1], modelo@beta[2], modelo@beta[3], modelo@beta[4])
  names(coef) = c("inter", "tama", "freq", "foot")
  ptama = dados$tamapad*coef["tama"] + coef["inter"]
  pfreq = dados$freqpad*coef["freq"] + coef["inter"]
  pfoot = dados$footpad*coef["foot"] + coef["inter"]
  predlin = data.frame(ptama, pfreq, pfoot)
  pred = exp(predlin)
  
  # 1. Scatter Plots
  
  ## 1.1 Tempo ate amplexo vs DF
  p1 = ggplot(dados, aes(freqpad, ttot, color=freqpad)) +
    geom_point(shape=16, size=.05, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nFrequência dominante (Hz)\n(padronizada)") +
    ylab("Tempo até início da cópula\n") +
    geom_line(aes(y = pred$pfreq), size = 1, color = "black", linetype="dashed")
  
  ## 1.2 Tempo ate amplexo vs SVL
  p2 = ggplot(dados, aes(tamapad, ttot, color=tamapad)) +
    geom_point(shape=16, size=.05, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nTamanho corporal (mm)\n(padronizado)") +
    ylab("Tempo até início da cópula\n") +
    geom_line(aes(y = pred$ptama), size = 1, color = "black", linetype="dashed")
  
  ## 1.3 Tempo ate amplexo vs taxa de foot-flagging
  p3 = ggplot(dados, aes(footpad, ttot, color=footpad)) +
    geom_point(shape=16, size=.2, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nTaxa de foot-flagging\n(n eventos/min) (padronizada)") +
    ylab("Tempo até início da cópula\n") +
    geom_line(aes(y = pred$pfoot), size = 1, color = "black", linetype="dashed")
  grid.arrange(p1, p2, p3, nrow = 1)
}

# plot_tendencia_glm = function(dados, modlog, modinv){
#   
#   coeflog = summary(modlog)$coefficients[1:4,1]
#   ptamalog = dados$tamapad*coeflog["tamapad"] + coeflog["(Intercept)"]
#   pfreqlog = dados$freqpad*coeflog["freqpad"] + coeflog["(Intercept)"]
#   pfootlog = dados$footpad*coeflog["footpad"] + coeflog["(Intercept)"]
#   predlinlog = data.frame(ptamalog, pfreqlog, pfootlog)
#   predlog = exp(predlinlog)
#   
#   coefinv = summary(modinv)$coefficients[1:4,1]
#   ptamainv = dados$tamapad*coefinv["tamapad"] + coefinv["(Intercept)"]
#   pfreqinv = dados$freqpad*coefinv["freqpad"] + coefinv["(Intercept)"]
#   pfootinv = dados$footpad*coefinv["footpad"] + coefinv["(Intercept)"]
#   predlininv = data.frame(ptamainv, pfreqinv, pfootinv)
#   predinv = 1/(predlininv)
#   
#   
#   # 1. Scatter Plots
#   
#   ## 1.1 Tempo ate amplexo vs DF
#   p1 = ggplot(dados, aes(freqpad, ttot, color=freqpad)) +
#     geom_point(shape=16, size=.05, show.legend=F, alpha = .5) +
#     theme_minimal() +
#     scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
#     xlab("\nFrequência dominante (Hz)\n(padronizada)") +
#     ylab("Tempo até início da cópula\n") +
#     geom_line(aes(y = predlog$pfreq), size = 1, color = "black", linetype="dashed") +
#     geom_line(aes(y = predinv$pfreq), size = 1, color = "red", linetype="dashed")
#   
#   ## 1.2 Tempo ate amplexo vs SVL
#   p2 = ggplot(dados, aes(tamapad, ttot, color=tamapad)) +
#     geom_point(shape=16, size=.05, show.legend=F, alpha = .5) +
#     theme_minimal() +
#     scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
#     xlab("\nTamanho corporal (mm)\n(padronizado)") +
#     ylab("Tempo até início da cópula\n") +
#     geom_line(aes(y = predlog$ptama), size = 1, color = "black", linetype="dashed") +
#     geom_line(aes(y = predinv$ptama), size = 1, color = "red", linetype="dashed")
#   
#   ## 1.3 Tempo ate amplexo vs taxa de foot-flagging
#   p3 = ggplot(dados, aes(footpad, ttot, color=footpad)) +
#     geom_point(shape=16, size=.2, show.legend=F, alpha = .5) +
#     theme_minimal() +
#     scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
#     xlab("\nTaxa de foot-flagging\n(n eventos/min) (padronizada)") +
#     ylab("Tempo até início da cópula\n") +
#     geom_line(aes(y = predlog$pfoot), size = 1, color = "black", linetype="dashed") +
#     geom_line(aes(y = predinv$pfoot), size = 1, color = "red", linetype="dashed")
#   grid.arrange(p1, p2, p3, nrow = 1)
# }

#plot for 95% confidence interval
plotConf = function(model){
  # Dataframe with 95% confidence intervals
  conf = as.data.frame(confint(model, method="Wald"))
  conf = conf[-(1:3),]
  conf$coef = model@beta[-1]
  conf = tibble::rownames_to_column(conf, "Predictor")
  colnames(conf)[c(2,3)] = c("min25", "max975")

  # ggplot
  ggplot(conf, aes(x=coef, y=Predictor)) +
  geom_point() +
  geom_errorbar(aes(xmin=min25, xmax=max975), width=0.01)+
  xlab("\nIntervalos de confiança (95%)")+
  geom_vline(xintercept=0, colour="red")
}


# plotDiag = function(modelo){
#   colvec = #ff1111"
#     p1 = plot(modelo,type=c("p","smooth"))
#     p2 = plot(modelo,sqrt(abs(resid(.)))~fitted(.),
#                     col=colvec,
#                     type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))
#     p3 = plot(modelo,resid(.,type="pearson")~footpad,
#                     type=c("p","smooth"))
#     re<-ranef(modelo,condVar = TRUE)[[1]]
#     p4 = qqmath(~re);
#     abline(0,1)
#     
#     grid.arrange(p1, p2, p3)#, p4)
# }
