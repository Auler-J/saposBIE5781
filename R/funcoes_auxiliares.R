
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
  
  grid.arrange(p1, p2, p3, p4, hist, nrow = 2)
}

#plots para linha de tendencia dos modelos
linhatendencia = function(dados, pred){
  
  # 1. Scatter Plots
  
  ## 1.1 Tempo ate amplexo vs DF
  p1 = ggplot(dados, aes(freqpad, ttot, color=freqpad)) +
    geom_point(shape=16, size=.05, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nFrequência dominante (Hz)") +
    ylab("Tempo até início da cópula\n") +
    geom_line(aes(y = pred), size = 1)
  
  ## 1.2 Tempo ate amplexo vs SVL
  p2 = ggplot(dados, aes(tamapad, ttot, color=tamapad)) +
    geom_point(shape=16, size=.05, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nTamanho corporal (mm)") +
    ylab("Tempo até início da cópula\n") +
    geom_line(aes(y = pred), size = 1)
  
  ## 1.3 Tempo ate amplexo vs taxa de foot-flagging
  p3 = ggplot(dados, aes(footpad, ttot, color=footpad)) +
    geom_point(shape=16, size=.2, show.legend=F, alpha = .5) +
    theme_minimal() +
    scale_color_gradient(low = "darkseagreen1", high = "darkslategrey") +
    xlab("\nTaxa de foot-flagging (n eventos/min)") +
    ylab("Tempo até início da cópula\n") +
    geom_line(aes(y = pred), size = 1)
  grid.arrange(p1, p2, p3, nrow = 1)
}
