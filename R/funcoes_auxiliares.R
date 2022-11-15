
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