source("Estado.R")

## Classe e métodos para o problema dos 3 Missionários e 3 Aspirador
Aspirador <- function(desc = NULL, pai = NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Aspirador", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
## só importa se todos os quadrados estão limpos, pode ser em qualquer posição
Ops.Aspirador = function(obj1, obj2){
  if(.Generic == "=="){
    return(all(obj1$desc[3] == obj2$desc[1]))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("(X Y QuadradosSujos): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  if(is.null(atual$desc[3]))
    return(Inf)
  ## h(obj) = quantidade de quadrados sujos restantes
  return(sapply(strsplit(as.character(atual$desc[3]), ""), function (x) sum(x == 1))) 
}

geraFilhos.Aspirador <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()
  
  desc <- obj$desc
  
  ## transforma as coordenadas do "tabuleiro" em apenas um número de 0 a 3
  doisXMaisY <- as.numeric(2 * desc[1] + desc[2]) 
  
  ## gera filhos usando todos os operadores  
  if(doisXMaisY == 0){
    operadores <- list(c(0,0,0), c(0,0,1000), c(-1,0,0), c(0,-1,0), c(1,0,0), c(0,1,0))

  
    filhosDesc <- lapply(operadores, function(op) desc-op)
  }else if(doisXMaisY == 1) {
    operadores <- list(c(0,0,0), c(0,0,100), c(-1,0,0), c(0,-1,0), c(1,0,0), c(0,1,0))
    
    
    filhosDesc <- lapply(operadores, function(op) desc-op)
  }else if(doisXMaisY == 2) {
    operadores <- list(c(0,0,0), c(0,0,10), c(-1,0,0), c(0,-1,0), c(1,0,0), c(0,1,0))
  
    
    filhosDesc <- lapply(operadores, function(op) desc-op)
  }else {
    operadores <- list(c(0,0,0), c(0,0,1), c(-1,0,0), c(0,-1,0), c(1,0,0), c(0,1,0))
    
    filhosDesc <- lapply(operadores, function(op) desc-op)
  }
  
  ## função para verificar se o valor calculado para quadrados sujos filhos é válido
  ## como utilizamos 0 e 1 para marcar os quadrados sujos, utilizamos uma função que valida números binários
  verificaBinario <- function(valor){
    copia <- valor
    while(copia != 0){
      if(copia %% 10 > 1)
        return (FALSE)
      copia <- copia %/% 10
    }
    return (TRUE)
  }

  
  ## verifica estados filhos incompatíveis com o problema  
  incompativeis <- sapply(1:length(filhosDesc),
                          function(i) {
                            fDesc <- filhosDesc[[i]]
                            verificacaoBinario <- verificaBinario(fDesc[3])
                            if((any(fDesc[1:2] < 0)) ||     ##    x ou y menor que 0
                               (any(fDesc[1:2] > 1)) ||   ##    x ou y maior que 1
                               (!verificacaoBinario))     ## valor dos quadrados sujos válido
                              i ## é incompatível: retorna índice
                            else
                              0 ## senão é compatível
                          })
  ## mantém no vetor apenas os que são incompatíveis
  incompativeis <- incompativeis[incompativeis != 0]
  ## remove estados filhos incompatíveis
  filhosDesc <- filhosDesc[-incompativeis]
  
  
  ## gera os objetos Aspirador para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Aspirador(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    ## verifica de qual o estado o filho surgiu
    if(obj$desc[1] != filho$desc[1]) ## filho tem coordenada X diferente do pai
      filho$g <- obj$g + 1           ## então filho recebe um custo + 1
    if(obj$desc[2] != filho$desc[2]) ## filho tem coordenda y diferente do pai
      filho$g <- obj$g + 3           ## então filho recebe um custo + 3
    if(obj$desc[3] != filho$desc[3]) ## filho tem quadrados sujos diferentes do pai
      filho$g <- obj$g + 2           ## então filho recebe um custo + 2 
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}