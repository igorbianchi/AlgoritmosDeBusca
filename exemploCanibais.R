  debugSource("Aspirador.R")
  debugSource("Canibais.R")
  debugSource("buscaDesinformada.R")
  debugSource("buscaInformada.R")
  
  inicial <- Aspirador(desc = c(X = 1, Y = 1, 1110))
  objetivo <- Aspirador()
  objetivo$desc <- c(0)



cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(unlist(buscaEmProfundidade(inicial, objetivo)))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))

