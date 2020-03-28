#------------------------------------------------#
# INF-0611 Recuperacao de Informacao             #
#                                                #
# Trabalho Avaliativo 3                          #
#------------------------------------------------#
# Nome COMPLETO Aluna (o) 1:                     #
#                                                #
# Nome COMPLETO Aluna (o) 2:                     #
#                                                #
# Nome COMPLETO Aluna (o) 3:                     #
#                                                #
# Nome COMPLETO Aluna (o) 4:                     #                                              #
#------------------------------------------------#

#------------------------------------------------#
# Configuracao dos arquivos auxiliares           #
#------------------------------------------------#
# setwd("") # configure o caminho antes de descomentar essa linha
source("./ranking_metrics.R")
source("./trabalho3_base.R")

# caminho da pasta de imagens
path_soccer = './soccer/'

#------------------------------------------------#
# Leitura das imagens                            #
#------------------------------------------------#
imagens <- read_images(path_soccer)

#------------------------------------------------#
# Obtem classe de cada imagem                    #
#------------------------------------------------#
nome_classes <- get_classes(path_soccer)

#------------------------------------------------#
# Define classe relevante                        #
#------------------------------------------------#
classe_relevante <- 'barcelona'

#------------------------------------------------#
# obtem ground_truth                             #
#------------------------------------------------#

ground_truth <- get_ground_truth(nome_classes, classe_relevante)

#------------------------------------------------#
# define consulta classe relevante               #
#------------------------------------------------#
consulta <- c(11,19) #utilizar as mesmas do Trabalho 2
  
#------------------------------------------------#
# define tamanho do topK analisado               #
#------------------------------------------------#
top <- 20

#################################################
#################################################


#------------------------------------------------#
# Questao 1                                      #
#------------------------------------------------#


# obtem caracteristicas de cor                   

features_color <- function(imagens){
  #entrada: o conjunto de imagens carregadas
  #saida: uma matriz de caracteristicas de cor onde cada linha é referente a uma imagem

  ##UTILIZAR MESMA FUNCAO DO TRABALHO 2
  #implementar funcao que obtem histograma de imagens RGB (3 canais) - obter histograma para cada canal e concatenar 
  
  # número de imagens
  n_imgs <- length(imagens)
  
  # matriz que armazenara os vetores de 
  # carateristicas de todas as imagens
  features <- matrix(0, nrow = n_imgs, ncol = 765)
  
  # extraindo os vetores de carateristicas 
  # de todas as imagens (histograma)
  i <- 1;
  for(img in imagens) {
    r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts;
    g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts;
    b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts;
    features[i, ] <- c(r, g, b)
    i = i + 1;
  }
  
  return(features)
}

#------------------------------------------------#

# obtem caracteristicas de textura               

features_texture <- function(imagens){
  #entrada: o conjunto de imagens carregadas
  #saida: uma matriz de caracteristicas de textura onde cada linha é referente a uma imagem
  
  ##UTILIZAR MESMA FUNCAO DO TRABALHO 2
  #transformar imagem em escala de cinza usando greyscale
  #obter caracteristicas Matriz Coocorrências  das imagens 
  
  features <- NULL
  for(img in imagens){
    img<- grayscale(img)[,,1,1];
    #tranforma intensidade de pixels em valores de 0 a 255
    min_v<-min(img)
    max_v<-max(img)
    img<-((img-min_v)/(max_v-min_v))*255
    #obtem catacteristicas de textura
    values <- criarMatrizCoocorrencia(img, c(1,0))
    #adiciona uma nova linha na matriz de caracteristicas (uma nova imagem)
    features <- rbind(features,values)
  }
  
  return(features)
}

#------------------------------------------------#

# obtem caracteristicas de forma                 

features_shape <- function(imagens){
  #entrada: o conjunto de imagens carregadas
  #saida: uma matriz de caracteristicas de forma onde cada linha é referente a uma imagem
  
  ##UTILIZAR MESMA FUNCAO DO TRABALHO 2
  #transformar imagem em escala de cinza usando greyscale
  #obter momentos variando de M0,0 ; M0,1 ; ... ; M1,0 ; ... ; M10,10 das imagens

  features <- NULL
  for(img in imagens){
    img<- grayscale(img)[,,1,1]
    aux_line <- NULL
    for(i in 0:10){
      for(j in 0:10){
        #adiciona um novo momento como caracteristica no vetor de caracteristicas da imagem
        aux_line <- cbind(aux_line,moment(img, order=c(i,j), center=TRUE))
        
      }}
    #adiciona uma nova linha na matriz de caracteristicas (uma nova imagem)
    features <- rbind(features,aux_line)
  }
  
  return(features)
}

#################################################
#         Calcular Caracteristicas              #
#################################################

feature_color <- features_color(imagens);
feature_texture <- features_texture(imagens);
feature_shape <- features_shape(imagens);

#################################################
#################################################

#------------------------------------------------#
# Questao 2                                      #
#------------------------------------------------#


# agregando rankings por valor                          


#escolha duas consultas da classe barcelona (posicoes de 11 a 20 do vetor ground_truth)


#construa os rankings para cada metodo de agregacao e para cada consulta (3 rankings para cada consulta)

generate_distances <- function(features, query){
  #entrada: conjunto de caracteristicas que serao utilizadas para calculo de distancia e indice da consulta no vetor de caracteristicas
  #saida: vetor não-ordenado de distancias das imagens para a consulta

  ## calcular distancia euclidiana de todas as imagens (representada pelas caracteristicas) para a consulta
  distancia <- full(ecodist::distance(features, "euclidean"))
  distancias <- distancia[,query]
  print(distancias)
  return(distancias)
}

#################################################
#         Calcular Distancias                   #
#################################################

distance_color_c1 <- generate_distances(feature_color, consulta[1]);
distance_texture_c1 <- generate_distances(feature_texture, consulta[1]);
distance_shape_c1 <- generate_distances(feature_shape, consulta[1]);

distance_color_c2 <- generate_distances(feature_color, consulta[2]);
distance_texture_c2 <- generate_distances(feature_texture, consulta[2]);
distance_shape_c2 <- generate_distances(feature_shape, consulta[2]);


#################################################
#################################################

##FAZER para cada consulta 
## calcular rankings para a agregacao por COMBMIN 
## Consulta #1
ranking_combmin1 <- combmin(distance_color_c1, distance_texture_c1, distance_shape_c1) 
## Consulta #2
ranking_combmin2 <-combmin(distance_color_c2, distance_texture_c2, distance_shape_c2) 
## calcular rankings para a agregacao por COMBMAX 
## Consulta #1
ranking_combmax1 <- combmax(distance_color_c1, distance_texture_c1, distance_shape_c1)  
## Consulta #2  
ranking_combmax2 <-combmax(distance_color_c2, distance_texture_c2, distance_shape_c2)  
## calcular rankings para a agregacao por COMBSUM 
## Consulta #1
ranking_combsum1 <- combsum(distance_color_c1, distance_texture_c1, distance_shape_c1)  
## Consulta #2  
ranking_combsum2 <- combsum(distance_color_c2, distance_texture_c2, distance_shape_c2) 

#################################################
#################################################

# comparando  rankings                              

##FAZER
## utilize as funções do arquivo ranking_metrics.R para calcular a precisao e revocacao utilizando o ground_truth e os rankings obtidos (como o parametro predictions)

## gere um grafico de precisao X topK para cada consulta (contendo as curvas de todas os rankings para uma consulta)
## gere um grafico de revocacao X topK para cada consulta (contendo as curvas de todas os rankings para uma consulta)

## serao entao um total de 4 graficos (dois para cada consulta)

#################################################
## Precisão Consulta #1
## Precisao no topk para combmin
   prec_combMin1 <- mapply(precision, 1:k, 
                           MoreArgs = list(ground_truth = ground_truth, 
                                           prediction = ranking_combmin1))
## Precisao no topk para combmax
   prec_combMax1 <- mapply(precision, 1:k,
                           MoreArgs = list(ground_truth = ground_truth, 
                                           prediction = ranking_combmax1))
## Precisao no topk para combsum
   prec_combSum1 <- mapply(precision, 1:k, 
                           MoreArgs = list(ground_truth = ground_truth, 
                                           prediction = ranking_combsum1))
## Revocação Consulta #1   
## Revocação no topk para combmin
   recall_combMin1 <- mapply(recall, 1:k, 
                             MoreArgs = list(ground_truth = ground_truth, 
                                             prediction = ranking_combmin1))
## Revocação no topk para combmax
   recall_combMax1 <- mapply(recall, 1:k,
                             MoreArgs = list(ground_truth = ground_truth, 
                                             prediction = ranking_combmax1))
## Revocação no topk para combsum
   recall_combsum1 <- mapply(recall, 1:k, 
                             MoreArgs = list(ground_truth = ground_truth, 
                                             prediction = ranking_combsum1))  
#################################################
## Precisão Consulta #2
## Precisao no topk para combmin
   prec_combMin2 <- mapply(precision, 1:k, 
                           MoreArgs = list(ground_truth = ground_truth, 
                                           prediction = ranking_combmin2))
## Precisao no topk para combmax
   prec_combMax2 <- mapply(precision, 1:k,
                           MoreArgs = list(ground_truth = ground_truth, 
                                           prediction = ranking_combmax2))
## Precisao no topk para combsum
   prec_combSum2 <- mapply(precision, 1:k, 
                           MoreArgs = list(ground_truth = ground_truth, 
                                           prediction = ranking_combsum2))
## Revocação Consulta #2   
## Revocação no topk para combmin
   recall_combMin2 <- mapply(recall, 1:k, 
                             MoreArgs = list(ground_truth = ground_truth, 
                                             prediction = ranking_combmin2))
## Revocação no topk para combmax
   recall_combMax2 <- mapply(recall, 1:k,
                             MoreArgs = list(ground_truth = ground_truth, 
                                             prediction = ranking_combmax2))
## Revocação no topk para combsum
   recall_combsum2 <- mapply(recall, 1:k, 
                             MoreArgs = list(ground_truth = ground_truth, 
                                             prediction = ranking_combsum2)) 

#################################################

##NAO SE ESQUECA DO RELATORIO

#################################################
#################################################

#------------------------------------------------#
# Questao 3                                      #
#------------------------------------------------#

# agregando rankings por posicao                          


#escolha duas consultas da classe barcelona (posicoes de 11 a 20 do vetor ground_truth)
#construa os rankings para cada metodo de agregacao e para cada consulta (3 rankings para cada consulta)


#utilize a funcao da questão anterior generate_distance seguida pela generate_ranking para
#cada vetor de caracteristicas individualmente

generate_rankings <- function(distancias){
  #entrada: conjunto de caracteristicas que serao utilizadas para calculo de distancia e indice da consulta no vetor de caracteristicas
  #saida: vetor ordenado pelas distancias com os indices das imagens mais proximas à consulta
  
  ##FAZER
  ## ordenar distancias
  ## ordenar por menor distancia e retornar as posicoes das imagens com menor distancia (mais proximas)
  ranking <- order(distancias)
  print(ranking)
  return(ranking)
}

##FAZER para cada consulta
## calcular ranking para o metodo de agregacao BORDA 
bordacount <- function (...) {
  # obtem os rankings
  rankings <- mapply (rank , list (...) ,
                      SIMPLIFY = FALSE )
  # calcula a ordem baseada na soma
  # das posições dos rankings
  return (do.call(combsum , rankings ))
}

#################################################
#################################################

# comparando  rankings                              

##FAZER
## utilize as funções do arquivo ranking_metrics.R para calcular a precisao e revocacao utilizando o ground_truth e os rankings obtidos (como o parametro predictions)

## gere um grafico de precisao X topK para cada consulta (contendo as curvas dos rankings gerados pelo BORDA e pelo COMB escolhido)
## gere um grafico de revocacao X topK para cada consulta (contendo as curvas dos rankings gerados pelo BORDA e pelo COMB escolhido)

## serao entao um total de 4 graficos (dois para cada consulta)

#################################################
#################################################

##NAO SE ESQUECA DO RELATORIO

#################################################
#################################################

#------------------------------------------------#
# Questao 4                                      #
#------------------------------------------------#


# concatenando caracteristicas                      

## FAZER -- pode ser utilizado mesmo metodo do trabalho anterior
## obter vetores finais de caracteristicas pela concatenação de cada tipo de caracteristica (forma, cor, textura):
## - dos 3
f_color_texture_shape <- cbind(feature_color, feature_texture, feature_shape);

## utilizar a funcao generate_distance da questao 2 seguida da funcao generate_ranking da questao 3 para cada novo vetor de caracteristicas (com as mesmas consultas)

#################################################
#################################################

# comparando  rankings                              

##FAZER
## utilize as funções do arquivo ranking_metrics.R para calcular a precisao e revocacao utilizando o ground_truth e os rankings obtidos (como o parametro predictions)

## gere um grafico de precisao X topK para cada consulta (contendo as curvas dos rankings da agregacao escolhida e da concatenacao de caracteristicas)
## gere um grafico de revocacao X topK para cada consulta (contendo as curvas dos rankings da agregacao escolhida e da concatenacao de caracteristicas)

## serao entao um total de 4 graficos (dois para cada consulta)

#################################################
#################################################

##NAO SE ESQUECA DO RELATORIO

#################################################
#################################################





