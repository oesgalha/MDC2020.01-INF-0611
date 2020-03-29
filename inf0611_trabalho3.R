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
  return(distancias)
}

#################################################
#         Calcular Distancias                   #
#################################################

distance_color <- NULL
distance_texture <- NULL
distance_shape <- NULL

for (c in consulta) {
  distance_color <- cbind(distance_color, generate_distances(feature_color, c))
  distance_texture <- cbind(distance_texture, generate_distances(feature_texture, c))
  distance_shape <- cbind(distance_shape, generate_distances(feature_shape, c))
}

#################################################
#################################################

##FAZER para cada consulta
## calcular rankings para a agregacao por COMBMIN
## calcular rankings para a agregacao por COMBMAX
## calcular rankings para a agregacao por COMBSUM

ranking_combmin <- NULL
ranking_combmax <- NULL
ranking_combsum <- NULL

for (i in 1:length(consulta)) {
  ranking_combmin <- cbind(ranking_combmin, combmin(distance_color[,i], distance_texture[,i], distance_shape[,i]))
  ranking_combmax <- cbind(ranking_combmax, combmax(distance_color[,i], distance_texture[,i], distance_shape[,i]))
  ranking_combsum <- cbind(ranking_combsum, combsum(distance_color[,i], distance_texture[,i], distance_shape[,i]))
}

#################################################
#################################################

# comparando  rankings                              

##FAZER
## utilize as funções do arquivo ranking_metrics.R para calcular a precisao e revocacao utilizando o ground_truth e os rankings obtidos (como o parametro predictions)

## gere um grafico de precisao X topK para cada consulta (contendo as curvas de todas os rankings para uma consulta)
## gere um grafico de revocacao X topK para cada consulta (contendo as curvas de todas os rankings para uma consulta)

## serao entao um total de 4 graficos (dois para cada consulta)

# Função auxiliar para gerar o gráfico
plot_consulta_metric <- function(metric, consulta_index, metric_name, title) {
  # entrada:  metric -> função da métrica (ex: precision, recall)
  #           consulta_index -> índice do vetor de consultas
  #           metric_name -> Nome da métrica usada para etiquetar o eixo Y
  #           title -> Título do gráfico
  pr = data.frame()

  metric_cmin <- mapply(metric, 1:top,
                     MoreArgs = list(ground_truth = ground_truth,
                                     prediction = ranking_combmin[,consulta_index]))
  metric_cmax <- mapply(metric, 1:top,
                     MoreArgs = list(ground_truth = ground_truth,
                                     prediction = ranking_combmax[,consulta_index]))
  metric_csum <- mapply(metric, 1:top,
                     MoreArgs = list(ground_truth = ground_truth,
                                     prediction = ranking_combsum[,consulta_index]))

  ggplot(pr,aes(x = 1:top)) +
    geom_point(aes(y = metric_cmin),  color = 'red') +
    geom_line(aes(y = metric_cmin), color = 'red') +
    geom_text(aes(0, 1,label = "CombMin"), vjust= -0.3, color = 'red') +
    geom_point(aes(y = metric_cmax),  color = 'blue') +
    geom_line(aes(y = metric_cmax),  color = 'blue') +
    geom_text(aes(0, 0.9,label = "CombMax"), vjust= -0.3, color = 'blue') +
    geom_point(aes(y = metric_csum),color = 'green') +
    geom_line(aes(y = metric_csum),color = 'green') +
    geom_text(aes(0, 0.8, label = "CombSum"), vjust= -0.3,color = 'green') +
    theme_light() +
    labs(colour = element_blank(),
         title = title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(name = metric_name, limits = c(0, 1),
                       breaks = 0:10*0.1,
                       minor_breaks = NULL) +
    scale_x_continuous(name = "TopK", limits = c(0, top),
                       breaks = 0:top,
                       minor_breaks = NULL)
}

metrics = c(precision, recall)
metric_names = c("Precisão", "Revocação")

# Gera os gráficos de precisão e revocação para cada consulta
for (i in 1:length(consulta)) {
  for (m in 1:2) {
    title <- paste(metric_names[m], " x TopK barcelona_", sprintf("%02d", consulta[i] - 10), ".jpg", sep = "")
    plot_consulta_metric(metrics[[m]], i, metric_names[m], title)
    metric_prefix <- ifelse(metric_names[m] == "Precisão", "prec", "reca")
    file_name <- paste("questao2_", metric_prefix, "_barcelona", sprintf("%02d", consulta[i] - 10), ".png", sep = "")
    ggsave(file_name, width =12, height =6)
  }
}

#################################################
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
  return(ranking)
}

rankings_color <- NULL
rankings_texture <- NULL
rankings_shape <- NULL

for (i in 1:length(consulta)) {
  rankings_color <- cbind(rankings_color, generate_rankings(distance_color[,i]))
  rankings_texture <- cbind(rankings_texture, generate_rankings(distance_texture[,i]))
  rankings_shape <- cbind(rankings_shape, generate_rankings(distance_shape[,i]))
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

ranking_borda <- NULL
for (i in 1:length(consulta)) {
  ranking_borda <- cbind(ranking_borda, bordacount(rankings_color[,i], rankings_shape[,i], rankings_texture[,i]))
}

#################################################
#################################################

# comparando  rankings                              

##FAZER
## utilize as funções do arquivo ranking_metrics.R para calcular a precisao e revocacao utilizando o ground_truth e os rankings obtidos (como o parametro predictions)

## gere um grafico de precisao X topK para cada consulta (contendo as curvas dos rankings gerados pelo BORDA e pelo COMB escolhido)
## gere um grafico de revocacao X topK para cada consulta (contendo as curvas dos rankings gerados pelo BORDA e pelo COMB escolhido)

## serao entao um total de 4 graficos (dois para cada consulta)
# Função auxiliar para gerar o gráfico
plot_consulta_borda_metric <- function(metric, consulta_index, metric_name, title) {
  # entrada:  metric -> função da métrica (ex: precision, recall)
  #           consulta_index -> índice do vetor de consultas
  #           metric_name -> Nome da métrica usada para etiquetar o eixo Y
  #           title -> Título do gráfico
  pr = data.frame()

  metric_csum <- mapply(metric, 1:top,
                        MoreArgs = list(ground_truth = ground_truth,
                                        prediction = ranking_combsum[,consulta_index]))
  metric_borda <- mapply(metric, 1:top,
                        MoreArgs = list(ground_truth = ground_truth,
                                        prediction = ranking_borda[,consulta_index]))

  ggplot(pr,aes(x = 1:top)) +
    geom_point(aes(y = metric_csum),  color = 'red') +
    geom_line(aes(y = metric_csum), color = 'red') +
    geom_text(aes(0, 1,label = "CombSum"), vjust= -0.3, color = 'red') +
    geom_point(aes(y = metric_borda),  color = 'blue') +
    geom_line(aes(y = metric_borda),  color = 'blue') +
    geom_text(aes(0, 0.9,label = "Borda"), vjust= -0.3, color = 'blue') +
    theme_light() +
    labs(colour = element_blank(),
         title = title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(name = metric_name, limits = c(0, 1),
                       breaks = 0:10*0.1,
                       minor_breaks = NULL) +
    scale_x_continuous(name = "TopK", limits = c(0, top),
                       breaks = 0:top,
                       minor_breaks = NULL)
}

metrics = c(precision, recall)
metric_names = c("Precisão", "Revocação")

# Gera os gráficos de precisão e revocação para cada consulta
for (i in 1:length(consulta)) {
  for (m in 1:2) {
    title <- paste(metric_names[m], " x TopK barcelona_", sprintf("%02d", consulta[i] - 10), ".jpg", sep = "")
    plot_consulta_borda_metric(metrics[[m]], i, metric_names[m], title)
    metric_prefix <- ifelse(metric_names[m] == "Precisão", "prec", "reca")
    file_name <- paste("questao3_", metric_prefix, "_barcelona", sprintf("%02d", consulta[i] - 10), ".png", sep = "")
    ggsave(file_name, width =12, height =6)
  }
}


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





