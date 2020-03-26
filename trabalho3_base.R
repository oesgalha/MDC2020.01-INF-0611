library(IM)
library(e1071)
library(wvtool)
library(ecodist)
library(imager)


#------------------------------------------------#
# Funcao para exibir uma imagem                  #
#------------------------------------------------#
show_image <- function(img, classe){
  plot(img, axes = FALSE, main = classe)
}

#------------------------------------------------#
# Carrega e retorna todas as imagens             #
#------------------------------------------------#
read_images <- function(path_soccer){
  name_imgs <- list.files(path_soccer, full.names = TRUE)
  all_im <- lapply(name_imgs, load.image)
  
  return(all_im)
}

#------------------------------------------------#
# Exibe a classe de cada imagem (time)           #
#------------------------------------------------#
get_classes<- function(path_soccer){
  name_imgs <- list.files(path_soccer, full.names = FALSE)
  classes<-NULL
  for(name in name_imgs){
    name<-strsplit(name, '_')
    print(name[1])
    classes<-cbind(classes,name[[1]][1])
  }
  return(classes)
}

#------------------------------------------------#
# Retorna ground_truth escolhida classe relevante#
#------------------------------------------------#
get_ground_truth<- function(classes, classe_relevante){
  ground_truth <- integer(length(classes))
  ground_truth[which(classes %in% classe_relevante)] <-1
  return(ground_truth)
}

#------------------------------------------------#
# cria matriz de coocorrencia e retorna caract   #
#------------------------------------------------#

criarMatrizCoocorrencia <- function(img, Q){
  # discretizando as cores
  img <- (img) %/% 16
  # print(img)
  niveis <- 16
  
  n <- dim(img)[1]
  m <- dim(img)[2]
  
  P <- matrix(0, nrow = niveis, ncol = niveis)
  
  for (i in 1:n){
    for (j in 1:m){
      x <- j + Q[1]
      y <- i + Q[2]
      if (x >= 0 && x <= m && y >= 0 && y <= n){
        P[img[i, j] + 1, img[y, x] + 1] <- P[img[i, j] + 1, img[y, x] + 1] + 1
      }
    }
  }
  
  # print(P) 
  P <- P / sum(P)
  
  return(c(P)) 
}


#------------------------------------------------#
#                     BORDA                      #
#------------------------------------------------#

bordacount <- function(...) {
  # obtem os rankings
  rankings <- mapply(order, list(...), SIMPLIFY = FALSE)
  # calcula a ordem baseada na soma das posições dos rankings
  return(do.call(combsum, rankings))
}


#------------------------------------------------#
#                   COMBMIN                      #
#------------------------------------------------#

combmin <- function(...) {
  order(mapply(min, ...))
}

#------------------------------------------------#
#                   COMBMAX                      #
#------------------------------------------------#
combmax <- function(...) {
  order(mapply(max, ...))
}


#------------------------------------------------#
#                   COMBSUM                      #
#------------------------------------------------#

combsum <- function(...) {
  order(mapply(sum, ...))
}
