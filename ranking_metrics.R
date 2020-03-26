#######################################
#    AVALIACAO BINARIA DE RANKING     #
#######################################

##########     PRECISAO      ##########
precision <- function(ground_truth, prediction, top) {
  return(sum(ground_truth[prediction[1:top]]) / top)
}

##########     REVOCACAO      ##########
recall <- function(ground_truth, prediction, top) {
  relevant <- sum(ground_truth)
  if (relevant == 0) {
    return(0)
  }
  return(sum(ground_truth[prediction[1:top]]) / relevant)
}

##########  PRECISAO MEDIA    ##########
average_precision <- function(ground_truth, prediction, top) {
  topk <- c(1:top)
    
  relevant_obtained <- sum(ground_truth[prediction[topk]])
  if (relevant_obtained == 0) {
    return(0)
  }
  
  top_relevant <- topk[ground_truth[prediction[topk]] == 1]
    
  precisions <- mapply(precision, top=top_relevant, 
                       MoreArgs = list(ground_truth=ground_truth, 
                                       prediction=prediction))

  return(sum(precisions) / relevant_obtained)
}

###### MEDIA DAS PRECISOES MEDIAS ######
map <- function(ground_truth, predictions, top) {
    
  ap <- mapply(average_precision, prediction=predictions, MoreArgs= list(ground_truth=ground_truth, top=top))

  return(sum(ap) / length(predictions))
}

#######################################
#  AVALIACAO PONDERADA DE RANKING     #
#######################################

#######    GANHO CUMULATIVO    ########
cumulative_gain <- function(prediction, gain, top) {
  return(sum(gain[prediction[1:top]]))
}

##### GANHO CUMULATIVO DESCONTADO ######
discounted_cumulative_gain <- function(prediction, gain, top) {

  sequence <- c(1 / log((2:(top+1)), 2))
    
  return(sum((gain[prediction[1:top]]) * sequence))
}

### GANHO CUMULATIVO DESCONTADO NORMALIZADO ###
normalized_discounted_cumulative_gain <- function(prediction, gain, top) {

  order_gain <- order(gain, decreasing=TRUE)
    
  sequence <- c(1 / log((2:(top+1)), base=2))
    
  dcg <- sum((gain[prediction[1:top]]) * sequence)
    
  idcg <- sum((gain[order_gain[1:top]]) * sequence)

  return(dcg / idcg)
}

#######################################
#       COMPARACAO DE RANKING         #
#######################################

###### MEDIA DO RANKING RECIPROCO ######
mean_reciprocal_ranking <- function(ground_truth, predictions) {
    
  first_true <- function(prediction){
    binary <- ground_truth[prediction]
    return(1/min(which(binary == 1)))
  }
    
  first_position_sum <- sum(sapply(predictions, first_true))
    
  return(first_position_sum / length(predictions))
}

########## INDICE DE JACCARD #########
jaccard_index <- function(ranking_a, ranking_b, top) {
  uni <- union(ranking_a[1:top], ranking_b[1:top])
  inter <-  intersect(ranking_a[1:top], ranking_b[1:top])
  return(length(inter) / length(uni))
}

##### COEFICIENTE DE KENDALL-TAU #####
kendall_tau <- function(ranking_a, ranking_b) {
  count_total <- 0
  count_disagree <- 0
  for(i in 1:(length(ranking_a) -1)){
      for(j in (i+1):length(ranking_a)){
          count_total <- count_total+1
          if((ranking_a[i]>ranking_a[j] && ranking_b[i] < ranking_b[j]) || (ranking_a[i]<ranking_a[j] && ranking_b[i] > ranking_b[j])) count_disagree <- count_disagree + 1
      }
  }
  return(count_disagree / count_total)
}
