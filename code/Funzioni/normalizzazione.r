normalizza <- function(x){
  # x vettore
  return(x/sum(x))
}

apply(oliveoil[, 3:10] + 1, 2, normalizza)
