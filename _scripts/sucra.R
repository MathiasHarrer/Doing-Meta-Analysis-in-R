## Copy and paste the code underneath in its enterity into your console
## Then hit 'Enter ⏎'

sucra = function(rank.probability, lower.is.better = FALSE){

  rank.probability = rank.probability
  lower.is.better = lower.is.better

  # packages
  library(gemtc)

  # Convert rank.probability to matrix
  mat = as.matrix(rank.probability)

  # Loop over treatments, for each treatment: calculate SUCRA
  a = ncol(mat)
  j = nrow(mat)
  names = rownames(mat)

  sucra = numeric()
  for (x in 1:j){
    sucra[x] = sum(cumsum(mat[x,1:(a-1)]))/(a-1)
  }

  # If condition for lower.is.better
  if (lower.is.better==TRUE){
    sucra = numeric()
    for (x in 1:j){
      sucra[x] = 1-sum(cumsum(mat[x,1:(a-1)]))/(a-1)
    }
  }

  # Make data.frame
  res = data.frame("Treatment"=names, "SUCRA" = sucra)

  # Order
  res = res[order(-res$SUCRA),]
  rownames(res) = 1:j

  return(res)
}
