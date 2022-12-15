#Functions

#95%CIs
lCI    <- function(mean, sd, reps = reps){(mean - ((1.96 * (sd/sqrt(reps)))))}
uCI    <- function(mean, sd, reps = reps){(mean + ((1.96 * (sd/sqrt(reps)))))}

#Clean the list
sortMe <- function(x, n_v, reps = 250){
  avTempMat      <- do.call(cbind, x)
  avTempMatX     <- array(avTempMat, dim=c(dim(x[[1]]), length(x)))
  avTempMatm     <- apply(avTempMatX, c(1,2), mean, na.rm = T)
  avTempMatsd    <- apply(avTempMatX, c(1,2), sd, na.rm = T)
  avTempMatcil   <- avTempMatm
  avTempMatciu   <- avTempMatm
  avTempMatse    <- avTempMatm
  avTempMatm_sig <- avTempMatm
  for (i in 1:n_v){
    for (j in 1:n_v){
      avTempMatcil[i,j] <- lCI(avTempMatm[i,j], avTempMatsd[i,j], reps)
      avTempMatciu[i,j] <- uCI(avTempMatm[i,j], avTempMatsd[i,j], reps)
      if (avTempMatcil[i,j] < 0 & avTempMatciu[i,j] > 0){
        avTempMatm_sig[i,j] <- 0
      }
    }
  }

  avTempMatse <- avTempMatsd/sqrt(reps)

  return(
    list(
    mean = avTempMatm_sig,
    se   = avTempMatse,
    upperCI = avTempMatciu,
    lowerCI = avTempMatcil)
  )
}
