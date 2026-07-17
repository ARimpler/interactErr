
# QX ----------------------------------------------------------------------
QX<-function(mean_x, mean_z, var_x, var_z, cov_xz){
  # pre-compute E[X], E[Z], E[X^2], E[Z^2]
  Ex  <- mean_x
  Ez  <- mean_z
  Ex2 <- var_x + mean_x^2
  Ez2 <- var_z + mean_z^2

  # init full 4x4
  M <- matrix(NA, 4, 4)

  # first moments
  M[1,1] <- 1
  M[1,2] <- M[2,1] <- Ex
  M[1,3] <- M[3,1] <- Ez

  # second moments
  M[2,2] <- Ex2
  M[3,3] <- Ez2
  M[2,3] <- M[3,2] <- cov_xz + Ex*Ez

  # interaction cross terms
  M[1,4] <- M[4,1] <- M[2,3]  # E[XZ]
  M[2,4] <- M[4,2] <- Ez * Ex2 + 2*cov_xz*Ex
  M[3,4] <- M[4,3] <- Ex * Ez2 + 2*cov_xz*Ez

  # E[(XZ)^2]
  M[4,4] <- Ex2*Ez2 + 2*cov_xz^2 + 4*Ex*Ez*cov_xz

  # if(interaction){
  #   return(M)
  # } else {
  #   return(M[1:3,1:3])
  # }
  return(M)
}


# QD ----------------------------------------------------------------------
QD<-function(var_error_x, var_error_z,var_x,var_z,mean_x, mean_z){
  e<-matrix(0, 4,4)
  e[2,2]<-var_error_x
  e[3,3]<-var_error_z
  e[4,2] <- e[2,4] <- var_error_x * mean_z
  e[4,3] <- e[3,4] <- var_error_z * mean_x
  e[4,4]<-var_error_x * (var_z + mean_z^2) +
    var_error_z * (var_x + mean_x^2) +
    var_error_x * var_error_z
  # if(interaction){
  #   return(e)
  # } else {
  #   return(e[1:3,1:3])
  # }
  return(e)
}


# cross moments X and y ---------------------------------------------------

E_Xy<-function(beta, signal_matrix){
  EXY<-signal_matrix%*%beta
  return(EXY)
}


# estimation beta  -----------------------------------------------------
estimation<-function(XE, EXY){
  beta_hat<-solve(XE)%*%EXY
  return(beta_hat)
}


# residual measures -------------------------------------------------------

residual_measures<-function(beta, signal_matrix, XE, EXY,E_y,
                            interaction=TRUE){
  if(interaction){
    exp_residual<-t(beta)%*%signal_matrix%*%beta-
      t(EXY)%*%solve(XE)%*%EXY+E_y

  } else {
    exp_residual<-t(beta) %*% signal_matrix %*% beta -
      t(EXY[1:3]) %*% solve(XE[1:3,1:3]) %*% EXY[1:3]+E_y

  }
  r2<-1-exp_residual/(t(beta)%*%(signal_matrix-signal_matrix[,1]%*%
                                   t(signal_matrix[,1]))%*%beta+E_y)
  rse<-rmse<-sqrt(exp_residual)
  return(c(exp_residual, r2,rmse, rse))
}

tss_est<-function(beta, signal_matrix, E_y){
  tss<-(t(beta)%*%(signal_matrix-signal_matrix[,1]%*%
                     t(signal_matrix[,1]))%*%beta+E_y)
}

# sample fit  -------------------------------------------------------------

loglik <- function(ete, n, k){
  -n/2 * log(2*pi) - n/2 * log(ete/n) - n/2
}

aic <- function(ete, n, k) -2*loglik(ete, n, k) + 2*k
bic <- function(ete, n, k) -2*loglik(ete, n, k) + log(n)*k
