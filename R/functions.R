signal_variance <- function(beta, signal_matrix) {
  S <- signal_matrix - signal_matrix[, 1] %*% t(signal_matrix[, 1])
  as.numeric(t(beta) %*% S %*% beta)
}

error_variance_y <- function(beta, signal_matrix, R2_true) {
  R2 <- max(min(R2_true, 0.999999), 1e-12)
  signal_variance(beta, signal_matrix) * (1 - R2) / R2
}

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



# regression surface ------------------------------------------------------
plane_surface <- function(beta, x_seq, z_seq){
  outer(z_seq, x_seq,
        FUN = function(z, x){beta[1] + beta[2]*x + beta[3]*z})
}

curved_surface <- function(beta, x_seq, z_seq){
  outer(z_seq, x_seq,
        FUN = function(z, x){beta[1] + beta[2]*x + beta[3]*z + beta[4]*x*z})
}

f_tangent<-function(beta, x, z){
        beta[1]+beta[2]*x+beta[3]*z+beta[4]*x*z
}



# skew --------------------------------------------------------------------

get_dp_safe <- function(mean, var1, var2, cov, gamma1) {
  if (all(gamma1 == 0)) return(NULL)  # signal: use plain rmvnorm path
  cp <- list(mean = mean,
             var.cov = matrix(c(var1, cov, cov, var2), 2, 2),
             gamma1 = gamma1)
  dp <- tryCatch(sn::cp2dp(cp, family = "SN"), error = function(e) NULL)
  shiny::validate(shiny::need(!is.null(dp),
                              "This skewness/correlation combination isn't feasible for the skew-normal: Try smaller skewness values."))
  dp
}

rmvsn_safe <- function(n, dp, mean, var1, var2, cov) {
  if (is.null(dp)) {
    return(mvtnorm::rmvnorm(n, mean, matrix(c(var1, cov, cov, var2), 2, 2)))
  }
  sn::rmsn(n, dp = dp)
}


# tangent functions -------------------------------------------------------

make_ev_point <- function(b, x, z) {
  list(x = x, z = z, y = b[1] + b[2]*x + b[3]*z + b[4]*x*z)
}
make_tangent_x <- function(b, ev, z_seq) {
  ev$y + (b[3] + b[4]*ev$x) * (z_seq - ev$z)
}
make_tangent_z <- function(b, ev, x_seq) {
  ev$y + (b[2] + b[4]*ev$z) * (x_seq - ev$x)
}


equation_text <- function(b, eval_x, eval_z){
  ev_point<-make_ev_point(b, eval_x, eval_z)
  x_eval <- ev_point$x
  z_eval <- ev_point$z
  b0<-b[1]+b[2]*x_eval+b[3]*z_eval+b[4]*x_eval*z_eval
  b1<-b[2]+b[4]*z_eval
  b2<-b[3]+b[4]*x_eval
  b3<-b[4]
  sprintf(
    "At your specified evaluation point x= %.2f, z= %.2f the model parameters are b0=%.2f, b1= %.2f, b2=%.2f. The slope of the interaction term does not change and is b3= %.2f.",
    x_eval, z_eval, b0, b1, b2, b3
  )
}

flip_text<-function(b){
  b1b3=-b[2]/b[4]
  b2b3=-b[3]/b[4]
  sprintf("For an infinite sample the sign change of b1 relatig to predictor x
          is at z = %.2f, the sign change for b2 relating to predictor z is at x= %.2f.",
          b1b3, b2b3)
}



# residual surface --------------------------------------------------------

residual_surface <- function(beta, b, x_seq, z_seq, interaction = TRUE) {
  truth <- curved_surface(beta, x_seq, z_seq)
  fit   <- if (interaction) curved_surface(b, x_seq, z_seq) else plane_surface(b, x_seq, z_seq)
  truth - fit
}

simulate_cloud <- function(n, mean, var_x, var_z, cov_xz, var_ex, var_ez, var_ey,
                           beta, b_f, b_r, dp = NULL) {
  xz <- rmvsn_safe(n, dp, mean, var_x, var_z, cov_xz)
  x <- xz[, 1]
  z <- xz[, 2]
  y <- drop(cbind(1, x, z, x * z) %*% beta) + stats::rnorm(n, 0, sqrt(var_ey))

  xs <- x + stats::rnorm(n, 0, sqrt(var_ex))   # observed predictors
  zs <- z + stats::rnorm(n, 0, sqrt(var_ez))

  data.frame(
    x=x,z=z,
    xs = xs, zs = zs,
    y=y,
    res_full = y - drop(cbind(1, xs, zs, xs * zs) %*% b_f),
    res_red  = y - drop(cbind(1, xs, zs) %*% b_r)
  )
}
