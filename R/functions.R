
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
                              "This skewness/correlation combination isn't feasible for the skew-normal — try smaller skewness values."))
  dp
}

rmvsn_safe <- function(n, dp, mean, var1, var2, cov) {
  if (is.null(dp)) {
    return(mvtnorm::rmvnorm(n, mean, matrix(c(var1, cov, cov, var2), 2, 2)))
  }
  sn::rmsn(n, dp = dp)
}

# rmvsn_safe <- function(n, mean, var1, var2, cov, gamma1) {
#   if (all(gamma1 == 0)) {
#     return(mvtnorm::rmvnorm(n, mean, matrix(c(var1, cov, cov, var2), 2, 2)))
#   }
#   cp <- list(mean = mean,
#              var.cov = matrix(c(var1, cov, cov, var2), 2, 2),
#              gamma1 = gamma1)
#   dp <- tryCatch(sn::cp2dp(cp, family = "SN"), error = function(e) NULL)
#   shiny::validate(shiny::need(!is.null(dp),
#                               "This skewness/correlation combination isn't feasible for the skew-normal — try smaller skewness values."))
#   sn::rmsn(n, dp = dp)
# }



# plot synchronizing ------------------------------------------------------


get_camera <- function(x) {

  if (is.null(x)) {
    return(NULL)
  }

  # Case 1: nested scene.camera
  if (!is.null(x[["scene.camera"]])) {
    return(x[["scene.camera"]])
  }

  # Case 2: flattened camera properties
  camera_out <- list(
    eye = list(
      x = x[["scene.camera.eye.x"]],
      y = x[["scene.camera.eye.y"]],
      z = x[["scene.camera.eye.z"]]
    ),
    center = list(
      x = x[["scene.camera.center.x"]],
      y = x[["scene.camera.center.y"]],
      z = x[["scene.camera.center.z"]]
    ),
    up = list(
      x = x[["scene.camera.up.x"]],
      y = x[["scene.camera.up.y"]],
      z = x[["scene.camera.up.z"]]
    )
  )

  # Remove NULL components
  camera_out <- lapply(camera_out, function(part) {
    part[!vapply(part, is.null, logical(1))]
  })

  # If nothing useful was found, return NULL
  if (length(unlist(camera_out)) == 0) {
    return(NULL)
  }

  camera_out
}


apply_camera <- function(plot_id, cam) {

  if (is.null(cam)) {
    return()
  }

  plotly::plotlyProxy(
    plot_id,
    session = session
  ) |>
    plotly::plotlyProxyInvoke(
      "relayout",
      list(
        scene = list(
          camera = cam
        )
      )
    )
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
