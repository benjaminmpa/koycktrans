# koyck.R : estimation d'un modele a transformation de Koyck
# Les reperes [MODIF n] indiquent les endroits modifies par rapport
# a ta version. Tu peux supprimer ces reperes une fois verifies.



#' Estimation d'un modele a transformation de Koyck
#' Estime par MCO le modele transforme
#' \eqn{Y_t = \alpha(1-\lambda) + \lambda Y_{t-1} + \beta_0 X_t + v_t}.
#'
#' @param formula un objet de type formula specifiant la relation (y ~ x)
#' @param data un data.frame contenant les donnees
#' @return un objet de classe \code{koyck} contenant les coefficients,
#' lambda, beta_0, alpha, les retards moyen et median, et le modele lm.
#' @examples
#' set.seed(123)
#' n <- 200
#' x <- rnorm(n)
#' y <- numeric(n)
#' for (t in 2:n) {
#'   y[t] <- 2*(1 - 0.6) + 0.6*y[t-1] + 0.5*x[t] + rnorm(1, sd = 0.3)
#' }
#' donnees <- data.frame(y = y, x = x)
#' mod <- koyck_model(y ~ x, data = donnees)
#' mod
#' mod$lambda
#' @export
koyck_model <- function(formula, data) {

  cl <- match.call()


  # 1. Extraction des variables de la formule

  mf <- match.call(expand.dots = FALSE)
  m  <- match(c("formula", "data"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())

  y <- stats::model.response(mf)
  x <- stats::model.matrix(attr(mf, "terms"), mf)
  n <- length(y)

  if (n < 3) stop("Pas assez d'observations pour estimer le modele.")
  if (ncol(x) < 2) stop("La formule doit contenir au moins une variable explicative.")


  # 2. Creation du retard de Y_(t-1)

  y_t   <- y[-1]     # Y_t     : t = 2, ..., n
  y_lag <- y[-n]     # Y_(t-1) : t = 1, ..., n-1

  # X_t aligne sur Y_t (on retire la 1re observation et l'intercept)

  x_t <- x[-1, -1, drop = FALSE]


  # 3. Modele transforme par Koyck

  data_koyck <- data.frame(y_t = y_t, y_lag = y_lag, x_t, check.names = FALSE)
  fit        <- stats::lm(y_t ~ ., data = data_koyck)

  # 4. Recuperation des parametres de Koyck

  coefficients <- stats::coef(fit)
  lambda       <- unname(coefficients["y_lag"])
  beta_0       <- coefficients[-(1:2)]
  alpha        <- unname(coefficients["(Intercept)"]) / (1 - lambda)


  # 5. Calcul des retards moyen et median

  if (is.na(lambda) || lambda <= 0 || lambda >= 1) {

    warning("lambda doit etre compris entre 0 et 1 : retards moyen et median non definis.")
    mean_lag   <- NA_real_
    median_lag <- NA_real_
  } else {
    mean_lag   <- lambda / (1 - lambda)
    median_lag <- log(0.5) / log(lambda)
  }


  # 6. Structure de l'objet retourne

  result <- list(
    coefficients = coefficients,
    lambda       = lambda,
    beta_0       = beta_0,
    alpha        = alpha,
    mean_lag     = mean_lag,
    median_lag   = median_lag,
    model_fit    = fit,
    call         = cl
  )

  class(result) <- "koyck"
  return(result)
}

#' Affichage d'un objet koyck
#'
#' @param x un objet de classe \code{koyck}
#' @param ... non utilise
#' @export
print.koyck <- function(x, ...) {

  cat("Modele a transformation de Koyck\n\n")
  print(x$call)
  cat("\nlambda :", round(x$lambda, 4), "\n")
  cat("alpha  :", round(x$alpha, 4), "\n")
  cat("beta_0 :"); print(round(x$beta_0, 4))
  cat("Retard moyen  :", round(x$mean_lag, 4), "\n")
  cat("Retard median :", round(x$median_lag, 4), "\n")
  invisible(x)
}
