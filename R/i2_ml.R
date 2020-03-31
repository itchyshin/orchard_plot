#' @title i2_ml
#' @description I2 (I-squared) for mulilevel meta-analytic models, based on Nakagawa & Santos (2012). Under multilevel models, we can have a multiple I2 (see also Senior et al. 2016). Alternatively, the method based method by Wolfgang Viechtbauer (http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate?s[]=multilevel).
#' @param model
#' @param method
#' @return A data frame containing all the model results including mean effect size estimate, confidence and prediction intervals with estimates converted back to r
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au 
#' @export

i2_ml <- function(model, method = c("ns", "wv")) {
  
  ## evaluate choices
  method <- match.arg(method)
  
  # Wolfgang's method
  if (method == "wv") {
    W <- solve(model$V)
    X <- model.matrix(model)
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    I2_total <- sum(model$sigma2) / (sum(model$sigma2) + (model$k - model$p) / sum(diag(P)))
    I2_each <- model$sigma2 / (sum(model$sigma2) + (model$k - model$p) / sum(diag(P)))
    names(I2_each) <- paste0("I2_", model$s.names)
    # or my way
  } else {
    # sigma2_v = typical sampling error variance
    sigma2_v <- sum(1 / model$vi) * (model$k - 1) / (sum(1 / model$vi)^2 - sum((1 / model$vi)^2))
    I2_total <- sum(model$sigma2) / (sum(model$sigma2) + sigma2_v) # s^2_t = total variance
    I2_each <- model$sigma2 / (sum(model$sigma2) + sigma2_v)
    names(I2_each) <- paste0("I2_", model$s.names)
  }
  
  # putting all together
  I2s <- c(I2_total = I2_total, I2_each)
  
  return(I2s)
}

# TODO - ref 
