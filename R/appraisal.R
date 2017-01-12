#' appraisal
#'
#' Estimates of prediction model performance
#'
#' This function calculates simple measures of prediction model performance,
#' currently Harrell's c-index, O:E ratios and optional calibration plots.
#'
#' This function is currently only compatible with logistic regression models.
#'
#' @importFrom Hmisc somers2
#'
#' @param observed  Number of observations in the data set.
#' @param LP        Calculated linear predictor for all indiviuals in the data set
#'                  based on the prediction model being validated.
#' @param plots     Logical. If TRUE, calibration plots are returned.
#' @param title     Optional title string for calibration plots.
#' @param weight    Optional weights for weighted analyses.
#'
#' @return \code{appraisal} returns a matrix of performance estimates and
#'                          optional calibration plot
#' @export
#'
#' @examples
#' ## Example 1: validation of a simple prediction rule. Unweighted analysis
#' covmat <- matrix(c(0.2,0,0,0.2), nrow=2)
#' set.seed(123)
#' d <- datafy(obs = 100, means = c(0,0), covmat = covmat,
#'      var.names = c("X1", "X2", "Y"), genmod = c(-1.5, 1, 1))
#' m <- glm(d$Y ~ d$X1 + d$X2, family=binomial)
#' set.seed(456)
#' covmat <- matrix(c(0.4,0,0,0.4), nrow=2)
#' v <- datafy(obs = 100, means = c(0.0,0.0), covmat = covmat,
#'      var.names = c("X1", "X2", "Y"), genmod = c(-2, 1.2, 1.2))
#' LP <- m$coef[1] + m$coef[2]*v$X1 + m$coef[3]*v$X2
#' appraisal(obs = v$Y, LP = LP, plots = TRUE, title = "Calibration plot")

appraisal <- function(observed, LP, plots = TRUE, title = "", weight = rep(1, length(observed))){

  sum.tab <- matrix(rep(NA, 2), nrow = 1)
  colnames(sum.tab) <- c("O/E ratio", "C index")

  pred <- 1 / (1 + exp(- LP))

  sum.tab[1] <- (sum(observed * weight) / sum(weight)) / (sum(pred * weight) / sum(weight))

  c.total <- somers2(LP, observed, weights = weight)
  sum.tab[2] <- c.total[1]

  if(plots){ #calibration plot

    predw <- rep(pred, round(weight))
    obsw <- rep(observed, round(weight))
    minidf <- data.frame(cbind(obsw, predw))
    minidf <- minidf[with(minidf, order(predw)),]
    dec.pred <- dec.obs <- c()

    for (i in 1:10) {
      dec.obs[i] <- mean(minidf[(1 + (i - 1) *(sum(round(weight)) /
                          10)):((sum(round(weight)) / 10) * i), 1])
      dec.pred[i] <- mean(minidf[(1 + (i - 1) * (sum(round(weight)) /
                          10)):((sum(round(weight)) / 10) * i), 2])
    }

    plot(dec.pred,dec.obs, pch = 18, cex = 1.5, main = paste(title),
          col = "darkgreen", bg = "darkgreen", xlim = c(0,0.4),
          ylim = c(0,0.4), ylab = "Observed risk", xlab = "Predicted risk",
          cex.lab = 1.25)
    abline(a=0,b=1)
  }

  return(round(sum.tab, 4))
}

