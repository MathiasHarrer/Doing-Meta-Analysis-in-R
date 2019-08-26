#' A priori power calculator for subgroup contrasts
#'
#' This function performs an \emph{a priori} power estimation for a test for subgroup differences
#' within a meta-analysis.
#'
#' @usage power.analysis.subgroup(TE1, TE2, seTE1, seTE2, sd1, sd2, var1, var2,
#'     two.tailed=TRUE)
#'
#' @param TE1 Pooled effect size (e.g., standardized mean difference, Hedges' \eqn{g}, log-Odds Ratio or other
#' linear continuous effect size) of the first subgroup of studies.
#' @param TE2 Pooled effect size (e.g., standardized mean difference, Hedges' \eqn{g}, log-Odds Ratio or other
#' linear continuous effect size) of the second subgroup of studies.
#' @param seTE1 Pooled standard error of the first subgroup of studies. Either \code{seTE1/seTE2},
#' \code{sd1/sd2}, or \code{var1/var2} must be provided.
#' @param seTE2 Pooled standard error of the second subgroup of studies. Either \code{seTE1/seTE2},
#' \code{sd1/sd2}, or \code{var1/var2} must be provided.
#' @param sd1 Pooled standard deviation of the first subgroup of studies. Either \code{seTE1/seTE2},
#' \code{sd1/sd2}, or \code{var1/var2} must be provided.
#' @param sd2 Pooled standard deviation of the second subgroup of studies. Either \code{seTE1/seTE2},
#' \code{sd1/sd2}, or \code{var1/var2} must be provided.
#' @param var1 Pooled variance of the first subgroup of studies. Either \code{seTE1/seTE2},
#' \code{sd1/sd2}, or \code{var1/var2} must be provided.
#' @param var2 Pooled variance of the second subgroup of studies. Either \code{seTE1/seTE2},
#' \code{sd1/sd2}, or \code{var1/var2} must be provided.
#' @param two.tailed Logical. Should a two-tailed (\code{TRUE}) or one-tailed (\code{FALSE}) test (\eqn{\alpha = 0.05}) be assumed?
#' Default is \code{TRUE}.
#'
#' @details This function provides an estimate of the power \eqn{1-\beta} of a subgroup contrast analysis provided the
#' assumed effect sizes in each subgroup and their dispersion measures. The function implements the formulae
#' described by Hedges and Pigott (2001).
#'
#' @references
#' Hedges, L. V., & Pigott, T. D. (2001). The power of statistical tests in meta-analysis.
#' \emph{Psychological methods, 6}(3), 203.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @return  Returns the estimated power of the subgroup contrast, expressed as a value between 0 and 1 (i.e., 0\%-100\%).
#'
#' An additional plot is generated, showing the effect size difference (x), power (y), estimated power (red point) and
#' estimated power for changing effect size differences (blue line). A dashed line at 80\% power is also provided as a
#' visual threshold for sufficient power.
#'
#' @export power.analysis.subgroup
#'
#' @import ggplot2
#'
#' @seealso \code{\link{power.analysis}}
#'
#' @examples
#' # Example 1: using standard error and two-tailed test
#' power.analysis.subgroup(TE1=0.30, TE2=0.66, seTE1=0.13, seTE2=0.14)
#'
#' # Example 2: using variance and one-tailed test
#' power.analysis.subgroup(TE1=-0.91, TE2=-1.22, var1 = 0.0023, var2 = 0.0078,
#'     two.tailed = FALSE)

power.analysis.subgroup = function(TE1, TE2, seTE1, seTE2, sd1, sd2, var1, var2, two.tailed = TRUE) {
    gamma = abs(TE1 - TE2)

    if (missing(var1)) {

        if (missing(sd1)) {

            var1 = (((TE1 - 1.96 * seTE1) - TE1)/-1.96)^2
            var2 = (((TE2 - 1.96 * seTE2) - TE2)/-1.96)^2
            varg = var1 + var2

        } else {

            var1 = sd1^2
            var2 = sd2^2
            varg = var1 + var2

        }

    } else {

        varg = var1 + var2

    }

    # Define c_a
    ca_1tail = 1.64
    ca_2tail = 1.96

    # Calculate
    onetail = (1 - pnorm(ca_1tail - (gamma/sqrt(varg))))
    twotail = (1 - pnorm(ca_2tail - (gamma/sqrt(varg))) + pnorm(-ca_2tail - (gamma/sqrt(varg))))

    # Return
    if (two.tailed == TRUE) {

        if (gamma > 1) {

            gammas = (1:((gamma + 3) * 1000))/1000
            powervec = vector()
            for (i in 1:length(gammas)) {
                powervec[i] = (1 - pnorm(ca_2tail - (gammas[i]/sqrt(varg))) + pnorm(-ca_2tail - (gammas[i]/sqrt(varg))))
            }

        } else {

            gammas = (1:1000)/1000
            powervec = vector()
            for (i in 1:length(gammas)) {
                powervec[i] = (1 - pnorm(ca_2tail - (gammas[i]/sqrt(varg))) + pnorm(-ca_2tail - (gammas[i]/sqrt(varg))))
            }

        }

        plotdat = as.data.frame(cbind(gammas, powervec))
        plot = ggplot(data = plotdat, aes(x = gammas, y = powervec)) + geom_line(color = "blue", size = 2) +
            geom_point(aes(x = gamma, y = twotail), color = "red", size = 5) + theme_minimal() + geom_hline(yintercept = 0.8,
            color = "black", linetype = "dashed") + ylab("Power") + xlab("Effect size difference")
        plot(plot)

        if (!is.na(plotdat[plotdat$powervec >= 0.8, ][1, 1])) {
            cat("Minimum effect size difference needed for sufficient power: ", plotdat[plotdat$powervec >=
                0.8, ][1, 1], " (input: ", gamma, ")", "\n", sep = "")
        }

        cat("Power for subgroup difference test (two-tailed): \n")
        return(twotail)

    } else {

        if (gamma > 1) {

            gammas = (1:((gamma + 3) * 1000))/1000
            powervec = vector()
            for (i in 1:length(gammas)) {
                powervec[i] = (1 - pnorm(ca_1tail - (gammas[i]/sqrt(varg))))
            }

        } else {

            gammas = (1:1000)/1000
            powervec = vector()
            for (i in 1:length(gammas)) {
                powervec[i] = (1 - pnorm(ca_1tail - (gammas[i]/sqrt(varg))))
            }

        }

        plotdat = as.data.frame(cbind(gammas, powervec))
        plot = ggplot(data = plotdat, aes(x = gammas, y = powervec)) + geom_line(color = "blue", size = 2) +
            geom_point(aes(x = gamma, y = onetail), color = "red", size = 5) + theme_minimal() + geom_hline(yintercept = 0.8,
            color = "black", linetype = "dashed") + ylab("Power") + xlab("Effect size difference")
        plot(plot)

        if (!is.na(plotdat[plotdat$powervec >= 0.8, ][1, 1])) {
            cat("Minimum effect size difference needed for sufficient power: ", plotdat[plotdat$powervec >=
                0.8, ][1, 1], " (input: ", gamma, ")", "\n", sep = "")
        }

        cat("Power for subgroup difference test (one-tailed): \n")
        return(onetail)

    }

}




