#' Calculate the standard error from the effect size and p-value
#'
#' This function calculates the standard error of an effect size provided the exact
#' \eqn{p}-value and (continuous) effect size according to the formula
#' by \href{https://www.ncbi.nlm.nih.gov/pubmed/21824904}{Altman and Bland (2011)}.
#'
#' @usage se.from.p(effect.size, p, N, effect.size.type = 'difference',
#'     calculate.g = FALSE)
#'
#' @param effect.size Numeric vector or single number. The effect size, such as the
#' standardized mean difference, Hedges' \eqn{g} or other continuous effect size.
#' @param p Numeric vector or single number. The exact \eqn{p}-value corresponding to the
#' effect size.
#' @param N Numeric vector or single number. The total number of samples used to
#' calculate the effect size/\eqn{p}-value.
#' @param effect.size.type The type of effect sizes provided in \code{effect.size}. For
#' effect sizes based on differences (e.g., mean differences), this parameter has to be
#' set to \code{"difference"}. For effect sizes based on ratios (e.g., risk ratio, odds ratio),
#' this parameter has to be set to \code{"ratio"}.
#' @param calculate.g Logical. Calculates the standardized mean difference
#' corrected for small sample bias (Hedges' \eqn{g}). \code{FALSE} by default.
#'
#' @details This function calculates the standard error, standard deviation and 95\% confidence
#' interval of an effect size given the effect size and exact \eqn{p}-value. The function can be used for
#' \itemize{
#' \item effect sizes based on \strong{differences} (e.g., mean differences) by setting \code{effect.size.type}
#' to \code{"difference"}, or
#' \item effect sizes based on \strong{ratios} (e.g. risk ratios, odds ratios or
#' hazard ratios) by setting \code{effect.size.type} to \code{"ratio"}. When ratios are used, the
#' function returns the log-transformed effect sizes, standard error, standard deviation and confidence interval,
#' which can be used for meta-analytic pooling using the \code{\link[meta]{metagen}} function,
#' along with the original effect size and confidence interval.
#' }
#'
#' @references Altman D.G. & Bland J.M. (2011) How to obtain the confidence interval
#' of a \emph{p} value. \emph{BMJ 343}:d2090.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @import esc
#'
#' @return A dataframe containing the following columns:
#' \itemize{
#' \item \code{(log)EffectSize}: The input effect size. Log-transformed if \code{effect.size.type="ratio"}.
#' \item \code{Hedges.g}: The calculated Hedges' g values (only if \code{calculate.g=TRUE}).
#' \item \code{(log)StandardError}: The standard error (SE) for the effect size. Log-transformed if \code{effect.size.type="ratio"}.
#' \item \code{(log)LLCI} and \code{(log)ULCI}: The lower and upper 95\% confidence interval of the effect size. Log-transformed if \code{effect.size.type="ratio"}.}
#'
#' @export se.from.p
#'
#' @examples
#' # Example 1: one single effect size
#' se.from.p(effect.size = 0.71, p = 0.013, N = 75,
#'     effect.size.type= "difference", calculate.g = TRUE)
#'
#' # Example 2: vector of effect sizes (Odds Ratio)
#' effect.size = c(0.91, 1.01, 0.72, 0.43)
#' p = c(0.05, 0.031, 0.001, 0.09)
#' N = c(120, 86, 450, 123)
#' se.from.p(effect.size = effect.size, p = p, N = N,
#'     effect.size.type = "ratio")


se.from.p = function(effect.size, p, N, effect.size.type = "difference", calculate.g = FALSE) {

    # Set params
    ES = effect.size
    p = p
    N = N
    ES.type = effect.size.type
    calculate.g = calculate.g

    if (is.numeric(ES) == FALSE) {
        stop("'effect.size' is not of type numeric().")
    }

    if (is.numeric(p) == FALSE) {
        stop("'p' is not of type numeric().")
    }

    if (is.numeric(N) == FALSE) {
        stop("'N' is not of type numeric().")
    }

    if (ES.type %in% c("difference", "ratio") == FALSE) {
        stop("'effect.size.type' must be either 'difference' or 'ratio'.")
    }

    # Difference vs. Ratio

    # Difference
    if (ES.type == "difference") {

        if (calculate.g == TRUE) {
            ES = hedges_g(d = ES, totaln = N)
            z = -0.862 + sqrt(0.743 - 2.404 * log(p))
            SE = ES/z
            SD = SE * sqrt(N)
            LLCI = ES - 1.96 * SE
            ULCI = ES + 1.96 * SE
            data = data.frame(ES, SE, SD, LLCI, ULCI)
            colnames(data) = c("Hedges.g", "StandardError", "StandardDeviation", "LLCI", "ULCI")

        } else {

            z = -0.862 + sqrt(0.743 - 2.404 * log(p))
            SE = ES/z
            SD = SE * sqrt(N)
            LLCI = ES - 1.96 * SE
            ULCI = ES + 1.96 * SE
            data = data.frame(ES, SE, SD, LLCI, ULCI)
            colnames(data) = c("EffectSize", "StandardError", "StandardDeviation", "LLCI", "ULCI")
        }

    }

    if (ES.type == "ratio") {

        if (calculate.g == TRUE) {

            stop("Hedges' g cannot be calculated for ratios using this function; set 'calculate.g=FALSE'.")

        } else {

            z = -0.862 + sqrt(0.743 - 2.404 * log(p))
            ES = log(ES)
            SE = abs(ES/z)
            SD = SE * sqrt(N)
            LLCI = ES - 1.96 * SE
            ULCI = ES + 1.96 * SE

            # Exponentiate to get original scale
            expES = exp(ES)
            expLLCI = exp(LLCI)
            expULCI = exp(ULCI)

            data = data.frame(ES, SE, SD, LLCI, ULCI, expES, expLLCI, expULCI)
            colnames(data) = c("logEffectSize", "logStandardError", "logStandardDeviation", "logLLCI", "logULCI", "EffectSize",
                "LLCI", "ULCI")
        }
    }
    return(data)
}


