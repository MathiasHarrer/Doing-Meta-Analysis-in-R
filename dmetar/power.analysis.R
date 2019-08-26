#' A priori power calculator
#'
#' This function performs an \emph{a priori} power estimation of a meta-analysis
#' for different levels of assumed between-study heterogeneity.
#'
#' @usage power.analysis(d, OR, k, n1, n2, p = 0.05, heterogeneity = 'fixed')
#'
#' @param d The hypothesized, or plausible overall effect size of a treatment/intervention under study compared
#' to control, expressed as the standardized mean difference (SMD). Effect sizes must be positive
#' numerics (i.e., expressed as positive effect sizes).
#' @param OR The hypthesized, or plausible overall effect size of a treatment/intervention under study compared
#' to control, expressed as the Odds Ratio (OR). If both \code{d} and \code{OR} are specified, results
#' will only be computed for the value of \code{d}.
#' @param k The expected number of studies to be included in the meta-analysis.
#' @param n1 The expected, or plausible mean sample size of the treatment group in the studies to be included in the meta-analysis.
#' @param n2 The expected, or plausible mean sample size of the control group in the studies to be included in the meta-analysis.
#' @param p The alpha level to be used for the power computation. Default is \eqn{\alpha = 0.05}.
#' @param heterogeneity Which level of between-study heterogeneity to assume for the meta-analysis. Can be either
#' \code{"fixed"} for no heterogeneity/a fixed-effect model, \code{"low"} for low heterogeneity, \code{"moderate"}
#' for moderate-sized heterogeneity or \code{"high"} for high levels of heterogeneity. Default is \code{"fixed"}.
#'
#' @details While researchers conducting primary studies can plan the size of their sample
#' based on the effect size they want to find, the situation is a different in
#' meta-analysis, where one can only work with the published material.
#' However, researchers have some control over the number of studies they want to include in their
#' meta-analysis (e.g., through more leniently or strictly defined inclusion criteria).
#' Therefore, one can change the power to some extent by including more or less studies into
#' the meta-analysis. Conventionally, a power of \eqn{1-\beta = 0.8} is deemed suffienct to detect an existing effect.
#' There are four things one has to make assumptions about when assessing the power of meta-analysis a priori.
#'
#' \itemize{
#' \item The number of included or includable studies
#' \item The overall size of the studies we want to include (are the studies in the field rather small or large?)
#' \item The effect size to determine. This is particularly important, as assumptions have to be made
#' about how big an effect size has to be to still be clinically meaningful. One study calculated
#' that for interventions against depression, even effects as small as \emph{SMD}=0.24 may still
#' be meaningful for patients (Cuijpers et al. 2014). If the aim is to study negative effects of an
#' intervention (e.g., death or symptom deterioration), even very small effect sizes are extremely
#' important and should be detected.
#' \item The heterogeneity of our studies’ effect sizes, as this also affects the precision of the pooled estimate,
#' and thus its potential to find significant effects.
#' }
#'
#' The \code{power.analysis} function implements the formula by Borenstein et al. (2011) to calculate
#' the power estimate. Odds Ratios are converted to \code{d} internally before the power is estimated, and
#' are then reconverted.
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/power-analysis.html}{Chapter 13}
#'
#'Cuijpers, P., Turner, E.H., Koole, S. L., Van Dijke, A., & Smit, F. (2014).
#'What Is the Threshold for a Clinically Relevant Effect? The Case of Major Depressive Disorders.
#'\emph{Depression and Anxiety, 31}(5): 374–78.
#'
#'Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2011). Introduction to Meta-Analysis. John Wiley & Sons.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @return The \strong{estimated power} of the meta-analysis, expressed as a value between 0 and 1 (i.e., 0\%-100\%).
#'
#' An additional plot is generated, showing the effect size (x), power (y), estimated power (red point) and
#' estimated power for changing effect sizes (blue line). A dashed line at 80\% power is also provided as a
#' visual threshold for sufficient power.
#'
#' @export power.analysis
#'
#' @import ggplot2
#'
#' @seealso \code{\link{power.analysis.subgroup}}
#'
#' @examples
#'
#' # Example 1: Using SMD and fixed-effect model (no heterogeneity)
#' power.analysis(d=0.124, k=10, n1=50, n2=50, heterogeneity = 'fixed')
#'
#' # Example 2: Using OR and assuming moderate heterogeneity
#' power.analysis(OR=0.77, k=12, n1=50, n2=50, heterogeneity = 'high')


power.analysis = function(d, OR, k, n1, n2, p = 0.05, heterogeneity = "fixed") {

    odds = FALSE

    if (missing(OR) & missing(d)) {
        stop("Either 'd' or 'OR' must be provided.")
    }

    if (!(heterogeneity %in% c("fixed", "low", "moderate", "high"))) {
        stop("'heterogeneity' must be either 'fixed', 'low', 'moderate', 'high'.")
    }

    # Cohen's d not provided: calculate from OR
    if (missing(d)) {
        odds = TRUE
        d = log(OR) * (sqrt(3)/pi)
        cat("Power Analysis based on log-transformed OR. \n")
    } else {

    }

    es = d

    if (heterogeneity == "fixed") {

        het.factor = 1
        v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
        v.m = v.d/k
        lambda = (d/sqrt(v.m))
        plevel = 1 - (p/2)
        zval = qnorm(p = plevel, 0, 1)
        power = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))

        cat("Fixed-effect model used. \n")

    }

    if (heterogeneity == "low") {

        het.factor = 1.33
        v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
        v.m = v.d/k
        v.m = 1.33 * v.m
        lambda = (d/sqrt(v.m))
        plevel = 1 - (p/2)
        zval = qnorm(p = plevel, 0, 1)
        power = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))

        cat("Random-effects model used (low heterogeneity assumed). \n")

    }

    if (heterogeneity == "moderate") {

        het.factor = 1.67
        v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
        v.m = v.d/k
        v.m = 1.67 * v.m
        lambda = (d/sqrt(v.m))
        plevel = 1 - (p/2)
        zval = qnorm(p = plevel, 0, 1)
        power = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))

        cat("Random-effects model used (moderate heterogeneity assumed). \n")

    }

    if (heterogeneity == "high") {

        het.factor = 2
        v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
        v.m = v.d/k
        v.m = 2 * v.m
        lambda = (d/sqrt(v.m))
        plevel = 1 - (p/2)
        zval = qnorm(p = plevel, 0, 1)
        power = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))

        cat("Random-effects model used (high heterogeneity assumed). \n")

    }

    # Loop for data for plot
    dvec = (1:1000)/1000

    if (d > 1) {
        dvec = (1:(d * 1000))/1000
    }

    powvect = vector()

    for (i in 1:length(dvec)) {
        d = dvec[i]
        v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
        v.m = v.d/k
        v.m = het.factor * v.m
        lambda = (d/sqrt(v.m))
        plevel = 1 - (p/2)
        zval = qnorm(p = plevel, 0, 1)
        powvect[i] = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))
    }

    # Generate plot

    if (odds == FALSE) {
        plotdat = as.data.frame(cbind(dvec, powvect))
        plot = ggplot(data = plotdat, aes(x = dvec, y = powvect)) + geom_line(color = "blue", size = 2) +
            geom_point(aes(x = es, y = power), color = "red", size = 5) + theme_minimal() + geom_hline(yintercept = 0.8,
            color = "black", linetype = "dashed") + ylab("Power") + xlab("Effect size (SMD)")
    } else {
        dvecs = exp(dvec * (pi/sqrt(3)))
        dvec.inv = exp(-dvec * (pi/sqrt(3)))
        dvec = as.vector(rbind(dvec.inv, dvecs))
        powvect = as.vector(rbind(powvect, powvect))
        plotdat = as.data.frame(cbind(dvec, powvect))
        plot = ggplot(data = plotdat, aes(x = dvec, y = powvect)) + geom_line(color = "blue", size = 2) +
            geom_point(aes(x = exp(es * (pi/sqrt(3))), y = power), color = "red", size = 5) + theme_minimal() +
            geom_hline(yintercept = 0.8, color = "black", linetype = "dashed") + ylab("Power") + xlab("Effect size (OR)") +
            scale_x_log10()
    }

    plot(plot)
    cat("Power: \n")
    return(power)

}





