#' Pool the results of two treatment arms
#'
#' This function allows to pool the mean, standard deviation and sample size of two experimental groups
#' of a study. Results of two treatment arms may be pooled to mitigate the unit-of-analysis problem
#' and avoid "double-counting" in meta-analyses in which studies with more than two experimental groups
#' are included.
#'
#' @usage pool.groups(n1, n2, m1, m2, sd1, sd2)
#'
#' @param n1 Numeric vector or single number. The number of participants in arm 1.
#' @param n2 Numeric vector or single number. The number of participants in arm 2.
#' @param m1 Numeric vector or single number. The mean in arm 1.
#' @param m2 Numeric vector or single number. The mean in arm 2.
#' @param sd1 Numeric vector or single number. The standard deviation (\eqn{SD}) in arm 1.
#' @param sd2 Numeric vector or single number. The standard deviation (\eqn{SD}) in arm 2.
#'
#' @details Many randomized-controlled trials do not only include a single intervention
#' and control group, but compare the effect of two or more interventions to a control
#' group. It might be tempting in such a scenario to simply include all the comparisons
#' between the intervention groups and control within a study into one meta-analysis.
#' Yet, researchers should abstain from this practice, as this would mean that the
#' control group is used twice for the meta-analysis, thus “double-counting” the
#' participants in the control group. This results in a \strong{unit-of-analysis error}, as
#' the effect size are correlated, and thus not independent, but are treated as if they
#' would stem from independent samples.
#'
#' One way to deal with this is to synthesize the results of the intervention arms to
#' obtain one single comparison to the control group. Despite its practical limitations
#' (sometimes, this would mean synthesizing the results from extremely different types
#' of interventions), this procedure does get rid of the unit-of-analysis error problem.
#'
#' To synthesize the pooled effect size data (pooled Mean, Standard Deviation and \eqn{N}), the
#' following formulae are used:
#'
#'\deqn{N_{pooled}=N_1+N_2}
#'\deqn{M_{pooled}=\frac{N_1M_1+N_2M_2}{N_1+N_2}}
#'\deqn{SD_{pooled} = \sqrt{\frac{(N_1-1)SD^{2}_{1}+ (N_2-1)SD^{2}_{2}+\frac{N_1N_2}{N_1+N_2}(M^{2}_1+M^{2}_2-2M_1M_2)} {N_1+N_2-1}}}
#'
#' \strong{What should i do when an study has more than two intervention groups?}
#'
#' If a study has more than one two intervention groups you want to synthesize
#' (e.g. four arms, with three distinct intervention arms), you can pool the effect
#' size data for the first two interventions, and then synthesize the pooled data you
#' calculated with the data from the third group.
#'
#' @references Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/i.html}{Chapter 13.9}
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @return Returns a data.frame containing the following columns:
#' \itemize{
#' \item \code{Mpooled}: The pooled mean of both groups
#' \item \code{SDpooled}: The pooled standard deviation of both groups
#' \item \code{Npooled}: The pooled number of participants of both groups
#' }
#'
#' @export
#'
#' @seealso \code{\link{se.from.p}}
#'
#' @examples
#' pool.groups(n1 = 50, n2 = 56,
#'     m1 = 7.83, m2 = 8.32,
#'     sd1 = 3.52, sd2 = 2.25)



pool.groups = function(n1, n2, m1, m2, sd1, sd2) {

    n1 = n1
    n2 = n2
    m1 = m1
    m2 = m2
    sd1 = sd1
    sd2 = sd2

    if (is.numeric(n1) == FALSE) {
        stop("'n1' must by of type numeric().")
    }

    if (is.numeric(n2) == FALSE) {
        stop("'n2' must by of type numeric().")
    }

    if (is.numeric(m1) == FALSE) {
        stop("'m1' must by of type numeric().")
    }

    if (is.numeric(m2) == FALSE) {
        stop("'m2' must by of type numeric().")
    }

    if (is.numeric(sd1) == FALSE) {
        stop("'sd1' must by of type numeric().")
    }

    if (is.numeric(sd2) == FALSE) {
        stop("'sd2' must by of type numeric().")
    }

    Npooled = n1 + n2
    Mpooled = (n1 * m1 + n2 * m2)/(n1 + n2)
    SDpooled = sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2 + (((n1 * n2)/(n1 + n2)) * (m1^2 + m2^2 - 2 * m1 *
        m2)))/(n1 + n2 - 1))

    return(data.frame(Mpooled, SDpooled, Npooled))
}
