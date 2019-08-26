#' Calculate the number needed to treat (NNT)
#'
#' This function calculates the number needed to treat (\eqn{NTT}) using event data or
#' effect sizes (such as Cohen's \eqn{d} or Hedges' \eqn{g}).
#'
#' @usage NNT(d, CER, event.e, n.e, event.c, n.c, names, method)
#'
#' @param d A single numeric or concatenated vector of numerics representing the effect size expressed as
#' Cohen's \eqn{d} or Hedges' \eqn{g}. If this is the only parameter specified in the function, the method by
#' Kraemer and Kupfer is used automatically to calculate \eqn{NNT}s.
#' @param CER The control group event ratio. Furukawa's method (Furukawa & Leucht, 2011) to calculate NNTs
#' from \code{d} requires that the assumed response ("event") ratio in the control group (\eqn{\frac{n_{responders}}{N_{total}}})
#' is specified. The CER can assume values from 0 to 1. If a value is specified for \code{CER}, Furukawa's method is
#' used automatically. Parameter \code{method} has to be set to \code{"KraemerKupfer"} to override this.
#' @param event.e Single number or numeric vector. The number of (favourable) events in the experimental group.
#' @param n.e Single number or numeric vector. The number participants in the experimental group.
#' @param event.c Single number or numeric vector. The number of (favourable) events in the control group.
#' @param n.c Single number or numeric vector. The number participants in the control group.
#' @param names Optional. Character vector of equal length as the vector supplied to \code{d} or \code{event.e} containing
#' study/effect size labels.
#' @param method The method to be used to calculate the NNT from \code{d}. Either \code{"KraemerKupfer"} for the
#' method proposed by Kraemer and Kupfer (2006) or \code{"Furukawa"} for the Furukawa method (Furukawa & Leucht, 2011).
#' Please note that the Furukawa method can only be used when \code{CER} is specified.
#'
#' @details This function calculates the number needed to treat (\eqn{NNT}) from effect sizes (Cohen's \eqn{d} and Hedges' \eqn{g})
#' or, alternatively, from raw event data.
#'
#' Two methods to calculate the \eqn{NTT} from \code{d} are implemented in this function.
#' \itemize{
#' \item The method by \strong{Kraemer and Kupfer} (2006),
#' calculates\eqn{NTT} from the Area Under the Curve (\eqn{AUC}) defined as the probability that a patient in the treatment
#' has an outcome preferable to one in the control. This method allows to calculate the NNT directly from \code{d} without
#' any extra variables.
#' \item The method by \strong{Furukawa} calculates the \eqn{NNT} from \code{d} using a reasonable estatimate
#' of \eqn{CER}, in this context the response rate in the control group.
#' }
#'
#' Furukawa's method has been shown to be superior in predicting
#' the \eqn{NNT} compared to the Kraemer & Kupfer method (Furukawa & Leucht, 2011). If reasonable assumptions can be made concerning
#' the \eqn{CER}, Furukawa's method should therefore be preferred.
#'
#' When event data is used for the function, the \eqn{CER} and \eqn{EER} (experimental group event rate) is calculated internally, and
#' the standard definition of the \eqn{NTT}, \eqn{\frac{1}{EER-CER}}, is used.
#'
#' Please note that negative NNT values returned by the function refer to the number needed to harm (\eqn{NNH}), as the intervention
#' is assumed to be inferior to the control group treatment based on the effect size data supplied to the function.
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/smallstudyeffects.html}{Chapter 9.1}
#'
#'Furukawa, T. A., & Leucht, S. (2011). How to obtain NNT from Cohen's d: comparison of two methods. \emph{PloS one, 6}(4), e19070.
#'
#' Kraemer H.C., Kupfer D.J. (2006) Size of treatment effects and their importance
#' to clinical research and practice. {Biol. Psychiatry 59}: 990–996.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @export NNT
#'
#' @seealso \code{\link{se.from.p}}
#'
#' @examples
#'
#' # Example 1: Convert Cohen's d using the Kraemer & Kupfer method
#' d = c(-0.123, 0.234, 0.123, 1.234, 0.12)
#' NNT(d)
#'
#' # Example 2: Convert Cohen's d using the Furukawa method
#' d = c(-0.123, 0.234, 0.123, 1.234, 0.12)
#' CER = c(0.42, 0.35, 0.26, 0.21, 0.23)
#' NNT(d, CER)
#'
#' # Example 3: Convert event data
#' NNT(event.e = 10, event.c = 20, n.e = 200, n.c = 200)


NNT = function(d, CER, event.e, n.e, event.c, n.c, names, method) {


    # Calculate Cohens-d
    if (missing(event.e)) {

        if (missing(CER)) {

            # Use Kraemer
            NNT = 1/((2 * pnorm(d/sqrt(2)) - 1))
            cat("Kraemer & Kupfer's method used. \n")

        } else {

            if (missing(method)) {
                NNT = 1/(pnorm(d + qnorm(CER)) - CER)
                cat("Furukawa's method used. \n")

            } else {

                if (method == "KraemerKupfer") {

                  NNT = 1/((2 * pnorm(d/sqrt(2)) - 1))
                  cat("Kraemer & Kupfer's method used. \n")

                } else {

                }

                if (method == "Furukawa") {

                  if (missing(CER) | class(CER) != "numeric") {

                    stop("To use Furukawa's method, provide a numeric value for CER. \n")

                  } else {

                    NNT = 1/(pnorm(d + qnorm(CER)) - CER)
                    cat("Furukawa's method used. \n")
                  }

                } else {

                }

            }

        }
    } else {

        # Calculate from raw event data
        if (class(event.e) == "numeric" & missing(d)) {
            EER = event.e/n.e
            CER = event.c/n.c
            NNT = abs(1/(EER - CER))

            if (missing(method)) {

            } else {

                if (method %in% c("KraemerKupfer", "Furukawa")) {
                  cat("NNTs were calculated from raw data, so neither Kraemer & Kupfer nor Furukawa method was used.")
                } else {

                }

            }

        }
    }

    if (sum(NNT < 0) > 0) {
        cat("Negative NNT values refer to the number needed to harm (NNT) \n")
    }

    if (missing(names)) {

        return(NNT)

    } else {

        data = data.frame(Name = names, NNT = NNT)

        return(data)
        return(NNT)
    }

}

