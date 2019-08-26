#' Toy Dataset for Multivariate Meta-Regression
#'
#' This is a toy dataset containing simulated effect size data of a fictitious
#' meta-analysis examining the effect of various putative effect moderators. Effect size
#' data is provided as the standardized mean difference (SMD) between the intervention
#' and control group and its corresponding standard error for each study at post.
#' Columns are named in after arguments of the \code{\link[metafor]{rma.uni}}
#' function to facilitate out-of-the-box usage.
#'
#'
#' @format A data.frame with 6 columns.
#' \describe{
#' \item{yi}{Numeric. The calculated standardized mean difference at post-test between the intervention and control group}
#' \item{sei}{Numeric. The standard error of the standardized mean difference}
#' \item{reputation}{Numeric. The mean-centered score signifying the
#' "reputation" (for example, impact factor) of the journal the respective study was published in.}
#' \item{quality}{Numeric. The methodological quality of the study, rated from 0-10 (low to high).}
#' \item{pubyear}{Numeric. The z-standardized year of publication.}
#' \item{continent}{Numeric. The continent the study was conducted in.}
#' }
#'
#' @source Simulated data.
#'
#' @usage data("MVRegressionData")
#'
#' @author Mathias Harrer, David Daniel Ebert
#'
"MVRegressionData"
