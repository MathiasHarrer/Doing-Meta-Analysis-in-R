#' Toy Dataset for Network Meta-Analysis using the gemtc package
#'
#' This is a toy dataset containing simulated effect size data of a fictitious
#' network meta-analysis examining the effect of psychotherapies. Effect size
#' data is provided as the standardized mean difference (SMD) between the intervention
#' and control group and its corresponding standard error for each study at post.
#' The dataframe layout is optimized for out-of-the-box usage using the \code{data.re}
#' argument of the \code{\link[gemtc]{mtc.network}} function.
#'
#'
#' @format A data.frame with 4 columns.
#' \describe{
#' \item{study}{Character. The name of the included study.}
#' \item{treatment}{Character. The name of the treatment under study. Includes psychotherapies for
#' the treatment of depression, "CBT" (Cognitive Behavioral Therapy), "PDT" (Psychodynamic Therapy),
#' "IPT" (Interpersonal Therapy), "PST" (Problem-solving Therapy) and "SUP" (Supportive Counseling),
#' and comparison conditions, "TAU" (Treatment as usual), "Placebo" (Placebo), and "WLC" (Waitlist control).
#' Each treatment condition in a study is displayed in its own row of the dataframe.}
#' \item{diff}{Numeric. The standardized mean difference of the comparison. The row in each study in which
#' this variable is \code{NA} represents the comparison condition for the effect size displayed above.}
#' \item{std.err}{Numeric. The standard error of the comparison. The row in each study in which
#' this variable is \code{NA} represents the comparison condition for the standard error of
#' the effect size displayed above.}
#' }
#'
#' @source Simulated data.
#'
#' @usage data("NetDataGemtc")
#'
#' @author Mathias Harrer, David Daniel Ebert
#'
"NetDataGemtc"
