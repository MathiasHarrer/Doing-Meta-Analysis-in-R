#' Toy Dataset for Network Meta-Analysis using the netmeta package
#'
#' This is a toy dataset containing simulated effect size data of a fictitious
#' network meta-analysis examining the effect of psychotherapies. Effect size
#' data is provided as the standardized mean difference (SMD) between the intervention
#' and control group and its corresponding standard error for each study at post.
#' The dataframe layout is optimized for out-of-the-box usage using
#' the \code{\link[netmeta]{netmeta}} function.
#'
#'
#' @format A data.frame with 5 columns.
#' \describe{
#' \item{studlab}{Character. The name of the included study.}
#' \item{treat1}{Character. The name of the first treatment. Includes psychotherapies for
#' the treatment of depression, "CBT" (Cognitive Behavioral Therapy), "PDT" (Psychodynamic Therapy),
#' "IPT" (Interpersonal Therapy), "PST" (Problem-solving Therapy) and "SUP" (Supportive Counseling),
#' and standard comparison conditions, "TAU" (Treatment as usual), "Placebo" (Placebo), and "WLC" (Waitlist control).}
#' \item{treat2}{Character. The name of the treatment the first treatment was compared to. Includes psychotherapies for
#' the treatment of depression, "CBT" (Cognitive Behavioral Therapy), "PDT" (Psychodynamic Therapy),
#' "IPT" (Interpersonal Therapy), "PST" (Problem-solving Therapy) and "SUP" (Supportive Counseling),
#' and standard comparison conditions, "TAU" (Treatment as usual), "Placebo" (Placebo), and "WLC" (Waitlist control).}
#' \item{TE}{Numeric. The standardized mean difference of the comparison.}
#' \item{seTE}{Numeric. The standard error of the comparison.}
#' }
#'
#' @source Simulated data.
#'
#' @usage data("NetDataNetmeta")
#'
#' @author Mathias Harrer, David Daniel Ebert
#'
"NetDataNetmeta"
