#' 'Third-Wave' cognitive behavioral interventions for perceived stress in college students dataset
#'
#' This is a toy dataset containing pre-calculated effect size data of a meta-analysis on
#' randomized controlled trials comparing the effectiveness of 'third-wave' CBT interventions
#' for perceived stress in college students to inactive controls. Effect size data is provided
#' as the standardized mean difference (SMD) between the intervention and control group
#' and its corresponding standard error for each study at post.
#' The dataset also contains columns for study characteristics which may serve as potential
#' effect size moderators.
#'
#' @format A data.frame with 8 columns.
#' \describe{
#' \item{Author}{Character. The study label containing the author(s) of the study}
#' \item{TE}{Numeric. The calculated standardized mean difference at post-test between the intervention and control group}
#' \item{seTE}{Numeric. The standard error of the standardized mean difference}
#' \item{RiskOfBias}{Character. The risk of bias rating according to the Cochrance Risk of Bias Tool.}
#' \item{TypeControlGroup}{Character. The type of control group used in the study}
#' \item{InterventionDuration}{Character. The dichotomized duration of the intervention}
#' \item{InterventionType}{Character. The type of third-wave intervention rationale used}
#' \item{ModeOfDelivery}{Character. The mode of delivery used for the intervention}
#' }
#'
#' @source Slightly changed dataset of a meta-analysis on third-wave CBT interventions for
#' perceived stress in college students.
#'
#' @usage data(ThirdWave)
#'
#' @author Mathias Harrer, Eva-Maria Rathner, David Daniel Ebert
#'
"ThirdWave"
