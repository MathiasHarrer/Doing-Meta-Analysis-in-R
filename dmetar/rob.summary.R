#' Create a RevMan-style risk of bias summary chart
#'
#' This function generates summary plots for study quality assessments using the
#' \href{https://bit.ly/2KGQtfG}{Cochrance Risk of Bias Tool}.
#' Summary plots follow the style of \href{https://bit.ly/30eJK29}{RevMan} RoB summary charts.
#'
#' @usage rob.summary(data, name.high="High", name.unclear="Unclear",
#'     name.low="Low", studies, name.missing, table = FALSE)
#'
#' @param data A \code{data.frame} containing a column for each risk of bias criterion, where
#' rows represent each individual studies. The risk of bias assessment for each criterion for each
#' study must be coded as a character string. Up to four codes can be used, referring to low risk of bias,
#' unclear risk of bias, high risk of bias, or missing information. The string used to specify the categories
#' must be specified in \code{name.high}, \code{name.unclear}, \code{name.low} and/or \code{name.missing},
#' unless defaults for those parameters are used.
#' @param name.high Character specifying how the "high risk of bias" category was coded in \code{data}
#' (e.g., \code{name.high = "high"}). Default is \code{"High"}.
#' @param name.unclear Character specifying how the "unclear risk of bias" category was coded in \code{data}
#' (e.g., \code{name.unclear = "unclear"}). Default is \code{"Unclear"}.
#' @param name.low Character specifying how the "low risk of bias" category was coded in \code{data}
#' (e.g., \code{name.low = "low"}). Default is \code{"Low"}.
#' @param name.missing Character specifying how missing information was coded in \code{data}
#' (e.g., \code{name.missing = "missing"}). Default is \code{"Missing"}. All ratings, including missing
#' information, must be coded as strings, so using \code{NA} in \code{data} to signify missing information
#' is not valid.
#' @param studies A vector of the same length as the number of rows in \code{data} specifying the study
#' labels for the risk of bias ratings. Only has to be specified when \code{table = TRUE}.
#' @param table Should an additional RevMan style risk of bias table be produced? If set to \code{TRUE},
#' \code{studies} must be specified. \code{FALSE} by default.
#'
#' @details The function automatically removes seperators like "-" or "." from column names/risk of bias criteria. To produce
#' a "clean" plot, you may therefore seperate words in the column names of the \code{data} data frame using these
#' symbols (e.g. \code{"Allocation_Concealment"} to return "Allocation Concealment").
#'
#' @references Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803.
#' \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/creating-a-revman-style-risk-of-bias-summary.html}{Chapter 10}
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @import ggplot2
#' @importFrom tidyr gather
#'
#' @export rob.summary
#'
#' @seealso
#' \code{\link{direct.evidence.plot}}
#'
#' @examples
#' # Example 1: No missing information, only produce summary plot
#' data = data.frame(
#'        "study" = c("Higgins et al., 2011", "Borenstein et al., 2008", "Holm, 1971",
#'                    "Zajonc et al., 2005", "Cuijpers, 2014"),
#'        "Allocation_concealment" = c("Low", "High", "High", "Unclear", "High"),
#'        "Randomization" = c("Low", "High", "Unclear", "Low", "High"),
#'        "Sequence_generation" = c("Low", "High", "Unclear", "Unclear", "High"),
#'        "ITT.Analyses" = c("Low", "High", "Unclear", "Unclear", "Unclear"),
#'        "Selective_outcome_reporting" = c("Low", "High", "High", "High", "Unclear")
#'        )
#' rob.summary(data)
#'
#' # Example 2: Missing information, produce additional summary table
#' data2 = data.frame(
#'         "study" = c("Higgins et al., 2011", "Borenstein et al., 2008", "Holm, 1971",
#'                     "Zajonc et al., 2005", "Cuijpers, 2014"),
#'         "Allocation_concealment" = c("low", "high", "high", "uc", "high"),
#'         "Randomization" = c("low", "high", "miss", "low", "high"),
#'         "Sequence_generation" = c("low", "high", "uc", "uc", "high"),
#'         "ITT.Analyses" = c("low", "high", "uc", "uc", "uc"),
#'         "Selective_outcome_reporting" = c("low", "high", "high", "high", "uc")
#'        )
#' rob.summary(data2, name.high = "high", name.unclear = "uc", name.low = "low",
#'     name.missing = "miss", studies = data2$study, table = TRUE)


rob.summary = function(data,
                       name.high="High",
                       name.unclear="Unclear",
                       name.low="Low",
                       studies,
                       name.missing,
                       table = FALSE){

  # Class Checks
  if (class(data) != "data.frame"){
    stop("'data' must be of class 'data.frame'.")
  }


  if (missing(name.missing)){

    # Only select columns with RoB data

    colnames.rob = character()

    for (i in 1:ncol(data)){

      vect = as.character(data[,i])

      for (j in 1:length(data[,i])){

        if (vect[j] %in% c(name.high, name.unclear, name.low)){

          colnames.rob[i] = TRUE

        } else {

          colnames.rob[i] = FALSE
          message(cat("Column '", colnames(data)[i],
                      "' removed from plot because it did not contain the specified RoB ratings (only). \n",
                      sep=""))
          break

        }
      }
    }

    # Use mask: rob data
    rob = data[ , as.logical(colnames.rob)]

    # Relevel for plot
    for (i in 1:ncol(rob)){

      rob[,i] = as.character(rob[,i])
      rob[rob[,i]==name.high,i] = "High"
      rob[rob[,i]==name.unclear,i] = "Unclear"
      rob[rob[,i]==name.low,i] = "Low"

    }

    # Make table
    if (table == TRUE){

      if (missing(studies)){
        stop("'studies' has to be specified when 'table = TRUE'.")
      }

      if (length(as.vector(studies)) != nrow(data)){
        stop("'studies' vector is not of equal length as the data.")
      }

      robby = rob
      robby$study = studies
      robby = gather(robby, condition, measurement, -study)
      robby$condition = gsub("_"," ", robby$condition)
      robby$condition = gsub("-"," ", robby$condition)
      robby$condition = gsub("\\."," ", robby$condition)
      robby[robby$measurement=="Low", "measurement"] = "+"
      robby[robby$measurement=="Unclear", "measurement"] = "?"
      robby[robby$measurement=="High", "measurement"] = "-"

      rob.table = ggplot(data = robby, aes(y = condition, x = study)) +
        geom_tile(color="black", fill="white", size = 0.8) +
        geom_point(aes(color=as.factor(measurement)), size=20) +
        geom_text(aes(label = measurement), size = 8) +
        scale_x_discrete(position = "top") +
        scale_color_manual(values = c("?" = "#E2DF07",
                                      "-" = "#BF0000",
                                      "+" = "#02C100")) +
        theme_minimal() +
        coord_equal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = 15, color = "black"),
              axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust=0),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())

    }

    # Make long format, clean the factors
    rob.long = gather(rob, condition, measurement, factor_key = TRUE)
    rob.long$condition = gsub("_"," ",rob.long$condition)
    rob.long$condition = gsub("-"," ",rob.long$condition)
    rob.long$condition = gsub("\\."," ",rob.long$condition)
    rob.long$measurement = as.factor(rob.long$measurement)
    rob.long$measurement = factor(rob.long$measurement, levels(rob.long$measurement)[c(1, 3, 2)])

    # Make plot
    rob.plot = ggplot(data = rob.long) +
      geom_bar(mapping = aes(x = condition, fill = measurement), width = 0.7,
               position = "fill", color = "black") +
      coord_flip(ylim = c(0, 1)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual("Risk of Bias",
                        labels = c("    High risk of bias          ",
                                   "    Unclear risk of bias       ",
                                   "    Low risk of bias  "),
                        values = c(Unclear = "#E2DF07", High = "#BF0000", Low = "#02C100")) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 18, color = "black"),
            axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.background = element_rect(linetype = "solid", colour = "black"),
            legend.title = element_blank(),
            legend.key.size = unit(0.75, "cm"),
            legend.text = element_text(size = 14))

    plot(rob.plot)

    if (table == TRUE){
      plot(rob.table)
    }

  } else {

    # Only select columns with RoB data
    data = as.data.frame(data)

    colnames.rob = character()

    for (i in 1:ncol(data)){

      vect = as.character(data[,i])

      for (j in 1:length(data[,i])){

        if (vect[j] %in% c(name.high, name.unclear, name.low, name.missing)){

          colnames.rob[i] = TRUE

        } else {

          colnames.rob[i] = FALSE
          message(cat("Column '", colnames(data)[i],
                      "' removed from plot because it did not contain the specified RoB ratings (only). \n",
                      sep=""))
          break

        }
      }
    }

    # Use mask: rob data
    rob = data[ , as.logical(colnames.rob)]

    # Relevel for plot
    for (i in 1:ncol(rob)){

      rob[,i] = as.character(rob[,i])
      rob[rob[,i]==name.high,i] = "High"
      rob[rob[,i]==name.unclear,i] = "Unclear"
      rob[rob[,i]==name.low,i] = "Low"
      rob[rob[,i]==name.missing,i] = "Missing"

    }

    # Make Table

    if (table == TRUE){

      if (missing(studies)){
        stop("'studies' has to be specified when 'table = TRUE'.")
      }

      if (length(as.vector(studies)) != nrow(data)){
        stop("'studies' vector is not of equal length as the data.")
      }

      robby = rob
      robby$study = studies
      robby = gather(robby, condition, measurement, -study)
      robby$condition = gsub("_"," ", robby$condition)
      robby$condition = gsub("-"," ", robby$condition)
      robby$condition = gsub("\\."," ", robby$condition)
      robby[robby$measurement=="Low", "measurement"] = "+"
      robby[robby$measurement=="Unclear", "measurement"] = "?"
      robby[robby$measurement=="High", "measurement"] = "-"
      robby[robby$measurement=="Missing", "measurement"] = " "

      rob.table = ggplot(data = robby, aes(y = condition, x = study)) +
        geom_tile(color="black", fill="white", size = 0.8) +
        geom_point(aes(color=as.factor(measurement)), size=20) +
        geom_text(aes(label = measurement), size = 8) +
        scale_x_discrete(position = "top") +
        scale_color_manual(values = c("?" = "#E2DF07",
                                      "-" = "#BF0000",
                                      "+" = "#02C100",
                                      " " = "white")) +
        theme_minimal() +
        coord_equal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = 15, color = "black"),
              axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust=0),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())

    }


    # Make long format, clean the factors
    rob.long = gather(rob, condition, measurement, factor_key = TRUE)
    rob.long$condition = gsub("_"," ",rob.long$condition)
    rob.long$condition = gsub("-"," ",rob.long$condition)
    rob.long$condition = gsub("\\."," ",rob.long$condition)
    rob.long$measurement = as.factor(rob.long$measurement)
    rob.long$measurement = factor(rob.long$measurement, levels(rob.long$measurement)[c(3,1,4,2)])

    rob.plot = ggplot(data = rob.long) +
      geom_bar(mapping = aes(x = condition, fill = measurement), width = 0.7,
               position = "fill", color = "black") +
      coord_flip(ylim = c(0, 1)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual("Risk of Bias",
                        labels = c("  Missing information  ",
                                   "  High risk of bias   ",
                                   "  Unclear risk of bias  ",
                                   "  Low risk of bias  "),
                        values = c(Unclear = "#E2DF07",
                                   High = "#BF0000",
                                   Low = "#02C100",
                                   Missing = "white")) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 18, color = "black"),
            axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.background = element_rect(linetype = "solid", colour = "black"),
            legend.title = element_blank(),
            legend.key.size = unit(0.75, "cm"),
            legend.text = element_text(size = 14))

    plot(rob.plot)

    if (table == TRUE){
      plot(rob.table)
    }

  }

}


