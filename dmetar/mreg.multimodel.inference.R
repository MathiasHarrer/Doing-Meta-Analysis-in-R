#' Perform multimodel inference on a meta-regression model
#'
#' This function performs multimodel inference to evaluate the importance of predictors
#' in a meta-analytical meta-regression model.
#'
#' @usage multimodel.inference(TE, seTE, data, predictors, method='REML', test='knha',
#'     eval.criterion='AICc', interaction=FALSE, seed = 123)
#'
#' @param TE The precalculated effect size for each study. Must be supplied as the name of the effect size
#' column in the dataset (in quotation marks; e.g. \code{TE = "effectsize"}).
#' @param seTE The precalculated standard error for each study. Must be supplied as the name of the standard error
#' column in the dataset (in quotation marks; e.g. \code{seTE = "se"}).
#' @param data A \code{data.frame} containing columns for the effect size, standard error and
#' meta-regression predictors of each study/effect.
#' @param predictors A concentenated array of characters specifying the predictors to be used
#' for multimodel inference. Names of the predictors must be identical to the names of the column
#' names of the \code{data.frame} supplied to \code{data}.
#' @param method Meta-analysis model to use for pooling effect sizes. Use \code{'FE'} for the
#' fixed-effect model. Different random-effect models are available: \code{'DL', 'HE', 'SJ', 'ML', 'REML', 'EB', 'HS', 'GENQ'}.
#' If \code{'FE'} is used, the \code{test} argument is automatically set to \code{'z'}, as the Knapp-Hartung
#' method is not meant to be used with fixed-effect models. Default is \code{'REML'}, and it is strongly advised to remain with
#' this option to use a standard (mixed-effects) meta-regression model.
#' @param test Method to use to compute test statistics and confidence intervals. Default is \code{'knha'}
#' which uses the Knapp-Hartung (Knapp & Hartung, 2003) adjustment method. "Conventional" Wald-type tests and
#' CIs are calculated by setting this argument to \code{'z'}. When \code{method='FE'}, this argument is
#' set to \code{'z'} automatically as the Knapp-Hartung method was not meant to be used with fixed-effect models.
#' @param eval.criterion Evaluation criterion to sort the multiple models by. Can be either \code{'AICc'}
#' (default; corrected Akaike's Information Criterion), \code{'AIC'} (Akaike's Information Criterion) or
#' \code{'BIC'} (Bayesian Information Criterion).
#' @param interaction If set to \code{FALSE} (default), no interactions between predictors are considered. Setting this parameter to
#' \code{TRUE} means that all interactions are modeled.
#' @param seed Optional. Set a seed for the function.
#'
#' @details Multi-model methods differ from stepwise methods as they do not try to successively build
#' the “best” one (meta-regression) model explaining most of the variance. Instead, in this procedure,
#' all possible combinations of a predifined selection of predictors are modeled, and evaluated using
#' a criterion such as Akaike’s Information Criterion, which rewards simpler models.
#' This enables a full eximination of all possible models, and how they perform.
#' A common finding using this procedure is that there are many different kinds of predictor
#' combinations within a model which lead to a good fit. In multimodel inference, the estimated
#' coefficients of predictors can then be synthesized across all possible models to infer how
#' important certain predictors are overall.
#'
#' Multimodel Inference can be a useful way to obtain a comprehensive look on which predictors are
#' more or less important for predicting differences in effect sizes. Despite avoiding some of the
#' problems of stepwise regression methods, it should be noted that this method should still be rather
#' seen as exploratory, and may be used when there are no prior knowledge on how our predictors are
#' related to effect sizes in the research field under study.
#'
#' The \code{multimodel.inference} function calls the \code{\link[metafor]{rma.uni}} function internally,
#' which is then fed forward to the \code{\link[MuMIn]{dredge}} function for multimodel inference through
#' two utility functions returned by the \code{multimodel.inference} function.
#'
#' Parts of the computations in this function are based on a vignette by Wolfgang Viechtbauer, which can be found
#' \href{http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin}{here}.
#'
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/smallstudyeffects.html}{Chapter 9.1}
#'
#' Knapp, G., & Hartung, J. (2003). Improved tests for a random effects meta-regression with a single covariate.
#' \emph{Statistics in Medicine, 22}, 2693–2710.
#'
#' Viechtbauer, W. (2019). \emph{Model Selection using the glmulti and MuMIn Packages}. \href{http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin}{Link}.
#' Last accessed 01-Aug-2019.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @import MuMIn ggplot2
#' @importFrom metafor rma rma.uni
#'
#' @return Returns four tables and a plot:
#' \itemize{
#' \item \strong{Final Results (Summary Table)}: Displays the number of fitted models, model formula,
#' method to calculate test statistics and confidence intervals, interactions, and evaluation criterion used.
#' \item \strong{Best 5 Models}: Displays the top five models in terms of the evaluation criterion used.
#' Predictors are displayed as columns of the table, and models as rows. A number (weight) or \code{+}
#' sign (for categorical predictors) indicates that a predictor/interaction term was used for the
#' model, while empty cells indicate that the predictor was omitted in this model. Other metrics such as the
#' \code{weight}, evaluation metric \code{delta} compared to the best model, logLikelihood and degrees of freedom
#' are also diplayed
#' \item \strong{Multimodel Inference Coefficients}: Displays the coefficients and statistical significance
#' of each regression term in the model.
#' \item \strong{Predictor Importance}: Displays the importance value for each model term. The table is sorted from
#' highest to lowest. A common rule of thumb is to consider a predictor as important when its importance value is above 0.8.
#' \item \strong{Predictor Importance Plot}: A bar plot for the predictor importance data along with a reference line for the
#' 0.8 value often used as a crude threshold to characterize a predictor as important.
#' }
#'
#' @export multimodel.inference
#'
#' @seealso \code{\link[MuMIn]{dredge}}
#'
#' @examples
#' \dontrun{
#' # Example 1: Perform multimodel inference with default settings
#' data('MVRegressionData')
#' library(metafor)
#' mmi = multimodel.inference(TE = 'yi', seTE = 'sei', data = MVRegressionData,
#'                            predictors = c('pubyear', 'quality',
#'                                           'reputation', 'continent'))
#'
#' # Example 2: Model Interaction terms, set method to 'DL',
#' # change evaluation criterion to bic
#' multimodel.inference(TE = 'yi', seTE = 'sei', data = MVRegressionData,
#'                      predictors = c('pubyear', 'quality',
#'                                     'reputation', 'continent'),
#'                      method='DL', eval.criterion = 'BIC', interaction = TRUE)
#'
#' # Example 3: Use only categorical predictors
#' data('ThirdWave')
#' multimodel.inference(TE = 'TE', seTE = 'seTE', data = ThirdWave,
#'                      predictors = colnames(ThirdWave)[4:7], interaction = FALSE)}

multimodel.inference = function(TE, seTE, data, predictors, method = "REML", test = "knha", eval.criterion = "AICc",
    interaction = FALSE, seed = 123) {

    # Define MuMIn link
    MetaforMuMIn = function() {

        coefTable.rma = function(model, ...) {

            makeCoefTable = function (x, se, df = NA_real_, coefNames = names(x))
            {
              # Unexported function from the MuMIn package, see MuMIn:::.makeCoefTable
              if (n <- length(x)) {
                xdefined <- !is.na(x)
                ndef <- sum(xdefined)
                if (ndef < n) {
                  if (length(se) == ndef) {
                    y <- rep(NA_real_, n)
                    y[xdefined] <- se
                    se <- y
                  }
                  if (length(df) == ndef) {
                    y <- rep(NA_real_, n)
                    y[xdefined] <- df
                    df <- y
                  }
                }
              }
              if (n && n != length(se))
                stop("length(x) is not equal to length(se)")
              ret <- matrix(NA_real_, ncol = 3L, nrow = length(x), dimnames = list(coefNames,
                                                                                   c("Estimate", "Std. Error", "df")))
              if (n)
                ret[, ] <- cbind(x, se, rep(if (is.null(df)) NA_real_ else df,
                                            length.out = n), deparse.level = 0L)
              class(ret) <- c("coefTable", "matrix")
              ret
            }

            makeCoefTable(model$b, model$se, coefNames = rownames(model$b))
        }

        expr.split = function (x, split = ":", paren.open = c("(", "[", "{"), paren.close = c(")",
                                                                                              "]", "}"), quotes = c("\"", "'", "`"), esc = "\\", prepare = NULL)
        {
          x0 <- x
          if (is.function(prepare))
            x <- prepare(x)
          m <- length(x)
          n <- nchar(x)
          res <- vector("list", m)
          for (k in 1L:m) {
            pos <- integer(0L)
            inquote <- ch <- ""
            inparen <- integer(3L)
            for (i in seq.int(n[k])) {
              chprv <- ch
              ch <- substr(x[k], i, i)
              if (inquote != "") {
                if (chprv == esc && ch == esc)
                  ch <- " "
                else if (chprv != esc && ch == inquote)
                  inquote <- ""
              }
              else {
                inparen[j] <- inparen[j <- (inparen != 0L) &
                                        (ch == paren.close)] - 1L
                if (ch %in% quotes)
                  inquote <- ch
                else if (any(j <- (ch == paren.open)))
                  inparen[j] <- inparen[j] + 1L
                else if (all(inparen == 0L) && ch == split)
                  pos <- c(pos, i)
              }
            }
            res[[k]] <- substring(x0[k], c(1L, pos + 1L), c(pos -
                                                              1L, n[k]))
          }
          res
        }


        fixCoefNames = function (x, peel = TRUE)
        {
          if (!length(x))
            return(x)
          ox <- x
          ia <- grep(":", x, fixed = TRUE)
          if (!length(ia))
            return(structure(x, order = rep.int(1L, length(x))))
          x <- ret <- x[ia]
          if (peel) {
            if (all(substr(x, 1L, pos <- regexpr("_", x, fixed = TRUE)) %in%
                    c("count_", "zero_"))) {
              ret <- substr(ret, pos + 1L, 256L)
              k <- TRUE
              suffix <- ""
            }
            else {
              k <- grepl("^\\w+\\(.+\\)$", x, perl = TRUE)
              fname <- substring(x[k], 1L, attr(regexpr("^\\w+(?=\\()",
                                                        x[k], perl = TRUE), "match.length"))
              k[k] <- !vapply(fname, exists, FALSE, mode = "function",
                              envir = .GlobalEnv)
              if (any(k)) {
                pos <- vapply(x[k], function(z) {
                  parens <- lapply(lapply(c("(", ")"), function(s) gregexpr(s,
                                                                            z, fixed = TRUE)[[1L]]), function(y) y[y >
                                                                                                                     0L])
                  parseq <- unlist(parens, use.names = FALSE)
                  p <- cumsum(rep(c(1L, -1L), sapply(parens,
                                                     length))[order(parseq)])
                  if (any(p[-length(p)] == 0L))
                    -1L
                  else parseq[1L]
                }, 1L, USE.NAMES = FALSE)
                k[k] <- pos != -1L
                pos <- pos[pos != -1]
                if (any(k))
                  ret[k] <- substring(x[k], pos + 1L, nchar(x[k]) -
                                        1L)
              }
              suffix <- ")"
            }
          }
          else k <- FALSE
          spl <- expr.split(ret, ":", prepare = function(x) gsub("((?<=:):|:(?=:))",
                                                                 "_", x, perl = TRUE))
          ret <- vapply(lapply(spl, base::sort), paste0, "", collapse = ":")
          if (peel && any(k))
            ret[k] <- paste0(substring(x[k], 1L, pos), ret[k], suffix)
          ox[ia] <- ret
          ord <- rep.int(1, length(ox))
          ord[ia] <- sapply(spl, length)
          structure(ox, order = ord)
        }

        .getCoefNames = function (formula, data, contrasts, envir = parent.frame())
        {
          colnames(eval(call("model.matrix.default", object = formula,
                             data = data, contrasts.arg = contrasts), envir = envir))
        }

        makeArgs.default = function (obj, termNames, opt, ...)
        {
          #Unexported function from the MuMIn package, see MuMIn:::makeArgs.default
          reportProblems <- character(0L)
          termNames[termNames %in% opt$interceptLabel] <- "1"
          f <- reformulate(c(if (!opt$intercept) "0" else if (!length(termNames)) "1",
                             termNames), response = opt$response)
          environment(f) <- opt$gmFormulaEnv
          ret <- list(formula = f)
          if (!is.null(opt$gmCall$start)) {
            coefNames <- fixCoefNames(.getCoefNames(f, opt$gmDataHead,
                                                    opt$gmCall$contrasts, envir = opt$gmEnv))
            idx <- match(coefNames, opt$gmCoefNames)
            if (anyNA(idx))
              reportProblems <- append(reportProblems, "cannot subset 'start' argument. Coefficients in the model do not exist in 'global.model'")
            else ret$start <- substitute(start[idx], list(start = opt$gmCall$start,
                                                          idx = idx))
          }
          attr(ret, "problems") <- reportProblems
          ret
        }

        makeArgs.rma = function(obj, termNames, comb, opt, ...) {
            ret <- makeArgs.default(obj, termNames, comb, opt)
            names(ret)[1L] <- "mods"
            ret
        }

        assign("coefTable.rma", coefTable.rma, envir = .GlobalEnv)
        assign("makeArgs.rma", makeArgs.rma, envir = .GlobalEnv)
        return(invisible())

    }

    MetaforMuMIn()

    inner_mmi = function() {
        # Set supplied seed
        seed = seed
        set.seed(seed)

        # Check 'method'; if 'FE', switch test to 'z'.

        if (method %in% c("FE", "DL", "HE", "SJ", "ML", "REML", "EB", "HS", "GENQ")) {
            if (method == "FE" & test != "z") {
                test = "z"
                cat("Knapp-Hartung adjustments are only meant to be used for random-effects models. \n Parameter 'test' has therefore been changed to 'z'. \n")
            } else {

            }
        } else {
            stop("'method' must be either 'FE', 'DL', 'HE', 'SJ', 'ML', 'REML', 'EB', 'HS', or 'GENQ'.")
        }


        # Change supplied df to conform to glmulti
        if (TE %in% colnames(data)) {

        } else {
            stop("Column '", TE, "' not found in dataset.")
        }

        if (seTE %in% colnames(data)) {

        } else {
            stop("Column '", seTE, "' not found in dataset.")
        }

        for (i in 1:length(predictors)) {
            if (predictors[i] %in% colnames(data)) {

            } else {
                stop("Predictor '", predictors[i], "' not found in dataset.")
            }
        }

        if (eval.criterion[1] %in% c("AICc", "BIC", "AIC")) {

        } else {
            stop("'eval.criterion' must be either 'AICc' (default), 'AIC' or 'BIC'.")
        }


        TE = data[TE]
        seTE = data[seTE]
        preds = data[predictors]
        glm.data = data.frame(TE = TE, seTE = seTE)
        colnames(glm.data) = c("TE", "seTE")
        glm.data = cbind(glm.data, preds)

        # Build the formula
        interaction = interaction
        if (interaction == FALSE) {
            predictor.string = paste(predictors, collapse = "+")
        } else {
            predictor.string = paste(predictors, collapse = "*")
        }
        form = as.formula(paste("~", predictor.string, collapse = ""))


        # Build rma model
        full = suppressMessages(suppressWarnings(metafor::rma(yi = TE, sei = seTE, mods = form, data = glm.data, method = method,
            test = test)))


        # Multimodel Inference
        if (eval.criterion == "AICc") {
            res = suppressMessages(suppressWarnings(dredge(full, trace = 2, rank = "AICc")))
        }

        if (eval.criterion == "AIC") {
            res = suppressMessages(suppressWarnings(MuMIn::dredge(full, trace = 2, rank = "AIC")))
        }

        if (eval.criterion == "BIC") {
            res = suppressMessages(suppressWarnings(MuMIn::dredge(full, trace = 2, rank = "BIC")))
        }

        # Save results for all models: all.models, top5.models
        all.models = res
        top5.models = res[1:5, ]

        # Create Multimodel Inference Coeffient Table and save: multimodel.coef
        multimodel.coef = summary(MuMIn::model.avg(res, revised.var = TRUE))
        multimodel.coef = multimodel.coef$coefmat.full

        # Create importance table and save: predictor.importance
        predictor.importance = data.frame(model = names(importance(res)), importance = as.numeric(importance(res)))

        # Print out results
        cat("\n", "Multimodel Inference: Final Results", "--------------------------", sep = "\n")
        cat("\n", "- Number of fitted models:", nrow(all.models))
        cat("\n", "- Full formula:", as.character(form))
        cat("\n", "- Coefficient significance test:", test)
        if (interaction == TRUE) {
            cat("\n", "- Interactions modeled: yes")
        } else {
            cat("\n", "- Interactions modeled: no")
        }
        cat("\n", "- Evaluation criterion:", eval.criterion, "\n")
        cat("\n", "Best 5 Models", "--------------------------", "\n", sep = "\n")
        print(top5.models)
        cat("\n", "Multimodel Inference Coefficients", "--------------------------", "\n", sep = "\n")
        print(multimodel.coef)
        cat("\n", "Predictor Importance", "--------------------------", "\n", sep = "\n")
        print(predictor.importance)

        # Print graph
        ggpredictor = ggplot(predictor.importance, aes(x = reorder(model, importance), y = importance)) +
            geom_bar(stat = "identity") + coord_flip() + geom_hline(yintercept = 0.8, color = "blue") + theme_minimal() +
            theme(axis.title.y = element_blank()) + ylab("Predictor Importance")
        suppressWarnings(suppressMessages(plot(ggpredictor)))


        # Return results
        invisible(list(all.models = all.models, top5.models = top5.models, multimodel.coef = multimodel.coef,
            predictor.importance = predictor.importance, predictor.importance.plot = suppressWarnings(suppressMessages(ggpredictor)),
            formula = form, fitted.models = nrow(all.models), eval.criterion = eval.criterion))

    }

    invisible(inner_mmi())
}



