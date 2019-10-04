mreg.multimodel.inference = function(TE,
                                     seTE,
                                     data,
                                     predictors,
                                     method="REML",
                                     test="knha",
                                     eval.criterion="aicc",
                                     level=1){

  # Parts of the computatations for this function are based on:
  # http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti

  library(metafor)
  library(glmulti)

  # Change supplied df to conform to glmulti
  TE = data[TE]
  seTE = data[seTE]
  preds = data[predictors]
  glm.data = data.frame("TE"=TE, "seTE"=seTE)
  colnames(glm.data) = c("TE", "seTE")
  glm.data = cbind(glm.data, preds)

  # Build the formula
  predictor.string = paste(predictors, collapse="+")
  form = paste("TE ~", predictor.string, collapse = "")

  # Set up function
  rma.glmulti = function(formula, data, ...)
    rma(formula, seTE, data=data, method=method, test=test)

  # Loop over all possible models
  result = glmulti(y="TE", xr=predictors, data=glm.data,
                         level=level,
                         fitfunction=rma.glmulti,
                         crit=eval.criterion,
                         confsetsize=1000)

  # Save results for all models: all.models, top5.models
  all.models = weightable(result)
  top5.models = weightable(result)[1:5,]

  # Create Multimodel Inference Coeffient Table and save: multimodel.coef
  setOldClass("rma.uni")
  setMethod('getfit', 'rma.uni', function(object, ...) {
    if (object$test==test) {
      cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=Inf)
    } else {
      cbind(estimate=coef(object), se=sqrt(diag(vcov(object))), df=object$k-object$p)
    }
  })
  multimodel.coef = coef(result)

  # Create List with model results for all models: model.details
  model.details = list()
  for (x in 1:length(result@objects)){
    model.details[[x]] = result@objects[x]
  }


  # Print out results
  cat("\n", "Multimodel Inference: Final Results", "--------------------------", sep="\n")
  cat("\n", "- Number of fitted models:", nrow(all.models))
  cat("\n", "- Full formula:", form)
  cat("\n", "- Coefficient significance test:", test)
  cat("\n", "- Modeled interaction level:", level)
  cat("\n", "- Evaluation criterion:", eval.criterion, "\n")
  cat("\n", "Best 5 Models", "--------------------------", "\n", sep="\n")
  print(top5.models)
  cat("\n", "Multimodel Inference Coefficients", "--------------------------", "\n", sep="\n")
  print(multimodel.coef)

  # Print graph
  plot(result, type="s")

  # Return results
  invisible(list("all.models"=all.models,
                 "top5.models"=top5.models,
                 "multimodel.coef"=multimodel.coef,
                 "model.details"=model.details,
                 "formula"=form,
                 "fitted.models"=nrow(all.models),
                 "eval.criterion"=eval.criterion))

}



