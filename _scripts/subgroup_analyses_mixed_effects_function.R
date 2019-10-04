## This is the function code for the subgroup.analysis.mixed.effects function
## Copy and paste the code underneath in its enterity into your console
## Then hit 'Enter ⏎'

subgroup.analysis.mixed.effects<-function(m, subgroups, exclude = "none", hakn=FALSE){

  library(meta)
  library(metafor)

  # Define variables
  m = m
  subgroups = subgroups
  value.hakn = hakn
  exclude = exclude

  # Levels of subgroup
  subgroups = as.factor(subgroups)
  k=as.vector(summary(subgroups))
  levels = levels(subgroups)
  k.level.df = data.frame("level"=levels, "k"=k)

  # Out Loop for wrong input
  if (length(subgroups)!=length(m$studlab)){
    stop("Subgroup variable does not contain the same number of cases as the 'meta' object. You need to define a variable which provides a subgroup value for each effect size included in your 'meta' results object.")
  }

  # get "Exclude" Subgroup level names
  if (exclude[1]!="none"){
    levels = levels[!levels %in% exclude]
    k = k.level.df[(k.level.df$level %in% levels),]$k
  }

  # Create Loop for subgroups
  list = list()
  for (x in levels){
    list[[x]] = which(subgroups %in% c(paste(x)))
  }

  # Loop over list to generate subgroup results
  sg.results = list()
  for (x in 1:length(list)){
    sg.results[[x]] = update.meta(m, subset = list[[x]])
  }

  # Loop over sg.results to get effect size estimates
  ES = vector()
  SE = vector()
  Qsg = vector()
  I2sg = vector()
  I2sg.lower = vector()
  I2sg.upper = vector()
  for (x in 1:length(sg.results)){
    ES[x] = sg.results[[x]]$TE.random
    SE[x] = sg.results[[x]]$seTE.random
    Qsg[x] = sg.results[[x]]$Q
    I2sg[x] = sg.results[[x]]$I2
    I2sg.lower[x] = sg.results[[x]]$lower.I2
    I2sg.upper[x] = sg.results[[x]]$upper.I2
  }

  me.data = data.frame("Subgroup"=levels, "TE"=ES, "seTE"=SE)

  # Fixed Meta-Analysis betweens subgroups
  meta = metagen(TE,
                 seTE,
                 data=me.data,
                 comb.fixed = TRUE,
                 comb.random = FALSE,
                 byvar = Subgroup,
                 hakn = value.hakn)

  # Create full output dataset

  subgroup.results = data.frame("Subgroup"=me.data$Subgroup,
                                "k"=k,
                                "TE"=me.data$TE,
                                "seTE"=me.data$seTE,
                                "LLCI"=round(meta$lower,3),
                                "ULCI"=round(meta$upper,3),
                                "p"=meta$pval,
                                "Q"=Qsg,
                                "I2"=round(I2sg,2),
                                "I2.lower"=round(I2sg.lower,2),
                                "I2.upper"=round(I2sg.upper,2))

  mixedeffects.results = data.frame("Q"=meta$Q, "df"=meta$df.Q, "p"=meta$pval.Q, row.names = "Between groups")

  res = list("within.subgroup.results"=subgroup.results, "subgroup.analysis.results"=mixedeffects.results)

  cat("Subgroup Results:","--------------", sep="\n")
  print(subgroup.results)
  cat("","Test for subgroup differences (mixed/fixed-effects (plural) model):","--------------", sep="\n")
  print(mixedeffects.results)
  cat("", sep="\n")
  cat("- Total number of studies included in subgroup analysis: ", sum(k))
  cat("", sep="\n")
  cat("- Tau estimator used for within-group pooling: ", m$method.tau)

  invisible(res)
}

