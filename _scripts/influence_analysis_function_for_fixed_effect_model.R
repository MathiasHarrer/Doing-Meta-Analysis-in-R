### Influence Analysis function for fixed-effect-model meta-analyses

influence.analysis.fixed<-function(data){

  data<-data
  TE<-data$TE
  seTE<-data$seTE

    res <- rma(yi=TE, sei=seTE, measure="ZCOR",
               data=influence.data,
               method = "FE")
    res
    inf <- influence(res)
    influence.data<-metainf(data)
    influence.data$I2<-format(round(influence.data$I2,2),nsmall=2)
    plot(inf)
    baujat(data)
    forest(influence.data,
           sortvar=I2,
           rightcols = c("TE","ci","I2"),
           smlab = "Sorted by I-squared")
    forest(influence.data,
           sortvar=TE,
           rightcols = c("TE","ci","I2"),
           smlab = "Sorted by Effect size")
}

