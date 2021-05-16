library(ggplot2)

power.analysis.bw = function (d, OR, k, n1, n2, p = 0.05, heterogeneity = "fixed")
{
  odds = FALSE
  if (missing(OR) & missing(d)) {
    stop("Either 'd' or 'OR' must be provided.")
  }
  if (!(heterogeneity %in% c("fixed", "low", "moderate", "high"))) {
    stop("'heterogeneity' must be either 'fixed', 'low', 'moderate', 'high'.")
  }
  if (missing(d)) {
    odds = TRUE
    d = log(OR) * (sqrt(3)/pi)
    token1 = "log"
  }
  else {
    token1 = "no.log"
  }
  es = d
  if (heterogeneity == "fixed") {
    het.factor = 1
    v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
    v.m = v.d/k
    lambda = (d/sqrt(v.m))
    plevel = 1 - (p/2)
    zval = qnorm(p = plevel, 0, 1)
    power = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))
    token2 = "fixed"
  }
  if (heterogeneity == "low") {
    het.factor = 1.33
    v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
    v.m = v.d/k
    v.m = 1.33 * v.m
    lambda = (d/sqrt(v.m))
    plevel = 1 - (p/2)
    zval = qnorm(p = plevel, 0, 1)
    power = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))
    token2 = "low"
  }
  if (heterogeneity == "moderate") {
    het.factor = 1.67
    v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
    v.m = v.d/k
    v.m = 1.67 * v.m
    lambda = (d/sqrt(v.m))
    plevel = 1 - (p/2)
    zval = qnorm(p = plevel, 0, 1)
    power = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))
    token2 = "moderate"
  }
  if (heterogeneity == "high") {
    het.factor = 2
    v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
    v.m = v.d/k
    v.m = 2 * v.m
    lambda = (d/sqrt(v.m))
    plevel = 1 - (p/2)
    zval = qnorm(p = plevel, 0, 1)
    power = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))
    token2 = "high"
  }
  dvec = (1:1000)/1000
  if (d > 1) {
    dvec = (1:(d * 1000))/1000
  }
  powvect = vector()
  for (i in 1:length(dvec)) {
    d = dvec[i]
    v.d = ((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2)))
    v.m = v.d/k
    v.m = het.factor * v.m
    lambda = (d/sqrt(v.m))
    plevel = 1 - (p/2)
    zval = qnorm(p = plevel, 0, 1)
    powvect[i] = 1 - (pnorm(zval - lambda)) + (pnorm(-zval -
                                                       lambda))
  }
  if (odds == FALSE) {
    plotdat = as.data.frame(cbind(dvec, powvect))
    plot = ggplot(data = plotdat, aes(x = dvec, y = powvect)) +
      geom_line(color = "gray70", size = 2) + geom_point(aes(x = es,
                                                           y = power), color = "gray30", size = 5) + theme_minimal() +
      geom_hline(yintercept = 0.8, color = "black", linetype = "dashed") +
      ylab("Power") + xlab("Effect size (SMD)")
  }
  else {
    dvecs = exp(dvec * (pi/sqrt(3)))
    dvec.inv = exp(-dvec * (pi/sqrt(3)))
    dvec = as.vector(rbind(dvec.inv, dvecs))
    powvect = as.vector(rbind(powvect, powvect))
    plotdat = as.data.frame(cbind(dvec, powvect))
    plot = ggplot(data = plotdat, aes(x = dvec, y = powvect)) +
      geom_line(color = "gray70", size = 2) + geom_point(aes(x = exp(es *
                                                                     (pi/sqrt(3))), y = power), color = "gray30", size = 5) +
      theme_minimal() + geom_hline(yintercept = 0.8, color = "black",
                                   linetype = "dashed") + ylab("Power") + xlab("Effect size (OR)") +
      scale_x_log10()
  }
  return.list = list(Plot = plot, Power = power)
  class(return.list) = c("power.analysis", token1, token2)
  invisible(return.list)
  return.list
}
