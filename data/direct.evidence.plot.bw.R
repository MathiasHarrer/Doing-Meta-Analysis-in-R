library(ggplot2)
library(gridExtra)

direct.evidence.plot.bw <- function (x, random = FALSE, comparison.label.size = 2, numeric.label.size = 3,
                                     subplot.ratio = c(5, 1.3, 1.3))
{
  x = x
  random = random
  cts = comparison.label.size
  nts = numeric.label.size
  spr = subplot.ratio
  if (class(x) != "netmeta") {
    stop("Input to this function has to be an object of class 'netmeta' created by the 'netmeta::netmeta' function.")
  }
  measures = netmeasures(x, random = random)$proportion
  indirect = 1 - measures
  measures = data.frame(comparison = names(measures), direct = measures,
                        indirect = indirect)
  rownames(measures) = c()
  measures$direct = round(measures$direct, 4)
  measures$indirect = round(measures$indirect, 4)
  measures.reshape = with(measures, {
    data.frame(comparison = rep(comparison, 2), variable = rep(c("direct",
                                                                 "indirect"), each = nrow(measures)), value = c(direct,
                                                                                                                indirect))
  })
  names = measures.reshape[measures.reshape$variable == "direct",
  ]$comparison
  direct = measures.reshape[measures.reshape$variable == "direct",
  ]$value
  names = names[order(match(names, direct))]
  measures$comparison = factor(measures$comparison, levels = measures$comparison[rev(order(measures$direct))])
  levels = levels(measures$comparison)
  measures.reshape$comparison = factor(measures.reshape$comparison,
                                       levels = levels)
  PlotDirectEvidence = ggplot2::ggplot(measures.reshape, aes(x = factor(comparison,
                                                                        levels = rev(levels(comparison))), fill = factor(variable,
                                                                                                                         levels = c("indirect", "direct")), y = value)) + geom_bar(stat = "identity",
                                                                                                                                                                                   position = "fill") + coord_flip() + theme_minimal() +
    theme(legend.position = "left") + scale_y_continuous(labels = scales::percent) +
    ylab("Percentage") + xlab("Network Estimate") + guides(fill = guide_legend(title = "Evidence")) +
    scale_fill_manual(values = c("lightgray", "gray30")) +
    geom_hline(aes(yintercept = 0.25), color = "white") +
    geom_hline(aes(yintercept = 0.5), color = "white") +
    geom_hline(aes(yintercept = 0.75), color = "white")
  mpath = netmeasures(x, random = random)$meanpath
  path.df = data.frame(comparison = names(mpath), mpath = mpath)
  rownames(path.df) = c()
  path.df$comparison = factor(path.df$comparison, levels = levels)
  PlotMeanPathLength_s = ggplot2::ggplot(path.df, aes(x = factor(comparison,
                                                                 levels = rev(levels(comparison))), y = mpath)) + geom_bar(stat = "identity",
                                                                                                                           fill = "lightgray") + coord_flip() + geom_hline(aes(yintercept = 2),
                                                                                                                                                                           color = "gray30") + geom_text(aes(x = comparison, y = 0.4,
                                                                                                                                                                                                           label = comparison), color = "gray23", size = cts) +
    geom_text(aes(x = comparison, y = mpath + 0.1, label = round(mpath,
                                                                 1)), size = nts) + ylab("Mean Path Length") + theme(axis.title.y = element_blank(),
                                                                                                                     axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                                                                                                                     axis.ticks.x = element_blank(), panel.background = element_blank()) +
    scale_x_discrete(position = "top")
  mpar = netmeasures(x, random = random)$minpar
  mpar.df = data.frame(comparison = names(mpar), mpar = mpar)
  rownames(mpar.df) = c()
  mpar.df$comparison = factor(mpar.df$comparison, levels = levels)
  PlotMinimalParallelism_s = ggplot2::ggplot(mpar.df, aes(x = factor(comparison,
                                                                     levels = rev(levels(comparison))), y = mpar)) + geom_bar(stat = "identity",
                                                                                                                              fill = "lightgray") + coord_flip() + geom_text(aes(x = comparison,
                                                                                                                                                                                 y = mpar + 0.1, label = round(mpar, 1)), size = nts) +
    geom_text(aes(x = comparison, y = 0.4, label = comparison),
              color = "gray23", size = cts) + ylab("Minimal Parallelism") +
    theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(),
          panel.background = element_blank())
  data = data.frame(proportion.direct = measures$direct, proportion.indirect = measures$indirect,
                    meanpath = mpath, minpar = mpar)
  if (random == FALSE) {
    plot_title = "Direct evidence proportion for each network estimate (fixed-effect model)"
  }
  else {
    plot_title = "Direct evidence proportion for each network estimate (random-effects model)"
  }
  grid = gridExtra::arrangeGrob(PlotDirectEvidence, PlotMinimalParallelism_s,
                                PlotMeanPathLength_s, ncol = 3, widths = spr, heights = c(4),
                                top = plot_title)
  returnlist = list(data = data, plot = grid)
  class(returnlist) = "direct.evidence.plot"
  invisible(returnlist)
  returnlist
}
