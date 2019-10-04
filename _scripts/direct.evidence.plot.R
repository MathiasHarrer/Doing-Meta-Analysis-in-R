## Copy and paste the code underneath in its enterity into your console
## Then hit 'Enter ⏎'

direct.evidence.plot = function(x){

  # Packages
  library(netmeta)
  library(ggplot2)
  library(reshape2)

  # Get Measures
  measures = netmeasures(x)$proportion
  indirect = 1- measures
  measures = data.frame(comparison = names(measures), direct = measures, indirect = indirect)
  rownames(measures) = c()
  measures$direct = round(measures$direct, 4)
  measures$indirect = round(measures$indirect, 4)
  measures.reshape = melt(measures, id.vars = "comparison", measure.vars = c("direct", "indirect"))
  names = measures.reshape[measures.reshape$variable=="direct",]$comparison
  direct = measures.reshape[measures.reshape$variable=="direct",]$value
  names = names[order(match(names, direct))]

  # Plot

  gg = ggplot(measures.reshape,
              aes(x=reorder(comparison, value),
                  fill=factor(variable, levels=c("indirect","direct")),
                  y=value)) +
       geom_bar(stat="identity", position="fill") +
       coord_flip() +
       theme_minimal() +
       scale_x_discrete(limits=names)+
       ggtitle("Direct evidence proportion \n for each network estimate")+
       scale_y_continuous(labels = scales::percent) +
       ylab("Percentage")+
       xlab("Network Estimate")+
       guides(fill = guide_legend(title="Evidence"))+
       scale_fill_manual(values=c('lightblue', 'orange'))

  return(list(plot = gg, data = measures))

}
