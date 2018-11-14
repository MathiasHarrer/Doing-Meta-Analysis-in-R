## This is the function code for the subgroup.analysis.mixed.effects function
## Copy and paste the code underneath in its enterity into your console
## Then hit 'Enter ⏎'

subgroup.analysis.mixed.effects<-function(data,sg.var,n.sg,subgroup1,subgroup2,subgroup3,subgroup4,subgroup5,subgroup6){

  n.sg<-n.sg

  if(n.sg==2){

    data<-data
    sg.var<-sg.var
    subgroup1<-subgroup1
    subgroup2<-subgroup2

    sg1<-update.meta(data,
                     subset = sg.var==paste(subgroup1))

    sg2<-update.meta(data,
                     subset = sg.var==paste(subgroup2))

    estimate<-c(sg1$TE.random,
                sg2$TE.random)

    stderror<-c(sg1$seTE.random,
                sg2$seTE.random)

    meta<-c(paste(subgroup1),
            paste(subgroup2))

    data.comp<-data.frame(estimate,stderror,meta)
    print(metagen(TE=estimate,
                  seTE = stderror,
                  data=data.comp,
                  comb.fixed = TRUE,
                  comb.random = FALSE,
                  byvar = meta))
  }

  if(n.sg==3){

    data<-data
    sg.var<-sg.var
    subgroup1<-subgroup1
    subgroup2<-subgroup2
    subgroup3<-subgroup3

    sg1<-update.meta(data,
                     subset = sg.var==paste(subgroup1))

    sg2<-update.meta(data,
                     subset = sg.var==paste(subgroup2))

    sg3<-update.meta(data,
                     subset = sg.var==paste(subgroup3))

    estimate<-c(sg1$TE.random,
                sg2$TE.random,
                sg3$TE.random)

    stderror<-c(sg1$seTE.random,
                sg2$seTE.random,
                sg3$seTE.random)

    meta<-c(paste(subgroup1),
            paste(subgroup2),
            paste(subgroup3))

    data.comp<-data.frame(estimate,stderror,meta)
    print(metagen(TE=estimate,
                  seTE = stderror,
                  data=data.comp,
                  comb.fixed = TRUE,
                  comb.random = FALSE,
                  byvar = meta))
  }

  if(n.sg==4){

    data<-data
    sg.var<-sg.var
    subgroup1<-subgroup1
    subgroup2<-subgroup2
    subgroup3<-subgroup3
    subgroup4<-subgroup4

    sg1<-update.meta(data,
                     subset = sg.var==paste(subgroup1))

    sg2<-update.meta(data,
                     subset = sg.var==paste(subgroup2))

    sg3<-update.meta(data,
                     subset = sg.var==paste(subgroup3))

    sg4<-update.meta(data,
                     subset = sg.var==paste(subgroup4))

    estimate<-c(sg1$TE.random,
                sg2$TE.random,
                sg3$TE.random,
                sg4$TE.random)

    stderror<-c(sg1$seTE.random,
                sg2$seTE.random,
                sg3$seTE.random,
                sg4$seTE.random)

    meta<-c(paste(subgroup1),
            paste(subgroup2),
            paste(subgroup3),
            paste(subgroup4))

    data.comp<-data.frame(estimate,stderror,meta)
    print(metagen(TE=estimate,
                  seTE = stderror,
                  data=data.comp,
                  comb.fixed = TRUE,
                  comb.random = FALSE,
                  byvar = meta))
  }

  if(n.sg==5){

    data<-data
    sg.var<-sg.var
    subgroup1<-subgroup1
    subgroup2<-subgroup2
    subgroup3<-subgroup3
    subgroup4<-subgroup4
    subgroup5<-subgroup5

    sg1<-update.meta(data,
                     subset = sg.var==paste(subgroup1))

    sg2<-update.meta(data,
                     subset = sg.var==paste(subgroup2))

    sg3<-update.meta(data,
                     subset = sg.var==paste(subgroup3))

    sg4<-update.meta(data,
                     subset = sg.var==paste(subgroup4))

    sg5<-update.meta(data,
                     subset = sg.var==paste(subgroup5))

    estimate<-c(sg1$TE.random,
                sg2$TE.random,
                sg3$TE.random,
                sg4$TE.random,
                sg5$TE.random)

    stderror<-c(sg1$seTE.random,
                sg2$seTE.random,
                sg3$seTE.random,
                sg4$seTE.random,
                sg5$seTE.random)

    meta<-c(paste(subgroup1),
            paste(subgroup2),
            paste(subgroup3),
            paste(subgroup4),
            paste(subgroup5))

    data.comp<-data.frame(estimate,stderror,meta)
    print(metagen(TE=estimate,
                  seTE = stderror,
                  data=data.comp,
                  comb.fixed = TRUE,
                  comb.random = FALSE,
                  byvar = meta))
  }

  if(n.sg==6){

    data<-data
    sg.var<-sg.var
    subgroup1<-subgroup1
    subgroup2<-subgroup2
    subgroup3<-subgroup3
    subgroup4<-subgroup4
    subgroup5<-subgroup5
    subgroup6<-subgroup6

    sg1<-update.meta(data,
                     subset = sg.var==paste(subgroup1))

    sg2<-update.meta(data,
                     subset = sg.var==paste(subgroup2))

    sg3<-update.meta(data,
                     subset = sg.var==paste(subgroup3))

    sg4<-update.meta(data,
                     subset = sg.var==paste(subgroup4))

    sg5<-update.meta(data,
                     subset = sg.var==paste(subgroup5))

    sg6<-update.meta(data,
                     subset = sg.var==paste(subgroup6))

    estimate<-c(sg1$TE.random,
                sg2$TE.random,
                sg3$TE.random,
                sg4$TE.random,
                sg5$TE.random,
                sg6$TE.random)

    stderror<-c(sg1$seTE.random,
                sg2$seTE.random,
                sg3$seTE.random,
                sg4$seTE.random,
                sg5$seTE.random,
                sg6$seTE.random)

    meta<-c(paste(subgroup1),
            paste(subgroup2),
            paste(subgroup3),
            paste(subgroup4),
            paste(subgroup5),
            paste(subgroup6))

    data.comp<-data.frame(estimate,stderror,meta)
    print(metagen(TE=estimate,
                  seTE = stderror,
                  data=data.comp,
                  comb.fixed = TRUE,
                  comb.random = FALSE,
                  byvar = meta))
  }

}
