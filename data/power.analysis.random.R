power.analysis.random<-function(d,k,n1,n2,p,heterogeneity){
  n1<-n1
  n2<-n2
  d<-d
  k<-k
  p<-p
  heterogeneity<-heterogeneity

  if(heterogeneity=="low"){
    v.d<-((n1+n2)/(n1*n2))+((d*d)/(2*(n1+n2)))
    v.m<-v.d/k
    v.m<-1.33*v.m
    lambda<-(d/sqrt(v.m))
    plevel<-1-(p/2)
    zval<-qnorm(p=plevel, 0,1)
    power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
    return(power)
  }

  if(heterogeneity=="moderate"){

    v.d<-((n1+n2)/(n1*n2))+((d*d)/(2*(n1+n2)))
    v.m<-v.d/k
    v.m<-1.67*v.m
    lambda<-(d/sqrt(v.m))
    plevel<-1-(p/2)
    zval<-qnorm(p=plevel, 0,1)
    power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
    return(power)
  }

  if(heterogeneity=="high"){

    v.d<-((n1+n2)/(n1*n2))+((d*d)/(2*(n1+n2)))
    v.m<-v.d/k
    v.m<-2*v.m
    lambda<-(d/sqrt(v.m))
    plevel<-1-(p/2)
    zval<-qnorm(p=plevel, 0,1)
    power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
    return(power)
  }

}
