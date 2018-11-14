

library(shiny)
library(scales)
library(ggplot2)
library(reshape)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Power Calculator for Meta-Analysis"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("d",
                     "Effect size (SMD):",
                     min = 0,
                     max = 2,
                     value = 0.3,
                     step = 0.01),

         numericInput("n1",
                      "Number of participants (Group 1):",
                      min=1,
                      value=25,
                      step=1),
         numericInput("n2",
                      "Number of participants (Group 2):",
                      min=1,
                      value=25,
                      step=1),
         numericInput("k",
                      "Number of studies (k):",
                      min=1,
                      value=10,
                      step=1),
         radioButtons("method",
                      "Model:",
                      choices=c("Fixed-Effect Model"="FEM",
                                "Random-Effects Model (low heterogeneity)"="REMl",
                                "Random-Effects Model (moderate heterogeneity)"="REMm",
                                "Random-Effects Model (high heterogeneity)"="REMh")),
         numericInput("alpha",
                      "alpha (p-level)",
                      min = 0.0001,
                      max = 0.9999,
                      value = 0.05,
                      step= 0.01)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         uiOutput("powerheader"),
         textOutput("power"),
         hr(),
         h4("Power Plot"),
         plotOutput("powerplot"),
         h5("Calculations are based on:"),
         HTML("<p><i>Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2011). Introduction to meta-analysis. John Wiley & Sons. (Chapter 29).</i></p>"),
         hr(),
         HTML("<p>This Calculator is part of the book <b>Doing Meta-Analysis in R</b> (Chapter 13). Read the full chapter online."),
         br(),
         a(href="https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/power-analysis.html","View the Chapter")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$power <- renderText({




     if(input$method=="FEM"){
       req(c(input$d,input$n1,input$n2,input$k,input$alpha,input$method))
       v.d<-((input$n1+input$n2)/(input$n1*input$n2))+((input$d*input$d)/(2*(input$n1+input$n2)))
       v.m<-v.d/input$k
       lambda<-(input$d/sqrt(v.m))
       plevel<-1-(input$alpha/2)
       zval<-qnorm(p=plevel, 0,1)
       power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
       power.percent<-percent(power,digits=3)
       print(power.percent)
     }

     if(input$method=="REMl"){
       req(c(input$d,input$n1,input$n2,input$k,input$alpha,input$method))
       v.d<-((input$n1+input$n2)/(input$n1*input$n2))+((input$d*input$d)/(2*(input$n1+input$n2)))
       v.m<-v.d/input$k
       v.m<-1.33*v.m
       lambda<-(input$d/sqrt(v.m))
       plevel<-1-(input$alpha/2)
       zval<-qnorm(p=plevel, 0,1)
       power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
       power.percent<-percent(power,digits=3)
       print(power.percent)
     }

     if(input$method=="REMm"){
       req(c(input$d,input$n1,input$n2,input$k,input$alpha,input$method))
       v.d<-((input$n1+input$n2)/(input$n1*input$n2))+((input$d*input$d)/(2*(input$n1+input$n2)))
       v.m<-v.d/input$k
       v.m<-1.67*v.m
       lambda<-(input$d/sqrt(v.m))
       plevel<-1-(input$alpha/2)
       zval<-qnorm(p=plevel, 0,1)
       power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
       power.percent<-percent(power,digits=3)
       print(power.percent)
     }

     if(input$method=="REMh"){
       req(c(input$d,input$n1,input$n2,input$k,input$alpha,input$method))
       v.d<-((input$n1+input$n2)/(input$n1*input$n2))+((input$d*input$d)/(2*(input$n1+input$n2)))
       v.m<-v.d/input$k
       v.m<-2*v.m
       lambda<-(input$d/sqrt(v.m))
       plevel<-1-(input$alpha/2)
       zval<-qnorm(p=plevel, 0,1)
       power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
       power.percent<-percent(power,digits=3)
       print(power.percent)
     }

     print(power.percent)

   })

   output$powerheader<-renderUI({h4("Your power estimate is:")})


   output$powerplot <- renderPlot({

     if(input$method=="FEM"){
       req(c(input$d,input$n1,input$n2,input$k,input$alpha,input$method))
       v.d<-((input$n1+input$n2)/(input$n1*input$n2))+((input$d*input$d)/(2*(input$n1+input$n2)))
       v.m<-v.d/input$k
       lambda<-(input$d/sqrt(v.m))
       plevel<-1-(input$alpha/2)
       zval<-qnorm(p=plevel, 0,1)
       power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
       power.percent<-percent(power,digits=3)

       ## Power Analysis function

       power.analysis.fixed<-function(d,k,n1,n2,p){

         n1<-n1
         n2<-n2
         d<-d
         k<-k
         p<-p
         title<-c("Power for a fixed-effect meta-analysis:")

         v.d<-((n1+n2)/(n1*n2))+((d*d)/(2*(n1+n2)))
         v.m<-v.d/k
         lambda<-(d/sqrt(v.m))
         plevel<-1-(p/2)
         zval<-qnorm(p=plevel, 0,1)
         power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
         return(power)
       }

       ## Graph data

       k <- seq(0, 50, length=1000)
       pow.vals01<-lapply(k,function(k) power.analysis.fixed(d=input$d,k=k,n1=input$n1,n2=input$n2,p=input$alpha))
       pow.vals01<-as.numeric(pow.vals01)
       data<-data.frame(k,pow.vals01)
       labelplot<-as.character(paste(c("Power:", power.percent), collapse = " "))
       powerplot<-ggplot()+
         geom_line(data = data, aes(x = k, y = pow.vals01), color = "blue",size=2) +
         xlab('Number of Studies') +
         ylab('Power')+
         scale_y_continuous(labels = scales::percent)+
         theme(
           axis.line= element_line(color = "black",size = 1,linetype = "solid"),
           legend.position = "bottom",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           legend.background = element_rect(linetype="solid",
                                            colour ="black"),
           legend.title = element_blank(),
           legend.key.size = unit(0.75,"cm"),
           legend.text=element_text(size=14))+
         annotate("text", x = 40, y = 0.1, label = labelplot,size=8,color="red")+
         geom_hline(yintercept=0.8,linetype="dashed")+
         geom_point(aes(x = input$k, y = power), color = "red",size=8)


     }

     if(input$method=="REMl"){
       req(c(input$d,input$n1,input$n2,input$k,input$alpha,input$method))
       v.d<-((input$n1+input$n2)/(input$n1*input$n2))+((input$d*input$d)/(2*(input$n1+input$n2)))
       v.m<-v.d/input$k
       v.m<-1.33*v.m
       lambda<-(input$d/sqrt(v.m))
       plevel<-1-(input$alpha/2)
       zval<-qnorm(p=plevel, 0,1)
       power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
       power.percent<-percent(power,digits=3)

       ## Power Analysis function

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

       ## Graph data

       k <- seq(0, 50, length=1000)
       pow.vals01<-lapply(k,function(k) power.analysis.random(d=input$d,k=k,n1=input$n1,n2=input$n2,p=input$alpha,heterogeneity="low"))
       pow.vals01<-as.numeric(pow.vals01)
       data<-data.frame(k,pow.vals01)
       labelplot<-as.character(paste(c("Power:", power.percent), collapse = " "))
       powerplot<-ggplot()+
         geom_line(data = data, aes(x = k, y = pow.vals01), color = "blue",size=2) +
         xlab('Number of Studies') +
         ylab('Power')+
         scale_y_continuous(labels = scales::percent)+
         theme(
           axis.line= element_line(color = "black",size = 1,linetype = "solid"),
           legend.position = "bottom",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           legend.background = element_rect(linetype="solid",
                                            colour ="black"),
           legend.title = element_blank(),
           legend.key.size = unit(0.75,"cm"),
           legend.text=element_text(size=14))+
         annotate("text", x = 40, y = 0.1, label = labelplot,size=8,color="red")+
         geom_hline(yintercept=0.8,linetype="dashed")+
         geom_point(aes(x = input$k, y = power), color = "red",size=8)


     }

     if(input$method=="REMm"){
       req(c(input$d,input$n1,input$n2,input$k,input$alpha,input$method))
       v.d<-((input$n1+input$n2)/(input$n1*input$n2))+((input$d*input$d)/(2*(input$n1+input$n2)))
       v.m<-v.d/input$k
       v.m<-1.67*v.m
       lambda<-(input$d/sqrt(v.m))
       plevel<-1-(input$alpha/2)
       zval<-qnorm(p=plevel, 0,1)
       power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
       power.percent<-percent(power,digits=3)

       ## Power Analysis function

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

       ## Graph data

       k <- seq(0, 50, length=1000)
       pow.vals01<-lapply(k,function(k) power.analysis.random(d=input$d,k=k,n1=input$n1,n2=input$n2,p=input$alpha,heterogeneity="moderate"))
       pow.vals01<-as.numeric(pow.vals01)
       data<-data.frame(k,pow.vals01)
       labelplot<-as.character(paste(c("Power:", power.percent), collapse = " "))
       powerplot<-ggplot()+
         geom_line(data = data, aes(x = k, y = pow.vals01), color = "blue",size=2) +
         xlab('Number of Studies') +
         ylab('Power')+
         scale_y_continuous(labels = scales::percent)+
         theme(
           axis.line= element_line(color = "black",size = 1,linetype = "solid"),
           legend.position = "bottom",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           legend.background = element_rect(linetype="solid",
                                            colour ="black"),
           legend.title = element_blank(),
           legend.key.size = unit(0.75,"cm"),
           legend.text=element_text(size=14))+
         annotate("text", x = 40, y = 0.1, label = labelplot,size=8,color="red")+
         geom_hline(yintercept=0.8,linetype="dashed")+
         geom_point(aes(x = input$k, y = power), color = "red",size=8)
     }

     if(input$method=="REMh"){
       req(c(input$d,input$n1,input$n2,input$k,input$alpha,input$method))
       v.d<-((input$n1+input$n2)/(input$n1*input$n2))+((input$d*input$d)/(2*(input$n1+input$n2)))
       v.m<-v.d/input$k
       v.m<-2*v.m
       lambda<-(input$d/sqrt(v.m))
       plevel<-1-(input$alpha/2)
       zval<-qnorm(p=plevel, 0,1)
       power<-1-(pnorm(zval-lambda))+(pnorm(-zval-lambda))
       power.percent<-percent(power,digits=3)

       ## Power Analysis function

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

       ## Graph data

       k <- seq(0, 50, length=1000)
       pow.vals01<-lapply(k,function(k) power.analysis.random(d=input$d,k=k,n1=input$n1,n2=input$n2,p=input$alpha,heterogeneity="high"))
       pow.vals01<-as.numeric(pow.vals01)
       data<-data.frame(k,pow.vals01)
       labelplot<-as.character(paste(c("Power:", power.percent), collapse = " "))
       powerplot<-ggplot()+
         geom_line(data = data, aes(x = k, y = pow.vals01), color = "blue",size=2) +
         xlab('Number of Studies') +
         ylab('Power')+
         scale_y_continuous(labels = scales::percent)+
         theme(
           axis.line= element_line(color = "black",size = 1,linetype = "solid"),
           legend.position = "bottom",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           legend.background = element_rect(linetype="solid",
                                            colour ="black"),
           legend.title = element_blank(),
           legend.key.size = unit(0.75,"cm"),
           legend.text=element_text(size=14))+
         annotate("text", x = 40, y = 0.1, label = labelplot,size=8,color="red")+
         geom_hline(yintercept=0.8,linetype="dashed")+
         geom_point(aes(x = input$k, y = power), color = "red",size=8)
     }

    powerplot

   })
}

# Run the application
shinyApp(ui = ui, server = server)

