# meta to metafor

library(meta)

data(smoking)

m1 <- metainc(d.smokers, py.smokers,
              d.nonsmokers, py.nonsmokers,
              data=smoking, studlab=study)
print(m1, digits=2)

m1$TE
m1$seTE

rma(yi = m1$TE, sei = m1$seTE, slab = m1$studlab, method=m1$method.tau)


# metaSEM

library(metaSEM)
options(scipen = 999)

# Fixed Effects Model #############################################
m1 = meta(y = lifesat, v = lifesat_var, data = wvs94a, RE.constraints = 0)
summary(m1)

# Random Effects Model ##############################################
m2 = meta(y = lifesat, v = lifesat_var, data = wvs94a)
summary(m2)

# Compare
anova(m2, m1)
# LRT Delta Chi2 df1 = 26.97127

# Get random effects
coef(m2, select="random")

# Plot
plot(m2)

# Multivariate MA ##############################################
m3 = meta(y = cbind(lifesat, lifecon), v = cbind(lifesat_var, inter_cov, lifecon_var),
          data = wvs94a)
summary(m3)

# Likelihood-based CIs instead of Wald-type
m3 = meta(y = cbind(lifesat, lifecon), v = cbind(lifesat_var, inter_cov, lifecon_var),
          data = wvs94a, intervals.type = "LB")
summary(m3)


# Select random effects, make matrix, calculate cor
coef(m3, select = "random") %>% vec2symMat() %>% cov2cor()
plot(m3, axis.labels = c("Life satisfaction", "Life control"))
# The small circle dots are the observed effect sizes, whereas
# the dashed ellipses around them are the 95% confidence ellipses.
# A confidence ellipse is the bivariate generalization of the CI
# (see Friendly et al., 2013). If we were able to repeat Study i
# by collecting new data, 95% of such ellipses constructed in the
# replications will contain Study i’s true bivariate effect sizes.
# The confidence ellipses around the studies are not tilted in the figure,
# showing that the effect sizes are conditionally independent.
# The solid square in the location (−4.8338,−4.0960) represents the estimated
# average population effect sizes for the vaccinated and the nonvaccinated groups.
# The small ellipse in a solid line is the 95% confidence ellipse of the average
# effect sizes. It indicates the best estimates of the average population effect
# sizes for the vaccinated and the nonvaccinated groups in the long run. The large
# ellipse in a dashed line indicates the random effects for the 95% of studies
# that may fall inside this ellipse. It is constructed based on the estimated
# variance component of the ran- dom effects, which is a bivariate generalization
# of the 95% plausible value interval (Raudenbush, 2009). If we randomly select
# studies, 95% of the selected studies may fall inside the ellipse in long run.
# Therefore, the true population effect sizes of the studies vary greatly.
# Moreover, we also calculate the average effect size for the vaccinated group
# (−4.8338 in the x-axis) and the average effect size for the nonva- ccinated group
# (−4.0960 in the y-axis) and their 95% CIs. They are shown by the diamonds near the
# x-axis and the y-axis.

# Calculate cov
cor2cov.bivariate = function(TE1, seTE1, TE2, seTE2, cor,
                             name1 = "", name2 = ""){

  var1 = seTE1^2
  var2 = seTE2^2
  cov = seTE1*seTE2*cor
  df = data.frame(TE1, var1, TE2, var2, cov)

  if (name1 == "none" & name2 == "none"){

    colnames(df) = c("TE1", "var1", "TE2", "var2", "cov")

  } else {

    colnames(df) = c(paste("TE_", name1, sep=""),
                     paste("var_", name1, sep=""),
                     paste("TE_", name2, sep=""),
                     paste("var_", name2, sep=""),
                     paste("cov_", name1, "_", name2, sep=""))

  }

  return(df)

}

cor2cov.bivariate(
  TE1 = wvs94a$lifesat,
  seTE1 = wvs94a$lifesat_var,
  TE2 = wvs94a$lifecon,
  seTE2 = wvs94a$lifecon_var,
  cor = 0.23,
  name1 = "lifesat",
  name2 = "lifecon"
)

# Baseline model for simulataneous model test; fit model with null hypothesis that y=0
m3.baseline = meta(y = cbind(lifesat, lifecon), v = cbind(lifesat_var, inter_cov, lifecon_var),
                   data = wvs94a, intercept.constraints = c(0,0))
anova(m3, m3.baseline)


# Regress Effect size on Effect size

# Create A Matrix (regression coefficients)

A <- matrix(c(0, "0.1*beta1_2", 0, 0,
              0, 0, 0, 0,
              1, 0, 0, 0,
              0, 1, 0, 0),
            ncol=4, nrow=4, byrow=TRUE)
dimnames(A) <- list(c("f_lifesat","f_lifecon",
                      "lifesat","lifecon"),
                    c("f_lifesat","f_lifecon","lifesat",
                      "lifecon"))

A = as.mxMatrix(A)

# Sampling variance matrix S

S <- mxMatrix(type="Symm", nrow=4, ncol=4, byrow=TRUE,
              free=c(TRUE,
                     FALSE,TRUE,
                     FALSE,FALSE,FALSE,
                     FALSE,FALSE,FALSE,FALSE),
              values=c(0.1,
                       0,0.1,
                       0,0,0,
                       0,0,0,0),
              labels=c("tau2_1_1",
                       NA,"tau2_2_2",
                       NA,NA,"data.lifesat_var",
                       NA,NA,"data.inter_cov","data.lifecon_var"),
              name = "S")




# Selection matrix F (observed variables)

F <- matrix(c(0, 0, 1, 0,
              0, 0, 0, 1), nrow = 2, ncol = 4, byrow = TRUE)
dimnames(F) <- list(c("lifesat","lifecon"),
                    c("f_lifesat","f_lifecon","lifesat",
                      "lifecon"))
F = as.mxMatrix(F)

# Mean Matrix (only latent f_x are estimated)

M <- matrix(c("0*beta1_0","0*beta2_0",0,0), nrow=1, ncol=4)
dimnames(M)[[2]] <- c("f_lifesat","f_lifecon",
                      "lifesat","lifecon")
M <- as.mxMatrix(M)

# Formula for R2
R2 = mxAlgebra(beta1_2^2*tau2_2_2/(beta1_2^2*tau2_2_2 + tau2_1_1), name="R2")

# Convert RAM
RAMexp = mxExpectationRAM(A="A", S="S",
                          F="F", M="M",
                          dimnames = c("f_lifesat","f_lifecon",
                                       "lifesat","lifecon"))

# Data
my.df <- wvs94a[!is.na(wvs94a$gnp), -1]
my.df$gnp <- scale(my.df$gnp, scale=FALSE)/10000
mxdata = mxData(observed=my.df, type="raw")

# Build
reg = mxModel("Regression",
              mxdata,
              A, S, F, M, R2, mxCI("R2"), # Request CI on R2
              RAMexp,
              mxFitFunctionML())

# Run
reg.fit <- mxRun(reg, intervals=TRUE, silent=TRUE)
reg.fit@output$status[[1]]
summary(reg.fit)

library(semPlot)

# Mediation ###################################

# A Matrix
A <- matrix(c(0,0,0,0,0,
              "0*gamma1",0,"0*beta1_2",0,0,
              "0*gamma2",0,0,0,0,
              0,1,0,0,0,
              0,0,1,0,0), ncol=5, nrow=5, byrow=TRUE)
dimnames(A) <- list(c("gnp","f_lifesat","f_lifecon",
                      "lifesat","lifecon"),
                    c("gnp","f_lifesat","f_lifecon",
                      "lifesat","lifecon"))
A <- as.mxMatrix(A)


# S Matrix (Sampling Variance)

S = mxMatrix(type="Symm", nrow=5, ncol=5, byrow=TRUE,
             free=c(TRUE,
                    FALSE,TRUE,
                    FALSE,FALSE,TRUE,
                    FALSE,FALSE,FALSE,FALSE,
                    FALSE,FALSE,FALSE,FALSE,FALSE),
             values=c(1,
                      0,0.01,
                      0,0,0.1,
                      0,0,0,0,
                      0,0,0,0,0),
             labels=c("sigma2_x",
                      NA,"tau2_1_1",
                      NA,NA,"tau2_2_2",
                      NA,NA,NA,"data.lifesat_var",
                      NA,NA,NA,"data.inter_cov","data.lifecon_var"),
             name="S")

# Selection matrix F (observed variables)

F = matrix(c(1,0,0,0,0,
             0,0,0,1,0,
             0,0,0,0,1), nrow=3, ncol=5, byrow=TRUE)
dimnames(F) <- list(c("gnp","lifesat","lifecon"),
                    c("gnp","f_lifesat","f_lifecon",
                      "lifesat","lifecon"))
F <- as.mxMatrix(F)


# Mean Matrix (only latent f_x are estimated)

M <- matrix(c("0*mu_x","0*beta1_0","0*beta2_0",0,0),
            nrow=1, ncol=5)
dimnames(M)[[2]] <- c("gnp", "f_lifesat","f_lifecon",
                      "lifesat","lifecon")
M <- as.mxMatrix(M)


# Define indirect, direct, total effect
direct <- mxAlgebra(gamma1, name="direct")
indirect <- mxAlgebra(gamma2*beta1_2, name="indirect")
total <- mxAlgebra(gamma1+gamma2*beta1_2, name="total")

med <- mxModel("Mediation",
               mxData(observed=my.df, type="raw"),
               A, S, F, M, direct, indirect, total,
               mxCI(c("direct","indirect","total")),
               mxExpectationRAM(A="A", S="S", F="F", M="M",
                                dimnames=c("gnp","f_lifesat","f_lifecon",
                                           "lifesat","lifecon")),
               mxFitFunctionML())


med.fit = mxRun(med, intervals = TRUE)


#### SEM full-blown #############

r1 = matrix(c(   1, 0.23, 0.56,
                 0.23,    1, 0.84,
                 0.56, 0.84,    1),
            nrow=3, ncol=3, byrow = TRUE)

vechs(r1)


r2 = vector(0.56)
r3 = vector(0.23)

X1 = matrix(c(1, 0, 0,
              0, 1, 0,
              0, 0, 1),
            nrow=3, ncol=3, ndim = 3, byrow = TRUE)
X2 = matrix(c(0, 0, 0,
              0, 1, 0,
              0, 0, 0),
            nrow=3, ncol=3, byrow = TRUE)
X3 = matrix(c(1, 0, 0,
              0, 0, 0,
              0, 0, 0),
            nrow=3, ncol=3, byrow = TRUE)


# CFA #######################################
Digman97$data

# Stage 1: FEM #######
fixed = tssem1(Digman97$data, Digman97$n, method="FEM")
fixed$total.n
fixed$no.es
fixed$no.miss
fixed$mx.model
summary(fixed)
coef(fixed)

# Define Variables: starting with observed, then latent
vars = c("A","C","ES","E","I","f_Alpha","f_Beta")

# S Matrix (Sampling Variance)
Observed = Diag(c("0.2*var_1", "0.2*var_2", "0.2*var_3", "0.2*var_4", "0.2*var_5"))
Latent = matrix(c(1, "0.3*cor",
                  "0.3*cor", 1), nrow=2, ncol=2)
S = bdiagMat(list(Observed, Latent))
dimnames(S)[[1]] = vars
dimnames(S)[[2]] = vars
S = as.mxMatrix(S)

# A Matrix (Arrows, unidirectional)
A_1 = matrix(rep(0, 5*5), nrow=5, ncol=5)
A_2 = matrix(rep(0, 7*2), nrow=2, ncol=7)
A_3 = matrix(c("0.3*a_A", 0,
               "0.3*a_C", 0,
               "0.3*a_ES", 0,
               0, "0.3*b_E",
               0, "0.3*b_I"),
             nrow=5, ncol=2, byrow=TRUE)
gs = rbind(cbind(A_1, A_3), A_2)
dimnames(A)[[1]] = vars
dimnames(A)[[2]] = vars
A = as.mxMatrix(A)

# F matrix (observed variabled; fixed)
F_1 = diag(1, 5)
F_2 = matrix(rep(0, 5*2), nrow=5, ncol=2)
F = cbind(F_1, F_2)
dimnames(F)[[1]] = vars[1:5]
dimnames(F)[[2]] = vars
F = as.mxMatrix(F)


# Fit
fixed2 <- tssem2(fixed, Amatrix=A, Smatrix=S, Fmatrix=F,
                 diag.constraints=FALSE)
summary(fixed2)

# Plot
semplot = meta2semPlot(fixed2)
labels<-c("Ideal Life","Excellent","Satisfied","Important","Change","SWL\nScale", "No Change")
semPaths(semplot, whatLabels = "est", edge.color = "black", nodeLabels = labels)


# FEM with Clusters #################################

# Introduce NAs
Digman97$data$`Digman 1 (1994)`[,5] = NA
Digman97$data$`Digman 1 (1994)`[5,] = NA

random = tssem1(Digman97$data, Digman97$n, method = "REM", RE.type = "Diag")
summary(random)

fixed.mat = coef(random, "fixed")
fixed.mat = vec2symMat(fixed.mat, diag=FALSE)

random.mat = coef(random, "random")
random.mat = vec2symMat(random.mat, diag=FALSE)
random.mat[upper.tri(random.mat, diag=TRUE)] = NA
dimnames(random.mat)[[1]] = vars[1:5]
dimnames(random.mat)[[2]] = vars[1:5]


# Stage 2
random2 <- tssem2(random, Amatrix=A, Smatrix=S, Fmatrix=F,
                  diag.constraints=FALSE)
summary(random2)

semplot = meta2semPlot(random2)
labels<-c("Ideal Life","Excellent","Satisfied","Important","Change","SWL\nScale", "No\nChange")
semPaths(semplot, whatLabels = "est", edge.color = "black", nodeLabels = labels)


# Regression Models #######################################
# Mediation ###############################################

vars = c("SAT_Math", "Spatial", "SAT_Verbal")


fixed.cluster = tssem1(Becker94$data, Becker94$n, cluster = Becker94$gender, method = "FEM")
summary(fixed.cluster)

random = tssem1(Becker94$data, Becker94$n, method = "REM", RE.type = "Diag") # RE are independent
summary(random)

coef(random, "fixed")
mat.fe = vec2symMat(coef(random, "fixed"), diag = FALSE)
dimnames(mat.fe)[[1]] = vars
dimnames(mat.fe)[[2]] = vars

mat.i2 = summary(random)$I2.values[,2]
mat.i2 = vec2symMat(mat.i2, diag=FALSE)
dimnames(mat.i2)[[1]] = vars
dimnames(mat.i2)[[2]] = vars


# F Matrix

F = Diag(1, 3)
dimnames(F)[[1]] = vars
dimnames(F)[[2]] = vars
F = as.mxMatrix(F)

# S Matrix
S = matrix(c(1, 0, 0,
             0, "0.2*var_spatial", 0,
             0, 0, "0.2*var_verbal"),
           nrow=3, ncol=3)
dimnames(S)[[1]] = vars
dimnames(S)[[2]] = vars
S = as.mxMatrix(S)

# A Matrix (directed arrows)

A = matrix(c(0, 0, 0,
             "0.2*Math2Spatial", 0, 0,
             "0.2*Math2Verbal", "0.2*Spatial2Verbal", 0),
           nrow=3, ncol=3, byrow=TRUE)
dimnames(A)[[1]] = vars
dimnames(A)[[2]] = vars
A = as.mxMatrix(A)

# Model
random2 = tssem2(random, Amatrix = A, Smatrix = S,
                 diag.constraints = FALSE,
                 mx.algebras=list(indirect = mxAlgebra(Math2Spatial*Spatial2Verbal,
                                                       name = "indirect"),
                                  total = mxAlgebra(Math2Verbal + Math2Spatial*Spatial2Verbal,
                                                    name = "total")),
                 intervals.type = "LB")
summary(random2)

semp = meta2semPlot(random2)
semPaths(semp)



### Mediation Model 2 ##########################

Hunter83

pattern.na(Hunter83$data, show.na=TRUE)
pattern.n(Hunter83$data, Hunter83$n)

# Are the matrices positive definitive? If not can
# lead to estimation problems

is.pd(Hunter83$data)

random = tssem1(Hunter83$data, Hunter83$n, method = "REM",
                RE.type = "Diag")
summary(random)
rerun(random)

betas = vec2symMat(coef(random, "fixed"), diag=FALSE)
dimnames(betas)[[1]] = dimnames(betas)[[2]] = vars

# A Matrix
vars = dimnames(Hunter83$data$`Campbell et al. (1973)`)[[1]]

# Ability --> Job Knowledge --> Supervisor Rating
#     |             |                 A
#     |             V                 |
#     L______>Work sample_____________|


i = matrix(rep(0, 4*4), nrow=4, ncol=4)
dimnames(i)[[1]] = dimnames(i)[[2]] = vars

A = matrix(c(0, 0, 0, 0,
             "0.2*A2J", 0, 0, 0,
             "0.2*A2W", "0.2*J2W", 0, 0,
             0, "0.2*J2S", "0.2*W2S", 0),
           ncol = 4, nrow=4, byrow=TRUE)
dimnames(A)[[1]] = dimnames(A)[[2]] = vars
A = as.mxMatrix(A)



# S Matrix

S = Diag(c(1, "0.1*ErrVarJ", "0.1*ErrVarW", "0.1*ErrVarS"))
dimnames(S)[[1]] = dimnames(S)[[2]] = vars
S = as.mxMatrix(S)


# Model


random2 = tssem2(random, Amatrix = A, Smatrix = S,
                 intervals.type = "LB", diag.constraints = TRUE,
                 mx.algebras = list(Ind=mxAlgebra(A2J*J2S+ A2J*J2W*W2S +A2W*W2S,
                                                  name="Ind")))

sem.path = meta2semPlot(random2)
labels = c("Resilience","Emotion\nRegulation","Coping","Depres-\nsion")
semPaths(sem.path, whatLabels = "est", edge.color = "black", layout="tree2", rotation=2,
         nodeLabels = labels)

# Insomnia: Sleep Quality, Sleep Latency, Sleep Efficiency
# Lassitude: Daytime Dysfunction, Hypersomnia



# 7 studies

library(purrr)

for (i in 1:14){

  dimnames(Digman97$data[[i]])[[1]] = dimnames(Digman97$data[[i]])[[2]] = c("Quality", "Latency",
                                                                            "Efficiency",
                                                                            "DTDysf", "HypSomnia")

}




names(Hunter83$data)

names = c("Guttman",
"McCaffrey",
"Loescher",
"O'Malley",
"Hay",
"Twiraga",
"Wanzer",
"Arthur",
"Frondel",
"Mill",
"Ilan",
"Severence",
"Devegvar",
"Matloff")


year = round(rnorm(14, 1999, 7))

name = paste(names, " et al. (", year, ")", sep="")



names(Hunter83$data) = name

Hunter83$data$`Guttman et al. (2003)`

for (i in 1:14){

  dimnames(Hunter83$data[[i]])[[1]] = dimnames(Hunter83$data[[i]])[[2]] = c("Resilience",
                                                                            "EmotReg",
                                                                            "Coping",
                                                                            "Depression")

}



