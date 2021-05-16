library(MASS)
library(purrr)
library(extraDistr)
library(magrittr)
library(randomNames)
library(tidyr)

# Simulate Data

do.call(rbind, list(

  # Individual vs. Group
  rnorm(7, -0.32, sqrt(0.04)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "ind", treat2 = "grp"),

  # Individual vs. GSH
  rnorm(4, -0.12, sqrt(0.01)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "ind", treat2 = "gsh"),

  # Individual vs. Telephone
  rnorm(4, -0.04, sqrt(0.01)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "ind", treat2 = "tel"),

  # Individual vs. WLC
  rnorm(18, -1.08, sqrt(0.16)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "ind", treat2 = "wlc"),

  # Individual vs. CAU
  rnorm(30, -0.52, sqrt(0.07)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "ind", treat2 = "cau"),

  # Individual vs. PLA
  rnorm(2, -0.40, sqrt(0.01)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "ind", treat2 = "pla"),

  # Group vs. gsh
  rnorm(5, 0.20, sqrt(0.01)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "grp", treat2 = "gsh"),

  # Group vs. ush
  rnorm(1, -0.06, sqrt(0.01)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "grp", treat2 = "ush"),

  # Group vs. wlc
  rnorm(18, -1.32, sqrt(0.64)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "grp", treat2 = "wlc"),

  # Group vs. cau
  rnorm(21, -0.83, sqrt(0.39)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "grp", treat2 = "cau"),

  # Gsh vs. ush
  rnorm(5, -0.37, sqrt(0.01)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "gsh", treat2 = "ush"),

  # gsh vs. wlc
  rnorm(35, -0.81, sqrt(0.19)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "gsh", treat2 = "wlc"),

  # gsh vs. cau
  rnorm(8, -0.56, sqrt(0.1)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "gsh", treat2 = "cau"),

  # tel vs. wlc
  rnorm(1, -0.69, sqrt(0.01)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "tel", treat2 = "wlc"),

  # tel vs. cau
  rnorm(6, -0.63, sqrt(0.25)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "tel", treat2 = "cau"),

  # ush vs. wlc
  rnorm(11, -0.48, sqrt(0.01)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "ush", treat2 = "wlc"),

  # ush vs. cau
  rnorm(9, -0.14, sqrt(0.03)) %>%
    map(function(x){rnorm(runif(1, 50, 300), x, runif(1, 0.05, 0.5))}) %>%
    map(function(x) c(mean(x), sd(x))) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames(c("TE", "seTE")) %>%
    cbind(., treat1 = "ush", treat2 = "cau")

)) -> data


# Sample mv normal for ind vs. gsh vs. wlc trial
# We assume a tau of sqrt(0.09) = 0.3
mu = c(-0.12, -1.08)
sigma = matrix(c(0.3, 0.3/2, 0.3/2, 0.3), 2, 2)
mvrnorm(1, mu, sigma) %>%
  mvrnorm(100, ., sigma) %>%
  data.frame() -> dat.multiarm

# Delete 2 individual ES
# Add study names
set.seed(123)
names = c(randomNames(nrow(data)-10, ethnicity = c(5), which.names = "last"),
          randomNames(5, ethnicity = c(4), which.names = "last"),
          randomNames(5, ethnicity = c(2), which.names = "last")) %>%
  paste0(., ", ", round(runif(nrow(data), 1984, 2018)))

data = cbind(names, data)
data[data$treat1 == "ind" & data$treat2 == "gsh",][1,1:3] = c("Breiman, 2001", mean(dat.multiarm[,1]), sd(dat.multiarm[,1]))
data[data$treat1 == "ind" & data$treat2 == "wlc",][1,1:3] = c("Breiman, 2001", mean(dat.multiarm[,2]), sd(dat.multiarm[,2]))

within(data, {
  TE = as.numeric(TE) %>% round(3)
  seTE = as.numeric(seTE) %>% round(3)
}) -> data

rbind(data, data.frame(names = "Breiman, 2001", TE = -0.664, seTE =  0.514, treat1 = "gsh", treat2 = "wlc")) -> data

data$treat1.long = recode(data$treat1, ind = "Individual", grp = "Group",
                          gsh = "Guided Self-Help", tel = "Telephone", ush = "Unguided Self-Help")

data$treat2.long = recode(data$treat2, grp = "Group",
                          gsh = "Guided Self-Help", tel = "Telephone", wlc = "Waitlist",
                          cau = "Care As Usual", pla = "Pill Placebo", ush = "Unguided Self-Help")



m.netmeta <- netmeta(TE = TE,
                     seTE = seTE,
                     treat1 = treat1.long,
                     treat2 = treat2.long,
                     studlab = names,
                     data = data,
                     sm = "SMD",
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     reference.group = "Care As Usual",
                     details.chkmultiarm = TRUE,
                     sep.trts = " vs ")

netgraph(m.netmeta, seq = c("Group", "Guided Self-Help", "Telephone",
                            "Unguided Self-Help", "Waitlist", "Care As Usual",
                            "Pill Placebo", "Individual"))


library(dmetar)

direct.evidence.plot(m.netmeta, random = TRUE)
forest(m.netmeta, sortvar = TE)


colnames(data)[1] = "author"



data %>%
  select(1:5) %>%
  cbind(., "TE2" = NA, "seTE2" = NA) %>%
  pivot_longer(cols = )

pivot_longer(data %>% select(1:5),
             columns = all_of(TE, seTE, treat1, treat2, TE2, seTE2),
             names_to = "treat",
             values_to = "count") %>% arrange(author)


who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)

anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )

data %>%
  dplyr::select(1:5) %>%
  pivot_longer(-author,
               names_to = c(".value"),
               names_pattern = "(..)"
  ) -> TherapyFormatsGeMTC

colnames(TherapyFormatsGeMTC) = c("study", "diff", "std.err", "treatment")

TherapyFormatsGeMTC = data.frame(TherapyFormatsGeMTC)
TherapyFormatsGeMTC[TherapyFormatsGeMTC$study == "Breiman, 2001",]

TherapyFormatsGeMTC[-c(15),] -> TherapyFormatsGeMTC


TherapyFormatsGeMTC = data.frame(TherapyFormatsGeMTC)

rownames(TherapyFormatsGeMTC) = 1:nrow(TherapyFormatsGeMTC)

data %>%
  select(1:5) %>%
  pivot_longer(-author,
               names_to = c(".value", ".value"),
               names_pattern = "(.....)(....)"
  )



data %>%
  select(1:5) %>%
  set_colnames(c("author", "diff"))
  pivot_longer(-author,
               names_to = c(".value", "arm"),
               names_sep = "\."
  )


family %>%
  pivot_longer(
    -family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = FALSE
  )

longer <- pivot_longer(dat,
                       cols=-1,
                       names_pattern = "(.*)(..)$",
                       names_to = c("limit", "name"))

colnames()

TherapyFormatsGeMTC %>% dplyr::arrange(diff) %>% dplyr::filter(!is.na(diff)) %>% dplyr::pull(study) -> RoB


TherapyFormatsGeMTC <- TherapyFormatsGeMTC2

rbind(TherapyFormatsGeMTC2, TherapyFormatsGeMTC$data[TherapyFormatsGeMTC$data$study == "Breiman, 2001",] ) -> TherapyFormatsGeMTC2


data.frame(study = RoB,
           low.rob = c(rbinom(83, 1, 0.8), rbinom(100, 1, 0.2))) -> study.info


TherapyFormatsGeMTC = list(data = TherapyFormatsGeMTC,
                           study.info = study.info)

rbind(TherapyFormatsGeMTC$data[!TherapyFormatsGeMTC$data$study == "Breiman, 2001",],
  TherapyFormatsGeMTC$data[TherapyFormatsGeMTC$data$study == "Breiman, 2001",]) -> TherapyFormatsGeMTC$data


TherapyFormatsGeMTC$data$treatment = recode(TherapyFormatsGeMTC$data$treatment, Group = "grp", "Individual" = "ind",
                                            GuidedSelfHelp =" gsh", Telephone = "tel", Waitlist = "wlc",
                                            CareAsUsual = "cau", UnguidedSelfHelp = "ush")

save(TherapyFormatsGeMTC, file = "data/TherapyFormatsGeMTC.rda")



treat.codes = data.frame(id = TherapyFormatsGeMTC$data$treatment %>% unique(),
                         description = TherapyFormatsGeMTC$data$treatment %>% unique() %>% dplyr::recode(grp = "Group", ind = "Individual",
                                                                                     gsh = "Guided Self-Help", tel = "Telephone", wlc = "Waitlist",
                                                                                     cau = "Care As Usual", pla = "Pill Placebo", ush = "Unguided Self-Help"))

TherapyFormatsGeMTC[["treat.codes"]] = treat.codes



str_replace_all(TherapyFormatsGeMTC$data$treatment, " ", "") %>%
  str_replace_all("-", "") -> TherapyFormatsGeMTC$data$treatment

network <- mtc.network(data.re = TherapyFormatsGeMTC$data,
                       treatments = treat.codes)


model <- mtc.model(network,
                   likelihood = "normal",
                   link = "identity",
                   linearModel = "random",
                   n.chain = 4)


model <- mtc.model(network,
                   likelihood = "normal",
                   link = "identity",
                   linearModel = "random",
                   n.chain = 4)

mcmc1 <- mtc.run(model, n.adapt = 50, n.iter = 1000, thin = 10)
mcmc2 <- mtc.run(model, n.adapt = 5000, n.iter = 100000, thin = 10)

gemtc::forest(relative.effect(mcmc2, t1 = "cau"), use.description = TRUE)



regressor <- list(coefficient = 'shared',
                  variable = 'low.rob',
                  control = 'cau')

network.mr <- mtc.network(data.re = TherapyFormatsGeMTC$data,
                          studies = TherapyFormatsGeMTC$study.info)

model.mr <- mtc.model(network.mr,
                      type = "regression",
                      regressor = regressor)

mcmc3 <- mtc.run(model.mr,
                 n.adapt = 5000,
                 n.iter = 100000,
                 thin = 10)


summary(mcmc3)


forest(mcmc3)





library(MASS)
Sigma <- matrix(c(10,3,3,2),2,2)


rep(0.64)

theta = m.netmeta$TE.fixed

matrix(rep(0.15, 64), 8,8) -> tau
diag(tau) = 0.3

Sigma
var(mvrnorm(n = 1000, rep(0, 2), Sigma))
var(mvrnorm(n = 1000, rep(0, 2), Sigma, empirical = TRUE))


m.netmeta$TE.fixed



theta = c(-0.72, -0.63, -0.63, -0.47, -0.13, 0, 0.39)
matrix(rep(0.15, 64), 8,8) -> tau
diag(tau) = 0.3


theta = as.numeric(m.netmeta$TE.fixed)
matrix(rep(0.15, 4096), 64, 64) -> tau
diag(tau) = 0.3


mvrnorm(n = 186, theta, tau) -> dat
as.data.frame(dat) -> dat

cnames = dimnames(m.netmeta$TE.fixed)[[1]]

ls = list()
for (i in 1:8){
  paste0(cnames[i], " vs ", cnames) -> ls[[i]]
}

unlist(ls) -> colnames


TherapyFormats$versus = paste0(TherapyFormats$treat1, " vs ", TherapyFormats$treat2)



replacer = function(data, var){
  n = nrow(data[data$versus == var,])
  data[data$versus == var,"seTE"] = runif(n, 0.09, 0.35) -> seTE
  data[data$versus == var,"TE"] = sample(dat[,var], n)

  return(data)
}

## Node-splitting analysis of inconsistency
## ========================================
##
##   comparison  p.value CrI
##
## 9  d.ind.grp   0.00020
## 10 -> direct           0.34 (0.035, 0.64)
## 11 -> indirect         -0.31 (-0.48, -0.14)
## 12 -> network          -0.15 (-0.31, 0.0046)
## 13 d.grp.gsh   0.00185
## 14 -> direct           -0.17 (-0.53, 0.19)
## 15 -> indirect         0.46 (0.28, 0.65)
## 16 -> network          0.33 (0.17, 0.50)
## 17 d.grp.wlc   0.00000
## 18 -> direct           1.5 (1.3, 1.7)
## 19 -> indirect         0.87 (0.69, 1.1)
## 20 -> network          1.1 (1.0, 1.3)
## 21 d.grp.ush   0.07020
## 22 -> direct           -0.025 (-0.77, 0.71)
## 23 -> indirect         0.68 (0.46, 0.90)
## 24 -> network          0.62 (0.41, 0.84)

## 29 d.gsh.wlc   0.00125
## 30 -> direct           0.68 (0.54, 0.83)
## 31 -> indirect         1.1 (0.90, 1.4)
## 32 -> network          0.81 (0.69, 0.94)

## 41 d.ind.cau   0.00640
## 42 -> direct           0.52 (0.37, 0.68)
## 43 -> indirect         0.89 (0.68, 1.1)
## 44 -> network          0.66 (0.53, 0.78)


## 45 d.grp.cau   0.07560
## 46 -> direct           0.92 (0.73, 1.1)
## 47 -> indirect         0.66 (0.45, 0.88)
## 48 -> network          0.81 (0.66, 0.95)

##

data("TherapyFormats")

Breiman = TherapyFormats[TherapyFormats$author == "Breiman, 2001",]
TherapyFormats$versus = paste0(TherapyFormats$treat1, " vs ", TherapyFormats$treat2)
TherapyFormats[TherapyFormats$versus == "ind vs grp", "TE"] = TherapyFormats[TherapyFormats$versus == "ind vs grp", "TE"] + 0.2
TherapyFormats[TherapyFormats$versus == "ind vs grp", "seTE"] = TherapyFormats[TherapyFormats$versus == "ind vs grp", "seTE"] + 0.1
TherapyFormats[TherapyFormats$versus == "grp vs gsh", "TE"] = TherapyFormats[TherapyFormats$versus == "grp vs gsh", "TE"] - 0.4
TherapyFormats[TherapyFormats$versus == "grp vs wlc", "TE"] = TherapyFormats[TherapyFormats$versus == "grp vs wlc", "TE"] + 0.8
TherapyFormats[TherapyFormats$versus == "grp vs ush", "TE"] = TherapyFormats[TherapyFormats$versus == "grp vs ush", "TE"] - 0.7
TherapyFormats[TherapyFormats$versus == "gsh vs wlc", "TE"] = TherapyFormats[TherapyFormats$versus == "grp vs wlc", "TE"] - 0.2
TherapyFormats[TherapyFormats$versus == "ind vs cau", "TE"] = TherapyFormats[TherapyFormats$versus == "ind vs cau", "TE"] - 0.2
TherapyFormats[TherapyFormats$versus == "grp vs cau", "TE"] = TherapyFormats[TherapyFormats$versus == "grp vs cau", "TE"] + 0.5
TherapyFormats[TherapyFormats$author == "Breiman, 2001",] = Breiman

TherapyFormats[TherapyFormats$treat1 != "pla" & TherapyFormats$treat2 != "pla",] -> TherapyFormats

m.netmeta <- netmeta(TE = TE, seTE = seTE,
                     treat1 = treat1, treat2 = treat2, studlab = author,
                     data = TherapyFormats,
                     sm = "SMD",
                     comb.fixed = F, comb.random = T,
                     reference.group = "cau", details.chkmultiarm = TRUE,
                     sep.trts = " vs ")

netsplit(m.netmeta)


netheat(m.netmeta)




TherapyFormatsGeMTC$data

class(TherapyFormats)


TFGMTC[TFGMTC$author == "Breiman, 2001",]


