

# NOTE: Below changes working directory to the presentation folder
# You must first run the main code to create all the variables and such

if (Sys.info()["user"]=="travismcarthur") {
  work.dir <- "/Users/travismcarthur/git/econ-718-paper/paper-building/"
}

if (Sys.info()["user"]=="Mint") {
  work.dir <- stop("Specify your directory")
}



# install.packages("texreg")
library("texreg")




##### SUR with only cap X con interaction, comparing developing-only vs. full sample



eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif)

# + gtdep.early:total.tau.1985.early
inst1 <- ~ d.ln.tau.capint.con
inst2 <- ~ ln.capital.stock.p.c.dif
instlist <- list( inst1, inst2 )

fit.SUR.only.developing <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developing" & final.wide.df$country!="KNA", ],
  method3sls = "GMM" )

#summary(fit.3sls.iv.only.developing)


eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con*income.class.early ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)

inst1 <- ~ d.ln.tau.capint.con*income.class.early 
inst2 <- ~ ln.capital.stock.p.c.dif*income.class.early
instlist <- list( inst1, inst2 )

fit.SUR.world <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$country!="KNA", ],
  method3sls = "GMM" )


get.t.stats.systemfit <- function(models) {
  ret <- list()
  for ( i in 1:length(models)) {
    eq.names <- unique(gsub("_.*", "", names(coef(models[[i]]) )))
    for (j in eq.names) {
      t.stats <- coef(models[[i]])/sqrt(diag(vcov(models[[i]])))
      ret[[length(ret)+1]] <- t.stats[grepl(paste0("^", j, "_"), names(coef(models[[i]])))]
    }   
  }
  ret
}



tr1 <- extract(fit.SUR.only.developing)
tr1[[2]]@gof <- c(tr1[[2]]@gof, cov2cor(fit.SUR.only.developing$residCov )[2,1])
tr1[[2]]@gof.names <- c(tr1[[2]]@gof.names, "Residual $\\rho$ between equations")
tr1[[2]]@gof.decimal <- c(tr1[[2]]@gof.decimal, TRUE)

tr2 <- extract(fit.SUR.world)
tr2[[2]]@gof <- c(tr2[[2]]@gof, cov2cor(fit.SUR.world$residCov )[2,1])
tr2[[2]]@gof.names <- c(tr2[[2]]@gof.names, "Residual $\\rho$ between equations")
tr2[[2]]@gof.decimal <- c(tr2[[2]]@gof.decimal, TRUE)

# trying something out in accordance with http://stackoverflow.com/questions/19888757/add-p-value-of-hausman-test-or-other-additional-gof-measure-to-texreg-table


fit.SUR.texreg <- texreg( list(tr1[[1]], tr1[[2]], tr2[[1]], tr2[[2]]),
  custom.model.names = c("$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$", "$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$"),
  digits=3,
  dcolumn=TRUE,
  use.packages=FALSE,
  caption = "First difference SUR without instrument on first equation \\newline (t-statistics in parentheses)", 
  caption.above = TRUE,
  override.se = get.t.stats.systemfit(list(fit.SUR.only.developing, fit.SUR.world)),
  stars = c(0.01, 0.05, 0.1))

# Thar be dragons below...and hacks

fit.SUR.texreg <- strsplit(fit.SUR.texreg, "\n")[[1]]

fit.SUR.texreg <- c(fit.SUR.texreg[1:6], "\\toprule
     &
    \\multicolumn{4}{c}{Sample} \\\\
    \\cmidrule(lr){2-5} 
    &
    \\multicolumn{2}{c}{Developing countries} &
    \\multicolumn{2}{c}{World} \\\\
    \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
     &
    \\multicolumn{4}{c}{Dependent variable} \\\\
    \\cmidrule(lr){2-5} \\\\
  & \\multicolumn{1}{c}{1st eqn} & \\multicolumn{1}{c}{2nd eqn} & \\multicolumn{1}{c}{1st eqn} & \\multicolumn{1}{c}{2nd eqn} \\\\
  " ,
  fit.SUR.texreg[-(1:6)])
# Thanks to http://tex.stackexchange.com/questions/59478/multi-column-problem

first.model.nobs <- colSums(!is.na(resid(fit.SUR.only.developing)))
second.model.nobs <- colSums(!is.na(resid(fit.SUR.world)))



fit.SUR.texreg[grepl( "Num. obs.", fit.SUR.texreg)] <- paste0("Num. obs. & ", 
  paste0(c(first.model.nobs, second.model.nobs), collapse=" & "), "            \\\\" )


cat(fit.SUR.texreg, file=paste0(work.dir, "SUR-reg-parsimonious.tex"),
  sep="\n")

### TEST OF WEAK INSTRUMENT


test.weak.inst.developing <- lm( d.ln.tau.capint.con ~ gatt75.later:total.tau.1985.early, data=final.wide.df[final.wide.df$income.class.later=="developing" & final.wide.df$country!="KNA", ])


test.weak.inst.world <- lm( d.ln.tau.capint.con ~ gatt75.later:total.tau.1985.early, data=final.wide.df[final.wide.df$country!="KNA", ])


fit.weak.instr.texreg <- texreg( list(test.weak.inst.developing, test.weak.inst.world),
  custom.model.names = c("Developing only", "World"),
  digits=3,
  dcolumn=TRUE,
  use.packages=FALSE,
  caption = "Test of instrument strength. \\newline Dependent variable: $\\Delta \\ln(1+\\tau_{K}/100)\\times\\ln(1+\\tau_{C}/100)$ \\newline (t-statistics in parentheses)", 
  caption.above = TRUE,
  override.se = list(
    summary(test.weak.inst.developing)$coefficients[, "t value"], 
    summary(test.weak.inst.world)$coefficients[, "t value"]))

#cat(fit.weak.instr.texreg, file=paste0(work.dir, "table2.tex"),
#  sep="\n")

##### 3SLS with only cap X con interaction, comparing developing-only vs. full sample





eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif)

# + gtdep.early:total.tau.1985.early
inst1 <- ~ gatt75.later:total.tau.1985.early
inst2 <- ~ ln.capital.stock.p.c.dif
instlist <- list( inst1, inst2 )

fit.3sls.iv.only.developing <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developing" & final.wide.df$country!="KNA", ],
  method3sls = "GMM" )

#summary(fit.3sls.iv.only.developing)


eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con*income.class.early ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)

inst1 <- ~ gatt75.later:total.tau.1985.early*income.class.early 
inst2 <- ~ ln.capital.stock.p.c.dif*income.class.early
instlist <- list( inst1, inst2 )

fit.3sls.iv.world <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$country!="KNA", ],
  method3sls = "GMM" )


tr1 <- extract(fit.3sls.iv.only.developing)
tr1[[2]]@gof <- c(tr1[[2]]@gof, cov2cor(fit.3sls.iv.only.developing$residCov )[2,1])
tr1[[2]]@gof.names <- c(tr1[[2]]@gof.names, "Residual $\\rho$ between equations")
tr1[[2]]@gof.decimal <- c(tr1[[2]]@gof.decimal, TRUE)

tr1[[1]]@gof <- c(tr1[[1]]@gof,summary(test.weak.inst.developing )$fstatistic["value"])
tr1[[1]]@gof.names <- c(tr1[[1]]@gof.names, "F-stat on first stage")
tr1[[1]]@gof.decimal <- c(tr1[[1]]@gof.decimal, TRUE)


tr2 <- extract(fit.3sls.iv.world)
tr2[[2]]@gof <- c(tr2[[2]]@gof, cov2cor(fit.3sls.iv.world$residCov )[2,1])
tr2[[2]]@gof.names <- c(tr2[[2]]@gof.names, "Residual $\\rho$ between equations")
tr2[[2]]@gof.decimal <- c(tr2[[2]]@gof.decimal, TRUE)

tr2[[1]]@gof <- c(tr2[[1]]@gof,summary(test.weak.inst.world )$fstatistic["value"])
tr2[[1]]@gof.names <- c(tr2[[1]]@gof.names, "F-stat on first stage")
tr2[[1]]@gof.decimal <- c(tr2[[1]]@gof.decimal, TRUE)

# trying something out in accordance with http://stackoverflow.com/questions/19888757/add-p-value-of-hausman-test-or-other-additional-gof-measure-to-texreg-table



fit.3sls.iv.texreg <- texreg( list(tr1[[1]], tr1[[2]], tr2[[1]], tr2[[2]]),
  custom.model.names = c("$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$", "$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$"),
  digits=3,
  dcolumn=TRUE,
  use.packages=FALSE,
  caption = "First difference 3SLS with GATT membership $\\times$ 1985 average tariff as instrument on first equation \\newline (t-statistics in parentheses)", 
  caption.above = TRUE,
  override.se = get.t.stats.systemfit(list(fit.3sls.iv.only.developing, fit.3sls.iv.world)),
  stars = c(0.01, 0.05, 0.1))

# Thar be dragons below...and hacks

fit.3sls.iv.texreg <- strsplit(fit.3sls.iv.texreg, "\n")[[1]]

fit.3sls.iv.texreg <- c(fit.3sls.iv.texreg[1:6], "\\toprule
     &
    \\multicolumn{4}{c}{Sample} \\\\
    \\cmidrule(lr){2-5} 
    &
    \\multicolumn{2}{c}{Developing countries} &
    \\multicolumn{2}{c}{World} \\\\
    \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
     &
    \\multicolumn{4}{c}{Dependent variable} \\\\
    \\cmidrule(lr){2-5} \\\\
  & \\multicolumn{1}{c}{1st eqn} & \\multicolumn{1}{c}{2nd eqn} & \\multicolumn{1}{c}{1st eqn} & \\multicolumn{1}{c}{2nd eqn} \\\\
  " ,
  fit.3sls.iv.texreg[-(1:6)])
# Thanks to http://tex.stackexchange.com/questions/59478/multi-column-problem

first.model.nobs <- colSums(!is.na(resid(fit.3sls.iv.only.developing)))
second.model.nobs <- colSums(!is.na(resid(fit.3sls.iv.world)))

fit.3sls.iv.texreg[grepl( "Num. obs.", fit.3sls.iv.texreg)] <- paste0("Num. obs. & ", 
  paste0(c(first.model.nobs, second.model.nobs), collapse=" & "), "            \\\\" )


cat(fit.3sls.iv.texreg, file=paste0(work.dir, "3sls-parsimonious.tex"),
  sep="\n")




##### SUR with all three tariff measures included, comparing developing-only vs. full sample



eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con + d.ln.tau.capint + d.ln.tau.con ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif)

# + gtdep.early:total.tau.1985.early
inst1 <- ~ d.ln.tau.capint.con  + d.ln.tau.capint + d.ln.tau.con
inst2 <- ~ ln.capital.stock.p.c.dif
instlist <- list( inst1, inst2 )

fit.SUR.saturated.only.developing <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developing" & final.wide.df$country!="KNA", ],
  method3sls = "GMM" )

#summary(fit.3sls.iv.only.developing)

eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con*income.class.early  + d.ln.tau.capint*income.class.early + d.ln.tau.con*income.class.early ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)


eq.system <- list(first = double.delta.capital.stock ~ (d.ln.tau.capint.con + d.ln.tau.capint + d.ln.tau.con)*income.class.early ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)

inst1 <- ~ (d.ln.tau.capint.con + d.ln.tau.capint + d.ln.tau.con)*income.class.early
inst2 <- ~ ln.capital.stock.p.c.dif*income.class.early
instlist <- list( inst1, inst2 )

fit.SUR.saturated.world <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$country!="KNA", ],
  method3sls = "GMM" )



tr1 <- extract(fit.SUR.saturated.only.developing)
tr1[[2]]@gof <- c(tr1[[2]]@gof, cov2cor(fit.SUR.saturated.only.developing$residCov )[2,1])
tr1[[2]]@gof.names <- c(tr1[[2]]@gof.names, "Residual $\\rho$ between equations")
tr1[[2]]@gof.decimal <- c(tr1[[2]]@gof.decimal, TRUE)

tr2 <- extract(fit.SUR.saturated.world)
tr2[[2]]@gof <- c(tr2[[2]]@gof, cov2cor(fit.SUR.saturated.world$residCov )[2,1])
tr2[[2]]@gof.names <- c(tr2[[2]]@gof.names, "Residual $\\rho$ between equations")
tr2[[2]]@gof.decimal <- c(tr2[[2]]@gof.decimal, TRUE)

# trying something out in accordance with http://stackoverflow.com/questions/19888757/add-p-value-of-hausman-test-or-other-additional-gof-measure-to-texreg-table



fit.SUR.saturated.texreg <- texreg( list(tr1[[1]], tr1[[2]], tr2[[1]], tr2[[2]]),,
  custom.model.names = c("$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$", "$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$"),
  digits=3,
  dcolumn=TRUE,
  use.packages=FALSE,
  caption = "First difference saturated SUR without instrument on first equation (t-statistics in parentheses)", 
  caption.above = TRUE,
  override.se = get.t.stats.systemfit(list(fit.SUR.saturated.only.developing, fit.SUR.saturated.world)), 
  stars = c(0.01, 0.05, 0.1))

# Thar be dragons below...and hacks

fit.SUR.saturated.texreg <- strsplit(fit.SUR.saturated.texreg, "\n")[[1]]

fit.SUR.saturated.texreg <- c(fit.SUR.saturated.texreg[1:6], "\\toprule
     &
    \\multicolumn{4}{c}{Sample} \\\\
    \\cmidrule(lr){2-5} 
    &
    \\multicolumn{2}{c}{Developing countries} &
    \\multicolumn{2}{c}{World} \\\\
    \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
     &
    \\multicolumn{4}{c}{Dependent variable} \\\\
    \\cmidrule(lr){2-5} \\\\
  & \\multicolumn{1}{c}{1st eqn} & \\multicolumn{1}{c}{2nd eqn} & \\multicolumn{1}{c}{1st eqn} & \\multicolumn{1}{c}{2nd eqn} \\\\
  " ,
  fit.SUR.saturated.texreg[-(1:6)])
# Thanks to http://tex.stackexchange.com/questions/59478/multi-column-problem

first.model.nobs <- colSums(!is.na(resid(fit.SUR.saturated.only.developing)))
second.model.nobs <- colSums(!is.na(resid(fit.SUR.saturated.world)))

fit.SUR.saturated.texreg[grepl( "Num. obs.", fit.SUR.saturated.texreg)] <- paste0("Num. obs. & ", 
  paste0(c(first.model.nobs, second.model.nobs), collapse=" & "), "            \\\\" )


cat(fit.SUR.saturated.texreg, file=paste0(work.dir, "SUR-reg-saturated.tex"),
  sep="\n")





# Below is where I convert the coefficient names

# install.packages("curl")
library("curl")

# Need curl since we are accessing github via https
source(curl("https://raw.githubusercontent.com/tdmcarthur/misc/master/authored-functions/authored-R-fns.R"))

 var.names <- read.csv(paste0(work.dir, "var names.csv"), stringsAsFactors=FALSE)
# Note to self: insert a space after each original variable name in the varnames file
# so that "replacement overreach" doesn't occur

replace.vars(replacement.matrix=var.names, 
  directory=work.dir, 
  file.pattern=".*tex", table.only=TRUE)











