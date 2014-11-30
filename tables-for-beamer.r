


# NOTE: Below changes working directory to the presentation folder
# You must first run the main code to create all the variables and such

if (Sys.info()["user"]=="travismcarthur") {
  work.dir <- "/Users/travismcarthur/Desktop/Dropbox/718 paper/871 Presentation/"
}

if (Sys.info()["user"]=="Mint") {
  work.dir <- "/Users/Mint/Dropbox/718 paper/871 Presentation/"
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
  data = final.wide.df[final.wide.df$income.class.later=="developing", ],
  method3sls = "GMM" )

#summary(fit.3sls.iv.only.developing)


eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con*income.class.early ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)

inst1 <- ~ d.ln.tau.capint.con*income.class.early 
inst2 <- ~ ln.capital.stock.p.c.dif*income.class.early
instlist <- list( inst1, inst2 )

fit.SUR.world <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df,
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


fit.SUR.texreg <- texreg( list(fit.SUR.only.developing, fit.SUR.world),
  custom.model.names = c("$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$", "$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$"),
  digits=3,
  dcolumn=TRUE,
  use.packages=FALSE,
  caption = "SUR without instrument on first equation \\newline (t-statistics in parentheses)", 
  caption.above = TRUE,
  override.se = get.t.stats.systemfit(list(fit.SUR.only.developing, fit.SUR.world)))

# Thar be dragons below...and hacks

fit.SUR.texreg <- strsplit(fit.SUR.texreg, "\n")[[1]]

fit.SUR.texreg <- c(fit.SUR.texreg[1:6], "\\toprule
     &
    \\multicolumn{4}{c}{Sample} &
    \\cmidrule(lr){2-5} 
    &
    \\multicolumn{2}{c}{Developing countries} &
    \\multicolumn{2}{c}{World} &
    \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
     &
    \\multicolumn{4}{c}{Dependent variable} &
   \\\\
    \\cmidrule(lr){2-5} " ,
  fit.SUR.texreg[-(1:6)])
# Thanks to http://tex.stackexchange.com/questions/59478/multi-column-problem

first.model.nobs <- colSums(!is.na(resid(fit.SUR.only.developing)))
second.model.nobs <- colSums(!is.na(resid(fit.SUR.world)))



fit.SUR.texreg[grepl( "Num. obs.", fit.SUR.texreg)] <- paste0("Num. obs. & ", 
  paste0(c(first.model.nobs, second.model.nobs), collapse=" & "), "            \\\\" )


cat(fit.SUR.texreg, file=paste0(work.dir, "testtable1.tex"),
  sep="\n")









##### 3SLS with only cap X con interaction, comparing developing-only vs. full sample



eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif)

# + gtdep.early:total.tau.1985.early
inst1 <- ~ gatt75.later:total.tau.1985.early
inst2 <- ~ ln.capital.stock.p.c.dif
instlist <- list( inst1, inst2 )

fit.3sls.iv.only.developing <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developing", ],
  method3sls = "GMM" )

#summary(fit.3sls.iv.only.developing)


eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con*income.class.early ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)

inst1 <- ~ gatt75.later:total.tau.1985.early*income.class.early 
inst2 <- ~ ln.capital.stock.p.c.dif*income.class.early
instlist <- list( inst1, inst2 )

fit.3sls.iv.world <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df,
  method3sls = "GMM" )



fit.3sls.iv.texreg <- texreg( list(fit.3sls.iv.only.developing, fit.3sls.iv.world),
  custom.model.names = c("$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$", "$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$"),
  digits=3,
  dcolumn=TRUE,
  use.packages=FALSE,
  caption = "3SLS with GATT membership $\\times$ 1985 average tariff as instrument on first equation \\newline (t-statistics in parentheses)", 
  caption.above = TRUE,
  override.se = get.t.stats.systemfit(list(fit.3sls.iv.only.developing, fit.3sls.iv.world)))

# Thar be dragons below...and hacks

fit.3sls.iv.texreg <- strsplit(fit.3sls.iv.texreg, "\n")[[1]]

fit.3sls.iv.texreg <- c(fit.3sls.iv.texreg[1:6], "\\toprule
     &
    \\multicolumn{4}{c}{Sample} &
    \\cmidrule(lr){2-5} 
    &
    \\multicolumn{2}{c}{Developing countries} &
    \\multicolumn{2}{c}{World} &
    \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
     &
    \\multicolumn{4}{c}{Dependent variable} &
   \\\\
    \\cmidrule(lr){2-5} " ,
  fit.3sls.iv.texreg[-(1:6)])
# Thanks to http://tex.stackexchange.com/questions/59478/multi-column-problem

first.model.nobs <- colSums(!is.na(resid(fit.3sls.iv.only.developing)))
second.model.nobs <- colSums(!is.na(resid(fit.3sls.iv.world)))

fit.3sls.iv.texreg[grepl( "Num. obs.", fit.3sls.iv.texreg)] <- paste0("Num. obs. & ", 
  paste0(c(first.model.nobs, second.model.nobs), collapse=" & "), "            \\\\" )


cat(fit.3sls.iv.texreg, file=paste0(work.dir, "testtable2.tex"),
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
  data = final.wide.df[final.wide.df$income.class.later=="developing", ],
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
  data = final.wide.df,
  method3sls = "GMM" )



fit.SUR.saturated.texreg <- texreg( list(fit.SUR.saturated.only.developing, fit.SUR.saturated.world),
  custom.model.names = c("$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$", "$\\%\\Delta$ of per capita K", "$\\ln(w_{H}/w_{L})$"),
  digits=3,
  dcolumn=TRUE,
  use.packages=FALSE,
  caption = "Saturated SUR without instrument on first equation \\newline (t-statistics in parentheses)", 
  caption.above = TRUE,
  override.se = get.t.stats.systemfit(list(fit.SUR.saturated.only.developing, fit.SUR.saturated.world)))

# Thar be dragons below...and hacks

fit.SUR.saturated.texreg <- strsplit(fit.SUR.saturated.texreg, "\n")[[1]]

fit.SUR.saturated.texreg <- c(fit.SUR.saturated.texreg[1:6], "\\toprule
     &
    \\multicolumn{4}{c}{Sample} &
    \\cmidrule(lr){2-5} 
    &
    \\multicolumn{2}{c}{Developing countries} &
    \\multicolumn{2}{c}{World} &
    \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
     &
    \\multicolumn{4}{c}{Dependent variable} &
   \\\\
    \\cmidrule(lr){2-5} " ,
  fit.SUR.saturated.texreg[-(1:6)])
# Thanks to http://tex.stackexchange.com/questions/59478/multi-column-problem

first.model.nobs <- colSums(!is.na(resid(fit.SUR.saturated.only.developing)))
second.model.nobs <- colSums(!is.na(resid(fit.SUR.saturated.world)))

fit.SUR.saturated.texreg[grepl( "Num. obs.", fit.SUR.saturated.texreg)] <- paste0("Num. obs. & ", 
  paste0(c(first.model.nobs, second.model.nobs), collapse=" & "), "            \\\\" )


cat(fit.SUR.saturated.texreg, file=paste0(work.dir, "testtable3.tex"),
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
  file.pattern="testtable.*tex", table.only=TRUE)










