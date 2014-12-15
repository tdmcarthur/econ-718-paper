# NOTE: Below changes working directory to the presentation folder
# You must first run the main code to create all the variables and such

if (Sys.info()["user"]=="travismcarthur") {
  work.dir <- "/Users/travismcarthur/git/econ-718-paper/paper-building/"
}

if (Sys.info()["user"]=="Mint") {
  work.dir <- stop("Specify your directory")
}




with(final.wide.df[complete.cases(final.wide.df[, c("ln.wage.NP.over.wage.P.dif", "ln.capital.stock.p.c.dif")]) & final.wide.df$country!="KNA", ], {
  
  income.color <- ifelse(income.class.early=="developing", "red", "blue")

  plot(ln.capital.stock.p.c.dif, ln.wage.NP.over.wage.P.dif, col="white")
  text(ln.capital.stock.p.c.dif, ln.wage.NP.over.wage.P.dif , labels=country, col=income.color)
  
}
)



eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con*income.class.early ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)

inst1 <- ~ d.ln.tau.capint.con*income.class.early 
inst2 <- ~ ln.capital.stock.p.c.dif*income.class.early
instlist <- list( inst1, inst2 )

fit.SUR.world <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$country!="KNA", ],
  method3sls = "GMM" )


# Thanks to http://stackoverflow.com/questions/14069629/plotting-confidence-intervals
newx <- seq(min(final.wide.df$ln.capital.stock.p.c.dif), 
  max(final.wide.df$ln.capital.stock.p.c.dif), length.out=100)



preds.developing <- predict(fit.SUR.world, newdata = 
    data.frame(ln.capital.stock.p.c.dif=newx, 
      income.class.early=unique(final.wide.df$income.class.early[final.wide.df$income.class.early=="developing"]), 
      # need to do above to get proper factor in there
      d.ln.tau.capint.con=0), 
  # set tariff to zero since it doesn't appear in 2nd eqn anyway
                 interval = "confidence")

preds.developing <- preds.developing[, 4:6] # get rid of first eqn

preds.developed <- predict(fit.SUR.world, newdata = 
    data.frame(ln.capital.stock.p.c.dif=newx, 
      income.class.early=unique(final.wide.df$income.class.early[final.wide.df$income.class.early!="developing"]), 
      # need to do above to get proper factor in there
      d.ln.tau.capint.con=0), 
                 interval = "confidence")

preds.developed <- preds.developed[, 4:6] # get rid of first eqn

# plot
#plot(y ~ x, data = df, type = 'n')
# add fill
#polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
# model
#abline(mod)
# intervals
lines(newx, preds.developing[ ,1], col = 'red')
lines(newx, preds.developing[ ,2], lty = 'dashed', col = 'red')
lines(newx, preds.developing[ ,3], lty = 'dashed', col = 'red')

#lines(newx, preds.developed[ ,1], col = 'blue')
#lines(newx, preds.developed[ ,2], lty = 'dashed', col = 'blue')
#lines(newx, preds.developed[ ,3], lty = 'dashed', col = 'blue')
# Ok, let's not include developed country set since it makes plot too
# busy and there is no stat signif effect




























# Check without KNA:


eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con*income.class.early ,
  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)

inst1 <- ~ d.ln.tau.capint.con*income.class.early 
inst2 <- ~   ln.capital.stock.p.c.dif*income.class.early
  # ln.i.price.integ.dif*income.class.early
instlist <- list( inst1, inst2 )

fit.SUR.world <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$country!="KNA", ],
  method3sls = "GMM" )

summary(fit.SUR.world, diagnostics=TRUE)
# KNA has GDP per cap of $21,260 ( with 51,538 people)

fit.SUR.world <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df,
  method3sls = "GMM")

summary(fit.SUR.world)

final.wide.df[final.wide.df$country=="KNA","d.ln.tau.capint.con"]
# just exclude KNA. It has missing val for 1st eqn.




summary(first.stage.plm <- lm(ln.capital.stock.p.c.dif ~ ln.i.price.integ.dif ,
      data=final.wide.df[!is.na(final.wide.df$ln.wage.NP.over.wage.P.dif ) & final.wide.df$country!="KNA",]))




summary(first.stage.plm <- lm(ln.wage.NP.over.wage.P.dif ~ ln.i.price.integ.dif ,
      data=final.wide.df[!is.na(final.wide.df$ln.wage.NP.over.wage.P.dif ) & final.wide.df$country!="KNA",]))


summary(final.wide.df$ln.i.price.integ.dif )


