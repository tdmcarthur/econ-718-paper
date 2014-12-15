# NOTE: Below changes working directory to the presentation folder
# You must first run the main code to create all the variables and such
# Also must run table-making.r

if (Sys.info()["user"]=="travismcarthur") {
  work.dir <- "/Users/travismcarthur/git/econ-718-paper/paper-building/"
}

if (Sys.info()["user"]=="Mint") {
  work.dir <- stop("Specify your directory")
}


pdf(file=paste0(work.dir, "wage-K-scatter.pdf"),
  width=6,
  height=6)


with(final.wide.df[complete.cases(final.wide.df[, c("ln.wage.NP.over.wage.P.dif", "ln.capital.stock.p.c.dif")]) & final.wide.df$country!="KNA", ], {
  
  income.color <- ifelse(income.class.early=="developing", "red", "blue")

  plot(ln.capital.stock.p.c.dif, ln.wage.NP.over.wage.P.dif, col="white",
    main="Figure 1: Capital accumulation raises wage \npremium for developing countries",
    xlab=expression(Delta*log(K)), ylab=expression(Delta*log(w[H]/w[L])))
  text(ln.capital.stock.p.c.dif, ln.wage.NP.over.wage.P.dif , labels=country, col=income.color)
  
  legend("bottomright",  legend=c("Developing", "Developed"), col=c("red", "blue"), pch=15,
    bty="n")
  #, inset=c(-.3, 0))
  # Something weird is happening with legend placement
  
  
}
)

# Very useful for plotmath to avoid a bunch of paste expressions:
# http://stackoverflow.com/questions/13955200/subscripts-in-r-when-adding-other-text



#eq.system <- list(first = double.delta.capital.stock ~ d.ln.tau.capint.con*income.class.early ,
#  second= ln.wage.NP.over.wage.P.dif ~ ln.capital.stock.p.c.dif*income.class.early)

#inst1 <- ~ d.ln.tau.capint.con*income.class.early 
#inst2 <- ~ ln.capital.stock.p.c.dif*income.class.early
#instlist <- list( inst1, inst2 )

#fit.SUR.world <- systemfit( eq.system, "3SLS", 
#  inst = instlist, 
#  data = final.wide.df[final.wide.df$country!="KNA", ],
#  method3sls = "GMM" )


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


#

dev.off()


with(final.wide.df[final.wide.df$income.class.later=="developing" & final.wide.df$country!="KNA", ],
  cor(d.ln.tau.capint , d.ln.tau.con, use="complete.obs")
)

