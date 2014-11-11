if (Sys.info()["user"]=="Mint") {
  work.dir <- "/Users/Mint/Dropbox/718 paper/data/"
}

# Other macro variables from WDI database 
library(WDI)

developing.countries <- c( "AR","BD","BO","BR","CL","CN","CI","CO","EC","GH","ID","IN","KR","LK","MA","MX","MY","NP","PK","PE","PH","PY","TH","TT","TR","UY","VE" )

INDICATORS <- c("NE.GDI.FTOT.KD","NEGDIKSTKKD","TM.TAX.MANF.SM.AR.ZS","TM.TAX.MANF.SM.FN.ZS","TM.TAX.MANF.WM.AR.ZS","TM.TAX.MANF.WM.FN.ZS")
# "Gross fixed capital formation (constant 2000 US$)" 
# "Estimated Capital stock (real 2005 USD)"
# "Tariff rate, applied, simple mean, manufactured products (%)"
# "Tariff rate, most favored nation, simple mean, manufactured products (%)" 
# "Tariff rate, applied, weighted mean, manufactured products (%)"
# "Tariff rate, most favored nation, weighted mean, manufactured products (%)"

LONG <- WDI( country=developing.countries, indicator=INDICATORS, start=1983, end=2009, extra=FALSE)

LONG.plm <- plm.data(LONG,indexes=c('iso2c','year'))

summary(first.stage.wdi <- plm(NEGDIKSTKKD ~ TM.TAX.MANF.WM.FN.ZS, 
                               data=LONG.plm, model="fd" ))
                        