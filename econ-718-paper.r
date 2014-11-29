

if (Sys.info()["user"]=="travismcarthur") {
  work.dir <- "/Users/travismcarthur/Desktop/Dropbox/718 paper/data/"
}

if (Sys.info()["user"]=="Mint") {
  work.dir <- "/Users/Mint/Dropbox/718 paper/data/"
}


library("foreign")
library("AER")


# Macro variables from Penn-World-Table -----------------------------------

pwt80.df <- read.dta(paste0(work.dir, "pwt80.dta"))
# From http://www.rug.nl/research/ggdc/data/penn-world-table

#attr(pwt80.df, "var.labels")                                             

colnames(pwt80.df)[colnames(pwt80.df)=="country"] <- "country.name"
colnames(pwt80.df)[colnames(pwt80.df)=="countrycode"] <- "country"
colnames(pwt80.df)[colnames(pwt80.df)=="rkna"] <- "capital.stock"
# rkna: "Capital stock at constant 2005 national prices (in mil. 2005US$)"    
colnames(pwt80.df)[colnames(pwt80.df)=="rgdpna"] <- "real.gdp"
colnames(pwt80.df)[colnames(pwt80.df)=="pop"] <- "population"

# Generate per capita variables:
pwt80.df$capital.stock.per.cap <- pwt80.df$capital.stock/pwt80.df$population
pwt80.df$real.gdp.per.cap <- pwt80.df$real.gdp/pwt80.df$population

# This below determines the year periods:
early.period <- 1985:1991
later.period <- 1999:2008

pwt80.df$period <- NA
pwt80.df$period[pwt80.df$year %in% early.period] <- "early"
pwt80.df$period[pwt80.df$year %in% later.period] <- "later"

# Wide data 
pwt80.wide.df <- reshape(pwt80.df, idvar = "country", timevar = "year", direction = "wide") 

# Generate growth rate of capital stocks: 
pwt80.wide.df <- within(pwt80.wide.df, {
  K.growth1 <- (log(capital.stock.per.cap.1990)-log(capital.stock.per.cap.1975))/15
  K.growth2 <- (log(capital.stock.per.cap.2004)-log(capital.stock.per.cap.1990))/14
  # K.growth1 (K.growth2): avg growth per annum during early (later) period
  
  # Growth rate of GDP (This part is to check ET results. Delete later.)
  GDP.growth1 <- (log(real.gdp.per.cap.1990)-log(real.gdp.per.cap.1975))/15
  GDP.growth2 <- (log(real.gdp.per.cap.2004)-log(real.gdp.per.cap.1990))/14
  }
)

# Remove years that we don't deal with:
pwt80.df <- pwt80.df[!is.na(pwt80.df$period), ]

# Create dataframe for aggregate capital:
capital.agg <- aggregate( x=pwt80.df[, c("capital.stock", "real.gdp", "population")],   
  by=list(country=pwt80.df$country,
          country.name=pwt80.df$country.name,
          period=pwt80.df$period),
  FUN=mean, na.rm=TRUE)

#colnames(capital.agg)[colnames(capital.agg)=="x"] <- "capital.stock"


# Wage data from OWW database  --------------------------------------------

oww3.df <- read.dta(paste0(work.dir, "oww3.dta"))

oww3.occ.classif.df <- read.csv(paste0(work.dir, "oww3 occ classification.csv"), stringsAsFactors=FALSE)

oww3.ind.classif.df <- read.csv(paste0(work.dir, "oww3 industry classification.csv"), stringsAsFactors=FALSE, na.strings="")

names(oww3.ind.classif.df) <- c("ind.class", "y3", "ind.description")
names(oww3.occ.classif.df) <- c("occ.class", "y4", "occ.description")

nrow(oww3.df)
oww3.df <- merge(oww3.df, oww3.ind.classif.df)
oww3.df <- merge(oww3.df, oww3.occ.classif.df)
nrow(oww3.df)

# Determine the year periods: 
oww3.df$period <- NA
oww3.df$period[oww3.df$y0 %in% early.period] <- "early"
oww3.df$period[oww3.df$y0 %in% later.period] <- "later"

# Remove non-manuf industries and years that we don't deal with:
oww3.df <- oww3.df[!is.na(oww3.df$ind.class) & !is.na(oww3.df$period), ]

# There are missing data for occupation class (production "P" or nonproduction "NP")
oww3.df$occ.class[oww3.df$occ.class==""] <- "P"
# setting the missings to P for production workers

# w3wl is:
# wage with country-specific calibration and imputation, lexicographic weighting
# http://www.nber.org/oww/

# Create dataframe for wages
wages.agg <- aggregate( x=oww3.df$hw3wlus,  
  by=list(ind.description=oww3.df$ind.description,
          occ.class=oww3.df$occ.class,
          country=oww3.df$country,
          period=oww3.df$period),
  FUN=mean, na.rm=TRUE)

# TODO: What is the "" country, i.e. country with no name?
oww3.df$y3[oww3.df$country==""] 
# weird. it has a missing code for World Bank code, but 
# nonmissing for the ILO code

#table(is.na(wages.agg$wage.NP.over.wage.P))
#table(oww3.df$occ.class, oww3.df$ind.description=="Iron and steel basic industries")
#table(oww3.df$occ.description, oww3.df$ind.description=="Iron and steel basic industries")
#table(oww3.df$occ.description, oww3.df$ind.description=="Manufacture of wearing apparel (except footwear)")

names(wages.agg)[names(wages.agg)=="x"] <-  "wage"

# Reshape long to wide 
wages.agg <- reshape(wages.agg, v.names="wage", timevar="occ.class",
  idvar=c("ind.description", "country", "period"),
  direction="wide")

# Computing wage ratio: indicator of wage inequality 
wages.agg$wage.NP.over.wage.P <- wages.agg$wage.NP / wages.agg$wage.P

wages.agg.final <- aggregate( x=oww3.df$hw3wlus,  
  by=list(occ.class=oww3.df$occ.class,
          country=oww3.df$country,
          period=oww3.df$period),
  FUN=mean, na.rm=TRUE)
# Note that this remove aggregation by industry, so this is equiv to taking simple average across industries

colnames(wages.agg.final)[colnames(wages.agg.final)=="x"] <- "wage"

wages.agg.final <- reshape(wages.agg.final, v.names="wage", timevar="occ.class",
  idvar=c("country", "period"),
  direction="wide")

wages.agg.final$wage.NP.over.wage.P <- wages.agg.final$wage.NP / wages.agg.final$wage.P


# Disaggregate and aggregate tariff data from ET 2008  --------------------

# Disaggregate tariffs

tariffs.df <- read.table(paste0(work.dir, "TariffsEarlyLateRev2.txt"), header=TRUE, stringsAsFactors=FALSE)
# From http://thedata.harvard.edu/dvn/dv/restat/faces/study/StudyPage.xhtml?studyId=92217&tab=files

colnames(tariffs.df) <- c("country", "tariff.yr", "tau.cap", "tau.con", "tau.int", "tau.nes")

# Determine the time periods 
tariffs.df$period <- ifelse(tariffs.df$tariff.yr < 1995, "early", "later")

# Aggregate tariffs

efwdata2005.df <- read.delim(paste0(work.dir, "REStatReplicationFiles/efwdata2005.tab"))
efwdata2005.df <- efwdata2005.df[efwdata2005.df$year==1985, c("isocode", "area4aiidata")]
names(efwdata2005.df) <- c("country", "total.tau.1985")

tariffs.df <- merge(tariffs.df, efwdata2005.df, all.x=TRUE)


# Income classification from World Bank  ----------------------------------

income.class.df <- read.csv(paste0(work.dir, "WB income classification.csv"), stringsAsFactors=FALSE)
# http://siteresources.worldbank.org/DATASTATISTICS/Resources/CLASS.XLS

table(income.class.df$Income.group)
unique(income.class.df$Income.group)

income.class.df$income.class <- ifelse(income.class.df$Income.group %in% c("High income: nonOECD", "High income: OECD"), "developed", "developing")

# Chile, HK, Korea, Trinidad & Tobago, Uruguay, classed as developed. Change to developing
income.class.df$income.class[
  income.class.df$Economy %in% c("Chile", "Korea, Rep.", "Trinidad and Tobago", "Uruguay")] <- "developing"

# Create dataframe for income class 
income.class.df <- income.class.df[, c("Code", "income.class")]
colnames(income.class.df) <- c("country", "income.class" )


# Combining data ----------------------------------------------------------

final.df <- merge(capital.agg, tariffs.df, all.x=TRUE)
final.df <- merge(final.df, income.class.df)
#final.df <- merge(final.df, wages.agg)

final.df$period.binary <- ifelse(final.df$period=="early", 0, 1)

library(plm)

final.plm.df <- plm.data(final.df, indexes=c("country", "period.binary"))

final.plm.df <- merge(final.plm.df, wages.agg.final, all.x=TRUE)

# IV variables ------------------------------------------------------------

# GATT membership in 1975 

roseaccession.df <- read.delim(paste0(work.dir, "roseaccession.tab"))
# from http://thedata.harvard.edu/dvn/dv/restat/faces/study/StudyPage.xhtml?studyId=92217&tab=files)

roseaccession.df$gatt75 <- 0
roseaccession.df$gatt75[roseaccession.df$rose <= 1975] <- 1
roseaccession.df$country <- roseaccession.df$isocode 

final.plm.df <- merge(final.plm.df, roseaccession.df, all.x=TRUE)


# Ratio of GDP per capita between 1935 and 1929 

madd2004gtdep.df <- read.delim(paste0(work.dir, "madd2004gtdep.tab"))
# from http://thedata.harvard.edu/dvn/dv/restat/faces/study/StudyPage.xhtml?studyId=92217&tab=files)
madd2004gtdep.df$country <- madd2004gtdep.df$isocode 

final.plm.df <- merge(final.plm.df, madd2004gtdep.df, all.x=TRUE)
final.plm.df <- final.plm.df[order(final.plm.df$country,final.plm.df$period), ]


# Overall tariff in 1985

final.plm.df$tau.k85 <- 0 
final.plm.df$tau.k85[final.plm.df$period=="later"] <- final.plm.df$tau.cap[final.plm.df$period=="later"]


# Modify final data frame -------------------------------------------------

# Remove some unneeded variables 
final.plm.df <- final.plm.df[,!colnames(final.plm.df) %in% c("maddname","rose","maddnum","gdppop","pop","gdp","ypop35","ypop29")]

# Reshape data frame from long to wide 
final.wide.df <- reshape(final.plm.df, idvar = "country", timevar = "period", direction = "wide") 
final.wide.df <- merge(final.wide.df, pwt80.wide.df[, c("country", "K.growth1", "K.growth2", "GDP.growth1", "GDP.growth2")], all.x=TRUE)


# Create variables for first stage OLS ------------------------------------

# dif log variables 

final.wide.df <- within(final.wide.df, {
  # tariffs 
  ln.tau.con.early <- log(1+tau.con.early/100)
  ln.tau.con.later <- log(1+tau.con.later/100)
  d.ln.tau.con <- ln.tau.con.later-ln.tau.con.early
  ln.tau.cap.early <- log(1+tau.cap.early/100)
  ln.tau.cap.later <- log(1+tau.cap.later/100)
  d.ln.tau.cap <- ln.tau.cap.later-ln.tau.cap.early
  ln.tau.int.early <- log(1+tau.int.early/100)
  ln.tau.int.later <- log(1+tau.int.later/100)
  d.ln.tau.int <- ln.tau.int.later-ln.tau.int.early
  # Simple avg between capital and intermediate tariffs
  ln.tau.capint.early <- log(1+0.5*tau.cap.early/100+0.5*tau.int.early/100)
  ln.tau.capint.later <- log(1+0.5*tau.cap.later/100+0.5*tau.int.later/100)
  d.ln.tau.capint <- ln.tau.capint.later-ln.tau.capint.early
  # Interaction of simple avg cap.int with con 
  ln.tau.capint.con.early <- ln.tau.capint.early*ln.tau.con.early
  ln.tau.capint.con.later <- ln.tau.capint.later*ln.tau.con.later
  d.ln.tau.capint.con <- ln.tau.capint.con.later-ln.tau.capint.con.early
  # Growth of log capital stock per cap
  double.delta.capital.stock <- K.growth2 - K.growth1
  # GDP (to check consistency with ET results. Delete later)
  double.delta.GDP <- GDP.growth2 - GDP.growth1 
  
  capital.stock.p.c.dif <- capital.stock.later/population.later - 
    capital.stock.early/population.early
  
  ln.capital.stock.p.c.dif <- log(capital.stock.later/population.later) - 
    log(capital.stock.early/population.early)
  
}
)

# Liberalization dummy variable 

final.wide.df <- within(final.wide.df, {
  # Median dif log tariffs
  med.d.ln.tau.con <- median(d.ln.tau.con, na.rm=TRUE)
  med.d.ln.tau.cap <- median(d.ln.tau.cap, na.rm=TRUE)
  med.d.ln.tau.int <- median(d.ln.tau.int, na.rm=TRUE)
  med.d.ln.tau.capint <- median(d.ln.tau.capint, na.rm=TRUE)
  med.d.ln.tau.capint.con <- median(d.ln.tau.capint.con, na.rm=TRUE)
  # Liberalization indicator (lib==1 if d.ln.tau < med.d.ln.tau)
  lib.con <- as.numeric(d.ln.tau.con < med.d.ln.tau.con)
  lib.cap <- as.numeric(d.ln.tau.cap < med.d.ln.tau.cap)
  lib.int <- as.numeric(d.ln.tau.int < med.d.ln.tau.int)
  lib.capint <- as.numeric(d.ln.tau.capint < med.d.ln.tau.capint)
  lib.capint.con <- as.numeric(d.ln.tau.capint.con < med.d.ln.tau.capint.con)
}
)

# FIRST STAGE: OLS regression with cont tariff ----------------------------
# Hypo: lower tau.capint/tau.capint.con, higher k growth 
#       lower tau.con, lower k growth 



# 1) capint + con

summary(first.stage.lm <- lm(double.delta.capital.stock ~ 
  d.ln.tau.capint + d.ln.tau.con, 
  data=final.wide.df[final.wide.df$income.class.early=="developing", ], 
  ))
# Expected signs

# 2) capint.con + con

summary(first.stage.lm <- lm(double.delta.capital.stock ~ 
  d.ln.tau.capint.con + d.ln.tau.con, 
  data=final.wide.df[final.wide.df$income.class.early=="developing", ], 
))
# Expected signs. Coeff of capint.con is -.075 (-.045 for capint) 


# FIRST STAGE: OLS regression with dummy liberalization-----------------
# Hypo: lib.conint and/or lib.conint.con increase k growth, 
#       lib.con decreases k growth

# 1) capint + con

summary(first.stage.lm <- lm(double.delta.capital.stock ~ 
    lib.capint + lib.con, 
    data=final.wide.df[final.wide.df$income.class.early=="developing", ], 
))
# Expected signs. Much lower P Values than continuous variable

summary(first.stage.lm <- lm(double.delta.capital.stock ~ 
    lib.capint.con + lib.con, 
    data=final.wide.df[final.wide.df$income.class.early=="developing", ], 
))
# Expected signs. Result in first spec is better (higher coeff, lower p)


# FIRST STAGE: IV  --------------------------------------------------------
# Two instruments: 
# 1) GATT membership in 1975 * overall tariff in 1985 
# 2) Ratio of GDP per cap between 1935 and 1929 * overall tariff in 1985


#final.wide.df$capital.stock.p.c.dif <- final.wide.df$capital.stock.later/final.wide.df$population.later - 
  #final.wide.df$capital.stock.early/final.wide.df$population.early

final.wide.df$log.capital.stock.p.c.dif <- log(final.wide.df$capital.stock.later/final.wide.df$population.later) - 
  log(final.wide.df$capital.stock.early/final.wide.df$population.early)

final.wide.df$tau.cap.interact.dif <- final.wide.df$tau.cap.later*final.wide.df$tau.con.later - final.wide.df$tau.cap.early*final.wide.df$tau.con.early

final.wide.df$log.tau.cap.interact.dif <- log(final.wide.df$tau.cap.later*final.wide.df$tau.con.later) - log(final.wide.df$tau.cap.early*final.wide.df$tau.con.early)

final.wide.df$log.tau.con.dif.old <- log(final.wide.df$tau.con.later) - log(final.wide.df$tau.con.early)
final.wide.df$log.tau.cap.dif <- log(final.wide.df$tau.cap.later) - log(final.wide.df$tau.cap.early)

final.wide.df <- within(final.wide.df, {
  log.tau.cap.AND.int.dif <- log(1+0.5*tau.cap.later/100+0.5*tau.int.later/100) - 
    log(1+0.5*tau.cap.early/100+0.5*tau.int.early/100) 

  
#  log.tau.cap.AND.int.interact.con.dif <- log((1+0.5*tau.cap.later/100+0.5*tau.int.later/100) * (1 + tau.con.later/100) )  - 
#    log((1+0.5*tau.cap.early/100+0.5*tau.int.early/100) * (1 + tau.con.early/100))  
  
    log.tau.cap.AND.int.interact.con.dif <- log(1+0.5*tau.cap.later/100+0.5*tau.int.later/100) * log(1 + tau.con.later/100)   - 
    log(1+0.5*tau.cap.early/100+0.5*tau.int.early/100) * log(1 + tau.con.early/100)
  
  log.tau.con.dif <- log(1+tau.con.later/100) - log(1+tau.con.early/100)
  
})

final.wide.df$log.tau.cap.dif <- log(final.wide.df$tau.cap.later) - log(final.wide.df$tau.cap.early)


summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.dif:tau.con.dif + tau.con.dif | gatt75.later:tau.k85.later + gtdep.later:tau.k85.later, 
      data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)
# Above is original regression without the interaction difference fix

summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.interact.dif + tau.con.dif + tau.cap.dif | gatt75.later:tau.k85.later + gtdep.later:tau.k85.later +
    gatt75.later:tau.con.later + gtdep.later:tau.con.later, 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)


summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.interact.dif + tau.con.dif  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)




summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.interact.dif + log.tau.con.dif  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)





summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.dif  | 
    gatt75.later:total.tau.1985.early +  gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)



summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.dif + log.tau.cap.AND.int.interact.con.dif  | 
    gatt75.later:total.tau.1985.early +  gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)



summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.dif + log.tau.cap.AND.int.interact.con.dif  | 
    gatt75.later:total.tau.1985.early +  gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)




summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.interact.con.dif  | 
    gatt75.later:total.tau.1985.early +  gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)


summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.interact.con.dif  | 
    gatt75.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)




summary(test.lm <- lm(double.delta.capital.stock ~ log.tau.cap.AND.int.interact.con.dif +
    log.tau.con.dif + log.tau.cap.AND.int.dif , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]))

summary(test.lm <- lm(double.delta.capital.stock ~ log.tau.cap.AND.int.interact.con.dif  , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]))


summary(test.lm <- lm(double.delta.capital.stock ~
    log.tau.con.dif , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]))


summary(test.lm <- lm(double.delta.capital.stock ~ 
    log.tau.cap.AND.int.dif , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]))




summary(test.lm <- lm( log.capital.stock.p.c.dif ~ log.tau.cap.AND.int.interact.con.dif +
    log.tau.con.dif + log.tau.cap.AND.int.dif , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]))


cor.test(final.wide.df$log.tau.cap.AND.int.dif, final.wide.df$log.tau.con.dif)

test.cond.num <- as.matrix(final.wide.df[, c("log.tau.cap.AND.int.interact.con.dif", "log.tau.con.dif", "log.tau.cap.AND.int.dif")])

test.cond.num <- test.cond.num[complete.cases(test.cond.num), ]

#test.cond.num <- cbind(test.cond.num, test.cond.num[, 1] + test.cond.num[, 2])

rcond(t(test.cond.num) %*% test.cond.num)



cor.test(final.wide.df$log.tau.cap.AND.int.dif, final.wide.df$log.tau.cap.AND.int.interact.con.dif)




summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.dif  | 
    gatt75.later:total.tau.1985.early  , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)





summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.dif  | 
    gatt75.later:total.tau.1985.early  , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)






summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.dif  | 
    gatt75.later:total.tau.1985.early +  gtdep.later:total.tau.1985.early , 
                               data=final.wide.df), diagnostics=TRUE)








summary(first.stage.plm <- ivreg(double.delta.capital.stock ~ log.tau.cap.AND.int.dif  | 
     gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)










summary(first.stage.plm <- ivreg(double.delta.GDP ~ log.tau.cap.AND.int.dif + GDP.growth1  | 
     gtdep.later:total.tau.1985.early  + GDP.growth1, 
                               data=final.wide.df), diagnostics=TRUE)


summary(first.stage.plm <- ivreg(double.delta.GDP ~ log.tau.cap.AND.int.dif + GDP.growth1  | 
    gatt75.later:total.tau.1985.early +  gtdep.later:total.tau.1985.early + GDP.growth1  , 
                               data=final.wide.df), diagnostics=TRUE)





summary(first.stage.plm <- ivreg(double.delta.GDP ~ log.tau.cap.AND.int.dif   | 
     gtdep.later:total.tau.1985.early , 
                               data=final.wide.df), diagnostics=TRUE)


summary(first.stage.plm <- ivreg(double.delta.GDP ~ log.tau.cap.AND.int.dif  | 
    gatt75.later:total.tau.1985.early , 
                               data=final.wide.df), diagnostics=TRUE)









summary(first.stage.plm <- ivreg(capital.stock.p.c.dif ~ tau.cap.interact.dif + tau.con.dif  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)




summary(test.lm <- ivreg(capital.stock.p.c.dif ~ tau.cap.interact.dif + tau.con.dif   , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]))



summary(test.lm <- ivreg(capital.stock.p.c.dif ~ tau.cap.interact.dif + tau.con.dif   , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]))




summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.interact.dif + tau.con.dif  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
     data=final.wide.df[final.wide.df$income.class.later=="developing", ],
  weights= I(real.gdp.early/population.early)), diagnostics=TRUE)


summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.interact.dif + tau.con.dif  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
     data=final.wide.df[final.wide.df$income.class.later=="developing", ],
  weights= real.gdp.early), diagnostics=TRUE)


summary(first.stage.plm <- ivreg( I(real.gdp.later - real.gdp.early) ~ 
    tau.cap.interact.dif + tau.con.dif  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
     data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)



summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.dif | 
    gatt75.later:total.tau.1985.early  , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)

summary(test.lm <- lm(capital.stock.dif ~ tau.cap.dif + tau.cap.interact.dif + tau.con.dif  , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]))

summary(first.stage.plm <- ivreg(capital.stock.dif ~ I(tau.cap.interact.dif/sd(tau.cap.interact.dif)) + I(tau.con.dif/sd(tau.con.dif))  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)



summary(first.stage.plm <- ivreg(capital.stock.dif ~ I(tau.cap.interact.dif/sd(tau.cap.interact.dif)) + I(tau.con.dif/sd(tau.con.dif))  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
    data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)



summary(first.stage.plm <- ivreg(I(capital.stock.dif/sd(capital.stock.dif)) ~ I(tau.cap.interact.dif/sd(tau.cap.interact.dif)) + I(tau.con.dif/sd(tau.con.dif))  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
    data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)



summary(first.stage.plm <- ivreg(I(capital.stock.dif/sd(capital.stock.dif)) ~  I(tau.con.dif/sd(tau.con.dif)) + I(tau.cap.dif/sd(tau.cap.dif))  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
    data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)





summary(first.stage.plm <- ivreg(I(capital.stock.dif/sd(capital.stock.dif)) ~  I(tau.con.dif/sd(tau.con.dif)) + I(tau.cap.dif/sd(tau.cap.dif))  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
    data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)








summary(first.stage.plm <- ivreg(I(capital.stock.dif/sd(capital.stock.dif)) ~ I(tau.cap.interact.dif/sd(tau.cap.interact.dif)) + I(tau.con.dif/sd(tau.con.dif))  | 
    gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early , 
    data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)







plot(final.wide.df[final.wide.df$income.class.later=="developing", c("tau.cap.interact.dif", "capital.stock.dif")])



summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.interact.dif + tau.con.dif + tau.cap.dif | gatt75.later:tau.k85.later + 
    gatt75.later:tau.con.early +
    gatt75.later:tau.con.early:tau.k85.later, 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)

summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.interact.dif  | 
    gatt75.later:total.tau.1985.early + 
    gatt75.later:tau.con.later +
    gatt75.later:tau.cap.interact.dif, 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)




final.wide.df$wage.NP.over.wage.P.dif <- final.wide.df$wage.NP.over.wage.P.later - final.wide.df$wage.NP.over.wage.P.early 

final.wide.df$ln.wage.NP.over.wage.P.dif <- log(final.wide.df$wage.NP.over.wage.P.later) - log(final.wide.df$wage.NP.over.wage.P.early )

summary( last.stage.lm <- 
    lm(wage.NP.over.wage.P.dif ~ capital.stock.dif, 
      data=final.wide.df[final.wide.df$income.class.later=="developing", ])
)

#install.packages("systemfit")
library("systemfit")

eq.system <- list(first = capital.stock.dif ~ tau.cap.interact.dif + tau.con.dif ,
  second= wage.NP.over.wage.P.dif ~ capital.stock.dif)



fit3sls <- systemfit( eq.system, "3SLS", 
  inst = ~ gatt75.later:tau.k85.later + gtdep.later:tau.k85.later, 
  data = final.wide.df[final.wide.df$income.class.later=="developing", ],
  method3sls = "GMM" )

summary(fit3sls)

inst1 <- ~ gatt75.later:total.tau.1985.early + gtdep.later:total.tau.1985.early
inst2 <- ~ tau.cap.interact.dif + tau.con.dif
instlist <- list( inst1, inst2 )

fit3sls <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developing", ],
  method3sls = "GMM" )

summary(fit3sls)



eq.system <- list(first = double.delta.capital.stock ~ log.tau.cap.AND.int.interact.con.dif ,
  second= wage.NP.over.wage.P.dif ~ capital.stock.p.c.dif)

inst1 <- ~ gatt75.later:total.tau.1985.early
inst2 <- ~ capital.stock.p.c.dif
instlist <- list( inst1, inst2 )

fit3sls <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developing", ],
  method3sls = "GMM" )

summary(fit3sls)



eq.system <- list(first = double.delta.capital.stock ~ log.tau.cap.AND.int.interact.con.dif ,
  second= wage.NP.over.wage.P.dif ~ capital.stock.p.c.dif)

inst1 <- ~ gatt75.later:total.tau.1985.early
inst2 <- ~ capital.stock.p.c.dif
instlist <- list( inst1, inst2 )

fit3sls <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developed", ],
  method3sls = "GMM" )

summary(fit3sls)





eq.system <- list(first = double.delta.capital.stock ~ log.tau.cap.AND.int.interact.con.dif*income.class.early + log.tau.con.dif*income.class.early ,
  second= wage.NP.over.wage.P.dif ~ capital.stock.p.c.dif*income.class.early)

inst1 <- ~ gatt75.later:total.tau.1985.early*income.class.early + gtdep.early:total.tau.1985.early*income.class.early
inst2 <- ~ capital.stock.p.c.dif*income.class.early
instlist <- list( inst1, inst2 )

fit3sls <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df,
  method3sls = "GMM" )

summary(fit3sls)




eq.system <- list(first = double.delta.capital.stock ~ log.tau.cap.AND.int.interact.con.dif + log.tau.con.dif ,
  second= wage.NP.over.wage.P.dif ~ capital.stock.p.c.dif)

inst1 <- ~ gatt75.later:total.tau.1985.early
inst2 <- ~ capital.stock.p.c.dif
instlist <- list( inst1, inst2 )

fit3sls <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developing", ],
  method3sls = "GMM" )

summary(fit3sls)








with(final.wide.df, all(income.class.later == income.class.early))




















summary(lm(eq.system[[2]], data=final.wide.df[final.wide.df$income.class.later=="developing", ]))












summary(last.stage.plm <- ivreg(wage.NP.over.wage.P.dif ~ capital.stock.dif  | 
    tau.cap.dif:tau.con.dif + tau.con.dif, 
      data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)




data( "Kmenta" )
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list( demand = eqDemand, supply = eqSupply )

inst <- ~ income + farmPrice + trend
fit3sls <- systemfit( system, "3SLS", inst = inst, data = Kmenta,
method3sls = "GMM" )
print( fit3sls )





final.wide.df[, c("country", "income.class.later", "wage.NP.over.wage.P.later", "wage.NP.over.wage.P.early")]


final.wide.df[final.wide.df$income.class.later=="developing", c("country", "income.class.later", "wage.NP.over.wage.P.later", "wage.NP.over.wage.P.early")]

data.check 


data.check <- final.wide.df[final.wide.df$income.class.later=="developing", c("country", "income.class.later", "wage.NP.over.wage.P.later", "wage.NP.over.wage.P.early")]


# SECOND STAGE: Reg growth in wage ratio on growth in capital stock -------

# Remove redundant columns
unique(final.plm.df$country.name[!final.plm.df$country %in% 
                                   wages.agg.final$country[!is.na(wages.agg.final$wage.NP.over.wage.P)]])

unique(
  wages.agg.final$country[!wages.agg.final$country %in% final.plm.df$country][!is.na(wages.agg.final$wage.NP.over.wage.P)] )

# missing only Spain, France, and Sri Lanka in wages dataset (when only considering the set of countries that is also in the final.plm.df dataset )

#final.plm.df <- merge(final.plm.df, wages.agg.final, all.x=TRUE)









