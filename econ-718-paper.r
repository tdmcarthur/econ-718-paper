

if (Sys.info()["user"]=="travismcarthur") {
  work.dir <- "/Users/travismcarthur/Desktop/Dropbox/718 paper/data/"
}

if (Sys.info()["user"]=="Mint") {
  work.dir <- "/Users/Mint/Dropbox/718 paper/data/"
}


library("foreign")


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

# Reshape data from long to wide: 
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


# Create new dataframe combining all variables  ---------------------------

final.df <- merge(capital.agg, tariffs.df, all.x=TRUE)
final.df <- merge(final.df, income.class.df)
#final.df <- merge(final.df, wages.agg)

final.df$period.binary <- ifelse(final.df$period=="early", 0, 1)


# FIRST STAGE: Reg change in capital on change in tariffs -------------------

library("plm")
library("sandwich")
library("lmtest")

final.plm.df <- plm.data(final.df, indexes=c("country", "period.binary"))

final.plm.df <- merge(final.plm.df, wages.agg.final, all.x=TRUE)

summary(first.stage.plm <- plm(capital.stock.per.cap ~ tau.cap*tau.con, 
  data=final.plm.df[final.plm.df$income.class=="developing", ], 
  effect = "individual", model="fd"))

summary(first.stage.plm <- plm(capital.stock ~ tau.int*tau.con, 
  data=final.plm.df[final.plm.df$income.class=="developing", ], 
  effect = "individual", model="fd"))

coeftest(first.stage.plm, vcov=vcovBK(first.stage.plm, type="HC1"))

cor.test(final.plm.df$tau.cap[final.plm.df$period=="early"], final.plm.df$tau.con[final.plm.df$period=="early"])


cor.test(final.plm.df$tau.cap[final.plm.df$period=="later"], final.plm.df$tau.con[final.plm.df$period=="later"])

cor.test(final.plm.df$tau.cap[final.plm.df$period=="later"] -
    final.plm.df$tau.cap[final.plm.df$period=="early"], 
  final.plm.df$tau.con[final.plm.df$period=="later"] - 
    final.plm.df$tau.con[final.plm.df$period=="early"])


# FIRST STAGE: IV  --------------------------------------------------------
# Two instruments: 
# 1) GATT membership in 1975 * overall tariff in 1985 
# 2) Ratio of GDP per cap between 1935 and 1929 * overall tariff in 1985

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


# Remove some unneeded variables 
final.plm.df <- final.plm.df[,!colnames(final.plm.df) %in% c("maddname","rose","maddnum","gdppop","pop","gdp","ypop35","ypop29")]

# Reshape data frame from long to wide 
final.wide.df <- reshape(final.plm.df, idvar = "country", timevar = "period", direction = "wide") 
final.wide.df <- merge(final.wide.df, pwt80.wide.df[, c("country", "K.growth1", "K.growth2", "GDP.growth1", "GDP.growth2")], all.x=TRUE)


# Calculating dif variables 

final.wide.df$tau.cap.dif <- final.wide.df$tau.cap.later - final.wide.df$tau.cap.early
final.wide.df$tau.con.dif <- final.wide.df$tau.con.later - final.wide.df$tau.con.early
final.wide.df$capital.stock.dif <- final.wide.df$capital.stock.later - 
  final.wide.df$capital.stock.early

# Calculating growth in capital stock (double dif)

final.wide.df$double.delta.capital.stock <- final.wide.df$K.growth2 - 
  final.wide.df$K.growth1
final.wide.df$double.delta.GDP <- final.wide.df$GDP.growth2 - 
  final.wide.df$GDP.growth1
# This last one is to check ET estim. Delete later. 

# Wait, we had these reversed in the first version

final.wide.df$capital.stock.p.c.dif <- final.wide.df$capital.stock.later/final.wide.df$population.later - 
  final.wide.df$capital.stock.early/final.wide.df$population.early

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

# gen ltki1 = log(1+0.5*tcap1/100+0.5*tint1/100)
# gen ltki2 = log(1+0.5*tcap2/100+0.5*tint2/100)


library(AER)

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









