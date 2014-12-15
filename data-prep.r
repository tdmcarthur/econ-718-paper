## DATA PREP


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

#colnames(pwt80.df)[colnames(pwt80.df)=="pl_i"] <- "investment.price"
pwt80.df$investment.price <- pwt80.df$pl_i/pwt80.df$pl_c
# "pl_i": Price level of capital formation,  price level of USA GDPo in 2005=1"
# "pl_c":Price level of household consumption,  price level of USA GDPo in 2005=1

# This below determines the year periods:
early.period <- 1985:1991
later.period <- 1999:2008

early.period.K <- 1985:1990
later.period.K <- 1999:2004

pwt80.df$period.K <- NA
pwt80.df$period.K[pwt80.df$year %in% early.period.K] <- "early"
pwt80.df$period.K[pwt80.df$year %in% later.period.K] <- "later"

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

pwt80.wide.df$i.price.integ1 <- rowMeans(pwt80.wide.df[, paste0("investment.price.", 1975:1990)])
pwt80.wide.df$i.price.integ2 <- rowMeans(pwt80.wide.df[, paste0("investment.price.", 1991:2004)])
# , na.rm=TRUE


# Remove years that we don't deal with:
pwt80.df <- pwt80.df[!is.na(pwt80.df$period.K), ]

# Create dataframe for aggregate capital:
capital.agg <- aggregate( x=pwt80.df[, c("capital.stock", "real.gdp", "population")],   
  by=list(country=pwt80.df$country,
          country.name=pwt80.df$country.name,
          period=pwt80.df$period.K),
  FUN=mean, na.rm=TRUE)

#colnames(capital.agg)[colnames(capital.agg)=="x"] <- "capital.stock"
colnames(capital.agg)[colnames(capital.agg)=="period.K"] <- "period"


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
efwdata2005.df[efwdata2005.df$year==1985 & efwdata2005.df$isocode=="ISR", "area4aiidata"] <- efwdata2005.df[efwdata2005.df$year==1995 & efwdata2005.df$isocode=="ISR", "area4aiidata"]
# Fixing Israel
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

roseaccession.df <- read.delim(paste0(work.dir, "roseaccession.tab"), stringsAsFactors=FALSE)
# from http://thedata.harvard.edu/dvn/dv/restat/faces/study/StudyPage.xhtml?studyId=92217&tab=files)

roseaccession.df$gatt75 <- 0
roseaccession.df$gatt75[roseaccession.df$rose <= 1975] <- 1

# Fix gernamny
roseaccession.df$country <- roseaccession.df$isocode 
roseaccession.df$country[roseaccession.df$country=="GER"] <- "DEU"

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
final.wide.df <- merge(final.wide.df, pwt80.wide.df[, c("country", "K.growth1", "K.growth2", "GDP.growth1", "GDP.growth2", "i.price.integ1", "i.price.integ2")], all.x=TRUE)


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
  i.price.integ.dif <- i.price.integ2 - i.price.integ1
  ln.i.price.integ.dif <- log(i.price.integ2) - log(i.price.integ1)
  double.ln.i.price.integ.dif <- log(log(i.price.integ2*100)) - log(log(i.price.integ1*100))
  
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



final.wide.df$wage.NP.over.wage.P.dif <- final.wide.df$wage.NP.over.wage.P.later - final.wide.df$wage.NP.over.wage.P.early 

final.wide.df$ln.wage.NP.over.wage.P.dif <- log(final.wide.df$wage.NP.over.wage.P.later) - log(final.wide.df$wage.NP.over.wage.P.early )






