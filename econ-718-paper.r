

if (Sys.info()["user"]=="travismcarthur") {
  work.dir <- "/Users/travismcarthur/Desktop/Dropbox/718 paper/data/"
}

if (Sys.info()["user"]=="Mint") {
  work.dir <- "/Users/Mint/Dropbox/718 paper/data/"
}



library("foreign")



pwt80.df <- read.dta(paste0(work.dir, "pwt80.dta"))
# From http://www.rug.nl/research/ggdc/data/penn-world-table

#attr(pwt80.df, "var.labels")
# ck: "Capital stock at current PPPs (in mil. 2005US$)"                                                  
# rkna: "Capital stock at constant 2005 national prices (in mil. 2005US$)"     
# I'll go with "rkna" variable
# Ok, now try other var:

colnames(pwt80.df)[colnames(pwt80.df)=="country"] <- "country.name"
colnames(pwt80.df)[colnames(pwt80.df)=="countrycode"] <- "country"
# Different measures of capital stock
colnames(pwt80.df)[colnames(pwt80.df)=="rkna"] <- "capital.stock"

# This below determines the year periods:

early.period <- 1985:1991
later.period <- 1999:2008

pwt80.df$period <- NA
pwt80.df$period[pwt80.df$year %in% early.period] <- "early"
pwt80.df$period[pwt80.df$year %in% later.period] <- "later"


pwt80.df <- pwt80.df[!is.na(pwt80.df$period), ]
# Removing years that we don't deal with


capital.agg <- aggregate( x=pwt80.df$capital.stock,   
  by=list(country=pwt80.df$country,
          country.name=pwt80.df$country.name,
          period=pwt80.df$period),
  FUN=mean, na.rm=TRUE)

colnames(capital.agg)[colnames(capital.agg)=="x"] <- "capital.stock"




oww3.df <- read.dta(paste0(work.dir, "oww3.dta"))

oww3.occ.classif.df <- read.csv(paste0(work.dir, "oww3 occ classification.csv"), stringsAsFactors=FALSE)

oww3.ind.classif.df <- read.csv(paste0(work.dir, "oww3 industry classification.csv"), stringsAsFactors=FALSE, na.strings="")

names(oww3.ind.classif.df) <- c("ind.class", "y3", "ind.description")
names(oww3.occ.classif.df) <- c("occ.class", "y4", "occ.description")

nrow(oww3.df)
oww3.df <- merge(oww3.df, oww3.ind.classif.df)
oww3.df <- merge(oww3.df, oww3.occ.classif.df)
nrow(oww3.df)


# View(oww3.df[oww3.df$country=="BOL",])


oww3.df$period <- NA
oww3.df$period[oww3.df$y0 %in% early.period] <- "early"
oww3.df$period[oww3.df$y0 %in% later.period] <- "later"




oww3.df <- oww3.df[!is.na(oww3.df$ind.class) & !is.na(oww3.df$period), ]
# Removing non-manuf industries and years that we don't deal with

oww3.df$occ.class[oww3.df$occ.class==""] <- "P"
# setting the missings to P for production workers


# w3wl is:
# wage with country-specific calibration and imputation, lexicographic weighting
# http://www.nber.org/oww/


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

wages.agg <- reshape(wages.agg, v.names="wage", timevar="occ.class",
  idvar=c("ind.description", "country", "period"),
  direction="wide"
)

wages.agg$wage.NP.over.wage.P <- wages.agg$wage.NP / wages.agg$wage.P





wages.agg.final <- aggregate( x=oww3.df$hw3wlus,  
  by=list(occ.class=oww3.df$occ.class,
          country=oww3.df$country,
          period=oww3.df$period),
  FUN=mean, na.rm=TRUE)
# Note that this remove aggregation by industry, so this is equiv to taking simple average
# across industries

colnames(wages.agg.final)[colnames(wages.agg.final)=="x"] <- "wage"


wages.agg.final <- reshape(wages.agg.final, v.names="wage", timevar="occ.class",
  idvar=c("country", "period"),
  direction="wide"
)

wages.agg.final$wage.NP.over.wage.P <- wages.agg.final$wage.NP / wages.agg.final$wage.P


tariffs.df <- read.table(paste0(work.dir, "TariffsEarlyLateRev2.txt"), header=TRUE, stringsAsFactors=FALSE)
# From http://thedata.harvard.edu/dvn/dv/restat/faces/study/StudyPage.xhtml?studyId=92217&tab=files

colnames(tariffs.df) <- c("country", "tariff.yr", "tau.cap", "tau.con", "tau.int", "tau.nes")

tariffs.df$period <- ifelse(tariffs.df$tariff.yr < 1995, "early", "later")

efwdata2005.df <- read.delim(paste0(work.dir, "REStatReplicationFiles/efwdata2005.tab"))

efwdata2005.df <- efwdata2005.df[efwdata2005.df$year==1985, c("isocode", "area4aiidata")]

names(efwdata2005.df) <- c("country", "total.tau.1985")

tariffs.df <- merge(tariffs.df, efwdata2005.df, all.x=TRUE)

income.class.df <- read.csv(paste0(work.dir, "WB income classification.csv"), stringsAsFactors=FALSE)
# http://siteresources.worldbank.org/DATASTATISTICS/Resources/CLASS.XLS

table(income.class.df$Income.group)
unique(income.class.df$Income.group)

income.class.df$income.class <- ifelse(income.class.df$Income.group %in% c("High income: nonOECD", "High income: OECD"), "developed", "developing")


# Chile, HK, Korea, Trinidad & Tobago, Uruguay, classed as developed here. 
# Let's do a few fixes:

income.class.df$income.class[
  income.class.df$Economy %in% c("Chile", "Korea, Rep.", "Trinidad and Tobago", "Uruguay")] <- "developing"



income.class.df <- income.class.df[, c("Code", "income.class")]
colnames(income.class.df) <- c("country", "income.class" )


final.df <- merge(capital.agg, tariffs.df)
final.df <- merge(final.df, income.class.df)
#final.df <- merge(final.df, wages.agg)

final.df$period.binary <- ifelse(final.df$period=="early", 0, 1)

library("plm")
library("sandwich")
library("lmtest")

final.plm.df <- plm.data(final.df, indexes=c("country", "period.binary"))

summary(first.stage.plm <- plm(capital.stock ~ tau.cap*tau.con, 
  data=final.plm.df[final.plm.df$income.class=="developing", ], 
  effect = "individual", model="fd"))

summary(first.stage.plm <- plm(capital.stock ~ tau.int*tau.con, 
  data=final.plm.df[final.plm.df$income.class=="developing", ], 
  effect = "individual", model="fd"))
# including tau.int with tau.con... both of them have the signs we expect. 
# level model seems to fit the data better

coeftest(first.stage.plm, vcov=vcovBK(first.stage.plm, type="HC1"))

cor.test(final.plm.df$tau.cap[final.plm.df$period=="early"], final.plm.df$tau.con[final.plm.df$period=="early"])


cor.test(final.plm.df$tau.cap[final.plm.df$period=="later"], final.plm.df$tau.con[final.plm.df$period=="later"])

cor.test(final.plm.df$tau.cap[final.plm.df$period=="later"] -
    final.plm.df$tau.cap[final.plm.df$period=="early"], 
  final.plm.df$tau.con[final.plm.df$period=="later"] - 
    final.plm.df$tau.con[final.plm.df$period=="early"])

developing.final.df <- final.plm.df[final.plm.df$income.class=="developing", ]


### Instrumental variable approach ###
# (Data from ET NBER working paper 14264 from http://thedata.harvard.edu/dvn/dv/restat/faces/study/StudyPage.xhtml?studyId=92217&tab=files)

# GATT membership in 1975 (Rose, 2001)
roseaccession.df <- read.delim(paste0(work.dir, "roseaccession.tab"))
roseaccession.df$gatt75 <- 0
roseaccession.df$gatt75[roseaccession.df$rose <= 1975] <- 1
roseaccession.df$country <- roseaccession.df$isocode 

final.plm.df <- merge(final.plm.df, roseaccession.df, all.x=TRUE)

# Historical GDP (Maddison, 2001)
madd2004gtdep.df <- read.delim(paste0(work.dir, "madd2004gtdep.tab"))
madd2004gtdep.df$country <- madd2004gtdep.df$isocode 
# gtdep = ypop35/ypop29

final.plm.df <- merge(final.plm.df, madd2004gtdep.df, all.x=TRUE)

final.plm.df <- final.plm.df[order(final.plm.df$country,final.plm.df$period), ]

final.plm.df$tau.k85 <- 0 
final.plm.df$tau.k85[final.plm.df$period=="later"] <- final.plm.df$tau.cap[final.plm.df$period=="later"]

final.plm.df <- final.plm.df[,!colnames(final.plm.df) %in% c("maddname","rose","maddnum","gdppop","pop","gdp","ypop35","ypop29")]

unique(final.plm.df$country.name[!final.plm.df$country %in% 
    wages.agg.final$country[!is.na(wages.agg.final$wage.NP.over.wage.P)]])

unique(
  wages.agg.final$country[!wages.agg.final$country %in% final.plm.df$country][!is.na(wages.agg.final$wage.NP.over.wage.P)] 
    )

# missing only Spain, France, and Sri Lanka in wages dataset (when only considering the set
# of countries that is also in the final.plm.df dataset )

final.plm.df <- merge(final.plm.df, wages.agg.final, all.x=TRUE)

final.wide.df <- reshape(final.plm.df, idvar = "country", timevar = "period", direction = "wide") 

final.wide.df$tau.cap.dif <- final.wide.df$tau.cap.later - final.wide.df$tau.cap.early
final.wide.df$tau.con.dif <- final.wide.df$tau.con.later - final.wide.df$tau.con.early
final.wide.df$capital.stock.dif <- final.wide.df$capital.stock.later - 
  final.wide.df$capital.stock.early
# Wait, we had these reversed in the first version

final.wide.df$tau.cap.interact.dif <- final.wide.df$tau.cap.later*final.wide.df$tau.con.later - final.wide.df$tau.cap.early*final.wide.df$tau.con.early

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

inst1 <- ~ gatt75.later:tau.k85.later + gtdep.later:tau.k85.later
inst2 <- ~ tau.cap.interact.dif + tau.con.dif
instlist <- list( inst1, inst2 )

fit3sls <- systemfit( eq.system, "3SLS", 
  inst = instlist, 
  data = final.wide.df[final.wide.df$income.class.later=="developing", ],
  method3sls = "GMM" )

summary(fit3sls)



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











