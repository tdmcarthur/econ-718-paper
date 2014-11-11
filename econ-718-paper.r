

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

early.period <- 1985:1989
later.period <- 2003:2007

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

colnames(wages.agg)[colnames(wages.agg)=="x"] <- "wage"


wages.agg <- reshape(wages.agg, v.names="wage", timevar="occ.class",
  idvar=c("ind.description", "country", "period"),
  direction="wide"
)

wages.agg$wage.NP.over.wage.P <- wages.agg$wage.NP / wages.agg$wage.P

table(is.na(wages.agg$wage.NP.over.wage.P))

table(oww3.df$occ.class, oww3.df$ind.description=="Iron and steel basic industries")

table(oww3.df$occ.description, oww3.df$ind.description=="Iron and steel basic industries")
table(oww3.df$occ.description, oww3.df$ind.description=="Manufacture of wearing apparel (except footwear)")


tariffs.df <- read.table(paste0(work.dir, "TariffsEarlyLateRev2.txt"), header=TRUE, stringsAsFactors=FALSE)
# From http://thedata.harvard.edu/dvn/dv/restat/faces/study/StudyPage.xhtml?studyId=92217&tab=files

colnames(tariffs.df) <- c("country", "tariff.yr", "tau.cap", "tau.con", "tau.int", "tau.nes")

tariffs.df$period <- ifelse(tariffs.df$tariff.yr < 1995, "early", "later")


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
roseaccession.df <- read.delim (paste0(work.dir, "roseaccession.tab"))
roseaccession.df$gatt75 <- 0
roseaccession.df$gatt75[roseaccession.df$rose <= 1975] <- 1
roseaccession.df$country <- roseaccession.df$isocode 

final.plm.df <- merge(final.plm.df, roseaccession.df)

# Historical GDP (Maddison, 2001)
madd2004gtdep.df <- read.delim(paste0(work.dir, "madd2004gtdep.tab"))
madd2004gtdep.df$country <- madd2004gtdep.df$isocode 
# gtdep = ypop35/ypop29

final.plm.df <- merge(final.plm.df, madd2004gtdep.df)

final.plm.df <- final.plm.df[order(final.plm.df$country,final.plm.df$period), ]

final.plm.df$tau.k85 <- 0 
final.plm.df$tau.k85[final.plm.df$period=="later"] <- final.plm.df$tau.cap[final.plm.df$period=="later"]

final.plm.df <- final.plm.df[,!colnames(final.plm.df) %in% c("maddname","rose","maddnum","gdppop","pop","gdp","ypop35","ypop29")]

final.wide.df <- reshape(final.plm.df, idvar = "country", timevar = "period", direction = "wide") 

final.wide.df$tau.cap.dif <- final.wide.df$tau.cap.early - final.wide.df$tau.cap.later
final.wide.df$tau.con.dif <- final.wide.df$tau.con.early - final.wide.df$tau.con.later
final.wide.df$capital.stock.dif <- final.wide.df$capital.stock.later - 
  final.wide.df$capital.stock.early

library(AER)

summary(first.stage.plm <- ivreg(capital.stock.dif ~ tau.cap.dif:tau.con.dif + tau.con.dif | gatt75.later:tau.k85.later + gtdep.later:tau.k85.later, 
                               data=final.wide.df[final.wide.df$income.class.later=="developing", ]), diagnostics=TRUE)
                          














