

if (Sys.info()["user"]=="travismcarthur") {
  work.dir <- "/Users/travismcarthur/Desktop/Dropbox/718 paper/data/"
}

if (Sys.info()["user"]=="patchachaikitmongkol") {
  work.dir <- ""
  # MINT: Put the directory of your data folder above
}



library("foreign")

oww3.df <- read.dta(paste0(work.dir, "oww3.dta"))

oww3.occ.classif.df <- read.csv("/Users/travismcarthur/Desktop/Dropbox/718 paper/data/oww3 occ classification.csv", stringsAsFactors=FALSE)

oww3.ind.classif.df <- read.csv("/Users/travismcarthur/Desktop/Dropbox/718 paper/data/oww3 industry classification.csv", stringsAsFactors=FALSE, na.strings="")

names(oww3.ind.classif.df) <- c("ind.class", "y3", "ind.description")
names(oww3.occ.classif.df) <- c("occ.class", "y4", "occ.description")

nrow(oww3.df)
oww3.df <- merge(oww3.df, oww3.ind.classif.df)
oww3.df <- merge(oww3.df, oww3.occ.classif.df)
nrow(oww3.df)


# This below determines the year periods:

early.period <- 1988:1992
later.period <- 1998:2002

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










