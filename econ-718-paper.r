

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

oww3.ind.classif.df <- read.csv("/Users/travismcarthur/Desktop/Dropbox/718 paper/data/oww3 industry classification.csv", stringsAsFactors=FALSE)

names(oww3.ind.classif.df) <- c("ind.class", "y3", "ind.description")
names(oww3.occ.classif.df) <- c("occ.class", "y4", "occ.description")

nrow(oww3.df)
oww3.df <- merge(oww3.df, oww3.ind.classif.df)
oww3.df <- merge(oww3.df, oww3.occ.classif.df)
nrow(oww3.df)
# Hmm. missing some observations now
















