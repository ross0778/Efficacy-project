# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(psych)
library(car)
library(plyr)
library(readr)

# Data Import

# 2006
CB06 <- read_delim(
  "../data/HEV_Student_2006_Y1.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    STUDYID,
    DICODE,
    SCHOOLNAME,
    HEVRMAP,
    GENDER,
    SATVRECN,
    SATMRECN,
    SATW,
    INCOME,
    ETHNSDQ,
    FATHEDUC,
    MOTHEDUC,
    CUMGPA,
    MATHABIL,
    SCIABIL,
    WRITABIL
  )
)

CB06.addl <- read_delim(
  "../data/HEV_Student_2006_Y1_Addl_Vars.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    HEVRMAP,
    HSGPA
  )
)

CB06.1 <- merge(
  CB06,
  CB06.addl,
  by = "HEVRMAP",
  all.x = TRUE
)


# 2007

CB07 <- read_delim(
  "../data/HEV_Student_2007_Y1.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    STUDYID,
    DICODE,
    SCHOOLNAME,
    HEVRMAP,
    GENDER,
    SATVRECN,
    SATMRECN,
    SATW,
    INCOME,
    ETHNSDQ,
    FATHEDUC,
    MOTHEDUC,
    CUMGPA,
    MATHABIL,
    SCIABIL,
    WRITABIL
  )
)

CB07.addl <- read_delim(
  "../data/HEV_Student_2007_Y1_Addl_Vars.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    HEVRMAP,
    HSGPA
  )
)

CB07.1 <- merge(
  CB07,
  CB07.addl,
  by = "HEVRMAP",
  all.x = TRUE
)


# 2008

CB08.1 <- read_delim(
  "../data/HEV_Student_2008_Y1.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    STUDYID,
    DICODE,
    SCHOOLNAME,
    HEVRMAP,
    GENDER,
    SATVRECN,
    SATMRECN,
    SATW,
    INCOME = INCOME08,
    ETHNSDQ,
    FATHEDUC,
    MOTHEDUC,
    CUMGPA,
    HSGPA,
    MATHABIL,
    SCIABIL,
    WRITABIL
  )
)


# 2009

CB09.1 <- read_delim(
  "../data/HEV_Student_2009_Y1.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    STUDYID,
    DICODE,
    SCHOOLNAME,
    HEVRMAP,
    GENDER,
    SATVRECN,
    SATMRECN,
    SATW,
    INCOME = INCOME08,
    ETHNSDQ,
    FATHEDUC,
    MOTHEDUC,
    CUMGPA,
    HSGPA,
    MATHABIL,
    SCIABIL,
    WRITABIL
  )
)

# Create variable cohort, attach to each cohort's data set
CB06.1$COHORT <- rep(2006, length(CB06.1$HEVRMAP))
CB07.1$COHORT <- rep(2007, length(CB07.1$HEVRMAP))
CB08.1$COHORT <- rep(2008, length(CB08.1$HEVRMAP))
CB09.1$COHORT <- rep(2009, length(CB09.1$HEVRMAP))


# Merge cohort data sets into data set of predictors across cohorts (CBmain)
CBmain<-rbind(CB06.1, CB07.1,CB08.1, CB09.1)

# Load the criterion data sets and select relevant variables

# 2008-2009
Crs1 <- read_delim(
  "../data/HEV_Coursework_SY0809 (3rd thru 06yr3 etc) (1).txt",
  delim = "\t",
  quote = "",
  na = c("NA", "."),
  col_select = c(1, 2, 3, 4, 5, 8, 9),
  col_types = cols(
    CRSETERM = col_character()
  )
)

# 2009-2010
Crs2 <- read_delim(
  "../data/HEV_Coursework_SY0910.txt",
  delim = "\t",
  quote = "",
  na = c("NA", "."),
  col_select = c(1, 2, 3, 4, 5, 8, 9),
  col_types = cols(
    CRSETERM = col_character()
)
)

# 2010-2011
Crs3 <- read_delim(
  "../data/HEV_Coursework_SY1011.txt",
  delim = "\t",
  quote = "",
  na = c("NA", "."),
  col_select = c(1, 2, 3, 4, 5, 8, 9),
  col_types = cols(
    CRSETERM = col_character()
)
)

# 2011-2012
Crs4 <- read_delim(
  "../data/HEV_Coursework_SY1112.txt",
  delim = "\t",
  quote = "",
  na = c("NA", "."),
  col_select = c(1, 2, 3, 4, 5, 8, 9),
  col_types = cols(
    CRSETERM = col_character()
)
)

# 2012-2013
Crs5 <- read_delim(
  "../data/HEV_Coursework_SY1213.txt",
  delim = "\t",
  quote = "",
  na = c("NA", "."),
  col_select = c(1, 2, 3, 4, 5, 8, 9),
  col_types = cols(
    CRSETERM = col_character()
)
)

# 2007-2008
Crs6 <- read_delim(
  "../data/HEV_Coursework (2nd thru 06yr2 and 07yr1).txt",
  delim = "\t",
  quote = "",
  na = c("NA", "."),
  col_select = c(1, 2, 3, 4, 5, 8, 9),
  col_types = cols(
    CRSETERM = col_character()
)
)

# Combine all coursework datasets
CrsDat <- rbind(Crs1, Crs2, Crs3, Crs4, Crs5, Crs6)
