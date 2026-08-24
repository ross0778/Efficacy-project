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
