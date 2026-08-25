# Script Settings and Resources
library(psych)
library(car)
library(plyr)
library(readr)
library(lme4)

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

# Remove rows in CrsDat that do not have a matching value for HEVRMAP in CBmain. (Ensure all students are included in both predictor and criterion data set).
## CBmain is the dataset containing student predictor data (eg HSGPA, SAT, etc)
CBmain<-subset(CBmain, (CBmain$HEVRMAP %in% CrsDat$HEVRMAP))
CrsDat<-subset(CrsDat, (CrsDat$HEVRMAP %in% CBmain$HEVRMAP))

# Create variable for DICODE (University code); attach to CrsDat
DICODE.temp1<-substr(CrsDat$HEVRMAP, 1, 7)
CrsDat$DICODE<-substr(DICODE.temp1, 4, 7)

# Subsetting out YR 5 and YR 6 courses. Only include YR 1 - YR 4 data
CrsDat<-subset(CrsDat, CrsDat[,3]!=5)
CrsDat<-subset(CrsDat, CrsDat[,3]!=6)

# Excluding Schools and Students that do not have 4 years of course data. Lose 84 schools, 453,253 students (Delaney - I've confirmed these numbers as well)
new.dat = CrsDat[CrsDat$CRSEYR=="4",]
un.stud = unique(new.dat$HEVRMAP)
un.school = unique(new.dat$DICODE)
CrsDat. = CrsDat[CrsDat$DICODE %in% un.school,]
CrsDat. = CrsDat[CrsDat$HEVRMAP %in% un.stud,]

# Subset for main data set on 4 years of data
CBmain. = CBmain[CBmain$HEVRMAP %in% unique(CrsDat.$HEVRMAP),]

# Create a variable CRSNUM that just has the numbers of each course.

CrsDat.$CRSELABEL <- iconv(CrsDat.$CRSELABEL, from = "latin1", to = "UTF-8", sub = "")
CrsDat.$CRSNUM <- as.numeric(gsub("[^0-9]", "", CrsDat.$CRSELABEL))

# Identifying course numbering systems. Quantiles for Course Numbers by each school. 
CrsScales = tapply(CrsDat.$CRSNUM, CrsDat.$DICODE, quantile, prob = c(0, .01, .10, .25, .50, .75, .90, .99, 1), na.rm=TRUE)

# Subsetting out schools w/ unique or odd scales. Some will be manually recoded below
## CrsDat2 schools are excluded. CrsDat3 schools are manually recoded. CrsDat2. schools are automatically recoded.
CrsDat2 = CrsDat.[CrsDat.$DICODE!="2193"& CrsDat.$DICODE!="2070" &CrsDat.$DICODE!="3351",]
CrsDat2. = CrsDat2[CrsDat2$DICODE!="4054"& CrsDat2$DICODE!="2660" &CrsDat2$DICODE!="4835" & CrsDat2$DICODE!="6682"& CrsDat2$DICODE!="2906" &CrsDat2$DICODE!="4065" & CrsDat2$DICODE!="3092"& CrsDat2$DICODE!="4387" &CrsDat2$DICODE!="2418" & CrsDat2$DICODE!="2760"& CrsDat2$DICODE!="2369" &CrsDat2$DICODE!="3083" & CrsDat2$DICODE!="3786"& CrsDat2$DICODE!="6820",]
CrsDat3 = CrsDat2[CrsDat2$DICODE=="4054"|CrsDat2$DICODE=="2660"|CrsDat2$DICODE=="4835"|CrsDat2$DICODE=="6682"|CrsDat2$DICODE=="2906"|CrsDat2$DICODE=="4065"|CrsDat2$DICODE=="3092"|CrsDat2$DICODE=="4387"|CrsDat2$DICODE=="2418"|CrsDat2$DICODE=="2760"|CrsDat2$DICODE=="2369"|CrsDat2$DICODE=="3083"|CrsDat2$DICODE=="3786"|CrsDat2$DICODE=="6820",]

# Manually coding schools w/ unique scales
## Recode up to
s1 = CrsDat3[CrsDat3$DICODE=="4054",]
s1[s1$CRSNUM<100,]$CRSNUM = as.numeric(1)
s1[s1$CRSNUM>99 & s1$CRSNUM<200,]$CRSNUM = as.numeric(2)
s1[s1$CRSNUM>199,]$CRSNUM = NA

# 1 - 399 - basic undergrad, 400-499 = advanced undergrad, 500+ = grad
s2 = CrsDat3[CrsDat3$DICODE=="2660",]
s2[s2$CRSNUM<400,]$CRSNUM = as.numeric(1)
s2[s2$CRSNUM>399 & s2$CRSNUM<500,]$CRSNUM = as.numeric(2)
s2[s2$CRSNUM>499,]$CRSNUM = as.numeric(3)

s3 = CrsDat3[CrsDat3$DICODE=="4835",]
s3[s3$CRSNUM<100,]$CRSNUM = as.numeric(1)
s3[s3$CRSNUM>99 & s3$CRSNUM<200,]$CRSNUM = as.numeric(2)
s3[s3$CRSNUM>199,]$CRSNUM = as.numeric(3)

s4 = CrsDat3[CrsDat3$DICODE=="6682",]
s4$CRSNUM = as.numeric(substr(s4$CRSNUM, 1, 3))
s4[s4$CRSNUM<200,]$CRSNUM = as.numeric(1)
s4[s4$CRSNUM>199 & s4$CRSNUM<300,]$CRSNUM = as.numeric(2)
s4[s4$CRSNUM>299 & s4$CRSNUM<400,]$CRSNUM = as.numeric(3)
s4[s4$CRSNUM>399 & s4$CRSNUM<500,]$CRSNUM = as.numeric(4)
s4[s4$CRSNUM>499 & s4$CRSNUM<600,]$CRSNUM = as.numeric(5)
s4[s4$CRSNUM>599,]$CRSNUM = NA

#### S5 was 2906, DON'T Include b/c 90% in same category of 2 ####

s6 = CrsDat3[CrsDat3$DICODE=="4065",]
s6[s6$CRSNUM<100,]$CRSNUM = as.numeric(1)
s6[s6$CRSNUM>99 & s6$CRSNUM<200,]$CRSNUM = as.numeric(2)
s6[s6$CRSNUM>199,]$CRSNUM = as.numeric(3)

s7 = CrsDat3[CrsDat3$DICODE=="3092",]
s7[s7$CRSNUM<100,]$CRSNUM = as.numeric(1)
s7[s7$CRSNUM>99 & s7$CRSNUM<200,]$CRSNUM = as.numeric(2)
s7[s7$CRSNUM>199,]$CRSNUM = as.numeric(3)

s8 = CrsDat3[CrsDat3$DICODE=="4387",]
s8[s8$CRSNUM<10000,]$CRSNUM = as.numeric(10000)
s8$CRSNUM = as.numeric(substr(s8$CRSNUM, 1, 3))
s8[s8$CRSNUM<200,]$CRSNUM = as.numeric(1)
s8[s8$CRSNUM>199 & s8$CRSNUM<300,]$CRSNUM = as.numeric(2)
s8[s8$CRSNUM>299 & s8$CRSNUM<400,]$CRSNUM = as.numeric(3)
s8[s8$CRSNUM>399,]$CRSNUM = as.numeric(4)

s9 = CrsDat3[CrsDat3$DICODE=="2418",]
s9$CRSNUM = as.numeric(substr(s9$CRSNUM, 1, 3))
s9 = s9[!is.na(s9$CRSNUM),]
s9[s9$CRSNUM<200,]$CRSNUM = as.numeric(1)
s9[s9$CRSNUM>199 & s9$CRSNUM<300,]$CRSNUM = as.numeric(2)
s9[s9$CRSNUM>299 & s9$CRSNUM<400,]$CRSNUM = as.numeric(3)
s9[s9$CRSNUM>399,]$CRSNUM = as.numeric(4)

s10 = CrsDat3[CrsDat3$DICODE=="2760",]
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
s10$CRSNUM = as.numeric(substrRight(s10$CRSNUM,3))
s10[s10$CRSNUM<100,]$CRSNUM = as.numeric(1)
s10[s10$CRSNUM>99 & s10$CRSNUM<200,]$CRSNUM = as.numeric(2)
s10[s10$CRSNUM>199 & s10$CRSNUM<300,]$CRSNUM = as.numeric(3)
s10[s10$CRSNUM>299 & s10$CRSNUM<400,]$CRSNUM = as.numeric(4)
s10[s10$CRSNUM>399 & s10$CRSNUM<500,]$CRSNUM = as.numeric(5)
s10[s10$CRSNUM>499 & s10$CRSNUM<600,]$CRSNUM = as.numeric(6)
s10[s10$CRSNUM>599,]$CRSNUM = as.numeric(7)

s11 = CrsDat3[CrsDat3$DICODE=="2369",]
s11[s11$CRSNUM<100,]$CRSNUM = as.numeric(1)
s11[s11$CRSNUM>99 & s11$CRSNUM<200,]$CRSNUM = as.numeric(2)
s11[s11$CRSNUM>199 & s11$CRSNUM<300,]$CRSNUM = as.numeric(3)
s11[s11$CRSNUM>299 & s11$CRSNUM<1000,]$CRSNUM = as.numeric(4)
s11[s11$CRSNUM>999,]$CRSNUM = NA

s12 = CrsDat3[CrsDat3$DICODE=="3083",]
s12[s12$CRSNUM<200,]$CRSNUM = as.numeric(1)
s12[s12$CRSNUM>199 & s12$CRSNUM<300,]$CRSNUM = as.numeric(2)
s12[s12$CRSNUM>299 & s12$CRSNUM<400,]$CRSNUM = as.numeric(3)
s12[s12$CRSNUM>399 & s12$CRSNUM<500,]$CRSNUM = as.numeric(4)
s12[s12$CRSNUM>499 & s12$CRSNUM<600,]$CRSNUM = as.numeric(5)
s12[s12$CRSNUM>599,]$CRSNUM = as.numeric(6)

s13 = CrsDat3[CrsDat3$DICODE=="3786",]
s13$CRSNUM = as.numeric(substr(s13$CRSNUM, 1, 3))
s13[s13$CRSNUM<200,]$CRSNUM = as.numeric(1)
s13[s13$CRSNUM>199 & s13$CRSNUM<300,]$CRSNUM = as.numeric(2)
s13[s13$CRSNUM>299 & s13$CRSNUM<400,]$CRSNUM = as.numeric(3)
s13[s13$CRSNUM>399 & s13$CRSNUM<500,]$CRSNUM = as.numeric(4)
s13[s13$CRSNUM>499,]$CRSNUM = as.numeric(5)

s14 = CrsDat3[CrsDat3$DICODE=="6820",]
s14[s14$CRSNUM<20000,]$CRSNUM = as.numeric(1)
s14[s14$CRSNUM>19999 & s14$CRSNUM<30000,]$CRSNUM = as.numeric(2)
s14[s14$CRSNUM>29999 & s14$CRSNUM<40000,]$CRSNUM = as.numeric(3)
s14[s14$CRSNUM>39999 & s14$CRSNUM<50000,]$CRSNUM = as.numeric(4)
s14[s14$CRSNUM>49999,]$CRSNUM = as.numeric(5)

# Binding the manually recoded schools together
man.dat = rbind(s1, s2, s3, s4, s6, s7, s8, s9, s10, s11, s12, s13, s14)

# Subsetting schools that have 3-digit, 4-digit, and 5-digit numbering scales.
CRSN.50 = tapply(CrsDat2.$CRSNUM, CrsDat2.$DICODE, quantile, prob = .50, na.rm=TRUE)
x = as.data.frame(CRSN.50)
x$DICODE = rownames(CRSN.50)

x3 = x[nchar(x$CRSN.50)=="3",]
x4 = x[nchar(x$CRSN.50)=="4",]

CrsDat.d3 = CrsDat2.[CrsDat2.$DICODE %in% x3$DICODE,]
CrsDat.d4 = CrsDat2.[CrsDat2.$DICODE %in% x4$DICODE,]

###########################################
#### 3 digit course numbering systems #####
###########################################

CrsDat.d3$CRSNUM[nchar(CrsDat.d3$CRSNUM)=="1"|nchar(CrsDat.d3$CRSNUM)=="2"] <- as.numeric(100)

# Recoding CRSNUM greater than 3 digits as the max 3 digit CRSNUM for that school. 

# Subset of schools needing recoding
recode.CrsDat.d3 = CrsDat.d3[which(nchar(CrsDat.d3$CRSNUM)>"3"),]
recode.CrsDat.d3 = CrsDat.d3[which(CrsDat.d3$DICODE %in% unique(recode.CrsDat.d3$DICODE)),]

# Subset of schools not needing recoding
norecode.CrsDat.d3 = subset(CrsDat.d3, !(CrsDat.d3$DICODE%in% unique(recode.CrsDat.d3$DICODE)))

# Loop to recode
levelFULL = unique(recode.CrsDat.d3$DICODE)

for(i in 1:length(levelFULL)){
  recode.CrsDat.d3[which(recode.CrsDat.d3$DICODE==levelFULL[i] & nchar(recode.CrsDat.d3$CRSNUM)>"3"),]$CRSNUM <- max(recode.CrsDat.d3[which(recode.CrsDat.d3$DICODE==levelFULL[i] & nchar(recode.CrsDat.d3$CRSNUM)=="3"),]$CRSNUM)
  print(i)
}

# Merging data back together
CrsDat.d3. = rbind(recode.CrsDat.d3, norecode.CrsDat.d3)

# Create variable of just first digit
CrsDat.d3.$RigPtsUnwtd = as.numeric(substr(CrsDat.d3.$CRSNUM, 1, 1))

### cut-off rule used- less than 1% in category gets rounded off. If number skips (ie 3% at level 7, 0% at level 8, 3% at level 9, highest level gets recoded as next highest with > 1%).

##1 - 2 scales ##
pt2 = as.data.frame(c("1871", "2520", "6081"))
colnames(pt2) = "DICODE"
#### 1 - 3 schools #####
pt3 = as.data.frame(c("3662","1302", "1440", "1463", "1843", "2073", "2351", "2418", "2532", "2654", "2656", "2659", "2925", "2928", "3096", "3280", "3530", "3724", "4007","4845","5183", "5568", "5814","5887"))
colnames(pt3) = "DICODE"
##### 1 - 4 schools ######
pt4 = c("1050", "1105", "1109", "1194", "1195", "1314", "1318", "1324", "1325", "1335", "1337", "1338", "1339", "1370", "1446", "1631","1836","1839","1842","1905","2013", "2324", "2361", "2367", "2372","2407","2411","2535","2650", "2798", "2814", "2929", "2977", "3436", "3481", "3519","3712","3748","3757","3759","3762","3919","3965","3966","4006", "4067","4088","4301","4330", "4403","4467","4605", "4695", "4705", "4846","4847","4850","4867","4947","4952", "5111", "5187", "5410", "5628", "5811", "5837", "5904", "6003", "6016", "6119", "6188", "2931", "5113", "5218","4951")
pt4 = as.data.frame(pt4)
colnames(pt4) = "DICODE"
##1 - 5 scales ##
pt5 = as.data.frame(c("2823", "3087", "3663", "4630","4852", "5003", "5222", "5398", "5613", "5818", "5913", "6665", "1208", "4854"))
colnames(pt5) = "DICODE"
##1 - 6 scales ####
pt6 = as.data.frame(c("1833", "2812", "6882", "4047"))
colnames(pt6) = "DICODE"
### 1 - 7 scales ###
pt7 = as.data.frame(c("1592"))
colnames(pt7) = "DICODE"
##1 - 8 scales ##
pt8 = as.data.frame(c("2927"))
colnames(pt8) = "DICODE"

# Recoding high values as max for scale
CrsDat.d3. = CrsDat.d3.[!is.na(CrsDat.d3.$RigPtsUnwtd),]

Scales.3.2 = CrsDat.d3.[which(CrsDat.d3.$DICODE %in% pt2$DICODE),]
Scales.3.2[Scales.3.2$RigPtsUnwtd>2,]$RigPtsUnwtd = as.numeric(2)
Scales.3.3 = CrsDat.d3.[which(CrsDat.d3.$DICODE %in% pt3$DICODE),]
Scales.3.3[Scales.3.3$RigPtsUnwtd>3,]$RigPtsUnwtd = as.numeric(3)
Scales.3.4 = CrsDat.d3.[which(CrsDat.d3.$DICODE %in% pt4$DICODE),]
Scales.3.4[Scales.3.4$RigPtsUnwtd>4,]$RigPtsUnwtd = as.numeric(4)
Scales.3.5 = CrsDat.d3.[which(CrsDat.d3.$DICODE %in% pt5$DICODE),]
Scales.3.5[Scales.3.5$RigPtsUnwtd>5,]$RigPtsUnwtd = as.numeric(5)
Scales.3.6 = CrsDat.d3.[which(CrsDat.d3.$DICODE %in% pt6$DICODE),]
Scales.3.6[Scales.3.6$RigPtsUnwtd>6,]$RigPtsUnwtd = as.numeric(6)
Scales.3.7 = CrsDat.d3.[which(CrsDat.d3.$DICODE %in% pt7$DICODE),]
Scales.3.7[Scales.3.7$RigPtsUnwtd>7,]$RigPtsUnwtd = as.numeric(7)
Scales.3.8 = CrsDat.d3.[which(CrsDat.d3.$DICODE %in% pt8$DICODE),]
Scales.3.8[Scales.3.8$RigPtsUnwtd>8,]$RigPtsUnwtd = as.numeric(8)

CrsDat3.Fin = rbind(Scales.3.2, Scales.3.4, Scales.3.5, Scales.3.6, Scales.3.7, Scales.3.8)
tapply(CrsDat3.Fin$RigPtsUnwtd, CrsDat3.Fin$DICODE, quantile, prob = c(.01, .10,.90, 1), na.rm=TRUE)

###########################################
#### 4 digit course numbering systems #####
###########################################

# Recoding cases less than 4 digits
CrsDat.d4$CRSNUM[nchar(CrsDat.d4$CRSNUM)=="1"|nchar(CrsDat.d4$CRSNUM)=="2"|nchar(CrsDat.d4$CRSNUM)=="3"] <- as.numeric(1000)

# Recoding CRSNUM greater than 3 digits as the max 3 digit CRSNUM for that school. 

# Subset of schools needing recoding
recode.CrsDat.d4 = CrsDat.d4[which(nchar(CrsDat.d4$CRSNUM)>"4"),]
recode.CrsDat.d4 = CrsDat.d4[which(CrsDat.d4$DICODE %in% unique(recode.CrsDat.d4$DICODE)),]

# Subset of schools not needing recoding
norecode.CrsDat.d4 = subset(CrsDat.d4, !(CrsDat.d4$DICODE%in% unique(recode.CrsDat.d4$DICODE)))

# Loop to recode
levelFULL = unique(recode.CrsDat.d4$DICODE)

for(i in 1:length(levelFULL)){
  recode.CrsDat.d4[which(recode.CrsDat.d4$DICODE==levelFULL[i] & nchar(recode.CrsDat.d4$CRSNUM)>"4"),]$CRSNUM <- max(recode.CrsDat.d4[which(recode.CrsDat.d4$DICODE==levelFULL[i] & nchar(recode.CrsDat.d4$CRSNUM)=="4"),]$CRSNUM)
  print(i)
}

# Merging data back together
CrsDat.d4. = rbind(recode.CrsDat.d4, norecode.CrsDat.d4)

# Create variable of just first digit
CrsDat.d4.$RigPtsUnwtd = as.numeric(substr(CrsDat.d4.$CRSNUM, 1, 1))

# 2 pt scales
pt2 = c("6825", "6919")
##3 pt scales ##
pt3 = c("1471", "1565", "4842", "6278", "6408", "6827", "6831")
##4 pt scales ##
pt4 = c("0359", "1058", "2259", "4284", "5219", "5248", "5855", "6032", "6481", "6570", "6619", "6647", "6667", "6826", "3075")
##5 pt scales ##
pt5 = c("4853", "5253", "5813","6870")
##6 pt scales ##
pt6 = c("5807")

# Recoding high values as max for scale
Scales.4.2 = CrsDat.d4.[which(CrsDat.d4.$DICODE %in% pt2),]
Scales.4.2[Scales.4.2$RigPtsUnwtd>2,]$RigPtsUnwtd = as.numeric(2)
Scales.4.3 = CrsDat.d4.[which(CrsDat.d4.$DICODE %in% pt3),]
Scales.4.3[Scales.4.3$RigPtsUnwtd>3,]$RigPtsUnwtd = as.numeric(3)
Scales.4.4 = CrsDat.d4.[which(CrsDat.d4.$DICODE %in% pt4),]
Scales.4.4[!is.na(Scales.4.4$RigPtsUnwtd) & Scales.4.4$RigPtsUnwtd>4,]$RigPtsUnwtd = as.numeric(4)
Scales.4.5 = CrsDat.d4.[which(CrsDat.d4.$DICODE %in% pt5),]
Scales.4.5[Scales.4.5$RigPtsUnwtd>5,]$RigPtsUnwtd = as.numeric(5)
Scales.4.6 = CrsDat.d4.[which(CrsDat.d4.$DICODE %in% pt6),]
Scales.4.6[Scales.4.6$RigPtsUnwtd>6,]$RigPtsUnwtd = as.numeric(6)

CrsDat4.Fin = rbind(Scales.4.3, Scales.4.4, Scales.4.5)

###### Recoding New Adds ########
man.dat$RigPtsUnwtd = man.dat$CRSNUM
dat = rbind(man.dat, CrsDat4.Fin, CrsDat3.Fin)

###############################################################
####### RIGOR SCORE ###########################################
###############################################################


### Excluding courses worth more than 10 credits
### (found to be largely internship, independent study)
dat. <- dat[dat$CRSECREDIT < 10, ]

### Weighting course Rigor by Course Credit
dat.$RigPtsWtd <- dat.$RigPtsUnwtd * dat.$CRSECREDIT

# Find the mean of each student's rigor scores across classes
RIGSCORE <- aggregate(
  dat.$RigPtsWtd,
  list(dat.$HEVRMAP),
  mean,
  na.rm = TRUE
)

# Rename columns
names(RIGSCORE)[names(RIGSCORE) == "Group.1"] <- "HEVRMAP"
names(RIGSCORE)[names(RIGSCORE) == "x"] <- "RIGSCORE"

# Add DICODE to RIGSCORE
DICODE.temp <- substr(RIGSCORE$HEVRMAP, 1, 7)
RIGSCORE$DICODE <- substr(DICODE.temp, 4, 7)

colnames(RIGSCORE)[2] <- "RIG"

# Standardize Rigor Score within schools
RIGSCORE$RIG.Z <- ave(
  RIGSCORE$RIG,
  RIGSCORE$DICODE,
  FUN = scale
)

# Make sure both datasets contain the same students
CBmain. <- subset(
  CBmain,
  CBmain$HEVRMAP %in% RIGSCORE$HEVRMAP
)

RIG.dat <- subset(
  RIGSCORE,
  RIGSCORE$HEVRMAP %in% CBmain$HEVRMAP
)

# Add standardized scores to school-specific criterion object
CBmain. <- merge(
  CBmain.,
  RIG.dat,
  by = "HEVRMAP"
)

colnames(CBmain.)[colnames(CBmain.) == "DICODE.x"] <- "DICODE"


###############################################################
####### CODE FOR PROPORTION OF ADVANCED COURSES ###############
###############################################################


dat. <- dat.[!is.na(dat.$DICODE), ]
dat.2 <- dat.[!is.na(dat.$CRSNUM), ]

### .83 was the cut-off score that produced an overall
### advanced course proportion of 33% across colleges

cut <- tapply(
  dat.2$RigPtsUnwtd,
  dat.2$DICODE,
  quantile,
  prob = c(.83),
  na.rm = TRUE
)

cut. <- as.data.frame(
  cbind(
    as.numeric(rownames(cut)),
    as.numeric(cut)
  )
)

colnames(cut.) <- c("DICODE", "Cut")

# Separate schools based on their .83 quantile cutoff
cut.2 <- cut.[cut.$Cut == "2", ]
cut.3 <- cut.[cut.$Cut == "3", ]
cut.4 <- cut.[cut.$Cut == "4", ]
cut.5 <- cut.[cut.$Cut == "5", ]
cut.6 <- cut.[cut.$Cut == "6", ]
cut.7 <- cut.[cut.$Cut == "7", ]
cut.8 <- cut.[cut.$Cut == "8", ]

# Keep courses from schools with each cutoff
c.2 <- dat.2[dat.2$DICODE %in% cut.2$DICODE, ]
c.3 <- dat.2[dat.2$DICODE %in% cut.3$DICODE, ]
c.4 <- dat.2[dat.2$DICODE %in% cut.4$DICODE, ]
c.5 <- dat.2[dat.2$DICODE %in% cut.5$DICODE, ]
c.6 <- dat.2[dat.2$DICODE %in% cut.6$DICODE, ]
c.7 <- dat.2[dat.2$DICODE %in% cut.7$DICODE, ]
c.8 <- dat.2[dat.2$DICODE %in% cut.8$DICODE, ]

# Recode courses as advanced/non-advanced based on school-specific cutoff
c.2[c.2$RigPtsUnwtd < 2, ]$RigPtsUnwtd <- as.numeric(1)
c.2[c.2$RigPtsUnwtd >= 2, ]$RigPtsUnwtd <- as.numeric(2)

c.3[c.3$RigPtsUnwtd < 3, ]$RigPtsUnwtd <- as.numeric(1)
c.3[c.3$RigPtsUnwtd >= 3, ]$RigPtsUnwtd <- as.numeric(2)

c.4[c.4$RigPtsUnwtd < 4, ]$RigPtsUnwtd <- as.numeric(1)
c.4[c.4$RigPtsUnwtd >= 4, ]$RigPtsUnwtd <- as.numeric(2)

c.5[c.5$RigPtsUnwtd < 5, ]$RigPtsUnwtd <- as.numeric(1)
c.5[c.5$RigPtsUnwtd >= 5, ]$RigPtsUnwtd <- as.numeric(2)

c.6[c.6$RigPtsUnwtd < 6, ]$RigPtsUnwtd <- as.numeric(1)
c.6[c.6$RigPtsUnwtd >= 6, ]$RigPtsUnwtd <- as.numeric(2)

c.7[c.7$RigPtsUnwtd < 7, ]$RigPtsUnwtd <- as.numeric(1)
c.7[c.7$RigPtsUnwtd >= 7, ]$RigPtsUnwtd <- as.numeric(2)

c.8[c.8$RigPtsUnwtd < 8, ]$RigPtsUnwtd <- as.numeric(1)
c.8[c.8$RigPtsUnwtd >= 8, ]$RigPtsUnwtd <- as.numeric(2)

# Combine all schools
dat.3 <- rbind(
  c.2, c.3, c.4, c.5,
  c.6, c.7, c.8
)

### Excluding a few schools with odd scales
dat.3 <- dat.3[
  dat.3$DICODE != "2927" &
    dat.3$DICODE != "1592" &
    dat.3$DICODE != "2760",
]

# Recode advanced courses: 0 = non-advanced, 1 = advanced
dat.3$Adv <- as.numeric(
  recode(
    dat.3$RigPtsUnwtd,
    "'1'=0;'2'=1"
  )
)

# Calculate proportion of advanced courses for each student
adv <- aggregate(
  dat.3$Adv,
  list(dat.3$HEVRMAP),
  mean,
  na.rm = TRUE
)

# Rename columns
names(adv)[names(adv) == "Group.1"] <- "HEVRMAP"
names(adv)[names(adv) == "x"] <- "Adv.p"

# Combine PAC data set and CBmain.

CBmain. <- readRDS("out/CBmain_dot_final.rds")
adv <- readRDS("out/adv_final.rds")

master <- merge(CBmain., adv, by = "HEVRMAP")

# Standardize within school
vars_to_std <- c("HSGPA","SATVRECN","SATMRECN", "SATW", "INCOME",
                 "MATHABIL","SCIABIL","WRITABIL","RIG","Adv.p")

for (v in vars_to_std) {
  master[[paste0(v, ".z")]] <- ave(master[[v]], master$DICODE,
                                   FUN = function(x) scale(x)[,1])
}

# Make the SES composite
master$SES_raw <- with(master, FATHEDUC + MOTHEDUC + log(INCOME))
master$SES.z <- ave(master$SES_raw, master$DICODE,
                    FUN = function(x) {
                      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
                    })

# Come back to creating the sample-size-weighted correlation matrix (matching table 2 of bunny hill black diamond paper)

# Regression 1  (variables are all standardized within school like bunny hill black diamond paper)
model_A_ACI <- lm(RIG.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z,
                  data = master)
summary(model_A_ACI)

# Results
# Call:
#   lm(formula = RIG.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z, data = master)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.6027 -0.6171  0.0239  0.6388  7.3060 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.009121   0.001704   5.353 8.64e-08 ***
#   MATHABIL.z  -0.039383   0.002985 -13.193  < 2e-16 ***
#   SCIABIL.z    0.066895   0.003345  20.001  < 2e-16 ***
#   WRITABIL.z  -0.073978   0.002488 -29.728  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9965 on 342062 degrees of freedom
# (80103 observations deleted due to missingness)
# Multiple R-squared:  0.003726,	Adjusted R-squared:  0.003717 
# F-statistic: 426.4 on 3 and 342062 DF,  p-value: < 2.2e-16

# Regression 2

model_A_PAC <- lm(Adv.p.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z,
                  data = master)
summary(model_A_PAC)

# Results
# Call:
#   lm(formula = Adv.p.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z, data = master)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.8766 -0.6492  0.0129  0.6642  6.0330 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.008990   0.001703    5.28 1.29e-07 ***
#   MATHABIL.z  -0.030817   0.002983  -10.33  < 2e-16 ***
#   SCIABIL.z    0.056113   0.003342   16.79  < 2e-16 ***
#   WRITABIL.z  -0.076304   0.002487  -30.68  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9958 on 342062 degrees of freedom
# (80103 observations deleted due to missingness)
# Multiple R-squared:  0.003888,	Adjusted R-squared:  0.003879 
# F-statistic:   445 on 3 and 342062 DF,  p-value: < 2.2e-16

##Having some sort of issue with the SES.x variable that I think it's failing silently or something, meaning I can't run the following regression models, need to figure out##

# Regression 3
model_B_ACI <- lm(RIG.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z +
                    HSGPA.z + SATVRECN.z + SATMRECN.z + SATW.z +
                    SES.z + GENDER,
                  data = master)
summary(model_B_ACI)

# Regression 4

model_B_PAC <- lm(Adv.p.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z +
                    HSGPA.z + SATVRECN.z + SATMRECN.z + SATW.z +
                    SES.z + GENDER,
                  data = master)
summary(model_B_PAC)

