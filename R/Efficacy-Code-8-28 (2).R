# Script Settings and Resources
library(psych)
library(car)
library(plyr)
library(readr)
library(lme4)
library(mice)

# Data Import

# 2006
CB06 <- read_delim(
  "HEV_Student_2006_Y1.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    STUDYID,
    DICODE,
    SCHOOLNAME,
    HEVRMAP,
    GENDER,
    DEGGOAL,
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
  "HEV_Student_2006_Y1_Addl_Vars.txt",
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
  "HEV_Student_2007_Y1.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    STUDYID,
    DICODE,
    SCHOOLNAME,
    HEVRMAP,
    GENDER,
    DEGGOAL,
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
  "HEV_Student_2007_Y1_Addl_Vars.txt",
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
  "HEV_Student_2008_Y1.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    STUDYID,
    DICODE,
    SCHOOLNAME,
    HEVRMAP,
    GENDER,
    DEGGOAL,
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
  "HEV_Student_2009_Y1.txt",
  delim = "\t",
  na = ".",
  col_select = c(
    STUDYID,
    DICODE,
    SCHOOLNAME,
    HEVRMAP,
    GENDER,
    DEGGOAL,
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
  "HEV_Coursework_SY0809 (3rd thru 06yr3 etc) (1).txt",
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
  "HEV_Coursework_SY0910.txt",
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
  "HEV_Coursework_SY1011.txt",
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
  "HEV_Coursework_SY1112.txt",
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
  "HEV_Coursework_SY1213.txt",
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
  "HEV_Coursework (2nd thru 06yr2 and 07yr1).txt",
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

colnames(RIGSCORE)[2] <- "ACI"

# Standardize Rigor Score within schools
RIGSCORE$ACI.Z <- ave(
  RIGSCORE$ACI,
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
names(adv)[names(adv) == "x"] <- "PAC"

# Combine PAC data set and CBmain.

master <- merge(CBmain., adv, by = "HEVRMAP")

# Standardization is performed below, after Degree Goal is recoded.

# Making the SES composite for cohorts 2006-2009, according to the paper and the income choices found in google drive. From SES Code_Jack.R

## 1. Recode income into dollar values ##

master$INCOME_dollars <- NA_real_

# 2006
income_2006 <- c(
  `0` = NA,
  `1` = 10000,
  `2` = 12500,
  `3` = 17500,
  `4` = 22500,
  `5` = 27500,
  `6` = 32500,
  `7` = 37500,
  `8` = 45000,
  `9` = 55000,
  `10` = 65000,
  `11` = 75000,
  `12` = 90000,
  `13` = 100000
)

# 2007
income_2007 <- c(
  `0` = NA,
  `1` = 10000,
  `2` = 15000,
  `3` = 25000,
  `4` = 35000,
  `5` = 45000,
  `6` = 55000,
  `7` = 65000,
  `8` = 75000,
  `9` = 85000,
  `13` = 100000
)

# 2008
income_2008 <- c(
  `0` = NA,
  `1` = 10000,
  `2` = 15000,
  `3` = 25000,
  `4` = 35000,
  `5` = 45000,
  `6` = 55000,
  `7` = 65000,
  `8` = 75000,
  `9` = 90000,
  `10` = 110000,
  `11` = 130000,
  `12` = 150000,
  `13` = 170000,
  `14` = 190000,
  `15` = 200000
)

# 2009
income_2009 <- c(
  `0` = NA,
  `1` = 10000,
  `2` = 15000,
  `3` = 25000,
  `4` = 35000,
  `5` = 45000,
  `6` = 55000,
  `7` = 65000,
  `8` = 75000,
  `9` = 90000,
  `10` = 110000,
  `11` = 130000,
  `12` = 150000,
  `13` = 170000,
  `14` = 190000,
  `15` = 200000
)

# Apply cohort-specific income mappings
master$INCOME_dollars[master$COHORT == 2006] <-
  income_2006[as.character(master$INCOME[master$COHORT == 2006])]

master$INCOME_dollars[master$COHORT == 2007] <-
  income_2007[as.character(master$INCOME[master$COHORT == 2007])]

master$INCOME_dollars[master$COHORT == 2008] <-
  income_2008[as.character(master$INCOME[master$COHORT == 2008])]

master$INCOME_dollars[master$COHORT == 2009] <-
  income_2009[as.character(master$INCOME[master$COHORT == 2009])]


## 2. Take natural log of income ##

master$logINCOME <- log(master$INCOME_dollars)

## 3. Recode parental education into years of education ##

educ_recode <- c(
  `0` = NA,
  `1` = 8,
  `2` = 10,
  `3` = 12,
  `4` = 13,
  `5` = 13,
  `6` = 14,
  `7` = 16,
  `8` = 17,
  `9` = 18
)

master$FATHEDUC_years <-
  educ_recode[as.character(master$FATHEDUC)]

master$MOTHEDUC_years <-
  educ_recode[as.character(master$MOTHEDUC)]


## 4. Standardize the three SES components using ##
##    cohort-specific unrestricted-sample parameters ##

master$SES_income_z <- NA_real_
master$SES_father_z <- NA_real_
master$SES_mother_z <- NA_real_

# 2006
i <- master$COHORT == 2006

master$SES_income_z[i] <-
  (master$logINCOME[i] - 3.948202407) / 0.744463961

master$SES_father_z[i] <-
  (master$FATHEDUC_years[i] - 14.44020406) / 2.646218409

master$SES_mother_z[i] <-
  (master$MOTHEDUC_years[i] - 14.25586432) / 2.485129721


# 2007
i <- master$COHORT == 2007

master$SES_income_z[i] <-
  (master$logINCOME[i] - 10.90208614) / 0.669461096

master$SES_father_z[i] <-
  (master$FATHEDUC_years[i] - 14.42645075) / 2.641907509

master$SES_mother_z[i] <-
  (master$MOTHEDUC_years[i] - 14.26804501) / 2.48281744


# 2008
i <- master$COHORT == 2008

master$SES_income_z[i] <-
  (master$logINCOME[i] - 11.18979) / 0.7494188

master$SES_father_z[i] <-
  (master$FATHEDUC_years[i] - 14.97054) / 2.532645

master$SES_mother_z[i] <-
  (master$MOTHEDUC_years[i] - 14.73808) / 2.409372


# 2009
i <- master$COHORT == 2009

master$SES_income_z[i] <-
  (master$logINCOME[i] - 11.21447476) / 0.778074222

master$SES_father_z[i] <-
  (master$FATHEDUC_years[i] - 15.00757131) / 2.628692289

master$SES_mother_z[i] <-
  (master$MOTHEDUC_years[i] - 14.79162323) / 2.478725999

## 5. Create final SES composite ##

master$SES <- NA_real_

master$SES[master$COHORT == 2006] <-
  (master$SES_income_z[master$COHORT == 2006] +
     master$SES_mother_z[master$COHORT == 2006] +
     master$SES_father_z[master$COHORT == 2006]) / 2.412329

master$SES[master$COHORT == 2007] <-
  (master$SES_income_z[master$COHORT == 2007] +
     master$SES_mother_z[master$COHORT == 2007] +
     master$SES_father_z[master$COHORT == 2007]) / 2.418734

master$SES[master$COHORT == 2008] <-
  (master$SES_income_z[master$COHORT == 2008] +
     master$SES_mother_z[master$COHORT == 2008] +
     master$SES_father_z[master$COHORT == 2008]) / 2.432533

master$SES[master$COHORT == 2009] <-
  (master$SES_income_z[master$COHORT == 2009] +
     master$SES_mother_z[master$COHORT == 2009] +
     master$SES_father_z[master$COHORT == 2009]) / 2.472351

# ============================================================================

# ============================================================================
# MULTIPLE IMPUTATION AND REGRESSIONS
# ============================================================================
# CHANGES IN THIS SECTION:
# 1. Degree Goal (DEGGOAL) is recoded exactly as described in the paper:
#    values 1-5 are retained; 6 ("Other") and 7 ("Undecided") are treated
#    as missing.
# 2. Multiple imputation is performed with mice using 40 imputed datasets,
#    matching the paper.
# 3. PMM (predictive mean matching) is retained, per request. The paper
#    specifies MICE and 40 imputations but does NOT report the exact
#    imputation method, so PMM is an explicit analytic choice here.
# 4. IMPORTANT: imputation is done on the ORIGINAL SCALE. Standardization
#    happens AFTER imputation. This is especially important for Degree Goal,
#    which is an ordered 1-5 variable.
# 5. The standardized variables are standardized within school (DICODE),
#    consistent with the paper.
# 6. The final criterion names are ACI and PAC (not RIG and Adv.p).
# 7. The full regression models control for Degree Goal.
# 8. Regression coefficients and standard errors are pooled across the
#    40 imputations using Rubin's rules.

# ============================================================================
# 1. RECODE DEGREE GOAL BEFORE IMPUTATION
# ============================================================================
# Degree Goal coding from the paper:
#   1 = specialized training/certificate
#   2 = associate degree
#   3 = bachelor's degree
#   4 = master's degree
#   5 = doctoral/related degree
#   6 = other       -> missing
#   7 = undecided   -> missing

master$DEGGOAL <- as.numeric(master$DEGGOAL)
master$DEGGOAL[master$DEGGOAL %in% c(6, 7)] <- NA_real_

# ============================================================================
# 1b. RECODE SELF-EFFICACY (ABILITY) VARIABLES
# ============================================================================
# Original coding: 1 = highest, 2 = above average, 3 = average,
#                   4 = below average, 0 = no response.
# New coding:   1 = below average, 2 = average, 3 = above average,
#                    4 = highest, with 0 (no response) treated as missing.
#
# Reversal for a 1-4 scale is new = 5 - old. 0 is recoded to NA first

ability_vars <- c("MATHABIL", "SCIABIL", "WRITABIL")

for (v in ability_vars) {
  cat(v, "value counts before recode:\n")
  print(table(master[[v]], useNA = "ifany"))
  
  bad <- master[[v]][!is.na(master[[v]]) & !(master[[v]] %in% c(0, 1, 2, 3, 4))]
  if (length(bad) > 0) {
    stop(
      v, " contains values outside 0-4: ",
      paste(unique(bad), collapse = ", "),
      ". Check coding before reverse-scoring."
    )
  }
}

# Recode 0 (no response) to NA, then reverse-score the remaining 1-4 scale.
for (v in ability_vars) {
  master[[v]] <- as.numeric(master[[v]])
  master[[v]][master[[v]] == 0] <- NA_real_
  master[[v]] <- 5 - master[[v]]
}

# Confirm the recode did what you expect: old 1 -> new 4, old 4 -> new 1,
# old 0 -> NA.
for (v in ability_vars) {
  cat(v, "value counts after recode:\n")
  print(table(master[[v]], useNA = "ifany"))
}


# ============================================================================
# 2. CREATE THE ORIGINAL-SCALE DATASET FOR MICE
# ============================================================================
# We intentionally do NOT impute standardized variables.
# Degree Goal is therefore imputed on its original 1-5 scale, and the
# completed values will be standardized within school afterward.

mi_vars <- c(
  "ACI", "PAC",
  "MATHABIL", "SCIABIL", "WRITABIL",
  "HSGPA", "SATVRECN", "SATMRECN", "SATW",
  "SES", "GENDER", "DEGGOAL"
)

mi_data <- master[, mi_vars]

# Ensure numeric variables are numeric.
numeric_vars <- c(
  "ACI", "PAC",
  "MATHABIL", "SCIABIL", "WRITABIL",
  "HSGPA", "SATVRECN", "SATMRECN", "SATW",
  "SES", "DEGGOAL"
)

for (v in numeric_vars) {
  mi_data[[v]] <- as.numeric(mi_data[[v]])
}

# Treat Gender as categorical.
mi_data$GENDER <- as.factor(mi_data$GENDER)

# Check missingness before imputation.
print(md.pattern(mi_data, plot = FALSE))

# ============================================================================
# 3. SPECIFY MICE IMPUTATION METHODS
# ============================================================================
# PMM is retained for the continuous/ordered variables, as requested.
# The paper itself does not specify the exact MICE method.
#
# ACI and PAC are the criterion variables and are not imputed here.
# Gender is not imputed because it was reported as having no missingness
# in the paper's corresponding analysis and is not a missing predictor
# in your current analytic data.

mi_methods <- make.method(mi_data)
mi_methods[] <- ""

mi_methods[c(
  "MATHABIL", "SCIABIL", "WRITABIL",
  "HSGPA", "SATVRECN", "SATMRECN", "SATW",
  "SES", "DEGGOAL"
)] <- "pmm"

mi_methods[c("ACI", "PAC", "GENDER")] <- ""

# If one of the variables above happens to have no missing values, do not
# ask mice to impute it.
for (v in names(mi_methods)) {
  if (mi_methods[v] != "" && !anyNA(mi_data[[v]])) {
    mi_methods[v] <- ""
  }
}

# ============================================================================
# 4. PREDICTOR MATRIX
# ============================================================================
# Include the substantive variables from the regression models in the
# imputation model, following the paper's description.
#
# DICODE is not included because it is a school identifier. The substantive
# variables will be standardized within DICODE after imputation.

pred <- make.predictorMatrix(mi_data)

# No variable predicts itself.
diag(pred) <- 0

# Variables with method = "" are not imputed.
for (v in names(mi_methods)) {
  if (mi_methods[v] == "") {
    pred[v, ] <- 0
  }
}

# ============================================================================
# 5. RUN 40 MULTIPLE IMPUTATIONS
# ============================================================================
set.seed(20260826)

imp <- mice(
  mi_data,
  m = 40,
  method = mi_methods,
  predictorMatrix = pred,
  maxit = 20,
  printFlag = TRUE
)

# Optional convergence diagnostic.
# This can produce a large plot on MSI, so it is commented out by default.
# plot(imp)

# ============================================================================
# 6. STANDARDIZE AFTER IMPUTATION, WITHIN SCHOOL
# ============================================================================
# The school identifier is taken from the original master data. Because the
# rows of mi_data come directly from master, school_id has the same row order.

school_id <- master$DICODE

standardize_within_school <- function(x, school) {
  ave(
    x,
    school,
    FUN = function(z) {
      m <- mean(z, na.rm = TRUE)
      s <- sd(z, na.rm = TRUE)

      if (is.na(s) || s == 0) {
        rep(0, length(z))
      } else {
        (z - m) / s
      }
    }
  )
}

# Create one completed, standardized analysis dataset for each of the
# 40 imputations.
completed <- vector("list", imp$m)
# ============================================================================
# SAVE MAJOR DATA OBJECTS AS RDS
# ============================================================================

saveRDS(master, "master_observed.rds")
saveRDS(imp, "mice_imputation_object.rds")
saveRDS(completed, "completed_imputed_datasets.rds")



for (i in seq_len(imp$m)) {

  dat <- complete(imp, action = i)

  # Criterion variables
  dat$ACI.z <- standardize_within_school(dat$ACI, school_id)
  dat$PAC.z <- standardize_within_school(dat$PAC, school_id)

  # Ability variables
  dat$MATHABIL.z <- standardize_within_school(dat$MATHABIL, school_id)
  dat$SCIABIL.z <- standardize_within_school(dat$SCIABIL, school_id)
  dat$WRITABIL.z <- standardize_within_school(dat$WRITABIL, school_id)

  # Total self-efficacy composite (sum of the three reverse-scored items, standardized within school like everything else)
  dat$TOTALABIL_raw <- dat$MATHABIL + dat$SCIABIL + dat$WRITABIL
  dat$TOTALABIL.z <- standardize_within_school(dat$TOTALABIL_raw, school_id)
  
  # Academic preparation
  dat$HSGPA.z <- standardize_within_school(dat$HSGPA, school_id)
  dat$SATVRECN.z <- standardize_within_school(dat$SATVRECN, school_id)
  dat$SATMRECN.z <- standardize_within_school(dat$SATMRECN, school_id)
  dat$SATW.z <- standardize_within_school(dat$SATW, school_id)

  # Degree Goal
  dat$DEGGOAL.z <- standardize_within_school(dat$DEGGOAL, school_id)

  completed[[i]] <- dat
}

# ============================================================================
# 7. FIT THE REGRESSIONS ACROSS ALL 40 IMPUTATIONS
# ============================================================================
# Models A: ability variables only.
# Models B: full models, including Degree Goal as a control.

model_A_ACI <- lapply(
  completed,
  function(dat) {
    lm(
      ACI.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z,
      data = dat
    )
  }
)

model_A_PAC <- lapply(
  completed,
  function(dat) {
    lm(
      PAC.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z,
      data = dat
    )
  }
)

model_B_ACI <- lapply(
  completed,
  function(dat) {
    lm(
      ACI.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z +
        HSGPA.z + SATVRECN.z + SATMRECN.z + SATW.z +
        SES + GENDER + DEGGOAL.z,
      data = dat
    )
  }
)

model_B_PAC <- lapply(
  completed,
  function(dat) {
    lm(
      PAC.z ~ MATHABIL.z + SCIABIL.z + WRITABIL.z +
        HSGPA.z + SATVRECN.z + SATMRECN.z + SATW.z +
        SES + GENDER + DEGGOAL.z,
      data = dat
    )
  }
)

# ============================================================================
# 8. POOL COEFFICIENTS AND STANDARD ERRORS
# ============================================================================
# Convert the lists of lm objects to the "mira" structure expected by mice's
# pool() function. Rubin's rules are then used to combine the 40 estimates.

as_mira <- function(fit_list) {
  class(fit_list) <- c("mira", "list")
  fit_list
}

pooled_A_ACI <- pool(as_mira(model_A_ACI))
pooled_A_PAC <- pool(as_mira(model_A_PAC))
pooled_B_ACI <- pool(as_mira(model_B_ACI))
pooled_B_PAC <- pool(as_mira(model_B_PAC))

# Pooled coefficient tables
results_A_ACI <- summary(pooled_A_ACI, conf.int = TRUE)
results_A_PAC <- summary(pooled_A_PAC, conf.int = TRUE)
results_B_ACI <- summary(pooled_B_ACI, conf.int = TRUE)
results_B_PAC <- summary(pooled_B_PAC, conf.int = TRUE)

print(results_A_ACI)
print(results_A_PAC)
print(results_B_ACI)
print(results_B_PAC)

# ============================================================================
# 9. R-SQUARED
# ============================================================================
# mice::pool() does not provide a conventional pooled R-squared.
# These are the mean R-squared values across the 40 fitted models and should
# be described as such rather than as Rubin-pooled R-squared values.

r2_results <- data.frame(
  Model = c("A_ACI", "A_PAC", "B_ACI", "B_PAC"),
  Mean_R2 = c(
    mean(sapply(model_A_ACI, function(x) summary(x)$r.squared)),
    mean(sapply(model_A_PAC, function(x) summary(x)$r.squared)),
    mean(sapply(model_B_ACI, function(x) summary(x)$r.squared)),
    mean(sapply(model_B_PAC, function(x) summary(x)$r.squared))
  )
)

print(r2_results)

# ============================================================================
# 10. SAVE RESULTS
# ============================================================================
# Save the pooled results so you can inspect/report them without rerunning
# the regressions.

dir.create("out", showWarnings = FALSE, recursive = TRUE)
# ============================================================================
# RDS BACKUP HELPER
# ============================================================================

save_rds <- function(object, filename) {
  saveRDS(object, file=file.path("out", filename), compress="gzip")
}


write.csv(results_A_ACI, "out/MI_A_ACI.csv", row.names = FALSE)
write.csv(results_A_PAC, "out/MI_A_PAC.csv", row.names = FALSE)
write.csv(results_B_ACI, "out/MI_B_ACI.csv", row.names = FALSE)
write.csv(results_B_PAC, "out/MI_B_PAC.csv", row.names = FALSE)
write.csv(r2_results, "out/MI_R2.csv", row.names = FALSE)

saveRDS(master, "out/master_with_degree_goal.rds")
saveRDS(imp, "out/mice_40_imputations.rds")

saveRDS(
  list(
    A_ACI = results_A_ACI,
    A_PAC = results_A_PAC,
    B_ACI = results_B_ACI,
    B_PAC = results_B_PAC,
    R2 = r2_results
  ),
  "out/pooled_regression_results.rds"
)


# ============================================================================
# 11. ADDITIONAL ANALYSES: TABLE-2-STYLE CORRELATION MATRIX
# ============================================================================
#
# IMPORTANT DISTINCTION FROM THE REGRESSIONS:
#
# Ori's paper describes the correlations in Table 2 as OBSERVED correlations
# calculated across the entire data set, N-weighted by school. The paper's
# multiple imputation procedure was used for the regression analyses, not for
# those reported Table 2 correlations.
#
# Therefore, this correlation matrix:
#   - uses the observed master dataset;
#   - uses pairwise deletion within school for each pair of variables; and
#   - N-weights the resulting school-specific correlations.
#
# Variables added to the Table-2-style matrix:
#   ACI, PAC, HSGPA, SAT Total, SAT-CR, SAT-M, SAT-W,
#   MATHABIL, SCIABIL, WRITABIL
#
# ============================================================================

# Create SAT Total from the observed SAT component scores.
master$SAT_Total <- with(
  master,
  SATVRECN + SATMRECN + SATW
)

# Create TOTALABIL from the observed (already reverse-scored) ability items.
master$TOTALABIL <- with( master, MATHABIL + SCIABIL + WRITABIL )

# Variables included in the correlation matrix.
cor_vars <- c(
  "ACI",
  "PAC",
  "HSGPA",
  "SAT_Total",
  "SATVRECN",
  "SATMRECN",
  "SATW",
  "MATHABIL",
  "SCIABIL",
  "WRITABIL",
  "TOTALABIL"
)

# ---------------------------------------------------------------------------
# School-weighted Pearson correlation with pairwise deletion
# ---------------------------------------------------------------------------
#
# For each pair of variables:
#   1. Retain only observations with both variables observed within a school.
#   2. Calculate the Pearson correlation within each school.
#   3. Weight each school-specific correlation by that school's pairwise N.
#
# This implements the method described in Ori's paper as closely as possible
# from the information reported in the manuscript. The paper does not give
# the authors' exact code, so this should not be interpreted as a claim that
# the code is byte-for-byte identical to theirs.
# ---------------------------------------------------------------------------

school_weighted_cor <- function(data, x, y, school = "DICODE") {

  schools <- unique(data[[school]])

  school_results <- lapply(schools, function(s) {

    d <- data[data[[school]] == s, c(x, y), drop = FALSE]

    # Pairwise deletion for this particular variable pair.
    d <- d[complete.cases(d), , drop = FALSE]

    n <- nrow(d)

    if (n < 3) {
      return(NULL)
    }

    # A correlation cannot be calculated when either variable has zero
    # within-school variance.
    if (sd(d[[x]]) == 0 || sd(d[[y]]) == 0) {
      return(NULL)
    }

    r <- cor(
      d[[x]],
      d[[y]],
      method = "pearson"
    )

    if (is.na(r)) {
      return(NULL)
    }

    data.frame(
      school = s,
      n = n,
      r = r
    )
  })

  school_results <- do.call(rbind, school_results)

  if (is.null(school_results) || nrow(school_results) == 0) {
    return(NA_real_)
  }

  weighted.mean(
    school_results$r,
    w = school_results$n,
    na.rm = TRUE
  )
}

# Create the correlation matrix.
cor_matrix <- matrix(
  NA_real_,
  nrow = length(cor_vars),
  ncol = length(cor_vars),
  dimnames = list(
    c(
      "ACI",
      "PAC",
      "HSGPA",
      "SAT Total",
      "SAT-CR",
      "SAT-M",
      "SAT-W",
      "MATHABIL",
      "SCIABIL",
      "WRITABIL",
      "TOTALABIL"
    ),
    c(
      "ACI",
      "PAC",
      "HSGPA",
      "SAT Total",
      "SAT-CR",
      "SAT-M",
      "SAT-W",
      "MATHABIL",
      "SCIABIL",
      "WRITABIL",
      "TOTALABIL"
    )
  )
)

for (i in seq_along(cor_vars)) {
  for (j in seq_along(cor_vars)) {

    if (i == j) {
      cor_matrix[i, j] <- 1
    } else {
      cor_matrix[i, j] <- school_weighted_cor(
        data = master,
        x = cor_vars[i],
        y = cor_vars[j],
        school = "DICODE"
      )
    }
  }
}

cor_matrix_rounded <- round(cor_matrix, 2)

print(cor_matrix_rounded)

write.csv(
  cor_matrix_rounded,
  "out/Table2_style_correlations_self_efficacy.csv",
  row.names = TRUE
)


# ============================================================================
# 12. ADDITIONAL REGRESSIONS: INCREMENTAL CONTRIBUTION OF SELF-EFFICACY
# ============================================================================
#
# DO NOT CHANGE THE EXISTING MODELS A AND B ABOVE.
#
# The following models are ADDITIONAL models.
#
# The primary incremental comparison is:
#
#   Model C = existing full predictor set WITHOUT self-efficacy
#   Model D = Model C + MATHABIL + SCIABIL + WRITABIL
#
# This directly answers whether the three self-efficacy variables explain
# additional variance in ACI/PAC beyond HSGPA, SAT, SES, Gender, and Degree
# Goal.
#
# Because your existing Model B already contains the self-efficacy variables,
# we cannot use Model B as the baseline for an incremental R-squared test.
# Model C reconstructs the same full model without the three self-efficacy
# variables, and Model D adds them back as a block.
# ============================================================================

# ---------------------------------------------------------------------------
# Model C: Existing full predictor set WITHOUT self-efficacy
# ---------------------------------------------------------------------------

model_C_ACI <- lapply(
  completed,
  function(dat) {
    lm(
      ACI.z ~
        HSGPA.z + SATVRECN.z + SATMRECN.z + SATW.z +
        SES + GENDER + DEGGOAL.z,
      data = dat
    )
  }
)

model_C_PAC <- lapply(
  completed,
  function(dat) {
    lm(
      PAC.z ~
        HSGPA.z + SATVRECN.z + SATMRECN.z + SATW.z +
        SES + GENDER + DEGGOAL.z,
      data = dat
    )
  }
)

# ---------------------------------------------------------------------------
# Model D: Existing full predictor set + self-efficacy
# ---------------------------------------------------------------------------

model_D_ACI <- lapply(
  completed,
  function(dat) {
    lm(
      ACI.z ~
        HSGPA.z + SATVRECN.z + SATMRECN.z + SATW.z +
        SES + GENDER + DEGGOAL.z +
        MATHABIL.z + SCIABIL.z + WRITABIL.z,
      data = dat
    )
  }
)

model_D_PAC <- lapply(
  completed,
  function(dat) {
    lm(
      PAC.z ~
        HSGPA.z + SATVRECN.z + SATMRECN.z + SATW.z +
        SES + GENDER + DEGGOAL.z +
        MATHABIL.z + SCIABIL.z + WRITABIL.z,
      data = dat
    )
  }
)

# Pool coefficients using Rubin's rules.
pooled_C_ACI <- pool(as_mira(model_C_ACI))
pooled_C_PAC <- pool(as_mira(model_C_PAC))
pooled_D_ACI <- pool(as_mira(model_D_ACI))
pooled_D_PAC <- pool(as_mira(model_D_PAC))

results_C_ACI <- summary(pooled_C_ACI, conf.int = TRUE)
results_C_PAC <- summary(pooled_C_PAC, conf.int = TRUE)
results_D_ACI <- summary(pooled_D_ACI, conf.int = TRUE)
results_D_PAC <- summary(pooled_D_PAC, conf.int = TRUE)

print(results_C_ACI)
print(results_C_PAC)
print(results_D_ACI)
print(results_D_PAC)

# ---------------------------------------------------------------------------
# R-squared and incremental R-squared
# ---------------------------------------------------------------------------
#
# As with the existing R2 analysis, these are mean R2 values across the 40
# imputations rather than Rubin-pooled R2 values.
#
# Delta R2 is calculated within the same set of imputations as:
#
#   mean(R2 Model D) - mean(R2 Model C)
# ---------------------------------------------------------------------------

r2_C_ACI <- sapply(
  model_C_ACI,
  function(x) summary(x)$r.squared
)

r2_D_ACI <- sapply(
  model_D_ACI,
  function(x) summary(x)$r.squared
)

r2_C_PAC <- sapply(
  model_C_PAC,
  function(x) summary(x)$r.squared
)

r2_D_PAC <- sapply(
  model_D_PAC,
  function(x) summary(x)$r.squared
)

incremental_R2 <- data.frame(
  Outcome = c("ACI", "PAC"),
  R2_without_self_efficacy = c(
    mean(r2_C_ACI),
    mean(r2_C_PAC)
  ),
  R2_with_self_efficacy = c(
    mean(r2_D_ACI),
    mean(r2_D_PAC)
  ),
  Delta_R2 = c(
    mean(r2_D_ACI) - mean(r2_C_ACI),
    mean(r2_D_PAC) - mean(r2_C_PAC)
  ),
  SD_R2_without_self_efficacy = c(
    sd(r2_C_ACI),
    sd(r2_C_PAC)
  ),
  SD_R2_with_self_efficacy = c(
    sd(r2_D_ACI),
    sd(r2_D_PAC)
  )
)

print(incremental_R2)


# ============================================================================
# 13. SAVE ADDITIONAL ANALYSIS RESULTS
# ============================================================================

dir.create("out", showWarnings = FALSE, recursive = TRUE)

write.csv(
  results_C_ACI,
  "out/Additional_Model_C_ACI.csv",
  row.names = FALSE
)

write.csv(
  results_C_PAC,
  "out/Additional_Model_C_PAC.csv",
  row.names = FALSE
)

write.csv(
  results_D_ACI,
  "out/Additional_Model_D_ACI_self_efficacy.csv",
  row.names = FALSE
)

write.csv(
  results_D_PAC,
  "out/Additional_Model_D_PAC_self_efficacy.csv",
  row.names = FALSE
)

write.csv(
  incremental_R2,
  "out/Incremental_R2_self_efficacy.csv",
  row.names = FALSE
)

saveRDS(
  list(
    Correlations = cor_matrix_rounded,
    Model_C_ACI = results_C_ACI,
    Model_C_PAC = results_C_PAC,
    Model_D_ACI = results_D_ACI,
    Model_D_PAC = results_D_PAC,
    Incremental_R2 = incremental_R2
  ),
  "out/additional_self_efficacy_analysis_results.rds"
)

# ============================================================================
# END OF ANALYSIS
# ============================================================================



# ============================================================================
# FINAL RDS ARCHIVE
# ============================================================================

final_analysis_objects <- list(
  master_observed = master,
  mice_imputation = imp,
  completed_imputations = completed,
  correlation_matrix = cor_matrix_rounded,
  model_A_ACI = model_A_ACI,
  model_A_PAC = model_A_PAC,
  model_B_ACI = model_B_ACI,
  model_B_PAC = model_B_PAC,
  model_C_ACI = model_C_ACI,
  model_C_PAC = model_C_PAC,
  model_D_ACI = model_D_ACI,
  model_D_PAC = model_D_PAC,
  results_C_ACI = results_C_ACI,
  results_D_ACI = results_D_ACI,
  results_C_PAC = results_C_PAC,
  results_D_PAC = results_D_PAC,
  incremental_R2 = incremental_R2
)

save_rds(final_analysis_objects, "all_analysis_objects.rds")
