# Santeri Rouhinen, 2017.12.12. santeri.rouhinen@helsinki.fi
# Load learning dataset and wrangle it.
# Link with information about the dataset:
# http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt


# Access libraries
library(dplyr)


# read the data into memory
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

# lrn14 has 183 observations with 60 variables (questions of learning strategy and self evaluations)


### Collapse questions into guestion classes. 

## Collapse most of the variables like in the JYTOPKYS2-meta.txt file: 
#   *Lar       Aa + Ac + Ad    *Learning as Reproducing
lar <- lrn14$Aa + lrn14$Ab + lrn14$Ad
learning2014 <- data.frame(lar)

# *Lat       Ab + Ae + Af      *Learning as Transforming
learning2014$lat <- lrn14$Ab + lrn14$Ae + lrn14$Af

#   *d_sm      D03 + D11 + D19 + D27     *Seeking Meaning, deep
learning2014$d_sm <- lrn14$D03 + lrn14$D11 + lrn14$D19 + lrn14$D27

# *d_ri      D07 + D14 + D22 + D30       *Relating Ideas, deep
learning2014$d_ri <- lrn14$D07 + lrn14$D14 + lrn14$D22 + lrn14$D30

# *d_ue      D06 + D15 + D23 + D31       *Use of Evidence, deep
learning2014$d_ue <- lrn14$D06 + lrn14$D15 + lrn14$D23 + lrn14$D31

# *su_lp     SU02 + SU10 + SU18 + SU26   *Lack of Purpose, surface
learning2014$su_lp <- lrn14$SU02 + lrn14$SU10 + lrn14$SU18 + lrn14$SU26

# *su_um     SU05 + SU13 + SU21 + SU29   *Unrelated Memorising, surface
learning2014$su_um <- lrn14$SU05 + lrn14$SU13 + lrn14$SU21 + lrn14$SU29

# *su_sb     SU08 + SU16 + SU24 + SU32   *Syllabus-boundness, surface
learning2014$su_sb <- lrn14$SU08 + lrn14$SU16 + lrn14$SU24 + lrn14$SU32

# *st_os     ST01 + ST09 + ST17 + ST25   *Organized Studying, strategic
learning2014$st_os <- lrn14$ST01 + lrn14$ST09 + lrn14$ST17 + lrn14$ST25

# *st_tm     ST04 + ST12 + ST20 + ST28   *Time Management, strategic
learning2014$st_tm <- lrn14$ST04 + lrn14$ST12 + lrn14$ST20 + lrn14$ST28

#   *Su_Ti     Ca + Cd + Ce + Ch   *Supporting understanding (related to a deep approach)
learning2014$su_ti <- lrn14$Ca + lrn14$Cd + lrn14$Ce + lrn14$Ch

# *D_Su      Cb + Cc + Cf + Cg     *Transmitting information (related to a surface approach)
learning2014$d_su <- lrn14$Cb + lrn14$Cc + lrn14$Cf + lrn14$Cg

#   *Obs!! Df and Dh must be reversed first:
lrn14$Df <- 6 - lrn14$Df
lrn14$Dh <- 6 - lrn14$Dh

#   *Stat_Conf           Da + Df  *Confidence in doing statistics
learning2014$stat_conf <- lrn14$Da + lrn14$Df

# *Value     Db + Dj              *Value of statistics
learning2014$value <- lrn14$Db + lrn14$Dj

# *Interest  Dc + De             *Interest in statistics
learning2014$interest <- lrn14$Dc + lrn14$De

# *Math_Conf           Dd + Di   *Confidence in doing math
learning2014$math_conf <- lrn14$Dd + lrn14$Di

# *Affect    Dg + Dh             *Affect toward statistics
learning2014$affect <- lrn14$Dg + lrn14$Dh


## Scale the variables to a mean of 0 and SD of 1.
learning2014 <- scale(learning2014)


## Take original values from lrn14
take_columns <- c("gender","Age","Points")
learning2014 <- cbind(learning2014, lrn14[take_columns])


## Rename column names so that Age and Points are not capitalized. 
names(learning2014) <- sapply(names(learning2014), tolower)


## Save data. There should be only a few files so save to root.
# The data now has 183 observations (students) and 20 variables.
write.csv(learning2014, file = "learning2014.csv")


## Testing code. Commented out on purpose.
# Load data after clearing working environment
# rm(list = ls())
# learning2014 <- read.csv("learning2014.csv",row.names = 1)

