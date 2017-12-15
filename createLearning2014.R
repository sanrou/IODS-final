# Santeri Rouhinen, 2017.12.12. santeri.rouhinen@helsinki.fi
# Load learning dataset and wrangle it. Save as .csv file.
# Link with information about the dataset:
# http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt


# Access libraries
library(dplyr)


# read the data into memory
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

# lrn14 has 183 observations with 60 variables (questions of learning strategy and self evaluations)


### Collapse questions into guestion classes. 

## Collapse most of the variables like in the JYTOPKYS2-meta.txt file: 
# Take a mean of the different questions so that scale is kept at 1-5.
#   *Lar       Aa + Ac + Ad    *Learning as Reproducing
lar <- rowMeans(lrn14[c('Aa', 'Ab', 'Ad')])
learning2014 <- data.frame(lar)

# *Lat       Ab + Ae + Af      *Learning as Transforming
learning2014$lat <- rowMeans(lrn14[c('Ab', 'Ae', 'Af')])

#   *d_sm      D03 + D11 + D19 + D27     *Seeking Meaning, deep
learning2014$d_sm <- rowMeans(lrn14[c('D03', 'D11', 'D19', 'D27')])

# *d_ri      D07 + D14 + D22 + D30       *Relating Ideas, deep
learning2014$d_ri <- rowMeans(lrn14[c('D07', 'D14', 'D22', 'D30')])

# *d_ue      D06 + D15 + D23 + D31       *Use of Evidence, deep
learning2014$d_ue <- rowMeans(lrn14[c('D06', 'D15', 'D23', 'D31')])

# *su_lp     SU02 + SU10 + SU18 + SU26   *Lack of Purpose, surface
learning2014$su_lp <- rowMeans(lrn14[c('SU02', 'SU10', 'SU18', 'SU26')])

# *su_um     SU05 + SU13 + SU21 + SU29   *Unrelated Memorising, surface
learning2014$su_um <- rowMeans(lrn14[c('SU05', 'SU13', 'SU21', 'SU29')])

# *su_sb     SU08 + SU16 + SU24 + SU32   *Syllabus-boundness, surface
learning2014$su_sb <- rowMeans(lrn14[c('SU08', 'SU16', 'SU24', 'SU32')])

# *st_os     ST01 + ST09 + ST17 + ST25   *Organized Studying, strategic
learning2014$st_os <- rowMeans(lrn14[c('ST01', 'ST09', 'ST17', 'ST25')])

# *st_tm     ST04 + ST12 + ST20 + ST28   *Time Management, strategic
learning2014$st_tm <- rowMeans(lrn14[c('ST04', 'ST12', 'ST20', 'ST28')])

#   *Su_Ti     Ca + Cd + Ce + Ch   *Supporting understanding (related to a deep approach)
learning2014$su_ti <- rowMeans(lrn14[c('Ca', 'Cd', 'Ce', 'Ch')])

# *D_Su      Cb + Cc + Cf + Cg     *Transmitting information (related to a surface approach)
learning2014$d_su <- rowMeans(lrn14[c('Cb', 'Cc', 'Cf', 'Cg')])

#   *Obs!! Df and Dh must be reversed first:
lrn14$Df <- 6 - lrn14$Df
lrn14$Dh <- 6 - lrn14$Dh

#   *Stat_Conf           Da + Df  *Confidence in doing statistics
learning2014$stat_conf <- rowMeans(lrn14[c('Dh', 'Df')])

# *Value     Db + Dj              *Value of statistics
learning2014$stat_value <- rowMeans(lrn14[c('Db', 'Dj')])

# *Interest  Dc + De             *Interest in statistics
learning2014$interest <- rowMeans(lrn14[c('Dc', 'De')])

# *Math_Conf           Dd + Di   *Confidence in doing math
learning2014$math_conf <- rowMeans(lrn14[c('Dd', 'Di')])

# *Affect    Dg + Dh             *Affect toward statistics
learning2014$affect <- rowMeans(lrn14[c('Dg', 'Dh')])


## Take original values from lrn14
take_columns <- c("gender","Age","Points")
learning2014 <- cbind(learning2014, lrn14[take_columns])

# Make gender boolean for easier visualizations. Rename to gender_m so one can remember what does 0 and 1 mean.
learning2014$gender <- learning2014$gender == 'M'
names(learning2014)[names(learning2014) == 'gender'] <- 'gender_m'

## Rename column names so that Age and Points are not capitalized.
names(learning2014) <- sapply(names(learning2014), tolower)

## Sort the variables alphabetically
learning2014 <- learning2014[ , order(names(learning2014))]



## Save data. There should be only a few files so save to root.
# The data now has 183 observations (students) and 20 variables.
write.csv(learning2014, file = "learning2014.csv")


## Testing code. Commented out on purpose.
# Load data after clearing working environment
# rm(list = ls())
# learning2014 <- read.csv("learning2014.csv",row.names = 1)

