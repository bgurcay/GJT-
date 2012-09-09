#### This file does the data analysis for the group vs. individual incentive paper (Lorenz). 
##
##
#############################################################################################

rm(list=ls())

# create a vector of true values for estimation questions

true.values <- c(3718691, 39.52, 2723, 7825200, 24901.4611, 47184, 43, 236, 16.4, 7.08, 2.7, 49.5, 24.9, 25, 41, 63)

### Read the data files into R ###

# read control condition

control.data<-read.csv("/Users/bgurcay86/Documents/Penn/Research/GJT/GroupIncentive/data/controldata.csv", sep = ",")

# read IO condition

io.data<-read.csv("/Users/bgurcay86/Documents/Penn/Research/GJT/GroupIncentive/data/IOdata.csv", sep = ",")

# read GO condition

go.data<-read.csv("/Users/bgurcay86/Documents/Penn/Research/GJT/GroupIncentive/data/GOData.csv", sep = ",")

### Control Condition Data Manipulation ###

# Pick 100 subjects randomly from the control condition and restructure the control condition data so that you only
# look at the estimations but not confidence ratings.

selection <- sort(sample(1:length(control.data$labID), 100, replace=F)) #the rows (Ss) to be selected from the raw control data file 
control.data.100 <- control.data[selection,]
numbers.con <- seq(1:length(names(control.data.100)))
est.con <- numbers.con[seq(2,length(names(control.data.100)),by=2)] #the cols that have the estimations
conf.con <- numbers.con[seq(3,length(names(control.data.100)),by=2)] #the cols that have the confidence ratings
control.data.est <- control.data.100[,c(1,est.con)] # selects the even cols (Estimations) and the Subj ID columns and saves it as a new data file
control.data.conf <- control.data.100[,c(1,conf.con)] # selects the odds cols (Confidence ratings) and the Subj ID columns and saves it as a new data file

#add a group number for each 10 subjects to form groups in the control condition

control.data.100$group <-rep(NA,nrow(control.data.100))
control.data.100$group <- c(rep(1,10), rep(2,10), rep(3,10), rep(4,10), rep(5,10), rep(6,10), rep(7,10), rep(8,10), rep(9,10), rep(10,10)) 
control.data.est$group <- c(rep(1,10), rep(2,10), rep(3,10), rep(4,10), rep(5,10), rep(6,10), rep(7,10), rep(8,10), rep(9,10), rep(10,10)) 
control.data.conf$group <- c(rep(1,10), rep(2,10), rep(3,10), rep(4,10), rep(5,10), rep(6,10), rep(7,10), rep(8,10), rep(9,10), rep(10,10)) 
control.data.100
control.data.est
control.data.conf

## calculate absolute percentage error

abs.percentage.error <-function(x,v) {
 abs(((x-v)/v)*100)
}

# transform the control.data files into absolute percentage error

output.con.est <- abs.percentage.error(control.data.est[,c(2:17)],true.values)
output.con.est$group <- control.data.est$group
output.con.est

### IO Condition Data Manipulation ###

#pick only the second estimation rows

numbers.IO <- seq(1:length(names(io.data)))
everyfour <- numbers.IO[seq(5,length(names(io.data)),by=4)] 
io.data <- io.data[,c(1,2,everyfour)]

### GO Condition Data Manipulation ###

#pick only the second estimation rows

go.data <- go.data[,c(1,2,everyfour)]
