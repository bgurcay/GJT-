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
# look at the estimations but not confidence ratings and vice versa.

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

absPercentErr.con.est <- abs.percentage.error(control.data.est[,c(2:17)],true.values)
absPercentErr.con.est$group <- control.data.est$group
absPercentErr.con.est

### IO Condition Data Manipulation ###

# get rid of rows that only have NAs

io.data <- io.data[-c(1,91),]

#pick only the first estimation cols

numbers.IO <- seq(1:length(names(io.data)))
everyfour.1st.est <- numbers.IO[seq(3,length(names(io.data)),by=4)]
io.data.1st.est <- io.data[,c(1,2,everyfour.1st.est)]

#pick only the second estimation cols

everyfour.2nd.est <- numbers.IO[seq(5,length(names(io.data)),by=4)] 
io.data.2nd.est <- io.data[,c(1,2,everyfour.2nd.est)]

# transform the io.data.1st.est and io.data.2nd.est into absolute percentage error

absPercentErr.IO.1st.est <- abs.percentage.error(io.data.1st.est[,c(3:length(names(io.data.1st.est)))],true.values)
absPercentErr.IO.2nd.est <- abs.percentage.error(io.data.2nd.est[,c(3:length(names(io.data.2nd.est)))],true.values)
absPercentErr.IO.1st.est
absPercentErr.IO.2nd.est

# pick only the first confidence rating cols

everyfour.1st.conf <- numbers.IO[seq(4,length(names(io.data)),by=4)]
io.data.1st.conf <- io.data[,c(1,2,everyfour.1st.conf)]

# pick only the second confidence rating cols

everyfour.2nd.conf <- numbers.IO[seq(6,length(names(io.data)),by=4)]
io.data.2nd.conf <- io.data[,c(1,2,everyfour.2nd.conf)]

# do a linear transformation on confidence ratings so that this condition matches the control condition

io.data.1st.conf.tr <- ceiling(io.data.1st.conf/2)
io.data.2nd.conf.tr <- ceiling(io.data.2nd.conf/2)

### GO Condition Data Manipulation ###

# pick only the first estimation cols


# pick only the second estimation cols

go.data.2nd.est <- go.data[,c(1,2,everyfour)]

# transform the go.data.1st.est and go.data.2nd.est into absolute percentage error

# pick only the first confidence rating cols

# pick only the second confidence rating cols

# o a linear transformation on confidence ratings so that this condition matches the control condition
