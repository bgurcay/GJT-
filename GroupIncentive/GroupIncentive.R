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

# transform the control.data.est columns into absolute percentage error and save them in a new data frame

q1.abs.perErr.con.est <- abs.percentage.error(control.data.est[,2],true.values[1])
q2.abs.perErr.con.est <- abs.percentage.error(control.data.est[,3],true.values[2])
q3.abs.perErr.con.est <- abs.percentage.error(control.data.est[,4],true.values[3])
q4.abs.perErr.con.est <- abs.percentage.error(control.data.est[,5],true.values[4])
q5.abs.perErr.con.est <- abs.percentage.error(control.data.est[,6],true.values[5])
q6.abs.perErr.con.est <- abs.percentage.error(control.data.est[,7],true.values[6])
q7.abs.perErr.con.est <- abs.percentage.error(control.data.est[,8],true.values[7])
q8.abs.perErr.con.est <- abs.percentage.error(control.data.est[,9],true.values[8])
q9.abs.perErr.con.est <- abs.percentage.error(control.data.est[,10],true.values[9])
q10.abs.perErr.con.est <- abs.percentage.error(control.data.est[,11],true.values[10])
q11.abs.perErr.con.est <- abs.percentage.error(control.data.est[,12],true.values[11])
q12.abs.perErr.con.est <- abs.percentage.error(control.data.est[,13],true.values[12])
q13.abs.perErr.con.est <- abs.percentage.error(control.data.est[,14],true.values[13])
q14.abs.perErr.con.est <- abs.percentage.error(control.data.est[,15],true.values[14])
q15.abs.perErr.con.est <- abs.percentage.error(control.data.est[,16],true.values[15])
q16.abs.perErr.con.est <- abs.percentage.error(control.data.est[,17],true.values[16])

abs.perErr.con.est <- data.frame(labID=control.data.est[,1], experiment_number=control.data.est[,18], q1est=q1.abs.perErr.con.est, q2est=q2.abs.perErr.con.est, q3est=q3.abs.perErr.con.est, q4est=q4.abs.perErr.con.est, q5est=q5.abs.perErr.con.est, q6est=q6.abs.perErr.con.est, q7est=q7.abs.perErr.con.est, q8est=q8.abs.perErr.con.est, q9est=q9.abs.perErr.con.est, q10est=q10.abs.perErr.con.est, q11est=q11.abs.perErr.con.est, q12est=q12.abs.perErr.con.est, q13est=q13.abs.perErr.con.est, q14est=q14.abs.perErr.con.est, q15est=q15.abs.perErr.con.est, q16est=q16.abs.perErr.con.est)

abs.perErr.con.est

### IO Condition Data Manipulation ###

# get rid of rows that only have NAs

io.data <- io.data[-c(1,27,91),]

#pick only the first estimation cols

numbers.IO <- seq(1:length(names(io.data)))
everyfour.IO.1st.est <- numbers.IO[seq(3,length(names(io.data)),by=4)]
io.data.1st.est <- io.data[,c(1,2,everyfour.IO.1st.est)]

#pick only the second estimation cols

everyfour.IO.2nd.est <- numbers.IO[seq(5,length(names(io.data)),by=4)] 
io.data.2nd.est <- io.data[,c(1,2,everyfour.IO.2nd.est)]

## transform the io.data.1st.est and io.data.2nd.est into absolute percentage error and save them in new data frames separately

# transformation for io.data.1st.est

q1.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,3],true.values[1])
q2.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,4],true.values[2])
q3.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,5],true.values[3])
q4.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,6],true.values[4])
q5.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,7],true.values[5])
q6.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,8],true.values[6])
q7.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,9],true.values[7])
q8.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,10],true.values[8])
q9.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,11],true.values[9])
q10.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,12],true.values[10])
q11.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,13],true.values[11])
q12.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,14],true.values[12])
q13.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,15],true.values[13])
q14.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,16],true.values[14])
q15.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,17],true.values[15])
q16.abs.perErr.IO.est1 <- abs.percentage.error(io.data.1st.est[,18],true.values[16])

abs.perErr.IO.est1 <- data.frame(labID=io.data.1st.est[,1], experiment_number=io.data.1st.est[,2], q1est1=q1.abs.perErr.IO.est1, q2est1=q2.abs.perErr.IO.est1, q3est1=q3.abs.perErr.IO.est1, q4est1=q4.abs.perErr.IO.est1, q5est1=q5.abs.perErr.IO.est1, q6est1=q6.abs.perErr.IO.est1, q7est1=q7.abs.perErr.IO.est1, q8est1=q8.abs.perErr.IO.est1, q9est1=q9.abs.perErr.IO.est1, q10est1=q10.abs.perErr.IO.est1, q11est1=q11.abs.perErr.IO.est1, q12est1=q12.abs.perErr.IO.est1, q13est1=q13.abs.perErr.IO.est1, q14est1=q14.abs.perErr.IO.est1, q15est1=q15.abs.perErr.IO.est1, q16est1=q16.abs.perErr.IO.est1)

abs.perErr.IO.est1

# transformation for io.data.2nd.est

q1.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,3],true.values[1])
q2.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,4],true.values[2])
q3.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,5],true.values[3])
q4.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,6],true.values[4])
q5.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,7],true.values[5])
q6.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,8],true.values[6])
q7.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,9],true.values[7])
q8.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,10],true.values[8])
q9.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,11],true.values[9])
q10.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,12],true.values[10])
q11.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,13],true.values[11])
q12.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,14],true.values[12])
q13.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,15],true.values[13])
q14.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,16],true.values[14])
q15.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,17],true.values[15])
q16.abs.perErr.IO.est2 <- abs.percentage.error(io.data.2nd.est[,18],true.values[16])

abs.perErr.IO.est2 <- data.frame(labID=io.data.2nd.est[,1], experiment_number=io.data.2nd.est[,2], q1est2=q1.abs.perErr.IO.est2, q2est2=q2.abs.perErr.IO.est2, q3est2=q3.abs.perErr.IO.est2, q4est2=q4.abs.perErr.IO.est2, q5est2=q5.abs.perErr.IO.est2, q6est2=q6.abs.perErr.IO.est2, q7est2=q7.abs.perErr.IO.est2, q8est2=q8.abs.perErr.IO.est2, q9est2=q9.abs.perErr.IO.est2, q10est2=q10.abs.perErr.IO.est2, q11est2=q11.abs.perErr.IO.est2, q12est2=q12.abs.perErr.IO.est2, q13est2=q13.abs.perErr.IO.est2, q14est2=q14.abs.perErr.IO.est2, q15est2=q15.abs.perErr.IO.est2, q16est2=q16.abs.perErr.IO.est2)

abs.perErr.IO.est2

# pick only the first confidence rating cols

everyfour.IO.1st.conf <- numbers.IO[seq(4,length(names(io.data)),by=4)]
io.data.1st.conf <- io.data[,c(1,2,everyfour.IO.1st.conf)]

# pick only the second confidence rating cols

everyfour.IO.2nd.conf <- numbers.IO[seq(6,length(names(io.data)),by=4)]
io.data.2nd.conf <- io.data[,c(1,2,everyfour.IO.2nd.conf)]

# do a linear transformation on confidence ratings so that this condition matches the control condition

io.data.1st.conf.tr <- ceiling(io.data.1st.conf/2)
io.data.2nd.conf.tr <- ceiling(io.data.2nd.conf/2)

### GO Condition Data Manipulation ###

# pick only the first estimation cols

numbers.GO <- seq(1:length(names(go.data)))
everyfour.GO.1st.est <- numbers.GO[seq(3,length(names(go.data)),by=4)]
go.data.1st.est <- go.data[,c(1,2,everyfour.GO.1st.est)]

# pick only the second estimation cols

everyfour.GO.2nd.est <- numbers.GO[seq(5,length(names(go.data)),by=4)]
go.data.2nd.est <- go.data[,c(1,2,everyfour.GO.2nd.est)]

## transform the go.data.1st.est and go.data.2nd.est into absolute percentage error and save them in new data frames separately

# transformation for go.data.1st.est

q1.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,3],true.values[1])
q2.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,4],true.values[2])
q3.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,5],true.values[3])
q4.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,6],true.values[4])
q5.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,7],true.values[5])
q6.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,8],true.values[6])
q7.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,9],true.values[7])
q8.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,10],true.values[8])
q9.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,11],true.values[9])
q10.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,12],true.values[10])
q11.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,13],true.values[11])
q12.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,14],true.values[12])
q13.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,15],true.values[13])
q14.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,16],true.values[14])
q15.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,17],true.values[15])
q16.abs.perErr.GO.est1 <- abs.percentage.error(go.data.1st.est[,18],true.values[16])

abs.perErr.GO.est1 <- data.frame(labID=go.data.1st.est[,1], experiment_number=go.data.1st.est[,2], q1est1=q1.abs.perErr.GO.est1, q2est1=q2.abs.perErr.GO.est1, q3est1=q3.abs.perErr.GO.est1, q4est1=q4.abs.perErr.GO.est1, q5est1=q5.abs.perErr.GO.est1, q6est1=q6.abs.perErr.GO.est1, q7est1=q7.abs.perErr.GO.est1, q8est1=q8.abs.perErr.GO.est1, q9est1=q9.abs.perErr.GO.est1, q10est1=q10.abs.perErr.GO.est1, q11est1=q11.abs.perErr.GO.est1, q12est1=q12.abs.perErr.GO.est1, q13est1=q13.abs.perErr.GO.est1, q14est1=q14.abs.perErr.GO.est1, q15est1=q15.abs.perErr.GO.est1, q16est1=q16.abs.perErr.GO.est1)

abs.perErr.GO.est1

# transformation for io.data.2nd.est

q1.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,3],true.values[1])
q2.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,4],true.values[2])
q3.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,5],true.values[3])
q4.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,6],true.values[4])
q5.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,7],true.values[5])
q6.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,8],true.values[6])
q7.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,9],true.values[7])
q8.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,10],true.values[8])
q9.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,11],true.values[9])
q10.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,12],true.values[10])
q11.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,13],true.values[11])
q12.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,14],true.values[12])
q13.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,15],true.values[13])
q14.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,16],true.values[14])
q15.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,17],true.values[15])
q16.abs.perErr.GO.est2 <- abs.percentage.error(go.data.2nd.est[,18],true.values[16])

abs.perErr.GO.est2 <- data.frame(labID=go.data.2nd.est[,1], experiment_number=go.data.2nd.est[,2], q1est2=q1.abs.perErr.GO.est2, q2est2=q2.abs.perErr.GO.est2, q3est2=q3.abs.perErr.GO.est2, q4est2=q4.abs.perErr.GO.est2, q5est2=q5.abs.perErr.GO.est2, q6est2=q6.abs.perErr.GO.est2, q7est2=q7.abs.perErr.GO.est2, q8est2=q8.abs.perErr.GO.est2, q9est2=q9.abs.perErr.GO.est2, q10est2=q10.abs.perErr.GO.est2, q11est2=q11.abs.perErr.GO.est2, q12est2=q12.abs.perErr.GO.est2, q13est2=q13.abs.perErr.GO.est2, q14est2=q14.abs.perErr.GO.est2, q15est2=q15.abs.perErr.GO.est2, q16est2=q16.abs.perErr.GO.est2)

abs.perErr.GO.est2

# pick only the first confidence rating cols

everyfour.GO.1st.conf <- numbers.GO[seq(4,length(names(go.data)),by=4)]
go.data.1st.conf <- go.data[,c(1,2,everyfour.GO.1st.conf)]

# pick only the second confidence rating cols

everyfour.GO.2nd.conf <- numbers.GO[seq(6,length(names(go.data)),by=4)]
go.data.2nd.conf <- go.data[,c(1,2,everyfour.GO.2nd.conf)]

# do a linear transformation on confidence ratings so that this condition matches the control condition

go.data.1st.conf.tr <- ceiling(go.data.1st.conf/2) #first confidence rating transformation
go.data.2nd.conf.tr <- ceiling(go.data.2nd.conf/2) #second confidence rating transformation
