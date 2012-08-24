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

# Pick 100 subjects randomly from the control condition and restructure the control condition data sothat you only
# look at the estimations but not confidence ratings.

selection <- sort(sample(1:length(control.data$labID), 100, replace=F)) #the rows (Ss) to be selected from the raw control data file 
control.data <- control.data[selection,]
numbers.con <- seq(1:length(names(control.data)))
even <- numbers.con[seq(2,length(names(control.data)),by=2)] 
control.data <- control.data[,c(1,even)] # selects the even cols (Estimations) and the Subj ID columns and saves it as a new data file

#add a group number for each 10 subjects to form groups in the control condition

control.data$group <-rep(NA,nrow(control.data))
control.data$group <- c(rep(1,10), rep(2,10), rep(3,10), rep(4,10), rep(5,10), rep(6,10), rep(7,10), rep(8,10), rep(9,10), rep(10,10)) 
control.data

## calculate absolute percentage error

abs.percentage.error <-function(x,v) {
 abs(((x-v)/v)*100)
}

# transform the control.data files into absolute percentage error

output <- abs.percentage.error(control.data[,c(2:17)],true.values)
output$group <- control.data$group
output

### IO Condition Data Manipulation ###

#pick only the second estimation rows

numbers.IO <- seq(1:length(names(io.data)))
everyfour <- numbers.IO[seq(5,length(names(io.data)),by=4)] 
io.data <- io.data[,c(1,2,everyfour)]

### GO Condition Data Manipulation ###

#pick only the second estimation rows

go.data <- go.data[,c(1,2,everyfour)]
