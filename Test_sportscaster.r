# Alberto Cereser, 27 November 2016
# Script to plot tracking data relative to football players
# Data from http://home.ifi.uio.no/paalh/dataset/alfheim/
# Reference paper: "Soccer video and player position dataset", by S. A. Pettersen et al.

clc <- function() cat(rep("\n",50))
# Read CSV into R
require(ggplot2)
require(reshape2)
require(lubridate)
require(zoo)
require(plot3D)
require(rgl)

# Select range of interest (in minutes)
min_start = 10
min_length = 40
frames_start = min_start*60*20
n_frames = min_length*60*20

# Load the dataset, with data from one team
MyData <- read.csv(file="2013-11-03_tromso_stromsgodset_raw_first.csv", 
                   header=FALSE, sep=",")
MyData1 <- read.table(file="2013-11-03_tromso_stromsgodset_raw_first.csv", 
                      sep = ",", as.is = TRUE)
colnames(MyData) <- c("Time", "Sensor", "X", "Y", "Dir1", "Dir2", "Energy", "Speed", "Dist_tot")
X <- as.numeric(MyData[frames_start:(frames_start+n_frames),3])
Y <- as.numeric(MyData[frames_start:(frames_start+n_frames), 4])
XY <- data.frame(Y,X)
# Load the time stamp
Time <- MyData1[frames_start:(frames_start+n_frames),1]
# Of the time, we only want to consider h, min and sec
foo <- data.frame(start.time = c(Time),
                  duration   = c(1:length(Time)))
t.str <- strptime(foo$start.time, "%Y-%m-%d %H:%M:%OS")
# Convert time as decimal hours
timestamp <- as.numeric(format(t.str, "%H")) +
  as.numeric(format(t.str, "%M"))/60 +
  as.numeric(format(t.str, "%OS"))/3600 

# Plot the field lines. Field size: 105x68
# Plot a subset of the data with a 5 meters buffer
ggplot(XY) + geom_point(aes(x=Y,y=X), size = 0.05) + xlim(-5, 73) + ylim(-5,110) + 
  geom_segment(aes(x=0, xend=68, y=0, yend=0), colour="blue") +
  geom_segment(aes(x=0, xend=68, y=52.5, yend=52.5), colour="blue") +
  geom_segment(aes(x=0, xend=68, y=105, yend=105), colour="blue") +
  geom_vline(xintercept=0) +
  geom_vline(xintercept=68)

# Plot the (x,y,t) coordinates in a 3D system
plot3d(X, Y, timestamp, col = "red", size = 2)

# Save each time frame, then combine them in a gif
dir.create("animation")
for (i in 1:90) {
  view3d(userMatrix=rotationMatrix(2*pi * i/90, 10, -10, -10))
  rgl.snapshot(filename=paste("animation/frame-",
                              sprintf("%03d", i), ".png", sep=""))
}

