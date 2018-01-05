getwd()
setwd(".\\Weather Data")

Chicago <- read.csv("Chicago-F.csv")
NewYork <- read.csv("NewYork-F.csv")
Houston <- read.csv("Houston-F.csv")
SanFrancisco <- read.csv("SanFrancisco-F.csv")

#===== Data Preparation =====

# Setting Row Names
Chicago <- read.csv("Chicago-F.csv", row.names = 1)
NewYork <- read.csv("NewYork-F.csv", row.names = 1)
Houston <- read.csv("Houston-F.csv", row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-F.csv", row.names = 1)

# Convert to Matrix (for use with apply functions)
Chicago <- as.matrix(Chicago)
NewYork <- as.matrix(NewYork)
Houston <- as.matrix(Houston)
SanFrancisco <- as.matrix(SanFrancisco)

# Create list of matrices
Weather <- list(Chicago=Chicago,
                NewYork=NewYork, 
                Houston=Houston,
                SanFrancisco=SanFrancisco)

#===== Deliverables =====

# Annual Averages
round(sapply(Weather, rowMeans), 2)

# Monthly Temperature Fluctuations (Using monthly minimum as reference)
sapply(Weather, function(x) round((x[1,] - x[2,])/x[2,], 2) )

# Annual Maximums
sapply(Weather, apply, 1, min)
# Annual Maximums
sapply(Weather, apply, 1, max) 

# Months where min/max appear per metric
sapply(Weather, apply, 1, function(x) names(which.min(x)))
sapply(Weather, apply, 1, function(x) names(which.max(x)))




