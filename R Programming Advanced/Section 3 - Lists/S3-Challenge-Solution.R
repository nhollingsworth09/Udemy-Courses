getwd()
setwd("..\\Section 3 - Lists")

cmine <- read.csv("Machine-Utilization.csv")

#===== Data Preparation =====

# Create Utilization Variable
cmine$Utilization <- {1 - cmine$Percent.Idle}

# Format Time
cmine$PosixTime <- as.POSIXct(cmine$Timestamp, format="%d/%m/%Y %H:%M")
cmine$Timestamp <- NULL

# Reordering Columns
cmine <- cmine[,c(4, 1, 2, 3)]


str(cmine)
summary(cmine)

head(cmine,10)

#===== Machine Names =====
mach_names <- levels(cmine$Machine)

#===== Seperating Dataframe by Machine =====
RL1 <- cmine[cmine$Machine == mach_names[1],]
RL2 <- cmine[cmine$Machine == mach_names[2],]
SR1 <- cmine[cmine$Machine == mach_names[3],]
SR4A <- cmine[cmine$Machine == mach_names[4],]
SR6 <- cmine[cmine$Machine == mach_names[5],]

# Remove NA
RL1_full <- RL1[!is.na(RL1$Percent.Idle),]
RL2_full <- RL2[!is.na(RL2$Percent.Idle),]
SR1_full <- SR1[!is.na(SR1$Percent.Idle),]
SR4A_full <- SR4A[!is.na(SR4A$Percent.Idle),]
SR6_full <- SR6[!is.na(SR6$Percent.Idle),]

#===== Min, Max, Mean Utilization =====

# Machine 1
RL1_stats <- c(min(RL1_full$Utilization), 
               mean(RL1_full$Utilization), 
               max(RL1_full$Utilization))

# Machine 2
RL2_stats <- c(min(RL2_full$Utilization), 
               mean(RL2_full$Utilization), 
               max(RL2_full$Utilization))

# Machine 3
SR1_stats <- c(min(SR1_full$Utilization), 
               mean(SR1_full$Utilization), 
               max(SR1_full$Utilization))

# Machine 4
SR4A_stats <- c(min(SR4A_full$Utilization), 
                mean(SR4A_full$Utilization), 
                max(SR4A_full$Utilization))

# Machine 5
SR6_stats <- c(min(SR6_full$Utilization), 
               mean(SR6_full$Utilization), 
               max(SR6_full$Utilization))

#===== Boolean Utilization (ever under 90%) =====
RL1_log <- as.logical(length(which(RL2_full$Utilization < 0.90)))
RL2_log <- as.logical(length(which(RL2_full$Utilization < 0.90)))
SR1_log <- as.logical(length(which(RL2_full$Utilization < 0.90)))
SR4A_log <- as.logical(length(which(RL2_full$Utilization < 0.90)))
SR6_log <- as.logical(length(which(RL2_full$Utilization < 0.90)))

#===== Hours where usage is NA =====
usage_NA <- cmine[is.na(cmine$Percent.Idle),]

usage_NA_RL1 <- usage_NA[usage_NA$Machine == mach_names[1], "PosixTime"]
usage_NA_RL2 <- usage_NA[usage_NA$Machine == mach_names[2], "PosixTime"]
usage_NA_SR1 <- usage_NA[usage_NA$Machine == mach_names[3], "PosixTime"]
usage_NA_SR4A <- usage_NA[usage_NA$Machine == mach_names[4], "PosixTime"]
usage_NA_SR6 <- usage_NA[usage_NA$Machine == mach_names[5], "PosixTime"]

#============= Time Series Plot of Utilization ===================
library(ggplot2)

p <- ggplot(data=cmine, aes(x=PosixTime, y=Utilization))
          
util_plot <- p + 
  ggtitle("Utilization per Hour") + 
  geom_line(aes(color=Machine),
            size = 1) +
  facet_grid(Machine ~.) +
  geom_hline(yintercept = 0.9, #Show 90% utilization benchmark
             color="Grey",
             size = 1.2,
             linetype = 2)

#================= List of Deliverables ===================
list_RL1 <- list(Machine = mach_names[1], 
                 Stats = RL1_stats, 
                 Low.Utilization = RL1_log, 
                 Unknown.Usage = usage_NA_RL1,
                 Data = RL1,
                 Plot = util_plot)

list_RL2 <- list(Machine = mach_names[2], 
                 Stats = RL2_stats, 
                 Low.Utilization = RL2_log, 
                 Unknown.Usage = usage_NA_RL2,
                 Data = RL2,
                 Plot = util_plot)

list_SR1 <- list(Machine = mach_names[3], 
                 Stats = SR1_stats, 
                 Low.Utilization = SR1_log, 
                 Unknown.Usage = usage_NA_SR1,
                 Data = SR1,
                 Plot = util_plot)

list_SR4A <- list(Machine = mach_names[4], 
                 Stats = SR4A_stats, 
                 Low.Utilization = SR4A_log, 
                 Unknown.Usage = usage_NA_SR4A,
                 Data = SR4A,
                 Plot = util_plot)

list_SR6 <- list(Machine = mach_names[5], 
                 Stats = SR6_stats, 
                 Low.Utilization = SR6_log, 
                 Unknown.Usage = usage_NA_SR6,
                 Data = SR6,
                 Plot = util_plot)
