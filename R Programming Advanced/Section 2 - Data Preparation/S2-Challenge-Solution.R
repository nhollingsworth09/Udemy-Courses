#==== Import Data ====
fin <- read.csv("Future-500.csv", na.strings=c(""))
head(fin, 20)

#==== Convert Non-Factors into Factors ====
fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

#==== Convert Factors into Non-Factors ====

#--Expenses
fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)

fin$Expenses <- as.numeric(fin$Expenses)
#--Growth
fin$Growth <- gsub("%","",fin$Growth)

fin$Growth <- as.numeric(fin$Growth)
#--Revenue
fin$Revenue <- gsub("\\$","", fin$Revenue)
fin$Revenue <- gsub(",","", fin$Revenue)

fin$Revenue <- as.numeric(fin$Revenue)


#Only Non-Missing Data
#fin[complete.cases(fin),]

#Only Missing Data
#fin[!complete.cases(fin),]

#==== Back Up ====
fin_backup <- fin #backup in case of error while cleaning data (do not rerun this line while cleaning)
#fin <- fin_backup #(run this line if we need to get backup)

#==== Remove Rows with Missing Industry ====
fin <- fin[!is.na(fin$Industry),]

#==== Reset Row Indexing ====
rownames(fin) <- NULL

#==== Replacing Missing Data ====

#--States
fin[is.na(fin$State) & fin$City=="New York","State"] <- "NY"
fin[is.na(fin$State) & fin$City=="San Francisco","State"] <- "CA"

#--Employees
med_empl_retail <- median(fin[fin$Industry=="Retail", "Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry=="Retail", "Employees"] <- med_empl_retail

med_empl_financial <- median(fin[fin$Industry=="Financial Services", "Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry=="Financial Services", "Employees"] <- med_empl_financial

#--Growth
med_growth_const <- median(fin[fin$Industry=="Construction","Growth"],na.rm = TRUE)
fin[is.na(fin$Growth) & fin$Industry=="Construction","Growth"] <- med_growth_const

#--Revenue and Expenses
med_rev_constr <- median(fin[fin$Industry=="Construction", "Revenue"] ,na.rm = TRUE)
fin[is.na(fin$Revenue) & fin$Industry=="Construction", "Revenue"] <- med_rev_constr

med_exp_constr <- median(fin[fin$Industry=="Construction", "Expenses"], na.rm = TRUE)
fin[is.na(fin$Expenses) & fin$Industry=="Construction", "Expenses"] <- med_exp_constr

#--Profit and Expenses
fin[is.na(fin$Profit), "Profit"] <- fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]
fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"] 


#==================== Visualizations ========================
#install.packages(ggplot2)
library(ggplot2)
library(scales)

plot <- ggplot(data=fin, aes(x=Revenue, y=Expenses, color=Industry))

# scatter1 - A scatterplot classified by industry showing revenue, expenses, profit

scatter1 <- plot + 
  geom_point(aes(color=Industry, size=Profit)) +
  ggtitle("Expenses vs Revenue (by Industry)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_size_continuous(labels=comma)

scatter1

# scatter2 - A scatterplot that includes industry trends for the expenses~revenue relationship

scatter2 <- plot + 
  geom_point() +
  geom_smooth(fill=NA) +
  ggtitle("Expenses vs Revenue (by Industry)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(ylim=c(0,10000000))
  
scatter2

# box1 - Boxplots showing growth by industry

plot2 <- ggplot(data=fin, aes(x=Industry, y=Growth, 
                              color=Industry))
box1 <- plot2 + 
  ggtitle("Growth Rate by Industry") +
  geom_jitter() +
  geom_boxplot(outlier.color = NA, alpha = 0.4, size = 1)

box1
