
#import dataset
data<-read.csv(file="athlete_events.csv", header=TRUE, sep=",")

# Question 1:
# a)Who are the medal winners from India?
# b)Find the sum of all the medals won in different Sports starting from 1960.
# c)Which Sport (among Indian awardees) has won the highest number of medals since 1960?

#Ans 1:
# (a)

medalIndia <- unique(subset(data,select=c("Name","Team"),Team =="India" & Medal!="NA"))
rownames(medalIndia) <- c()
medalIndiaNames <- medalIndia["Name"]
print(medalIndia["Name"]) 

# (b)

medalwinners <- subset(data,select=c("Team","Games","Year","Sport","Event","Medal"),Medal!="NA" & Team=="India" & Year >= 1960)
medal <- unique(medalwinners)
sportsWiseMedal<- setNames(aggregate(Medal~Sport,medal,length),nm=c("Sport","Medal_count"))
rownames(sportsWiseMedal) <- c()
print(sportsWiseMedal)

# (c)

HighestMedal <- sportsWiseMedal[which.max(sportsWiseMedal$Medal_count),]
rownames(HighestMedal) <- c()
print(HighestMedal)


#Question 2:
# a)Find the players who have participated in more than one edition of the Olympics.
# b)Find the players who have also won at multiple appearances.
# c)Find the player with the highest number of medals won in the same edition of the Olympics and display all relevant details.

#Ans 2:

# (a)

install.packages("data.table") 
library(data.table)
names_edition <- subset(data,select = c("Name","Games"))
unique_names <- data.table(unique(names_edition))
frq_count_participants <- unique_names[,list(count_more_than_1Edition=length(Games)),by=Name]
frq_count_name <- data.frame(frq_count_participants[frq_count_participants$count_more_than_1Edition >1])
rownames(frq_count_name) <- c()
name <- frq_count_name["Name"]
print(name)


# (b)

names_medal <-  subset(data,select = c("Name","Games","Medal"),Medal!="NA")
unique_name_medal <-setNames(unique(data.frame(names_medal$Name,names_medal$Games)),nm=c("Name","Games"))
frq_cnt_medalwinners <- setNames(aggregate(Games~Name,unique_name_medal,length),nm=c("Name","no_of_appearance"))
Participant_names <- frq_cnt_medalwinners[frq_cnt_medalwinners$no_of_appearance >1,]
rownames(Participant_names) <- c()
Names_part <- Participant_names["Name"]
print(Names_part)

# (c)

medalwinners_same_edition <- setNames(aggregate(Medal~Name+Games,names_medal,length),nm=c("Name","Edition","Medal"))
Participant_name <- medalwinners_same_edition[which(medalwinners_same_edition$Medal==max(medalwinners_same_edition$Medal)),]
rownames(Participant_name)<-c()
print(Participant_name)


#Question 3:
# a)For every country, for every edition of the Olympics,no. of male and female participants
# b)(mean, median, mode, IQR, SD, 90th percentile) of male and female
# c)Box plot of male and female

#Ans 3:

# (a)
data_items <- unique(subset(data,select = c("Name","Sex","NOC","Games")))
grouped_data <- setNames(aggregate(Name~NOC+Games+Sex,data_items,length),nm=c("Country","Edition","Sex","Count"))
rownames(grouped_data) <- c()
print(grouped_data)

# (b)
male_data <- subset(grouped_data,Sex=="M")
female_data <- subset(grouped_data,Sex=="F")

mode <- function(v) {
  uniq <- unique(v)
  uniq[which.max(tabulate(match(v, uniq)))]
}

print("male data statistics")
print(mean(male_data[["Count"]]))
print(median(male_data[["Count"]]))
print(mode(male_data[["Count"]]))
print(IQR(male_data[["Count"]]))
print(sd(male_data[["Count"]]))
print(quantile(male_data[["Count"]],0.9))

print("female data statistics")
print(mean(female_data[["Count"]]))
print( median(female_data[["Count"]]))
print(mode(female_data[["Count"]]))
print(IQR(female_data[["Count"]]))
print(sd(female_data[["Count"]]))
print(quantile(female_data[["Count"]],0.9))

boxplot(male_data[["Count"]],data=male_data,horizontal=TRUE,ylab="Male",xlab="no. of participants",main="Box plot of Male")
boxplot(female_data[["Count"]],data=female_data,horizontal=TRUE,xlab="no. of participants",ylab="Female",main="Box plot of Female")
boxplot(male_data[["Count"]],female_data[["Count"]],horizontal=TRUE,xlab="no. of participants",names=c("Male","Female"),main="Comparison of two box plots")

#Comparison : 
# The median of male distribution is almost as twice as female distribution.
# Male distribution is widely spread when compared to female distribution.
# Both the male and female distribution are right skewed
# The male distribution has more outliers than female distribution.



#Question 4:
# a)histogram of Medal winners over the years for India (no._of_players Vs Year).
# b)Correlation between age and height.

#Ans 4:

# (a)
medalwinnersYears <- subset(data,select=c("Name","Year","Medal"),Medal!="NA" & Team=="India")
#grouped_years <- setNames(aggregate(Medal~Year,medalwinnersYears,length),nm=c("Year","no_of_players"))

hist(medalwinnersYears$Year,xlab="Years",ylab="no. of players",ylim=c(0,40),xlim=c(1900,2018),breaks=20,right=FALSE,main = "Players Vs Year Histogram")

# Nature : the graph is unimodal and has edge peak distribution which looks like 
# the normal distribution except that it has a large peak at one tail.
# it also almost looks like symmetric distribution. 

# Inference :
# mean : 1956.142
# median : 1956
# mode : 1948
# standard deviation : 22.40 and is unskewed
# min : 1900
# max : 2016
# there are 4 gaps ranging from 1905-1920, 1940-1945, 1975-1980 and 1985-1995 respectively.
# In the graph the upper limit is not included in the range which has gaps, since the parameter
# 'right = FALSE' is put in hist() func, which says upper-limit is not included.


# (b)

correlation_data <- subset(data,select = c("Name","Age","Height","Sport","Medal"),Medal=="Gold" & Sport=="Athletics")
print(cor(correlation_data$Age,correlation_data$Height,use="complete.obs",method="pearson"))
# They are weakly correlated which is 0.007, which approximates to 0. Hence the age of a winner of a gold medal in 
# Olympics in Athletics has a weak correlation with the height of the athlete. 

