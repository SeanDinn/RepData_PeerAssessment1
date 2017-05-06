#read data into wd and load libraries

proj.data <- read.csv("activity.csv", stringsAsFactors = FALSE)
        library(dplyr)
        library(ggplot2)
#change data type of date column from factor to Date 
        proj.data$date <- as.Date(proj.data$date, format="%m/%d/%Y")
      
        
#Create histogram of #steps per day
        
        with.steps <- filter(proj.data, steps != "NA")
        by.date <- group_by(with.steps, date)
        sum.steps <- summarise(by.date, per_day = sum(steps))
        qplot(per_day, data = sum.steps, bins = 10)
        
#mean and median steps per day
        
        summarize(sum.steps, mean.steps=round(mean(per_day)),
                  median.steps=round(median(per_day)))
        
#time series plot showing average number of steps per interval
        by.interval <- group_by(with.steps, interval)
        steps.per.interval <- summarize(by.interval, per_interval = mean(steps))
        plot(steps.per.interval, type="l", main="Daily Activity Pattern", ylab="Number of Steps")
        
#Which interval has the maximum number of steps
        print ("The interval with the average maximum number of steps is: ")
        steps.per.interval$interval[steps.per.interval$per_interval==max
                                    (steps.per.interval$per_interval)]   
           
      
#Total number of missing values
        missing.value <- is.na(proj.data$steps)
        print("The total number of missing values is:")
        sum(missing.value)
        
#Impute missing values-substitute NA with mean value for that interval
        complete <- function(d1,d2,id = 1:length(d1)){
          for (i in id){
            if (d1[i,1] == "NA"){d1[i,1] <- mean(d2[,2])}
          }
        }
        
        
#Create a new factor to assign date to weekday or weekend
        is.weekend <- c("Saturday", "Sunday")
        with.steps$day <- factor(weekdays(with.steps$date) %in% is.weekend, levels=c(FALSE, TRUE),
                                 labels = c("Weekday", "Weekend"))
        
#Plot mean steps per interval split out between weekdays and weekends
        by.interval.day <- group_by(with.steps, interval, day)
        steps.per.interval.split <- summarize(by.interval.day, per_interval = mean(steps))
        qplot(interval, per_interval, data=steps.per.interval.split, facets = day ~ ., 
              ylab="Number of Steps", geom="line" )
        
        
