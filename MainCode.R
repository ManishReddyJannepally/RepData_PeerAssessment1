rm(list = ls())
getwd()
setwd("C:/Users/janne/Desktop/R - Coursera/RR-Project1")
dir()
#loading the data
unzip("Project1_data.zip")
activity_data = read.csv("activity.csv",header = T,na.strings = "NA")
str(activity_data)
#Data processing 
activity_data$date = as.Date(as.character(activity_data$date))


steps_perday <- aggregate(steps ~ date, data = activity_data, sum, na.rm = TRUE)
hist(steps_perday$steps, col = "grey",breaks = 16,ylim = c(0,10)
     , xlab = "Steps", main = "Histogram")
#with(activity_data,tapply(steps,date,mean,na.rm =TRUE))
#with(activity_data,tapply(steps,date,median,na.rm =TRUE))
mean_steps = aggregate(steps ~ date, data = activity_data, FUN = mean)
median_steps = aggregate(steps ~ date, data = activity_data, FUN = median)


#with(activity_data,plot(aggregate(steps~interval,FUN = mean),type = 'l'))
steps_interval = aggregate(steps~interval, data = activity_data, FUN = mean)
with(steps_interval,plot(steps~interval,type = 'l', xlab = "5-minute interval"))
#which.max(steps_interval$steps)
#steps_interval[104,1]
max_steps_interval = steps_interval[which.max(steps_interval$steps),1]
#max_steps_interval
missing_values = sum(is.na(activity_data)) 
activity_data_NA = activity_data[is.na(activity_data),]
activity_data_noNA = activity_data[!is.na(activity_data),]
activity_data_NA$steps = steps_interval$steps
sum(is.na(activity_data_NA))
activity_data_imputed = rbind(activity_data_NA,activity_data_noNA)
order(activity_data_imputed[,2],activity_data_imputed[,3])
activity_data_imputed = activity_data_imputed[order(activity_data_imputed[,2],
                                                    activity_data_imputed[,3]),]
steps_perday_imputed = aggregate(steps~date, data = activity_data_imputed,FUN = sum)
par(mfrow = c(1,2))
with(steps_perday_imputed,hist(steps, col = "grey", breaks = 16, xlab = "steps",
                               main = "imputed histogram"))
mean(steps_perday$steps)
mean(steps_perday_imputed$steps)
median(steps_perday$steps)
median(steps_perday_imputed$steps)

activity_data_imputed$weekday = as.factor(weekdays(activity_data_imputed$date))
levels(activity_data_imputed$weekday) = list(weekday = c("Monday", "Tuesday",
                "Wednesday","Thursday","Friday"), weekend = c("Saturday","Sunday"))
str(activity_data_imputed)
par(mfrow = c(2,1))
with(activity_data_imputed[activity_data_imputed$weekday == "weekday",],
     plot(aggregate(steps~interval, FUN = mean),type = 'l', main = "Weekdays"))
with(activity_data_imputed[activity_data_imputed$weekday == "weekend",],
     plot(aggregate(steps~interval, FUN = mean),type = 'l', main = "Weekends"))


















