#scrap work
data <- read.csv('activity.csv')


data_by_date <- split(data,data$date)

date_names <- names(data_by_date)

steps_per_day <-numeric(length(date_names))
for(i in 1:length(steps_per_day)){  #length(steps_per_day)
        temp<-data_by_date[[date_names[i]]]$steps
        steps_per_day[i] <- sum(temp)
        
}

hist(steps_per_day)

mean(steps_per_day,na.rm = TRUE)

median(steps_per_day,na.rm = TRUE)


library(ggplot2)

data_total <-data.frame(day = as.Date(date_names),value = steps_per_day)

p2 <- ggplot(data_total, aes(x = day, y = steps_per_day)) + 
        + geom_line() +
        + xlab("")
p2



steps_per_5min <-numeric(length(data_by_date[[date_names[1]]]$steps))
interval <- data_by_date[[date_names[1]]]$interval
cnt <-0
for(i in 1:length(date_names)){  #length(steps_per_day)
        temp<-data_by_date[[date_names[i]]]$steps
        temp[is.na(temp)] <-0
        steps_per_5min <- steps_per_5min + temp
        cnt <- cnt+1
}
avg_per_5min <- steps_per_5min/cnt
interval_avg <-data.frame(interval = interval,value = avg_per_5min)

p3 <- ggplot(interval_avg, aes(x = interval, y = value)) + 
        geom_line() +
        xlab("interval value, seconds")+
        ylab("Average number of steps accross all days")
p3

#5
max_ind <- which.max(steps_per_5min)
print(paste0("interval number ",max_ind))

print(paste0("At this many seconds into the day ",interval[max_ind]))





# 8
date_names_Date <- as.Date(date_names)
library(timeDate)



wd_steps_per_5min <-numeric(length(data_by_date[[date_names[1]]]$steps))
we_steps_per_5min <-numeric(length(data_by_date[[date_names[1]]]$steps))
interval <- data_by_date[[date_names[1]]]$interval
wd_cnt <-0
we_cnt <-0

for(i in 1:length(date_names)){  #length(steps_per_day)
        temp<-data_by_date[[date_names[i]]]$steps
        temp[is.na(temp)] <-0
        
        if(isWeekday(date_names_Date[i], wday=1:5)){
                wd_steps_per_5min <- wd_steps_per_5min + temp
                wd_cnt <- wd_cnt+1
                
        }
        else{
                we_steps_per_5min <- we_steps_per_5min + temp
                we_cnt <- wd_cnt+1
        }
}
wd_avg_per_5min <- wd_steps_per_5min/wd_cnt
we_avg_per_5min <- we_steps_per_5min/we_cnt
compare_avg <-data.frame(interval = interval,wd = wd_avg_per_5min,we = we_avg_per_5min)
we_avg <-data.frame(interval = interval,value = we_avg_per_5min)
wd_avg <-data.frame(interval = interval,value = wd_avg_per_5min)

p_we <- ggplot(we_avg, aes(x = interval, y = value)) + 
        geom_line() +
        xlab("interval value, seconds")+
        ylab("Average number of steps Weekend")

p_wd <- ggplot(wd_avg, aes(x = interval, y = value)) + 
        geom_line() +
        xlab("interval value, seconds")+
        ylab("Average number of steps Weekday")

library(grid)
library(gtable)
g_we_avg <- ggplotGrob(p_we)
g_wd_avg <- ggplotGrob(p_wd)
g <- rbind(g_we_avg,g_wd_avg,size = "first")

grid.newpage()
grid.draw(g)
