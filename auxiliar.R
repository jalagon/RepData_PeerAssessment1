hour <- as.integer(activity$interval/100)
minute <- as.integer(round((activity$interval/100 - hour) * 100)
table(minute)
table(hour)
class(minute)
class(hour)
time <- paste(hour,":",minute, sep = "")
activity$date
strptime(time, format = "%H:%M")

time <- paste(activity$date," ",hour,":",minute, sep = "")
time <- strptime(time, format = "%Y-%m-%d %H:%M")
activity$time <- time
str(activity)

knit2html("PA1_template.rmd")
