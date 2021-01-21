library(dplyr)
library(ggplot2)

data <- read.csv(unz("activity.zip", "activity.csv"))

head(data)

clean_data <- data[complete.cases(data), ]

data_per_day <- clean_data %>% group_by(date) %>%
  summarise(mean = mean(steps), sum = sum(steps))

hist(
  as.numeric(data_per_day[['sum']]),
  breaks = 10,
  main = "Total numbers of steps per day",
  xlab = "Number of steps per day"
)

lapply(data_per_day['sum'], mean)
lapply(data_per_day['sum'], median)

data_per_5min <- clean_data %>% group_by(interval) %>%
  summarise(mean = mean(steps), sum = sum(steps))

ggplot(data = data_per_5min, aes(x = interval, y = mean)) + geom_line(color =
                                                                        "#00AFBB", size = 2) + labs(title = "Average daily pattern", x = "5 minute interval", y = "Average number of steps")

data_per_5min$interval[which.max(data_per_5min$mean)]

sum(is.na(data))

merged_data <-
  merge(
    data_per_5min,
    data,
    by = c("interval"),
    all.y = TRUE,
    sort = FALSE
  )
merged_data$steps <-
  ifelse(is.na(merged_data$steps),
         as.integer(merged_data$mean),
         merged_data$steps)
merged_data$mean <- NULL
merged_data$sum <- NULL
merged_data <- merged_data[order(merged_data$date), ]
head(merged_data)

mdata_per_day <- merged_data %>% group_by(date) %>%
  summarise(sum = sum(steps))

hist(
  as.numeric(mdata_per_day[['sum']]),
  breaks = 10,
  main = "Total numbers of steps per day",
  xlab = "Number of steps per day"
)

lapply(mdata_per_day['sum'], mean)
lapply(mdata_per_day['sum'], median)

merged_data$wk <-
  ifelse(weekdays(as.Date(merged_data$date)) %in% c("Saturday", "Sunday"),
         "Weekend",
         "Weekday")

mdw_per_day <- merged_data %>% group_by(interval, wk) %>%
  summarise(mean = mean(steps), sum = sum(steps))

ggplot(mdw_per_day, aes(x = interval, y = mean, color = wk)) + geom_line(color =
                                                                           "#00AFBB", size = 2) + labs(title = "Average daily pattern", x = "5 minute interval", y = "Average number of steps") + facet_grid(wk ~
                                                                                                                                                                                                               .)
