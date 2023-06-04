library(ggplot2)
library(scales)
library(lubridate)

# BELGIUM ####
# Data
waves <- data.frame(
  Wave = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  From = c("2020-03-01", "2020-08-31", "2021-02-15", "2021-10-04", "2021-12-27", "2022-02-28", "2022-05-30", "2022-09-12", "2022-11-21","2023-01-23"),
  To = c("2020-06-21", "2021-02-14", "2021-06-27", "2021-12-26", "2022-02-27", "2022-05-29", "2022-09-11", "2022-11-20","2023-01-22",NA),
  Color = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", "#800080", "#FFA500", "#008000","purple")
)

# Convert dates to proper format
waves$From <- ymd(waves$From)
waves$To <- ymd(waves$To)
current_date = Sys.Date()

# Timeline plot
ggplot(waves) +
  geom_segment(aes(x = waves$From[2]-2, xend = waves$To[2]+1, y = 0, yend = 1), color = "white", size = 2) +
  geom_segment(aes(x = From, xend = To, y = Wave - Wave, yend = Wave - Wave, color = Color), size = 2) +
  geom_point(aes(x = To, y = Wave - Wave, color = Color), size = 5) +
  geom_text(aes(x = To, y = Wave - Wave, label = Wave), hjust = 0.6, vjust = 0.5, size = 3, fontface = "bold", color = "black") +
  geom_segment(aes(x = waves$From[10]+7, xend = current_date, y = 0, yend = 0, color = Color), size = 2, linetype = "dashed") +
  geom_point(aes(x = current_date, y = 0, color = Color), size = 5) +
  geom_text(aes(x = current_date, y = 0, label = waves$Wave[10]), hjust = 0.6, vjust = 0.5, size = 3, fontface = "bold", color = "black") +
  scale_x_date(labels = date_format("%b %Y"), expand = c(0.01, 0.01), date_breaks = "3 months") +
  scale_y_continuous(breaks = waves$Wave, labels = waves$Wave, expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Wave", title = "Epidemic waves in Belgium") +
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 50, 20, 50),
    legend.position = "none"
  )

# FRANCE ####
# Data
waves <- data.frame(
  Wave = c(1, 2, 3, 4, 5, "5b", 6, 7, 8, 9),
  From = c(as.Date("2020-03-01"), as.Date("2020-07-01"), as.Date("2020-12-01"), as.Date("2021-07-01"), as.Date("2021-10-01"), as.Date("2021-11-01"), as.Date("2022-03-01"), as.Date("2022-07-01"), as.Date("2022-09-01"), as.Date("2022-12-01")),
  To = c(as.Date("2020-06-30"), as.Date("2020-11-30"), as.Date("2021-06-30"), as.Date("2021-09-30"), as.Date("2021-12-31"), as.Date("2022-02-28"), as.Date("2022-04-30"), as.Date("2022-08-31"), as.Date("2022-11-30"), NA),
  Color = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", "#800080", "#FFA500", "#008000", "#000000")
)

# Get current date
current_date <- Sys.Date()

# Timeline plot
ggplot(waves) +
  geom_segment(aes(x = waves$From[2]-2, xend = waves$To[2]+1, y = 0, yend = 1), color = "white", size = 2) +
  geom_segment(aes(x = From, xend = To, y = 0, yend = 0, color = Color), size = 2) +
  geom_point(aes(x = To, y = 0, color = Color), size = 5) +
  geom_text(aes(x = To, y = 0, label = Wave), hjust = 0.6, vjust = 0.5, size = 3, fontface = "bold", color = "black") +
  geom_segment(aes(x = waves$From[10]+11, xend = current_date, y = 0, yend = 0, color = Color), size = 2, linetype = "dashed") +
  geom_point(aes(x = current_date, y = 0, color = Color), size = 5) +
  geom_text(aes(x = current_date, y = 0, label = waves$Wave[10]), hjust = 0.6, vjust = 0.5, size = 3, fontface = "bold", color = "black") +
  scale_x_date(labels = date_format("%b %Y"), expand = c(0.01, 0.01), date_breaks = "3 months") +
  scale_y_discrete(expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Wave", title = "Epidemic waves in France") +
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 50, 20, 50),
    legend.position = "none"
  )

