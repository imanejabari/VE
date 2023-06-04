library(ggplot2)
library(scales)
library(lubridate)
library(readxl)

covid_cases <- read_xlsx("Cases_BEL.xlsx")

total_cases <- aggregate(CASES ~ DATE, data = covid_cases, sum)
total_cases$DATE = as.Date(total_cases$DATE)

# Plot of the timeline
ggplot(total_cases, aes(x = DATE, y = CASES)) +
  geom_bar(stat = "identity", fill = "#4A96CD") +
  labs(x = "Date", y = "COVID-19 Cases")+ #, title = "COVID-19 Cases Over Time") +
  scale_x_date(labels = date_format("%b %Y"), expand = c(0.01, 0.01), date_breaks = "1 months") +
  scale_y_continuous(n.breaks = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1))+
  # Information found in Sciensano reports
  annotate("segment", x = as.Date("2020-03-01"), xend = as.Date("2020-03-01"), y = 0, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2020-03-07"), y = 78000, label = "1st wave", color = "navyblue", size = 3) +
  annotate("segment", x = as.Date("2020-08-31"), xend = as.Date("2020-08-31"), y = 0, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2020-08-31"), y = 78000, label = "2nd wave", color = "navyblue", size = 3) +
  annotate("segment", x = as.Date("2021-02-15"), xend = as.Date("2021-02-15"), y = 0, yend = 67000, color = "navyblue", linetype = "dashed") +
  annotate("segment", x = as.Date("2021-02-15"), xend = as.Date("2021-02-15"), y = 72000, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2021-02-15"), y = 78000, label = "3rd wave", color = "navyblue", size = 3) +
  annotate("segment", x = as.Date("2021-01-07"), xend = as.Date("2021-01-07"), y = 0, yend = 67000, color = "brown3", linetype = "dashed") +
  annotate("text", x = as.Date("2021-01-07"), y = 71000, label = "Nursing home residents", color = "brown3", size = 3) +
  annotate("text", x = as.Date("2021-01-07"), y = 69500, label = "& Healthcare", color = "brown3", size = 3) +
  annotate("text", x = as.Date("2021-01-07"), y = 68000, label = "workers", color = "brown3", size = 3) +
  annotate("segment", x = as.Date("2021-03-01"), xend = as.Date("2021-03-01"), y = 0, yend = 61000, color = "brown3", linetype = "dashed") +
  annotate("text", x = as.Date("2021-03-01"), y = 62000, label = "65+", color = "brown3", size = 3) +
  annotate("segment", x = as.Date("2021-03-15"), xend = as.Date("2021-03-15"), y = 0, yend = 67000, color = "brown3", linetype = "dashed") +
  annotate("text", x = as.Date("2021-03-15"), y = 69500, label = "65-", color = "brown3", size = 3) +
  annotate("text", x = as.Date("2021-03-15"), y = 68000, label = "with comorbidities", color = "brown3", size = 3) +
  annotate("segment", x = as.Date("2021-06-01"), xend = as.Date("2021-06-01"), y = 0, yend = 61000, color = "brown3", linetype = "dashed") +
  annotate("text", x = as.Date("2021-06-01"), y = 62000, label = "18+", color = "brown3", size = 3) +
  annotate("segment", x = as.Date("2021-06-05"), xend = as.Date("2021-06-05"), y = 0, yend = 61000, color = "brown3", linetype = "dashed") +
  annotate("segment", x = as.Date("2021-06-05"), xend = as.Date("2021-06-05"), y = 63000, yend = 67000, color = "brown3", linetype = "dashed") +
  annotate("text", x = as.Date("2021-06-05"), y = 68000, label = "16 and 17", color = "brown3", size = 3) +
  annotate("segment", x = as.Date("2021-07-07"), xend = as.Date("2021-07-07"), y = 0, yend = 61000, color = "brown3", linetype = "dashed") +
  annotate("text", x = as.Date("2021-07-07"), y = 62000, label = "12-15", color = "brown3", size = 3) +
  annotate("segment", x = as.Date("2021-09-09"), xend = as.Date("2021-09-09"), y = 0, yend = 51000, color = "darkgoldenrod", linetype = "dashed") +
  annotate("text", x = as.Date("2021-09-09"), y = 52000, label = "Immunocompromised", color = "darkgoldenrod", size = 3) +
  annotate("segment", x = as.Date("2021-10-06"), xend = as.Date("2021-10-04"), y = 0, yend = 51000, color = "darkgoldenrod", linetype = "dashed") +
  #annotate("segment", x = as.Date("2021-10-06"), xend = as.Date("2021-10-04"), y = 53000, yend = 57000, color = "darkgoldenrod", linetype = "dashed") +
  annotate("text", x = as.Date("2021-10-06"), y = 59500, label = "Nursing home residents", color = "darkgoldenrod", size = 3) +
  annotate("text", x = as.Date("2021-10-06"), y = 58000, label = "and 65+", color = "darkgoldenrod", size = 3) +
  # annotate("segment", x = as.Date("2021-10-06"), xend = as.Date("2021-10-06"), y = 0, yend = 51000, color = "darkgoldenrod", linetype = "dashed") +
  # annotate("segment", x = as.Date("2021-10-06"), xend = as.Date("2021-10-06"), y = 53000, yend = 57000, color = "darkgoldenrod", linetype = "dashed") +
  # annotate("text", x = as.Date("2021-10-06"), y = 60000, label = "Nursing home residents", color = "darkgoldenrod", size = 3) +
  annotate("segment", x = as.Date("2021-11-10"), xend = as.Date("2021-11-10"), y = 0, yend = 51000, color = "darkgoldenrod", linetype = "dashed") +
  annotate("text", x = as.Date("2021-11-10"), y = 52000, label = "18+", color = "darkgoldenrod", size = 3) +
  annotate("segment", x = as.Date("2021-10-04"), xend = as.Date("2021-10-04"), y = 0, yend = 51000, color = "navyblue", linetype = "dashed") +
  annotate("segment", x = as.Date("2021-10-04"), xend = as.Date("2021-10-04"), y = 53000, yend = 57000, color = "navyblue", linetype = "dashed") +
  annotate("segment", x = as.Date("2021-10-04"), xend = as.Date("2021-10-04"), y = 60000, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2021-10-04"), y = 78000, label = "4th wave", color = "navyblue", size = 3) +
  annotate("segment", x = as.Date("2021-12-27"), xend = as.Date("2021-12-27"), y = 0, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2021-12-27"), y = 78000, label = "5th wave", color = "navyblue", size = 3) +
  annotate("segment", x = as.Date("2022-02-28"), xend = as.Date("2022-02-28"), y = 0, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2022-02-28"), y = 78000, label = "6th wave", color = "navyblue", size = 3) +
  annotate("segment", x = as.Date("2022-05-24"), xend = as.Date("2022-05-24"), y = 0, yend = 51000, color = "darkgreen", linetype = "dashed") +
  annotate("text", x = as.Date("2022-05-24"), y = 55000, label = "Flanders: 85+", color = "darkgreen", size = 3) +
  annotate("text", x = as.Date("2022-05-24"), y = 53500, label = "and nursing home", color = "darkgreen", size = 3) +
  annotate("text", x = as.Date("2022-05-24"), y = 52000, label = "residents", color = "darkgreen", size = 3) +
  annotate("segment", x = as.Date("2022-05-30"), xend = as.Date("2022-05-30"), y = 0, yend = 51000, color = "navyblue", linetype = "dashed") +
  annotate("segment", x = as.Date("2022-05-30"), xend = as.Date("2022-05-30"), y = 56000, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2022-05-30"), y = 78000, label = "7th wave", color = "navyblue", size = 3) +
  annotate("segment", x = as.Date("2022-09-12"), xend = as.Date("2022-09-12"), y = 0, yend = 51000, color = "darkgreen", linetype = "dashed") +
  annotate("text", x = as.Date("2022-09-12"), y = 55000, label = "Progressively: ", color = "darkgreen", size = 3) +
  annotate("text", x = as.Date("2022-09-12"), y = 53500, label = "Immunocompromised, 65+, ", color = "darkgreen", size = 3) +
  annotate("text", x = as.Date("2022-09-12"), y = 52000, label = "healthcare workers and 50-64", color = "darkgreen", size = 3) +
  annotate("segment", x = as.Date("2022-11-21"), xend = as.Date("2022-11-21"), y = 0, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2022-11-21"), y = 78000, label = "9th wave", color = "navyblue", size = 3) +
  annotate("segment", x = as.Date("2023-01-23"), xend = as.Date("2023-01-23"), y = 0, yend = 77000, color = "navyblue", linetype = "dashed") +
  annotate("text", x = as.Date("2023-01-23"), y = 78000, label = "10th wave", color = "navyblue", size = 3)
  # scale_color_manual(values = c("navyblue", "brown3", "darkgoldenrod", "darkgreen"),
  #                    breaks = c("Waves", "Start of Vaccination", "1st Booster Dose", "2nd Booster Dose"),
  #                    labels = c("Waves", "Start of Vaccination", "1st Booster Dose", "2nd Booster Dose"))

# Manually creating the legend because not working :(
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE)
labels <- c("Waves", "Start of Vaccination", "1st Booster Dose", "2nd Booster Dose")
colors <- c("navyblue", "brown3", "darkgoldenrod", "darkgreen")
legend("topright", legend = labels, fill = colors, title = "Events")

