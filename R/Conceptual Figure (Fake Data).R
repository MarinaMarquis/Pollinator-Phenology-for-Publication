library(ggplot2)

set.seed(123)

fake_days <- rnorm(220, mean = 180, sd = 40)
fake_days[fake_days < 1] <- 1
fake_days[fake_days > 365] <- 365

df <- data.frame(day = fake_days)

p <- ggplot(df, aes(x = day)) +
  geom_histogram(
    bins = 50,
    color = "black",
    fill = "darkolivegreen3"
  ) +
  coord_cartesian(xlim = c(1, 365), ylim = c(0, 16)) +
  labs(title = "Fake Phenology Histogram",
       x = "Day of Year",
       y = "Frequency") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # remove major gridlines
    panel.grid.minor = element_blank(),   # remove minor gridlines
    panel.border = element_blank(),       # keep it clean (optional)
    axis.line = element_line(color = "black")
  )


ggsave("Figures/example_fake_data_histogram.png", p, width = 6, height = 5, dpi = 300)
