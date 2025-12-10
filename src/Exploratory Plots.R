bigfoot_lda <- read.csv("output/bigfoot_clean_lda.csv")

#classification by locations
ggplot(bigfoot_lda) +
  geom_point(aes(x = latitude, y = longitude, color = classification)) +
  labs(x = "Latitude",
      y = "Longitude",
      color = "Classification",
      title = "Credibility by Location") +
  scale_color_manual(
    values = c(
      "Class A" = brewer.pal(9, "Greens")[8], 
      "Class B" = brewer.pal(9, "Greens")[4]  ))
ggsave("output/Credibility by Location.pdf")

#credibility by cloud cover
ggplot(bigfoot_lda) +
  geom_histogram(aes(x=cloud_cover, fill=classification)) +
  labs(x="Cloud Cover", y="Count", fill="Classification", title = "Credibility by Cloud Cover") +
  scale_fill_manual(
    values = c(
      "Class A" = brewer.pal(9, "Greens")[8], 
      "Class B" = brewer.pal(9, "Greens")[4]  ))
ggsave("output/Credibility by Cloud Cover.pdf")

#classification by pressure
ggplot(bigfoot_lda) +
  geom_histogram(aes(x=pressure, fill=classification)) +
  labs(x="Pressure", y="Count", fill = "Classification", title="Credibility by Pressure") +
  scale_fill_manual(
    values = c(
      "Class A" = brewer.pal(9, "Greens")[8], 
      "Class B" = brewer.pal(9, "Greens")[4]  ))
ggsave("output/Credibility by Pressure.pdf")

#classification by wind speed
ggplot(bigfoot_lda) +
  geom_histogram(aes(x=wind_speed, fill=classification)) +
  labs(x="Wind Speed (mph)", y="Count", fill = "Classification", title = "Credibility by Wind Speed") +
  scale_fill_manual(
    values = c(
      "Class A" = brewer.pal(9, "Greens")[8], 
      "Class B" = brewer.pal(9, "Greens")[4]  ))
ggsave("output/Credibility by Wind Speed.pdf")

#classification by region
ggplot(bigfoot_lda) +
  geom_bar(aes(x = region, fill = classification)) +
  labs(x="Region", y="Count", title="Credibility by Region", fill="Classification") +
  scale_fill_manual(
    values = c(
      "Class A" = brewer.pal(9, "Greens")[8], 
      "Class B" = brewer.pal(9, "Greens")[4]  ))
ggsave("output/Credibility by Region.pdf")
