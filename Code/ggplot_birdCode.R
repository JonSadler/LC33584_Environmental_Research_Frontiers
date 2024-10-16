Bird <- read.csv("Bird_trends.csv", header=T) #Load in datafile

# Combine data for generalist and specialist into a single dataframe
library(ggplot2)

# Combine data for generalist and specialist into a single dataframe
woodland_bird_trends <- data.frame(
  Year = rep(Bird$Year, 2),
  Abundance = c(Bird$Wood_Gen_Abund_Ind, Bird$Wood_Spec_Abund_Ind),
  CI_2_5 = c(Bird$Wood_Gen_2_5CI, Bird$Wood_Spec_2_5CI),
  CI_97_5 = c(Bird$Wood_Gen_97_5CI, Bird$Wood_Spec_97_5CI),
  Type = rep(c("Generalist", "Specialist"), each = length(Bird$Year))
)

# Create ggplot
ggplot(woodland_bird_trends, aes(x = Year, y = Abundance, color = Type, linetype = Type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CI_2_5, ymax = CI_97_5, fill = Type), alpha = 0.2) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
  labs(
    x = "Year",
    y = "Abundance",
    title = "Time series of abundance of UK woodland birds"
  ) +
  theme(legend.position = "bottom") +
  guides(linetype = guide_legend(override.aes = list(size = 1)))


# Combine data for generalist and specialist into a single dataframe for farmland birds
farmland_bird_trends <- data.frame(
  Year = rep(Bird$Year, 2),
  Abundance = c(Bird$Fm_Gen_Abund_Ind, Bird$Fm_Spec_Abund_Ind),
  CI_2_5 = c(Bird$Fm_Gen_2_5CI, Bird$Fm_Spec_2_5CI),
  CI_97_5 = c(Bird$Fm_Gen_97_5CI, Bird$Fm_Spec_97_5CI),
  Type = rep(c("Generalist", "Specialist"), each = length(Bird$Year))
)

# Create ggplot for farmland birds
ggplot(farmland_bird_trends, aes(x = Year, y = Abundance, color = Type, linetype = Type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CI_2_5, ymax = CI_97_5, fill = Type), alpha = 0.2) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
  labs(
    x = "Year",
    y = "Abundance",
    title = "Time series of abundance of UK farmland birds"
  ) +
  theme(legend.position = "bottom") +
  guides(linetype = guide_legend(override.aes = list(size = 1)))

