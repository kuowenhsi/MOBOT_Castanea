library(tidyverse)
library(ggpubr)
library(lmerTest)
library(car)
library(cowplot)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Castanea")

Castanea_data <- read_csv("./data/Castanea_merged_data_tidy_20241015.csv")%>%
  mutate(Ozark_Chinquapin = case_when(Ozark_Chinquapin == FALSE ~ "Not native range", Ozark_Chinquapin == TRUE ~ "Native range"))

# MaternalLine is actually the site (Accession).
# FlowerHead is actually the individual (real maternal genotype).
Castanea_data_rep <- Castanea_data %>%
  group_by(Accession, Planting, State, County, PosNum)%>%
  summarize()%>%
  group_by(Accession, State, County)%>%
  summarise(rep = n())%>%
  arrange(State, County)

#################
### plot for latitude


unique(Castanea_data$num_traits)
# [1] "basDia" "leafL"  "leafN"  "leafW"  "stLeng" "stNum" 

dir.create("./figures")


p1 <- ggplot(data = filter(Castanea_data, num_traits == "basDia"), aes(x = Google_Latitude, y = num_values))+
  geom_point(aes(color = Ozark_Chinquapin))+
  stat_smooth(method = "lm", aes(color = Ozark_Chinquapin))+
  # stat_regline_equation()+
  stat_cor(aes(color = Ozark_Chinquapin), label.y.npc = 1, position = position_nudge(y = 1), show.legend = FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "Base diameter (mm)")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), legend.position = c(0.8, 0.15), legend.text = element_text(size = 18))+
  facet_wrap(.~Date, scales = "free")
p1

ggsave("./figures/Castanea_BaseDiameter.png", width = 10, height = 8, dpi = 600)


p2 <- ggplot(data = filter(Castanea_data, num_traits == "leafL"), aes(x = Google_Latitude, y = num_values))+
  geom_point(aes(color = Ozark_Chinquapin))+
  stat_smooth(method = "lm", aes(color = Ozark_Chinquapin))+
  # stat_regline_equation()+
  stat_cor(aes(color = Ozark_Chinquapin), label.y.npc = 1, position = position_nudge(y = 2), show.legend = FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "Length of the longest leaf (cm)")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), legend.position = c(0.8, 0.15), legend.text = element_text(size = 18))+
  facet_wrap(.~Date, scales = "free")
p2

ggsave("./figures/Castanea_LeafLength.png", width = 10, height = 8, dpi = 600)

p3 <- ggplot(data = filter(Castanea_data, num_traits == "leafN"), aes(x = Google_Latitude, y = num_values))+
  geom_point(aes(color = Ozark_Chinquapin))+
  stat_smooth(method = "lm", aes(color = Ozark_Chinquapin))+
  # stat_regline_equation()+
  stat_cor(aes(color = Ozark_Chinquapin), label.y.npc = 1, position = position_nudge(y = 3), show.legend = FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "Number of leaves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), legend.position = c(0.8, 0.15), legend.text = element_text(size = 18))+
  facet_wrap(.~Date, scales = "free")
p3

ggsave("./figures/Castanea_NumLeaves.png", width = 10, height = 8, dpi = 600)

p4 <- ggplot(data = filter(Castanea_data, num_traits == "leafW"), aes(x = Google_Latitude, y = num_values))+
  geom_point(aes(color = Ozark_Chinquapin))+
  stat_smooth(method = "lm", aes(color = Ozark_Chinquapin))+
  # stat_regline_equation()+
  stat_cor(aes(color = Ozark_Chinquapin), label.y.npc = 1, position = position_nudge(y = 1), show.legend = FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "Width of the longest leaf")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), legend.position = c(0.8, 0.15), legend.text = element_text(size = 18))+
  facet_wrap(.~Date, scales = "free")
p4

ggsave("./figures/Castanea_LeafWidth.png", width = 10, height = 8, dpi = 600)

p5 <- ggplot(data = filter(Castanea_data, num_traits == "stLeng"), aes(x = Google_Latitude, y = num_values))+
  geom_point(aes(color = Ozark_Chinquapin))+
  stat_smooth(method = "lm", aes(color = Ozark_Chinquapin))+
  # stat_regline_equation()+
  stat_cor(aes(color = Ozark_Chinquapin), label.y.npc = 1, position = position_nudge(y = 3), show.legend = FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "Stem Length (cm)")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), legend.position = c(0.8, 0.15), legend.text = element_text(size = 18))+
  facet_wrap(.~Date, scales = "free")
p5

ggsave("./figures/Castanea_StemLength.png", width = 10, height = 8, dpi = 600)

p6 <- ggplot(data = filter(Castanea_data, num_traits == "stNum"), aes(x = Google_Latitude, y = num_values))+
  geom_point(aes(color = Ozark_Chinquapin))+
  stat_smooth(method = "lm", aes(color = Ozark_Chinquapin))+
  # stat_regline_equation()+
  stat_cor(aes(color = Ozark_Chinquapin), label.y.npc = 1, position = position_nudge(y = 1), show.legend = FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "Number of stems")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), legend.position = c(0.8, 0.15), legend.text = element_text(size = 18))+
  facet_wrap(.~Date, scales = "free")
p6

ggsave("./figures/Castanea_NumStems.png", width = 10, height = 8, dpi = 600)

####################

p2 <- ggplot(data = filter(Castanea_data_DaysToFlower, num_traits == "numDiscF"), aes(x = Google_latitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Castanea_data_DaysToFlower, num_traits == "numDiscF", as.integer(DaysToFlower) == 150), color = "red")+
  geom_abline(data = filter(DaysToFlower_disk_lmm_anova, num_traits == "numDiscF"), aes(intercept = intercept, slope = slope), color = "blue")+
  geom_text(x = 38.6, y = 160, hjust = 0, mapping = aes(label = label_text), data = DaysToFlower_disk_lmm_anova)+
  geom_text(x = 38.6, y = 60, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = filter(DaysToFlower_disk_lmm_anova, num_traits == "numDiscF"), color = "blue")+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "Days to the first flower", limits = c(60, 165))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p2


p3 <- ggplot(data = Castanea_total_flowers_data, aes(x = mean_Google_latitude, y = total_flower))+
  geom_point()+
  geom_point(data = filter(Castanea_total_flowers_data, total_flower == 0), color = "red")+
  # geom_abline(data = Castanea_total_flowers_lmm_anova, aes(intercept = intercept, slope = slope), color = "blue")+
  geom_text(x = 38.6, y = 95, hjust = 0, mapping = aes(label = label_text), data = Castanea_total_flowers_lmm_anova)+
  # geom_text(x = 38.6, y = 10, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = Castanea_total_flowers_lmm_anova, color = "blue")+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Total flowers", limits = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p3


p4 <- ggplot(data = Castanea_leafLong_data, aes(x = Google_latitude, y = num_values))+
  geom_point()+
  # geom_abline(data = Castanea_total_flowers_lmm_anova, aes(intercept = intercept, slope = slope), color = "blue")+
  geom_text(x = 38.6, y = 38, hjust = 0, mapping = aes(label = label_text), data = Castanea_leafLong_lmm_anova)+
  # geom_text(x = 38.6, y = 10, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = Castanea_total_flowers_lmm_anova, color = "blue")+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Leaf length (cm)", limits = c(0, 42))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p4

p <- plot_grid(p1, p2, p3, p4, align = "hv", nrow = 2, rel_heights = c(0.95, 1))
p

ggsave("./figures/Castanea_poster_regression.png", width = 9, height = 7, dpi = 600)




##############


strip_labels = c(numFlwrB = "First flower bud", numRayF = "First open ray flowers", numDiscF = "First open disk flowers")

p <- ggplot(data = Castanea_data_DaysToFlower, aes(x = Google_latitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Castanea_data_DaysToFlower, as.integer(DaysToFlower) == 150), color = "red")+
  # stat_smooth(data = filter(Castanea_data_DaysToFlower, as.integer(DaysToFlower) < 150, num_traits == "numDiscF"), method = "lm", color = "blue")+
  geom_abline(data = filter(DaysToFlower_lmm_anova_H, num_traits == "numDiscF"), aes(intercept = intercept, slope = slope), color = "blue")+
  # stat_cor(data = filter(Castanea_data_DaysToFlower, as.integer(DaysToFlower) < 150),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  # stat_regline_equation(data = filter(Castanea_data_DaysToFlower, as.integer(DaysToFlower) < 150),label.x.npc = 0, label.y.npc = 0.95)+
  geom_text(x = 38.6, y = 160, hjust = 0, mapping = aes(label = label_text), data = DaysToFlower_lmm_anova_H)+
  geom_text(x = 38.6, y = 60, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = filter(DaysToFlower_lmm_anova_H, num_traits == "numDiscF"), color = "blue")+
  scale_x_continuous(name = "Latitude")+
  scale_y_continuous(name = "Days since planting", limits = c(60, 165))+
  facet_wrap(.~num_traits, nrow = 1, labeller = labeller(num_traits = strip_labels))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  

# ggsave("./figures/Castanea_days_to_flower_by_latitude.png", width = 13, height = 5)

### plot for longitude

# mixed effect model

DaysToFlower_bud_lmm <- lmer(as.integer(DaysToFlower) ~ Google_longitude + (1|Google_longitude/FlowerHead), data = filter(Castanea_data_DaysToFlower, num_traits == "numFlwrB", as.integer(DaysToFlower) < 150))
DaysToFlower_bud_lmm
as_tibble(VarCorr(DaysToFlower_bud_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov")
as_tibble_row(fixef(DaysToFlower_bud_lmm))%>%rename(slope = Google_longitude, intercept = `(Intercept)`)

DaysToFlower_bud_lmm_anova <- as_tibble(anova(DaysToFlower_bud_lmm, type = "II"))%>%
  mutate(num_traits = "numFlwrB", label_text = paste0("Days ~ Longitude + (1|Longitude/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_bud_lmm))%>%rename(slope = Google_longitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_bud_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_ray_lmm <- lmer(as.integer(DaysToFlower) ~ Google_longitude + (1|Google_longitude/FlowerHead), data = filter(Castanea_data_DaysToFlower, num_traits == "numRayF", as.integer(DaysToFlower) < 150))
VarCorr(DaysToFlower_ray_lmm)
DaysToFlower_ray_lmm_anova <- as_tibble(anova(DaysToFlower_ray_lmm, type = "II"))%>%
  mutate(num_traits = "numRayF", label_text = paste0("Days ~ Longitude + (1|Longitude/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_ray_lmm))%>%rename(slope = Google_longitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_ray_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_disk_lmm <- lmer(as.integer(DaysToFlower) ~ Google_longitude + (1|Google_longitude/FlowerHead), data = filter(Castanea_data_DaysToFlower, num_traits == "numDiscF", as.integer(DaysToFlower) < 150))
VarCorr(DaysToFlower_disk_lmm)
DaysToFlower_disk_lmm_anova <- as_tibble(anova(DaysToFlower_disk_lmm, type = "II"))%>%
  mutate(num_traits = "numDiscF", label_text = paste0("Days ~ Longitude + (1|Longitude/MaternalLine)", "\n", "Type II ANOVA, ", "F", "(", NumDF, ", ", round(DenDF, 0), ") = ", round(`F value`, 2), ", p = ", round(`Pr(>F)`, 2)))%>%
  bind_cols(as_tibble_row(fixef(DaysToFlower_disk_lmm))%>%rename(slope = Google_longitude, intercept = `(Intercept)`), as_tibble(VarCorr(DaysToFlower_disk_lmm))%>%select(grp, vcov)%>%pivot_wider(names_from = "grp", values_from = "vcov"))

DaysToFlower_lmm_anova <- bind_rows(DaysToFlower_bud_lmm_anova, DaysToFlower_ray_lmm_anova, DaysToFlower_disk_lmm_anova)%>%
  mutate(num_traits = factor(num_traits, levels = c("numFlwrB", "numRayF", "numDiscF")))
DaysToFlower_lmm_anova
colnames(DaysToFlower_lmm_anova)



p <- ggplot(data = Castanea_data_DaysToFlower, aes(x = Google_longitude, y = as.integer(DaysToFlower)))+
  geom_point()+
  geom_point(data = filter(Castanea_data_DaysToFlower, as.integer(DaysToFlower) == 150), color = "red")+
  # stat_smooth(data = filter(Castanea_data_DaysToFlower, as.integer(DaysToFlower) < 150, num_traits == "numDiscF"), method = "lm", color = "blue")+
  # geom_abline(data = filter(DaysToFlower_lmm_anova_H, num_traits == "numDiscF"), aes(intercept = intercept, slope = slope), color = "blue")+
  # stat_cor(data = filter(Castanea_data_DaysToFlower, as.integer(DaysToFlower) < 150),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  # stat_regline_equation(data = filter(Castanea_data_DaysToFlower, as.integer(DaysToFlower) < 150),label.x.npc = 0, label.y.npc = 0.95)+
  geom_text(x = -90.6, y = 160, hjust = 0, mapping = aes(label = label_text), data = DaysToFlower_lmm_anova)+
  # geom_text(x = 38.6, y = 60, hjust = 0, mapping = aes(label = paste0("y = ", round(intercept, 2), round(slope, 2), "x")), data = filter(DaysToFlower_lmm_anova_H, num_traits == "numDiscF"), color = "blue")+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Days since planting", limits = c(60, 165))+
  facet_wrap(.~num_traits, nrow = 1, labeller = labeller(num_traits = strip_labels))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")


# ggsave("./figures/Castanea_days_to_flower_by_longitude.png", width = 13, height = 5)

###########################################

unique(Castanea_data$num_traits)


# by County
Castanea_RosStem_data <- Castanea_data %>%
  filter(num_traits %in% c("numRos", "numStem"))%>%
  group_by(County, num_traits, Date)%>%
  summarise(average = mean(num_values, na.rm = TRUE), SE = sd(num_values, na.rm = TRUE)/sqrt(n()), sample_size = n())%>%
  ungroup()%>%
  mutate(average = case_when(num_traits == "numStem" ~ -average, TRUE ~ average),
         SE = case_when(num_traits == "numStem" ~ -SE, TRUE ~ SE))%>%
  left_join(Castanea_data_county_coordinations, by = "County")


p <- ggplot(data = Castanea_RosStem_data, aes(x = Date, y = average))+
  geom_col(aes(fill = num_traits))+
  geom_errorbar(aes(ymin = average, ymax = average + SE, color = num_traits), width = 0.2)+
  scale_y_continuous(name = "", breaks = c(-1, 0, 1), labels = c("1 stem", "0", "1 rosette"))+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(legend.position = "none")
p

ggsave("./figures/Castanea_RosStem_by_County.png", height = 10, width = 12)


p2 <- ggplot(data = filter(Castanea_RosStem_data, Date == as.Date("2024-07-11")), aes(x = reorder(labels, mean_Google_latitude), y = average))+
  geom_col(aes(fill = num_traits))+
  geom_errorbar(aes(ymin = average, ymax = average + SE, color = num_traits), width = 0.2)+
  scale_y_continuous(name = "", breaks = c(-1, 0, 1), labels = c("1 stem", "0", "1 rosette"))+
  scale_x_discrete("")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p

ggsave("./figures/Castanea_RosStem_by_County_20240711.png", width = 10, height = 1.75)


# by Site
Castanea_RosStem_data <- Castanea_data %>%
  filter(num_traits %in% c("numRos", "numStem"))%>%
  group_by(MaternalLine, num_traits, Date)%>%
  summarise(average = mean(num_values, na.rm = TRUE), SE = sd(num_values, na.rm = TRUE)/sqrt(n()), sample_size = n())%>%
  ungroup()%>%
  mutate(average = case_when(num_traits == "numStem" ~ -average, TRUE ~ average),
         SE = case_when(num_traits == "numStem" ~ -SE, TRUE ~ SE))%>%
  left_join(Castanea_data_site_coordinations, by = "MaternalLine")


p <- ggplot(data = Castanea_RosStem_data, aes(x = Date, y = average))+
  geom_col(aes(fill = num_traits))+
  geom_errorbar(aes(ymin = average, ymax = average + SE, color = num_traits), width = 0.2)+
  scale_y_continuous(name = "", breaks = c(-1, 0, 1), labels = c("1 stem", "0", "1 rosette"))+
  scale_x_date(name = "")+
  facet_wrap(.~reorder(labels, mean_Google_latitude), nrow = 3)+
  theme_bw()+
  theme(legend.position = "none")
p

ggsave("./figures/Castanea_RosStem_by_Site.png", height = 10, width = 16)

# by longitude


# if we do not want those dead plants in flowering ratio calculation
mutate(num_values = case_when(is.na(num_values) ~ 0, num_values == 0 ~ 0, num_values >= 1 ~ 1, TRUE ~ as.numeric(NA)))

##########################

p3 <- ggplot(data = filter(Castanea_data, num_traits == "leafLong", Date == as.Date("2024-05-15")), aes(x = reorder(County, Google_latitude), y = num_values))+
  geom_point(aes(color = County), alpha = 0.7, position = position_jitter(width = 0.1, height = 0))+
  geom_boxplot(aes(color = County), outlier.shape = NA, fill = NA)+
  stat_anova_test(label.y.npc = 0.1)+
  scale_x_discrete(name = "")+
  scale_y_continuous("Leaf length (mm)")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p
ggsave("./figures/Castanea_leafLong_20240515.png", width = 10, height = 1.75, dpi = 600)

Castanea_stemLength_data <- Castanea_data %>%
  filter(Date == as.Date("2024-07-11"), num_traits %in% c("stemLength", "numDeadF", "numDiscF", "numFlwrB", "numRayF"))%>%
  pivot_wider(names_from = "num_traits", values_from = "num_values")%>%
  rowwise()%>%
  mutate(total_flowers = sum(numDeadF, numDiscF, numFlwrB, numRayF))%>%
  ungroup()%>%
  filter(total_flowers > 0)


p4 <- ggplot(data = Castanea_stemLength_data, aes(x = reorder(County, Google_latitude), y = stemLength))+
  geom_point(aes(color = County), alpha = 0.7, position = position_jitter(width = 0.1, height = 0))+
  geom_boxplot(aes(color = County), outlier.shape = NA, fill = NA)+
  stat_anova_test(label.y.npc = 0.9, label.x.npc = 0.35)+
  scale_x_discrete(name = "")+
  scale_y_continuous("Stem length (cm)")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p4
ggsave("./figures/Castanea_stemLength_20240711.png", width = 10, height = 1.75, dpi = 600)


Castanea_total_flowers_data <- Castanea_data %>%
  filter(num_traits %in% c("numDeadF", "numDiscF", "numFlwrB", "numRayF"), Date == as.Date("2024-07-15"))%>%
  group_by(index, MaternalLine, County)%>%
  summarise(mean_Google_latitude = mean(Google_latitude), mean_Google_longitude = mean(Google_longitude), total_flower = sum(num_values, na.rm = TRUE))

p5 <- ggplot(data = Castanea_total_flowers_data, aes(x = reorder(County, mean_Google_latitude), y = total_flower))+
  geom_point(aes(color = County), alpha = 0.7, position = position_jitter(width = 0.1, height = 0))+
  geom_boxplot(aes(color = County), outlier.shape = NA, fill = NA)+
  stat_anova_test(label.y.npc = 0.9, label.x.npc = 0)+
  scale_x_discrete(name = "")+
  scale_y_continuous("Total flowers")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p
ggsave("./figures/Castanea_totalFlower_20240715.png", width = 10, height = 1.75, dpi = 600)


library(cowplot)

p<- plot_grid(p2, p3, p4, p5, p1, ncol = 1, align = "v", rel_heights = c(1.75, 1.75, 1.75, 1.75, 2.5))
ggsave("./figures/combined_plot_for_poster.png", width = 10, height = 9.5, dpi = 600)


##State plots##########
#######################

p <- ggplot(data = filter(Castanea_data_CountyLabels, num_traits == "stemLength"), aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  labs(title = "County & Stem Length")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Stem Length (cm)")+
  facet_grid(~reorder(labels, mean_Google_latitude), scales = "free_x", space = "free_x")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("./figures/County_stemlength.png", width = 30, height = 8, dpi = 600)

stemLength_data <- filter(Castanea_data_CountyLabels, num_traits == "stemLength")%>%
  filter(Date == as.Date("2024-07-03"))

p <- ggplot(data = stemLength_data, aes(x = MaternalLine, y = num_values))+
  geom_boxplot(aes(group = paste(Date, MaternalLine), fill = MaternalLine), position = position_dodge())+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  labs(title = "County & Stem Length")+
  scale_x_discrete("Accession (Site)")+
  scale_y_continuous("Stem Length (cm)")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p

stemLength_model <- as.formula(num_values ~ MaternalLine)
## Method 1

stemLength_aov <- aov(stemLength_model, data = stemLength_data)
print(stemLength_aov)
summary(stemLength_aov)

## Method 2

stemLength_anova <- anova(lm(stemLength_model, data = stemLength_data))
print(stemLength_anova)
summary(stemLength_anova)

## Method 3
library(car)

stemLength_data <- filter(Castanea_data_CountyLabels, num_traits == "stemLength")
stemLength_Anova <- Anova(lm(num_values ~ MaternalLine + Date + MaternalLine:Date, data = stemLength_data), type = "III")
print(stemLength_Anova)
summary(stemLength_Anova)

## Method 4
library(lmerTest)

stemLength_data <- filter(Castanea_data_CountyLabels, num_traits == "stemLength")
stemLength_model_random_effect <- as.formula(num_values ~ MaternalLine + (1|Date))

stemLength_Anova <- anova(lmer(stemLength_model_random_effect, data = stemLength_data), type = "II")
print(stemLength_Anova)
summary(stemLength_Anova)

p <-ggplot(data = filter(Castanea_data, num_traits == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = County, y = num_values))+
  geom_point()+
  labs(title = "County & Number of Flower Buds")
p
ggsave("./figures/County_numFlwrB.png", width = 15, height = 10, dpi = 600)

p <- ggplot(data = filter(Castanea_data, num_traits == "numLSt", Date == as.Date("2024-06-19")), aes(x = County, y = num_values))+
  geom_point()
p
ggsave("./figures/County_numLSt.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Castanea_data, num_traits == "numRos", Date == as.Date("2024-06-19")), aes(x = County, y = num_values))+
  geom_point()
p
ggsave("./figures/County_numRos", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Castanea_data, num_traits == "leafLong", Date == as.Date("2024-05-15")), aes(x = County, y = num_values))+
  geom_point()
p
ggsave("./figures/County_leaflong.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Castanea_data, num_traits == "leafWide", Date == as.Date("2024-05-15")), aes(x = County, y = num_values))+
  geom_point()
p



