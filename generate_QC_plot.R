library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Castanea/data/after_basic")

list.files(pattern = "*_avg_dp_gq_per_site.tsv")

avg_dp_gq_per_site <- lapply(list.files(pattern = "*_avg_dp_gq_per_site.tsv"), read_tsv) %>%
  bind_rows()

colnames(avg_dp_gq_per_site)
max(avg_dp_gq_per_site$AVG_DP)
min(avg_dp_gq_per_site$AVG_DP)
mean(avg_dp_gq_per_site$AVG_DP)
median(avg_dp_gq_per_site$AVG_DP)
sd(avg_dp_gq_per_site$AVG_DP)
sum(avg_dp_gq_per_site$AVG_DP > 25)

p <- ggplot(data = avg_dp_gq_per_site, aes(x = AVG_DP))+
  geom_histogram(bins = 100)+
  theme_bw()

ggsave("Castanea_DP_histogram.png", width = 6, height = 4)



p <- ggplot(data = avg_dp_gq_per_site, aes(x = POS))+
  geom_point(aes(y = AVG_DP), size = 0.1, alpha = 0.5) +
  facet_wrap(.~CHROM, ncol = 1, scales = "free_x")+
  scale_y_continuous(limits = c(0, 40))+
  theme_bw()+
  theme(panel.spacing.x = unit(0, "in"), axis.text.x = element_blank())


ggsave("avg_dp_gq_per_site.png", width = 10, height = 27)



list.files(pattern = "*per_sample_stats.tsv")


per_sample_stats <- lapply(list.files(pattern = "*per_sample_stats.tsv"), read_tsv) %>%
  bind_rows()%>%
  group_by(SAMPLE)%>%
  summarise_all(.funs = "mean")%>%
  mutate(SAMPLE = factor(SAMPLE, levels = unique(SAMPLE))) %>%
  mutate(group = rep(1:5, each = ceiling(n()/5), length.out = n()))%>%
  group_by(group)%>%
  mutate(xend = lead(SAMPLE), y = AVG_DP/40, yend = c(tail(AVG_DP/40, n = -1), NA))

p <- ggplot(data = per_sample_stats, aes(x = SAMPLE, y = MISSING_RATE))+
  geom_col(aes(fill = MISSING_RATE))+
  geom_segment(data =  per_sample_stats %>% drop_na(), aes(x=SAMPLE, xend = xend, y = y, yend = yend), color = "skyblue")+
  geom_hline(yintercept = 10/40, color = "skyblue", linetype = 2)+
  geom_hline(yintercept = 0.1, color = "orange", linetype = 2)+
  scale_y_continuous(
    name = "Missing Rate",
    sec.axis = sec_axis(~ . * 40, name = "Average DP")
  ) +
  facet_wrap(~ group, nrow = 5, scales = "free_x") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid = element_blank())

ggsave("per_sample_stats_MISSING_RATE.png", width = 12, height = 20, limitsize = FALSE)
