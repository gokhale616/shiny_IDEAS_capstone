# PAIGE'S FIGURES SCRIPT, WITH FILES SAVED AS .RDS FOR QUICK AND EASY GRAB BY THE DASHBOARD
library(tidyverse)
library(magrittr)

dat <- read.csv("./data/shiny_data.csv")

dat %<>% filter(Group!="primates")  # just for now bc disease is missing

dat %<>% distinct(HostCorrectedName, .keep_all = TRUE)

# summary of threat variable
table(dat$Threat)
table(dat$ThreatDic)
table(dat$ThreatDic2)

# summary of group
table(dat$Group)

# summary of group x threat
dat %>%
  group_by(Group, ThreatDic2, ThreatDis) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

dat %>%
  filter(ThreatDic==0, ThreatDis==0) %>%
  mutate(type="NonThreatened") -> dfNon

dat %>%
  filter(ThreatDic==1, ThreatDis==0) %>%
  mutate(type="ThreatOther") -> dfT

dat %>%
  filter(ThreatDic==1, ThreatDis==1) %>%
  mutate(type="ThreatDisease") -> dfTD

dat %>%
  filter(ThreatDic==1, ThreatDis==1) %>%
  mutate(type="Disease") -> dfD

dat2 <- bind_rows(dfNon, dfT, dfTD, dfD)

dat2 %>% group_by(Group, type) %>%
  tally()

dat %>% 
  group_by(Group) %>% 
  tally()

# threat variable on top 
# and colors should be more intuitive

plot_byThreat <- dat2 %>% 
  group_by(Group, type) %>% 
  tally() %>%
  ggplot(aes(fill=type, y=n, x=Group)) + 
  geom_bar(stat="identity", position = "fill") + 
  labs(y="", fill="")
saveRDS(plot_byThreat, "shiny-figures_files/plot_byThreat.RDS")

# filter by group type - host family
# make some sort of automated title
carn <- dat2 %>% 
  filter(Group=="carnivores") %>%
  group_by(HostFamily, type) %>% 
  tally() %>%
  ggplot(aes(fill=type, y=n, x=HostFamily)) + 
  geom_bar(stat="identity", position = "fill") + 
  labs(y="", fill="")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") # <<<<<<<<<<<<< PUT LEGEND ON TOP

ung <- dat2 %>% 
  filter(Group=="ungulates") %>%
  group_by(HostFamily, type) %>% 
  tally() %>%
  ggplot(aes(fill=type, y=n, x=HostFamily)) + 
  geom_bar(stat="identity", position = "fill") + 
  labs(y="", fill="")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") # <<<<<<<<<<<<< PUT LEGEND ON TOP

plot_byFamily <- plot_grid(carn, ung, nrow = 1)
saveRDS(plot_byFamily, "shiny-figures_files/plot_byFamily.RDS")

# filter by continent
NAmer <-dat2 %>% 
  filter(continent=="North America") %>%
  group_by(HostFamily, type) %>% 
  tally() %>%
  ggplot(aes(fill=type, y=n, x=HostFamily)) + 
  geom_bar(stat="identity", position = "fill") + 
  labs(y="", fill="")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") # <<<<<<<<<<<<< PUT LEGEND ON TOP

Africa <- dat2 %>% 
  filter(continent=="Africa") %>%
  group_by(HostFamily, type) %>% 
  tally() %>%
  ggplot(aes(fill=type, y=n, x=HostFamily)) + 
  geom_bar(stat="identity", position = "fill") + 
  labs(y="", fill="")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") # <<<<<<<<<<<<< PUT LEGEND ON TOP

plot_byCont <- plot_grid(NAmer, Africa, nrow = 1)
saveRDS(plot_byCont, "shiny-figures_files/plot_byCont.RDS")

# filter by mass
# 0-20 20-60 60+ 
plot_byMass <- dat2 %>% 
  filter(!is.na(massKG)) %>%
  mutate(mass2=ifelse(massKG<75, 0, 1)) %>%
  group_by(mass2, type, Group) %>% 
  tally() %>%
  ggplot(aes(fill=type, y=n, x=factor(mass2))) + 
  geom_bar(stat="identity", position = "fill") + 
  labs(y="", fill="")  + 
  facet_grid(Group ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
saveRDS(plot_byMass, "shiny-figures_files/plot_byMass.RDS")

# filter by home range
# 0-
plot_byHR <- dat2 %>% 
  filter(!is.na(hostHomeRange)) %>%
  mutate(range2 = ifelse(log(hostHomeRange) < 2, 0, 1)) %>%
  group_by(range2, type, Group) %>% 
  tally() %>%
  ggplot(aes(fill=type, y=n, x=factor(range2))) + 
  geom_bar(stat="identity", position = "fill") + 
  labs(y="", fill="")  + 
  facet_grid(Group ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
saveRDS(plot_byHR, "shiny-figures_files/plot_byHR.RDS")
```

