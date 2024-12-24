# Project: MiWaves
# Description: Descriptive plots and tables of proximal outcomes and other variables
# Author: Lauren Zimmermann

need_package <- c("dplyr","tidyverse","lubridate")
have_package <- need_package %in% rownames(installed.packages())
if (any(have_package==FALSE)) {
  install.packages(need_package[!have_package])
}

# set directory
library(tidyverse)
library(dplyr)
library(lubridate)
outdir<-"~/output"
proc_dat <- "//maize.umhsnas.med.umich.edu/Psychiatry/Restricted/MiWaves/Pilot/Processed Data/"

# load R datafile
load(file = paste0(proc_dat,"miwaves_analysis_dataset.Rdata"))

stats_byday15 <- prim_dat %>%
  mutate(day15_flag = ifelse(day<=15, 1, 
                             ifelse(day>15, 2, NA))) %>%
  #group_by(day15_flag) %>%
  #group_by(day15_flag, intmessage_randassignment) %>%
  #group_by(day15_flag,intmessage_randassignment,message_length) %>%
  group_by(day15_flag,intmessage_randassignment,message_type) %>%
  summarise(`mean prop with cannabis use (SD)` = paste0(round(mean(prop_awakeuse,na.rm=T),3)," (",round(sd(prop_awakeuse,na.rm=T),3),")"))%>%
  ungroup()


stats_randomizations %>%
  kable(
    align = "c", caption = "Statistics on Observed Randomization Probability")%>%
  kable_classic(full_width = F) %>%
  save_kable(file = paste0(rand_check,"Table statistics on observed randomization prob.png"))



prim_dat <- merge(prim_dat, cov_canuse, by=c("id","decision_point"),all.x=T )

# Mean proportion of hours using cannabis by intervention assignment
# Mean proportion of hours using cannabis by intervention assignment and time of day
# Mean proportion of hours using cannabis by message length
# Mean proportion of hours using cannabis by message type of interaction
library(ggplot2)
#colors <- c("#f75394", "#1ca9c9")
colors <- c("#dc0000ff","#00a087ff")
means1 <- prim_dat %>%
  group_by(id) %>%
  summarise(mean_canuse = mean(prop_awakeuse,na.rm=T))%>%
  ungroup()

p1 <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate(Action = factor(intmessage_randassignment,levels=c(0,1),labels=c("No Message","Yes Message"))
         #,canuse = prop_awakeuse - mean_canuse
         ) %>% 
  filter(!is.na(Action)) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 

print(p1)

p2 <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate(Action = factor(intmessage_randassignment,levels=c(0,1),labels=c("No Message","Yes Message"))
         #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  filter(!is.na(Action) & (Time=="Morning")) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt in the morning decision time",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 
print(p2)

p3 <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate(Action = factor(intmessage_randassignment,levels=c(0,1),labels=c("No Message","Yes Message"))
         #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  filter(!is.na(Action) & (Time=="Evening")) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  labs(title = "Prompt vs. no prompt in the evening decision time") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 
print(p3)

png(file=paste0(outdir,"/Figure_S2.png"), height = 600, width = 800) #,  res = 300
print(p1)
dev.off()




# png(file=paste0(outdir,"/Figure_S2b.png"), height = 600, width = 800) #,  res = 300
# print(p2)
# dev.off()
# 
# png(file=paste0(outdir,"/Figure_S2c.png"), height = 600, width = 800) #,  res = 300
# print(p3)
# dev.off()

png(file=paste0(outdir,"/Figure_S3.png"), height = 600, width = 1200) #,  res = 300
cowplot::plot_grid(
  p2,
  p3, ncol=2
)
dev.off()

colors2 <- c("grey50","#0066b7","#ffcb00")
p4a <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_length_num = ifelse(message_length == "Short",1,
                                      ifelse(message_length=="Long",2,message_length)),
          action_messagelength = ifelse(intmessage_randassignment==1,message_length_num,intmessage_randassignment),
          Action = factor(action_messagelength,levels=c(0,1,2),labels=c("No Message","Yes Message-Short","Yes Message-Long"))
         #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Action!=0) & !is.na(Action)) %>% 
  filter(!is.na(Action)) %>%
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors2) +
  scale_fill_manual(values = colors2) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt, by message length",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 

print(p4a)

p4b <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_length_num = ifelse(message_length == "Short",1,
                                      ifelse(message_length=="Long",2,message_length)),
          action_messagelength = ifelse(intmessage_randassignment==1,message_length_num,intmessage_randassignment),
          Action = factor(action_messagelength,levels=c(0,1,2),labels=c("No Message","Yes Message-Short","Yes Message-Long"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  filter(!is.na(Action) & (Time=="Morning")) %>% 
  #filter((Action!=0) & !is.na(Action)) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors2) +
  scale_fill_manual(values = colors2) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt in the morning decision time, by message length",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 

p4c <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_length_num = ifelse(message_length == "Short",1,
                                      ifelse(message_length=="Long",2,message_length)),
          action_messagelength = ifelse(intmessage_randassignment==1,message_length_num,intmessage_randassignment),
          Action = factor(action_messagelength,levels=c(0,1,2),labels=c("No Message","Yes Message-Short","Yes Message-Long"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  filter(!is.na(Action) & (Time=="Evening")) %>% 
  #filter((Action!=0) & !is.na(Action)) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors2) +
  scale_fill_manual(values = colors2) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt in the evening decision time, by message length",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 


png(file=paste0(outdir,"/Figure_S4_panel.png"), height = 1200, width = 1200) #,  res = 300
cowplot::plot_grid(
  p4a,
  p4b, p4c, ncol=2
)
dev.off()


colors3 <- c("grey50","#0066b7","#ffcb00","#a583b8")
p5a <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_type_num = ifelse(message_type == "A",1,
                                      ifelse(message_type=="B",2,
                                             ifelse(message_type=="C",3,message_type))),
          action_messagetype = ifelse(intmessage_randassignment==1,message_type_num,intmessage_randassignment),
          Action = factor(action_messagetype,levels=c(0,1,2,3),labels=c("No Message","Yes Message-Type A","Yes Message-Type B","Yes Message-Type C"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Action!=0) & !is.na(Action)) %>% 
  filter(!is.na(Action)) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors3) +
  scale_fill_manual(values = colors3) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt, by message interaction type",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 

p5b <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_type_num = ifelse(message_type == "A",1,
                                    ifelse(message_type=="B",2,
                                           ifelse(message_type=="C",3,message_type))),
          action_messagetype = ifelse(intmessage_randassignment==1,message_type_num,intmessage_randassignment),
          Action = factor(action_messagetype,levels=c(0,1,2,3),labels=c("No Message","Yes Message-Type A","Yes Message-Type B","Yes Message-Type C"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  filter(!is.na(Action) & (Time=="Morning")) %>%
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors3) +
  scale_fill_manual(values = colors3) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt in the morning decision time, by message interaction type",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 

p5c <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_type_num = ifelse(message_type == "A",1,
                                    ifelse(message_type=="B",2,
                                           ifelse(message_type=="C",3,message_type))),
          action_messagetype = ifelse(intmessage_randassignment==1,message_type_num,intmessage_randassignment),
          Action = factor(action_messagetype,levels=c(0,1,2,3),labels=c("No Message","Yes Message-Type A","Yes Message-Type B","Yes Message-Type C"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  filter(!is.na(Action) & (Time=="Evening")) %>%
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors3) +
  scale_fill_manual(values = colors3) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt in the evening decision time, by message interaction type",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 

png(file=paste0(outdir,"/Figure_S5_panel.png"), height = 1200, width = 1200) #,  res = 300
cowplot::plot_grid(
  p5a,
  p5b, p5c, ncol=2
)
dev.off()

p6_dat <- prim_dat %>%
  filter(intmessage_randassignment==1 & !is.na(message_id)) %>%
  mutate(message_topic_label = factor(message_topic_label, levels = c("Affect Regulation","Alternative Activities","Future Thinking"))
  ) 
  #filter(is.na(message_topic_label))
table(p6_dat$message_topic_label)

colors3 <- c("grey50","#0066b7","#ffcb00","#a583b8")
p6a <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_topic_num = ifelse(message_topic_label == "Affect Regulation",1,
                                    ifelse(message_topic_label=="Alternative Activities",2,
                                           ifelse(message_topic_label=="Future Thinking",3,message_topic_label))),
          action_messagetopic = ifelse(intmessage_randassignment==1,message_topic_num,intmessage_randassignment),
          Action = factor(action_messagetopic,levels=c(0,1,2,3),labels=c("No Message","Yes Message-Affect Regulation","Yes Message-Alternative Activities","Yes Message-Future Thinking"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Action!=0) & !is.na(Action)) %>% 
  filter(!is.na(Action)) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors3) +
  scale_fill_manual(values = colors3) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt, by message content topic",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 

p6b <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_topic_num = ifelse(message_topic_label == "Affect Regulation",1,
                                     ifelse(message_topic_label=="Alternative Activities",2,
                                            ifelse(message_topic_label=="Future Thinking",3,message_topic_label))),
          action_messagetopic = ifelse(intmessage_randassignment==1,message_topic_num,intmessage_randassignment),
          Action = factor(action_messagetopic,levels=c(0,1,2,3),labels=c("No Message","Yes Message-Affect Regulation","Yes Message-Alternative Activities","Yes Message-Future Thinking"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Action!=0) & !is.na(Action)) %>% 
  filter(!is.na(Action) & (Time=="Morning")) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors3) +
  scale_fill_manual(values = colors3) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt in the morning decision time, by message content topic",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 

p6c <- prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( message_topic_num = ifelse(message_topic_label == "Affect Regulation",1,
                                     ifelse(message_topic_label=="Alternative Activities",2,
                                            ifelse(message_topic_label=="Future Thinking",3,message_topic_label))),
          action_messagetopic = ifelse(intmessage_randassignment==1,message_topic_num,intmessage_randassignment),
          Action = factor(action_messagetopic,levels=c(0,1,2,3),labels=c("No Message","Yes Message-Affect Regulation","Yes Message-Alternative Activities","Yes Message-Future Thinking"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Action!=0) & !is.na(Action)) %>% 
  filter(!is.na(Action) & (Time=="Evening")) %>% 
  group_by(day, Action) %>% 
  summarise(mcanuse=mean(prop_awakeuse, na.rm = TRUE)) %>%
  ggplot(aes(day, mcanuse, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors3) +
  scale_fill_manual(values = colors3) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,0.5)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(title = "Prompt vs. no prompt in the evening decision time, by message content topic",
       x = "Day in Study",
       y = "Mean Self-Reported Proportion of Waking Hours Using Cannabis") 


png(file=paste0(outdir,"/Figure_S6_panel.png"), height = 1200, width = 1500) #,  res = 300
cowplot::plot_grid(
  p6a,
  p6b, p6c, ncol=2
)
dev.off()

plot_violin <- function(dat, metric, metric_lbl, low_limit, high_limit) {
  ggplot(dat, aes(x = message_topic_label, y = get(metric))) +
    geom_violin(alpha = 0.2, fill="grey20") +
    geom_point(size=1.7) +
    geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5,fill="grey40") +

    theme_minimal() +
    scale_y_continuous(breaks=scales::breaks_pretty(n = 10), limits=c(low_limit, high_limit)) +
    #scale_fill_manual(values = c("black")) +
    #scale_color_manual(values = c("black")) +
    labs(y = metric_lbl, x = "Message Content Topic") +
    theme(legend.title = element_blank(),
          text=element_text(size=18))
}

#Proportion of Awake Hours with Self-Reported Cannabis Use
p7 <-plot_violin(dat=p6_dat, metric="prop_awakeuse",metric_lbl="Self-Reported Proportion of Waking Hours Using Cannabis", low_limit = 0.0, high_limit=1)
print(p7)

png(file=paste0(outdir,"/Figure_S7.png"), height = 600, width = 800)
print(p7)
dev.off()


# randomization probability over time
overall_dat <- prim_dat %>% 
  mutate(Time = "Overall")
stacked_prim_dat <- rbind(overall_dat, prim_dat)
colors_rand <- c("grey40","#DB901C","#99AFD7")
p8 <- stacked_prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( prop_time = ifelse(Time == "Overall",0,
                             ifelse(Time=="Morning",1,
                                    ifelse(Time=="Evening",2,Time))),
          Group = factor(prop_time,levels=c(0,1,2),labels=c("Overall","Morning Decision Time","Evening Decision Time"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Group!=0) & !is.na(Group)) %>% 
  filter(!is.na(Group)) %>%
  group_by(day, Group) %>% 
  summarise(mrandprob=mean(intmessage_randprob, na.rm = TRUE)) %>%
  ggplot(aes(day, mrandprob, color = Group, fill=Group)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors_rand) +
  scale_fill_manual(values = colors_rand) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0.3,0.6)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(x = "Day in Study",
       y = "Mean Proportion of Decision Points Randomized to Yes Message") 

print(p8)

png(file=paste0(outdir,"/Figure_S8.png"), height = 600, width = 800)
print(p8)
dev.off()



## Intervention engagement plots
# Plot the distribution
#options(scipen = 999)
color1 <- "#dfcac3"
color2 <- "#ab715e"
color3 <- "#c2c7b9"
color4 <- "#788067"
color_dash <- "grey50"

hist_a <- function(dat, color_hex){
  ggplot(data=filter(dat,interv_engagement!=999),aes(x= interv_engagement )) +
    geom_histogram(alpha=.6,color=color_hex, fill=color_hex,stat = "count") +
    stat_count(binwidth = 1, 
               geom = 'text', 
               color = 'black', 
               aes(label = paste0(round((after_stat(count)/5198)*100,1),"%")),
               position = position_stack(vjust = 0.5)
               ) +
    theme_bw() +
    scale_x_continuous() +
    scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500,3000),expand = c(0,0.1), limits=c(0,3500)) +
    theme(text = element_text(family = "Helvetica",size = 12),
          plot.title        = element_text(size = 12, face = "bold")) + 
    #ggtitle(paste0("Distribution of intervention engagement")) + 
    labs(x = "Intervention Engagement Score (0-3)", y = "Frequency (Num. of Decision Points)")
}

hist_plot1a = hist_a(dat=prim_dat,color_hex=color2)
hist_plot1a <- hist_plot1a + coord_flip()
print(hist_plot1a)

png(file=paste0(outdir,"/Figure_hist_intvengag.png"), height = 300, width = 700)
print(hist_plot1a)
dev.off()


# plots of intervention engagement over time 
colors <- c("#dc0000ff","#00a087ff")
# means1 <- prim_dat %>%
#   group_by(id) %>%
#   summarise(mean_intengag = mean(interv_engagement,na.rm=T))%>%
#   ungroup()

p10a <- prim_dat %>% 
  #left_join(means1, by="id") %>% 
  mutate(Action = factor(intmessage_randassignment,levels=c(0,1),labels=c("No Message","Yes Message"))
         #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  filter(!is.na(Action)) %>% 
  group_by(day, Action) %>% 
  summarise(mintengag=mean(interv_engagement, na.rm = TRUE)) %>%
  ggplot(aes(day, mintengag, color = Action, fill=Action)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,3)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(x = "Day in Study",
       y = "Mean Intervention Engagement (Range: 0-3)") 

# proportion over time for engagement components: app use, EMA completion, 
# overall_dat <- prim_dat %>% 
#   mutate(Time = "Overall")
# stacked_prim_dat <- rbind(overall_dat, prim_dat)
colors_prop <- c("black")
p10b <- prim_dat %>% 
  #left_join(means1, by="id") %>% 
  mutate( prop_time = ifelse(Time == "Overall",0,
                             ifelse(Time=="Morning",1,
                                    ifelse(Time=="Evening",2,Time))),
          Group = factor(prop_time,levels=c(0,1,2),labels=c("Overall","Morning Decision Time","Evening Decision Time"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Group!=0) & !is.na(Group)) %>% 
  filter(!is.na(Group)) %>%
  group_by(day, Group) %>% 
  summarise(mapp_use=mean(app_use_flag, na.rm = TRUE)) %>%
  ggplot(aes(day, mapp_use
             #, color = Group, fill=Group
             )) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2, color="gray40") +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2, color="black") +
  #scale_color_manual(values = colors_prop) +
  #scale_fill_manual(values = colors_prop) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(x = "Day in Study",
       y = "Mean Proportion of App Use") 

p10c <- prim_dat %>% 
  #left_join(means1, by="id") %>% 
  mutate( prop_time = ifelse(Time == "Overall",0,
                             ifelse(Time=="Morning",1,
                                    ifelse(Time=="Evening",2,Time))),
          Group = factor(prop_time,levels=c(0,1,2),labels=c("Overall","Morning Decision Time","Evening Decision Time"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Group!=0) & !is.na(Group)) %>% 
  filter(!is.na(Group)) %>%
  group_by(day, Group) %>% 
  summarise(memafin=mean(finished_ema, na.rm = TRUE)) %>%
  ggplot(aes(day, memafin
             #, color = Group, fill=Group
  )) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2, color="gray40") +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2, color="black") +
  #scale_color_manual(values = colors_prop) +
  #scale_fill_manual(values = colors_prop) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(x = "Day in Study",
       y = "Mean Proportion of EMA Completion") 


# proportion over time for engagement components: app use, EMA completion, 
# overall_dat <- prim_dat %>% 
#   mutate(Time = "Overall")
# stacked_prim_dat <- rbind(overall_dat, prim_dat)
colors_prop <- c("black")
p10d <- prim_dat %>% 
  #left_join(means1, by="id") %>% 
  mutate( prop_time = ifelse(Time == "Overall",0,
                             ifelse(Time=="Morning",1,
                                    ifelse(Time=="Evening",2,Time))),
          Group = factor(prop_time,levels=c(0,1,2),labels=c("Overall","Morning Decision Time","Evening Decision Time"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Group!=0) & !is.na(Group)) %>% 
  filter(!is.na(Group)) %>%
  group_by(day, Group) %>% 
  summarise(mactresp=mean(activity_question_response, na.rm = TRUE)) %>%
  ggplot(aes(day, mactresp
             #, color = Group, fill=Group
  )) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2, color="gray40") +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2, color="black") +
  #scale_color_manual(values = colors_prop) +
  #scale_fill_manual(values = colors_prop) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(x = "Day in Study",
       y = "Mean Proportion of Activity Response=Yes") 

print(p10a)
print(p10b)
print(p10c)
print(p10d)

png(file=paste0(outdir,"/Figure_S10a.png"), height = 400, width = 600) #,  res = 300
p10a
dev.off()

png(file=paste0(outdir,"/Figure_S10b.png"), height = 650, width = 600) #,  res = 300
cowplot::plot_grid(
  p10b,
  p10c,
  p10d, ncol=1
)
dev.off()



overall_dat <- prim_dat %>% 
  mutate(Time = "Overall")
stacked_prim_dat <- rbind(overall_dat, prim_dat)
colors_rand <- c("grey40","#DB901C","#99AFD7")
p8 <- stacked_prim_dat %>% 
  left_join(means1, by="id") %>% 
  mutate( prop_time = ifelse(Time == "Overall",0,
                             ifelse(Time=="Morning",1,
                                    ifelse(Time=="Evening",2,Time))),
          Group = factor(prop_time,levels=c(0,1,2),labels=c("Overall","Morning Decision Time","Evening Decision Time"))
          #,canuse = prop_awakeuse - mean_canuse
  ) %>% 
  #filter((Group!=0) & !is.na(Group)) %>% 
  filter(!is.na(Group)) %>%
  group_by(day, Group) %>% 
  summarise(mrandprob=mean(intmessage_randprob, na.rm = TRUE)) %>%
  ggplot(aes(day, mrandprob, color = Group, fill=Group)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_point(size=2) +
  scale_color_manual(values = colors_rand) +
  scale_fill_manual(values = colors_rand) +
  geom_smooth(method = lm, alpha=0.25,linewidth=1.2) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30),expand = c(0,0.1),limits=c(1,30)) +
  theme_bw()+
  coord_cartesian(ylim=c(0.3,0.6)) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        plot.title=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  labs(x = "Day in Study",
       y = "Mean Proportion of Decision Points Randomized to Yes Message") 

print(p8)

png(file=paste0(outdir,"/Figure_S8.png"), height = 600, width = 800)
print(p8)
dev.off()

# done: awake cannabis use is correctly being identified.
# done: made total awake hours limited to 12 hours since by capping to 13 or 12
# done: adjusted the start anchor to be the top of the hour of randomization (by adding 1 hour to the start anchor time)
# done: removed participants with <=3 EMAs across study time-period 
# done: re-export plot for awake proportion of cannabis use
# done: re-export plot for proportion of cannabis use (fully unadjusted)
# done: created tables with the number of participants, decision points and events impact by each rule
# done: created table with summary statistics on proximal outcome
#      - done: added rows for hours of sleep, and total available hours
# need to: create additional covariates for:
#          - prior cannabis use (avg over the past 72 hours, 48, 24 hours), 
#          - day of week, and 
#          - prior intervention engagement with proximal cannabis use
# need to: save analysis dataset to dropbox
# need to: examine univariate associations in RMarkdown
# need to: create summary plots with trend of outcome over time

# prior cannabis use – avg. prop. of waking hours, over past 72 hours (also, examine past 24 hrs and 48 hrs), 
# time of day – binary (AM=0, PM=1),
# day of the week  – binary (weekday=1, weekend [Fri-Sun]=0), and examine discrete (Mon=1, …, Sun=7),
# prior intervention engagement – mHealth intervention engagement (operationalized the same as the proximal outcome) averaged over the past 72 hours (also examine over past 24 hours, 48 hours), 
# baseline motivation to change – scale from 0 (Not at all) to 10 (Very) at time of baseline survey,
# baseline cannabis use – self-reported hours of cannabis use at same time point (AM or PM) in prior day


stats_prox_use <- prim_dat %>%
  #group_by(Time) %>%
  summarise(`total hours of use (unadj.)` = sum(ct_use_unadj_l, na.rm=T),
            `total hours of use` = sum(ct_use, na.rm=T),
            `total hours of awake use` = sum(ct_awakeuse, na.rm=T),
            `total hours of asleep use` = `total hours of use` -`total hours of awake use`,
            `total hours of sleep (unadj.)` = sum(ct_sleep_unadj_l, na.rm=T),
            `total hours of sleep` = sum(ct_sleep, na.rm=T),
            `total hours awake` = sum(awake_ct_cap, na.rm=T),
            
            `mean (SD) hours of use (unadj.)` = paste0(round(mean(ct_use_unadj_l,na.rm=T),1)," (",round(sd(ct_use_unadj_l,na.rm=T),1),")"),
            `mean (SD) hours of use` = paste0(round(mean(ct_use,na.rm=T),1)," (",round(sd(ct_use,na.rm=T),1),")"),
            `mean (SD) hours of awake use` = paste0(round(mean(ct_awakeuse,na.rm=T),1)," (",round(sd(ct_awakeuse,na.rm=T),1),")"),
            `mean (SD) hours of sleep (unadj.)` = paste0(round(mean(ct_sleep_unadj_l,na.rm=T),1)," (",round(sd(ct_sleep_unadj_l,na.rm=T),1),")"),
            `mean (SD) hours of sleep` = paste0(round(mean(ct_sleep,na.rm=T),1)," (",round(sd(ct_sleep,na.rm=T),1),")"),
            `mean (SD) hours awake` = paste0(round(mean(awake_ct_cap,na.rm=T),1)," (",round(sd(awake_ct_cap,na.rm=T),1),")"),
            `mean (SD) prop of use (unadj.)` = paste0(round(mean(prop_use_unadj_l,na.rm=T),3)," (",round(sd(prop_use_unadj_l,na.rm=T),3),")"),
            `mean (SD) prop of use` = paste0(round(mean(prop_use,na.rm=T),3)," (",round(sd(prop_use,na.rm=T),3),")"),
            `mean (SD) prop of awake use` = paste0(round(mean(prop_awakeuse,na.rm=T),3)," (",round(sd(prop_awakeuse,na.rm=T),3),")"),

            `min hours of use (unadj.)` = round(min(ct_use_unadj_l,na.rm=T),1),            
            `min hours of use` = round(min(ct_use,na.rm=T),1),
            `min hours of awake use` = round(min(ct_awakeuse,na.rm=T),1),
            `min hours of sleep (unadj.)` = round(min(ct_sleep_unadj_l,na.rm=T),1),            
            `min hours of sleep` = round(min(ct_sleep,na.rm=T),1),
            `min hours awake` = round(min(awake_ct_cap,na.rm=T),1),
            `min prop of use (unadj.)` = round(min(prop_use_unadj_l,na.rm=T),3),
            `min prop of use` = round(min(prop_use,na.rm=T),3),
            `min prop of awake use` = round(min(prop_awakeuse,na.rm=T),3),

            `max hours of use (unadj.)` = round(max(ct_use_unadj_l,na.rm=T),1),
            `max hours of use` = round(max(ct_use,na.rm=T),1),
            `max hours of awake use` = round(max(ct_awakeuse,na.rm=T),1),
            `max hours of sleep (unadj.)` = round(max(ct_sleep_unadj_l,na.rm=T),1),
            `max hours of sleep` = round(max(ct_sleep,na.rm=T),1),
            `max hours awake` = round(max(awake_ct_cap,na.rm=T),1),
            `max prop of use (unadj.)` = round(max(prop_use_unadj_l,na.rm=T),3),
            `max prop of use` = round(max(prop_use,na.rm=T),3),
            `max prop of awake use` = round(max(prop_awakeuse,na.rm=T),3))
  #ungroup()

table_counts <- prim_dat %>% 
  group_by(gte1hr_from_emastart_l, Time) %>%
  summarise(n_dps = n(),
            n_participants = n_distinct(id),
            n_events = sum(ct_awakeuse,na.rm=T)
  ) %>%
  ungroup()

# for Rules 1, 2, 3, 4, and 5 run the below code 
#  after commenting in the appropriate line 
#  in the above section and compare to counts
dps_w_event <- prim_dat %>%
  mutate(event = ifelse(ct_awakeuse>0,1,
                        ifelse(ct_awakeuse==0,0,NA)))
table(dps_w_event$event)
dps_w_event %>% 
  group_by(Time) %>%
  summarise(num_events = sum(event,na.rm=T))%>%
  ungroup()

# for Rule 6
test<- prim_dat %>%
  filter(Time=="Evening") %>%
  filter(ct_awakeuse<ct_use)

# for Occurence 1 (i.e. #7)
temp<- prim_dat %>%
  group_by(Time) %>%
  summarise( awake_ct = sum(awake_ct,na.rm=T),
             awake_ct_cap = sum(awake_ct_cap,na.rm=T)) %>%
  ungroup() %>%
  mutate(hrs_wo_selection = awake_ct - awake_ct_cap)
test<- prim_dat %>%
  filter(Time=="Evening") %>%
  filter(awake_ct>awake_ct_cap)


  
# confirmed that the 21 decision points with no randomizations did not have a value in the proximal cannabis use outcome
tbl_missing_rand<-analysis_dat %>%
  filter(is.na(completed_rand))%>%
  group_by(Time,missing_rand,completed_ema)%>%
  summarise(n_dps = n(),
            n_participants = n_distinct(id)
  ) %>%
  ungroup()

dps_w_event <- prim_dat %>%
  mutate(event = ifelse(ct_awakeuse>0,1,
                        ifelse(ct_awakeuse==0,0,NA)))
table(dps_w_event$event)
dps_w_event %>% 
  group_by(Time) %>%
  summarise(num_events = sum(event,na.rm=T))%>%
  ungroup()
ct_ids_insample <- prim_dat %>%
  select(id) %>%
  distinct(id)
test<-prim_dat %>%
  filter(Time=="Morning")
  #filter(awakecan_am_ct >= 13) %>%
  #filter(sleepam_ct == 12) %>%
  filter(awakeam_ct >=13) %>%
  select(id,day,morning, decision_point, hr_start_anchor_local,hr_end_anchor_local,sleep_am_l, can_use_am_l, gte1hr_from_emastart_l, ct_awakeuse_am, sleepam_ct, awakeam_ct, awakeam_ct_cap)

test_2<-prim_dat %>%
  #filter(awakecan_pm_ct >= 13) %>%
  #filter(sleeppm_ct >= 13) %>%
  filter(awakepm_ct >= 13) %>%
  select(id,day,morning, decision_point, hr_start_anchor_local,hr_end_anchor_local,sleep_pm_l, can_use_pm_l, gte1hr_from_emastart_l, ct_awakeuse_pm, sleeppm_ct, awakepm_ct, awakepm_ct_cap)

# plot of proximal outcome over time by participant
library(ggplot2)
list_group = c(
  "Morning"
  ,"Evening"
)
prim_dat$Time <- factor(prim_dat$Time, levels = list_group)

color_group <- c("#df8f44","#00a1d5")

facet_plot = prim_dat %>%
  ggplot(aes(x = day, y = prop_awakeuse, color = Time))+
  geom_point(size=0.8)+
  facet_wrap(~id)+
  theme_bw() + 
  #scale_y_continuous(breaks = seq(0,1,1),
                     #labels = paste0(1:84,"") ,
  #                   limits = c(0,1)) +
  # to add ID numbers back in comment in the below line
  theme(legend.position = "none",
        strip.text = element_blank(),
        #strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        plot.title = element_text(size = 10, face = "bold")) + 
  scale_color_manual( values = color_group) +
  labs(x = "Day in Study", y = "Proportion of Awake Hours with Reported Cannabis Use (0=0 hrs, 0.5=~6 hrs, 1=~12-13 hrs)")
ggsave(plot = facet_plot, filename = "C:/Users/lzimm/OneDrive - Michigan Medicine/Documents/MiWaves/Analysis/lzimm/PropAwakeCanUse.perID.png", width = 10, height = 7)

facet_plot_unadj = prim_dat %>%
  ggplot(aes(x = day, y = prop_use_l, color = Time))+
  geom_point(size=0.8)+
  facet_wrap(~id)+
  theme_bw() + 
  #scale_y_continuous(breaks = seq(0,1,1),
  #labels = paste0(1:84,"") ,
  #                   limits = c(0,1)) +
  theme(legend.position = "none",
        #strip.text = element_blank(),
        strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        plot.title = element_text(size = 10, face = "bold")) + 
  scale_color_manual( values = color_group) +
  labs(x = "Day in Study", y = "Proportion of Hours with Reported Cannabis Use (0=0 hrs, 0.5=~6 hrs, 1=13 hrs)")
ggsave(plot = facet_plot_unadj, filename = "PropCanUseUnadj.perID.png", width = 10, height = 7)


# plot hours asleep
am_plot_dat <- analysis_dat %>%
  filter(morning==1)

sleepam_plot_dat_long <- am_plot_dat%>% 
  select(c(id,day,starts_with("sleepam_")))%>%
  pivot_longer(cols=starts_with("sleepam_"),names_to = c("hour"))%>%
  mutate( hour = gsub("sleepam_", "", hour))
can_useam_plot_dat_long <- am_plot_dat%>% 
  select(c(id,day,starts_with("can_useam_")))%>%
  pivot_longer(cols=starts_with("can_useam_"),names_to = c("hour"))%>%
  mutate( hour = gsub("can_useam_", "", hour))
am_plot_dat_long<- merge(x=sleepam_plot_dat_long,y=can_useam_plot_dat_long, by=c("id","day","hour"), all.x=T)%>%
  dplyr::rename(sleep_value = value.x, 
                can_use_value = value.y) %>%
  mutate(
          Asleep = ifelse(is.na(sleep_value), "Missing",
                          ifelse(sleep_value==1, "Asleep", "Awake")),
          Cannabis = ifelse(is.na(can_use_value), "Missing",
                          ifelse(can_use_value==1, "Cannabis", "Cannabis No")))
  #%>%filter(!(is.na(value)))

library(dplyr)

list_asleep = c(
  "Asleep"
  ,"Awake"
  ,"Missing"
)

list_cannabis = c(
  "Cannabis"
  ,"Cannabis No"
  ,"Missing"
)
list_hours_am = c(
  "none", "6pm","7pm","8pm","9pm","10pm","11pm","12pm","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12am"
)
am_plot_dat_long$hour <- factor(am_plot_dat_long$hour, levels = list_hours_am)
am_plot_dat_long$Asleep <- factor(am_plot_dat_long$Asleep, levels = list_asleep)
am_plot_dat_long$Cannabis <- factor(am_plot_dat_long$Cannabis, levels = list_cannabis)
#steelblue
color_asleep <- c("#1F7DAD", "#FFFFC5", "white")

#panel_sleepam <- ggplot(am_plot_dat_long, aes(y=day,x=hour,fill = Asleep)) +
panel_sleepam <- ggplot(am_plot_dat_long) +
  geom_tile(aes(y=day,x=hour,fill = Asleep)) + 
  facet_wrap(~id)+
  theme_bw() +
  scale_fill_manual(values = color_asleep) +
  theme(legend.position = "none",
        #strip.text = element_blank(),
        strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        axis.text.x = element_text(angle = 65, size=4),
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = "Hour in Epoch", y = "Day in Study", title = "Reported Sleep Hours (Awake = Yellow, Asleep = Blue): Morning EMA")

ggsave(plot = panel_sleepam, filename = "SleepDiary.perID-AM.png", width = 10, height = 7)


pm_plot_dat <- analysis_dat %>%
  filter(morning==0)
sleeppm_plot_dat_long <- pm_plot_dat%>% 
  select(c(id,day,starts_with("sleeppm_")))%>%
  pivot_longer(cols=starts_with("sleeppm_"),names_to = c("hour"))%>%
  mutate( hour = gsub("sleeppm_", "", hour))
can_usepm_plot_dat_long <- pm_plot_dat%>% 
  select(c(id,day,starts_with("can_usepm_")))%>%
  pivot_longer(cols=starts_with("can_usepm_"),names_to = c("hour"))%>%
  mutate( hour = gsub("can_usepm_", "", hour))
pm_plot_dat_long<- merge(x=sleeppm_plot_dat_long,y=can_usepm_plot_dat_long, by=c("id","day","hour"), all.x=T)%>%
  dplyr::rename(sleep_value = value.x, 
                can_use_value = value.y) %>%
  mutate(
    Asleep = ifelse(is.na(sleep_value), "Missing",
                    ifelse(sleep_value==1, "Asleep", "Awake")),
    Cannabis = ifelse(is.na(can_use_value), "Missing",
                      ifelse(can_use_value==1, "Cannabis", "Cannabis No")))


library(dplyr)

# list_asleep = c(
#   "Asleep"
#   ,"Awake"
#   ,"Missing"
# )
list_hours_pm = c(
  "none", "6am","7am","8am","9am","10am","11am","12am","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm","12pm"
)
pm_plot_dat_long$hour <- factor(pm_plot_dat_long$hour, levels = list_hours_pm)
pm_plot_dat_long$Asleep <- factor(pm_plot_dat_long$Asleep, levels = list_asleep)
pm_plot_dat_long$Cannabis <- factor(pm_plot_dat_long$Cannabis, levels = list_cannabis)
color_asleep <- c("#1F7DAD", "#FFFFC5", "white")

panel_sleeppm <- ggplot(pm_plot_dat_long) +
  geom_tile(aes(y=day,x=hour,fill = Asleep)) + 
  facet_wrap(~id)+
  theme_bw() +
  scale_fill_manual(values = color_asleep) +
  theme(legend.position = "none",
        strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        axis.text.x = element_text(angle = 65, size=4),
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = "Hour in Epoch", y = "Day in Study", title = "Reported Sleep Hours (Awake = Yellow, Asleep = Blue): Evening EMA")

ggsave(plot = panel_sleeppm, filename = "SleepDiary.perID-PM.png", width = 10, height = 7)


# overlay cannabis use to sleep diary plots in orange, 
#  such that the light orange (yellow + orange) reflects cannabis use awake 
#  and the burnt orange / brown (blue + orange) reflects cannabis use asleep
#FA8128 
#fill = alpha("#2C77BF", .5)
#FFF700 strong yellow
#FFFFC5 pale yellow
color_all <- c("#1F7DAD", "#FFF700", "white", "#FA8128", "white")
#color_cannabis <- c("#FA8128", "#00FFFFFF", "#00FFFFFF")

panel_am<-panel_sleepam + geom_tile(aes(y=day,x=hour,fill = Cannabis)) + scale_fill_manual(values = alpha(color_all,0.5)) +
  labs(x = "Hour in Epoch", y = "Day in Study", title = "Reported Hours (Awake = Yellow, Asleep = Blue, Cannabis Use Awake = Orange, Cannabis Use Asleep = Brown): Morning EMA")

ggsave(plot = panel_am, filename = "SleepDiarywCannabis.perID-AM.png", width = 10, height = 7)


panel_pm<-panel_sleeppm + geom_tile(aes(y=day,x=hour,fill = Cannabis)) + scale_fill_manual(values = alpha(color_all,0.5)) +
  labs(x = "Hour in Epoch", y = "Day in Study", title = "Reported Hours (Awake = Yellow, Asleep = Blue, Cannabis Use Awake = Orange, Cannabis Use Asleep = Brown): Evening EMA")

ggsave(plot = panel_pm, filename = "SleepDiarywCannabis.perID-PM.png", width = 10, height = 7)


# hours of cannabis use awake, asleep, and overall
pm_canuse_hrs <- pm_plot_dat_long %>%
  mutate( Overall_Cannabis_Use = sum(can_use_value, na.rm = T))

pm_canuse_hrs_asleep <- pm_plot_dat_long%>%
  group_by(Asleep) %>%
  summarise(Asleep_Cannabis_Use = sum(can_use_value, na.rm = T)) %>%
  ungroup()
  
  
  require(knitr)
require(kableExtra)
pm_canuse_hrs %>%
  kable(
    align = "c", caption = "Cannabis Use Overall, Asleep, Awake")%>%
  kable_classic(full_width = F) %>%
  save_kable(file = paste0(ema_check,"Table cannabis use.png"))