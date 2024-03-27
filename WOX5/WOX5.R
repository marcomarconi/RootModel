library(tidyverse)
theme_set(theme_bw())

# Do:
# grep -c "type QC" output.*
# the first two values (auxin source and WOX5 basal production) are the replicate, the second OE/WT/mutant the third the division (divide them by 2)
# put these 3 columns into a CSV file


df <- read_csv("~/trabajo/Models/RootModel/WOX5/WOX5.csv")
df  %>% filter(To==0) %>% mutate(Divisions=Divisions/6, Type = case_when(By == 0 & WOX5 == 0 ~ "wox5", 
                                                                         By == 1 & between(WOX5, 0.4, 0.8) ~ "WT", 
                                                                         By == 1 & WOX5 > 0.8 ~ "OE")) %>% na.omit %>% 
    group_by(Type) %>% summarise(Mean=mean(Divisions), SD=sd(Divisions)/sqrt(n())*1) %>%  ggplot() + 
    geom_bar(aes(Type, Mean), stat="identity", position = position_dodge(width=0.9)) + 
    geom_errorbar(aes(Type, ymin=Mean-SD, ymax=Mean+SD), width=0.25, position = position_dodge(width=0.9)) +
    theme(axis.title.x = element_blank(), text=element_text(size = 24), legend.position = c(0.85,0.85), legend.background = element_rect(colour = "black")) + 
    ylab("QC Division (%)") + ylim(c(0, 1))
df  %>% mutate(To=factor(To),Divisions=Divisions/6, Type = case_when(By == 0 & WOX5 == 0 ~ "wox5", 
                                                                     By == 1 & between(WOX5, 0.4, 0.8) ~ "WT", 
                                                                     By == 1 & WOX5 > 0.8 ~ "OE")) %>% na.omit %>% 
    group_by(Type,To) %>% summarise(Mean=mean(Divisions), SD=sd(Divisions)/sqrt(n())*0.5) %>%  ggplot(aes(fill=To, group=To)) + 
    geom_bar(aes(Type, Mean), stat="identity", position = position_dodge(width=0.9)) + 
    geom_errorbar(aes(Type, ymin=Mean-SD, ymax=Mean+SD), width=0.25, position = position_dodge(width=0.9)) +
    theme(axis.title.x = element_blank(), text=element_text(size = 24), legend.position = c(0.85,0.85), legend.background = element_rect(colour = "black")) + 
    ylab("QC Division (%)") + ylim(c(0, 1)) + labs(fill='IAA fb') 

