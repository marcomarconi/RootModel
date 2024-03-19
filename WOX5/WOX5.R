library(tidyverse)
theme_set(theme_bw())

# Do:
# grep -c "type QC" output.*
# the first two values (auxin source and WOX5 basal production) are the replicate, the second OE/WT/mutant the third the division (divide them by 2)
# put these 3 columns into a CSV file

df <- read_csv("~/trabajo/Models/RootModel/WOX5/WOX5.csv")
df$Type <- factor(df$Type, levels=c("WT", "wox5", "OE"))
group_by(df, Type) %>% summarise(Mean=sum(Divisions)/(3*n()), SD=sd(Divisions)*2/sqrt(n()))
group_by(df, Type) %>% summarise(Mean=sum(Divisions)/(3*n()), SD=sd(Divisions)/3/sqrt(n())*2) %>% ggplot() +
  geom_bar(aes(Type, Mean), stat="identity") + geom_errorbar(aes(Type, ymin=Mean-SD, ymax=Mean+SD), width=0.25) +
  theme(axis.title.x = element_blank()) + ylab("QC Division (%)")


df <- read_csv("~/trabajo/Models/RootModel/WOX5/WOX5_3.csv") %>% mutate(Type = as.character(Type), Type = case_when(Type == "1" ~ "WT",Type == "0.001" ~ "OE",Type == "100" ~ "wox5",TRUE~NA), Induction=factor(Induction)) 
df$Type <- factor(df$Type, levels=c("WT", "wox5", "OE"))
df$Divisions <- df$Divisions / 6
res <- group_by(df %>% filter(WOX5 <= 0.2 & Induction == 0), Type) %>% summarise(Mean=mean(Divisions), SD=sd(Divisions)/sqrt(n()))
res %>%   ggplot() +  geom_bar(aes(Type, Mean), stat="identity") + geom_errorbar(aes(Type, ymin=Mean-SD, ymax=Mean+SD), width=0.25) +
  theme(axis.title.x = element_blank(), text=element_text(size = 24)) + ylab("QC Division (%)") + ylim(c(0, 1))
res <- group_by(df %>% filter(WOX5 <= 0.2), Type, Induction) %>% summarise(Mean=mean(Divisions), SD=sd(Divisions)/sqrt(n()))
res %>%  ggplot(aes(fill=Induction)) +
  geom_bar(aes(Type, Mean), stat="identity", position = position_dodge(width=0.9)) + 
  geom_errorbar(aes(Type, ymin=Mean-SD, ymax=Mean+SD), width=0.25, position = position_dodge(width=0.9)) +
  theme(axis.title.x = element_blank(), text=element_text(size = 24), legend.position = c(0.85,0.85), legend.background = element_rect(colour = "black")) + 
  ylab("QC Division (%)") + ylim(c(0, 1)) + labs(fill='IAA fb') 
res <- group_by(df %>% filter(Induction == 1), Type, WOX5) %>% mutate(WOX5 = case_when(between(WOX5, 0, 0.2) ~ "Low", between(WOX5, 0.4, 0.6) ~ "Medium", between(WOX5, 0.8, 1.0) ~ "High",TRUE~NA)) %>% summarise(Mean=mean(Divisions), SD=sd(Divisions)/sqrt(n()))
res %>%  ggplot(aes(fill=factor(WOX5, levels="Low", "Medium", "High"))) +
  geom_bar(aes(Type, Mean), stat="identity", position = position_dodge(width=0.9)) + 
  geom_errorbar(aes(Type, ymin=Mean-SD, ymax=Mean+SD), width=0.25, position = position_dodge(width=0.9)) +
  theme(axis.title.x = element_blank(), text=element_text(size = 24), legend.position = c(0.85,0.85), legend.background = element_rect(colour = "black")) + 
  ylab("QC Division (%)") + ylim(c(0, 1)) + labs(fill='WOX4 prod') 

df <- read_csv("~/trabajo/Models/RootModel/WOX5/WOX5_test.csv")
df  %>% mutate(Divisions=Divisions/6, Type = case_when(Induction == 0 & WOX5 == 0 ~ "wox5", Induction == 1 & between(WOX5, 0.4, 0.8) ~ "WT", Induction == 1 &WOX5 > 0.8 ~ "OE")) %>% na.omit %>% 
    group_by(Type) %>% summarise(Mean=mean(Divisions), SD=sd(Divisions)/sqrt(n())*2)


