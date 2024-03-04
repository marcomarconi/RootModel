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

df <- read_csv("~/trabajo/Models/RootModel/WOX5/WOX5_2.csv")
df$Type <- factor(df$Type, levels=c("WT", "wox5", "OE"))
group_by(df, Type, Induction) %>% summarise(Mean=sum(Divisions)/(3*n()), SD=sd(Divisions)*2/sqrt(n()))
group_by(df, Type) %>% summarise(Mean=sum(Divisions)/(3*n()), SD=sd(Divisions)/3/sqrt(n())*2) %>% ggplot() +
    geom_bar(aes(Type, Mean), stat="identity") + geom_errorbar(aes(Type, ymin=Mean-SD, ymax=Mean+SD), width=0.25) +
    theme(axis.title.x = element_blank()) + ylab("QC Division (%)")
