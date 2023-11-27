library(tidyverse)
library(TTR)
library(ggthemes)
theme_set(theme_bw())

#for f in `ls output*_`; do grep "Root length [^ ]* Meristem length [^ ]* Time [0-9]*" $f  -o | sed 's/Root length \([0-9\.]*\) Meristem length \([0-9\.]*\) Time \([0-9]*\)/\3,\1,\2/' | uniq | grep -v nan > $f.out; done


dir <- "/home/marco/trabajo/Models/RootModel/Brassino/Brassino_New_Basal/"
setwd(dir)
files <- list()
for(f in list.files(".","*.out")) {
  df <- read_csv(f, show_col_types = FALSE, col_names = FALSE) 
  values <- sub("output.", "", f) %>% sub("_.out", "", .) %>% gsub("_", " ",.) %>% strsplit(., " ") %>% unlist
  df$Basal <- values[1]
  df$Type <- values[2] %>% factor()
  df$Delay <- values[3]
  colnames(df) <- c("Time", "Root", "Meristem", "Basal", "Type", "Delay")
  df$GR <- EMA(c(0, diff((df$Root))), 20)
  files[[f]] <- df
}

df <- do.call(rbind, files)
ggplot(df %>% filter(Time > 0 & Basal == 0.1)) + geom_line(aes(Time, GR, color=Type), linewidth=3) + scale_color_colorblind() + 
    theme(text=element_text(size=32)) + facet_wrap(~Delay) #+ ylim(c(0.3, 0.7))



{
dir <- "/home/marco/trabajo/Models/RootModel/"
setwd(dir)
system("grep Brassino _err > _a")
df <- read_csv("_a", col_names = F, show_col_types = F)
colnames(df) <- c("Brassino", "Type", "Mother", "Time", "Top", "Area", "GR", "Signal", "Auxin")
group_by(df, Type, Time=ceiling(Time), Top) %>% filter(Type>5 & !between(Type, 10, 11)) %>% summarize(M=mean(Area), SD=sd(Area)/sqrt(n())) %>% ggplot() + 
  geom_line(aes(Time, M, color=factor(Top)), linewidth=2) + geom_errorbar(aes(Time, ymin=M-SD, ymax=M+SD), color="gray") + 
  facet_wrap(~Type)# + ylim(c(0,0.05))
}
{
  system("grep \"Root length\" _out | cut -f 3 -d \" \" > _b")
  b <- scan("_b")
  plot.ts(diff(b))
}


system("grep Bras _out > _a")
a <- read_csv("_a", col_names=F) %>% mutate(Type=factor(X5), X=ceiling(X4)) %>% filter(!X2 %in% c(0:5, 10:11, 14:15))
z <- group_by(a, Type, X, X2) %>% summarize(Y=median(X6), SD=2*mad(X6)/sqrt(n()), N=n())
ggplot(z %>% filter(X<20)) + geom_line(aes(X, Y, col=Type), linewidth=2) + 
    geom_errorbar(aes(x=X, ymin=Y-SD, ymax=Y+SD), width=0.5, color="gray")+ facet_wrap(~X2) #+ ylim(c(0,0.03))

