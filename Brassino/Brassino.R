library(tidyverse)
library(TTR)
library(ggthemes)
theme_set(theme_bw())

#for f in `ls output*_`; do grep "Root length [^ ]* Meristem length [^ ]* Time [0-9]*" $f  -o | sed 's/Root length \([0-9\.]*\) Meristem length \([0-9\.]*\) Time \([0-9]*\)/\3,\1,\2/' | uniq | grep -v nan > $f.out; done

{
dir <- "/home/marco/trabajo/Models/RootModel/Brassino/Brassino_WallEK_GR_noFixed_2h//"
setwd(dir)
files <- list()
for(f in list.files(".","*.out")) {
  df <- read_csv(f, show_col_types = FALSE, col_names = FALSE) 
  values <- sub("output.", "", f) %>% sub("_.out", "", .) %>% gsub("_", " ",.) %>% strsplit(., " ") %>% unlist
  df$Wall <- values[1] %>% factor()
  df$Type <- values[2] %>% factor()
  df$Delay <- values[3] 
  df$WGR <- values[4] %>% factor()
  colnames(df) <- c("Time", "Root", "Meristem", "Wall", "Type", "Delay", "WGR")
  df$GR <- EMA(c(0, diff((df$Root))), 30)
  files[[f]] <- df
}
df <- do.call(rbind, files) %>% mutate(Delay = fct_reorder(Delay, as.numeric(Delay)))
ggplot(df %>% filter(Time > 0 & Wall == "0.01" & WGR == "1.0")) + geom_line(aes(Time, GR, color=Type), linewidth=3) + scale_color_colorblind() + 
    theme(text=element_text(size=32)) + facet_wrap(~Delay) + ylim(c(0, 1.6))
}

#for f in `ls output*_`; do grep trig $f | cut -f 39 -d " " > $f.div; done
{
files <- list()
for(f in list.files(".","*.div")) {
  df <- read_csv(f, show_col_types = FALSE, col_names = FALSE) 
  values <- sub("output.", "", f) %>% sub("_.out", "", .) %>% gsub("_", " ",.) %>% strsplit(., " ") %>% unlist
  df$Wall <- values[1]
  df$Type <- values[2] %>% factor()
  df$Delay <- values[3] 
  df$WGR <- values[4] %>% factor()
  colnames(df) <- c("Time", "Wall", "Type", "Delay", "WGR")
  files[[f]] <- df 
}
df <- do.call(rbind, files) %>% filter(Time < 1000 & Time > 250)%>% mutate(Delay = fct_reorder(Delay, as.numeric(Delay))) %>% group_by(Type, Delay, Wall, WGR) %>% summarize(Count=n())
ggplot(df %>% filter(  Wall == "0.01" & WGR == "1.0")) + geom_bar(aes(Type, Count), stat="identity") + facet_wrap(~Delay) +     theme(text=element_text(size=32))
}

# 
# #for f in `ls output*_`; do grep ^Brassino $f  > $f.brassino; done
# {
#   files <- list()
#   for(f in list.files(".","*.brassino")) {
#     df <- read_csv(f, show_col_types = FALSE, col_names = FALSE) 
#     values <- sub("output.", "", f) %>% sub("_.out", "", .) %>% gsub("_", " ",.) %>% strsplit(., " ") %>% unlist
#     df$Wall <- values[1]
#     df$Type <- values[2] %>% factor()
#     df$Delay <- values[3] 
#     df$WGR <- values[4] %>% factor()
#     colnames(df) <- c("Brassino", "CellType", "Mother", "Last", "Top", "Area", "CellGR", "Signal", "Auxin", "Wall", "Type", "Delay", "WGR")
#     files[[f]] <- df %>% filter(CellType==12) %>% group_by(Type, Delay, Wall, WGR) %>% summarize(Area=mean(Area), .groups="drop")
#   }
#   df <- do.call(rbind, files)%>% mutate(Delay = fct_reorder(Delay, as.numeric(Delay))) %>% group_by(Type, Delay, Wall, WGR) %>% summarize(Area=mean(Area))
#   ggplot(df %>% filter(  Wall == "0.01" & WGR == "1.0")) + geom_bar(aes(Type, Area), stat="identity") + facet_wrap(~Delay) +     theme(text=element_text(size=32))
# }
# 


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


r <- rep(10000, NA)
r[1] <- 0
for()
