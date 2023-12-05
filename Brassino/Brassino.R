library(tidyverse)
library(TTR)
library(ggthemes)
theme_set(theme_bw())

#for f in `ls output*_`; do grep "Root length [^ ]* Meristem length [^ ]* Time [0-9]*" $f  -o | sed 's/Root length \([0-9\.]*\) Meristem length \([0-9\.]*\) Time \([0-9]*\)/\3,\1,\2/' | uniq | grep -v nan > $f.out; done

{
dir <- "/home/marco/trabajo/Models/RootModel/Brassino/Brassino_Auxin_Basal_2h_30steps/"
setwd(dir)
files <- list()
for(f in list.files(".","*.out")) {
  df <- read_csv(f, show_col_types = FALSE, col_names = FALSE) 
  values <- sub("output.", "", f) %>% sub("_.out", "", .) %>% gsub("_", " ",.) %>% strsplit(., " ") %>% unlist
  df$Basal <- values[1] %>% factor()
  df$Type <- values[2] %>% factor()
  df$Delay <- values[3] 
  df$WGR <- values[4] %>% factor()
  colnames(df) <- c("Time", "Root", "Meristem", "Basal", "Type", "Delay", "WGR")
  df$GR <- EMA(c(0, diff((df$Root))), 30)
  files[[f]] <- df
}
df <- do.call(rbind, files) %>% mutate(Delay = fct_reorder(Delay, as.numeric(Delay)))
ggplot(df %>% filter(Time > 0 & Basal == "0.01" & WGR == "1.0")) + geom_line(aes(Time, GR, color=Type), linewidth=3) + scale_color_colorblind() + 
    theme(text=element_text(size=32)) + facet_wrap(~Delay) + ylim(c(0, 1.6))
}




#for f in `ls output*_`; do grep trig $f | cut -f 39 -d " " > $f.div; done
{
files <- list()
for(f in list.files(".","*.div")) {
  df <- read_csv(f, show_col_types = FALSE, col_names = FALSE) 
  values <- sub("output.", "", f) %>% sub("_.out", "", .) %>% gsub("_", " ",.) %>% strsplit(., " ") %>% unlist
  df$Basal <- values[1]
  df$Type <- values[2] %>% factor()
  df$Delay <- values[3] 
  df$WGR <- values[4] %>% factor()
  colnames(df) <- c("Time", "Basal", "Type", "Delay", "WGR")
  files[[f]] <- df 
}
df <- do.call(rbind, files) %>% filter(Time < 900 & Time > 300)%>% mutate(Delay = fct_reorder(Delay, as.numeric(Delay))) %>% 
    group_by(Type, Delay, Basal, WGR) %>% summarize(Count=n())
ggplot(df %>% filter(  Basal == "0.01" & WGR == "1.0")) + geom_bar(aes(Type, Count), stat="identity") + facet_wrap(~Delay) +     theme(text=element_text(size=32))
}


#for f in `ls output*_`; do grep ^Brassino $f  > $f.brassino; done
{
  files <- list()
  for(f in list.files(".","*.brassino")) {
    df <- read_csv(f, show_col_types = FALSE, col_names = FALSE)
    values <- sub("output.", "", f) %>% sub("_.out", "", .) %>% gsub("_", " ",.) %>% strsplit(., " ") %>% unlist
    df$Basal <- values[1]
    df$Type <- values[2] %>% factor()
    df$Delay <- values[3]
    df$WGR <- values[4] %>% factor()
    colnames(df) <- c("Brassino", "CellType", "Mother", "Last", "Top", "Area", "CellGR", "Signal", "Auxin", "Basal", "Type", "Delay", "WGR")
    files[[f]] <- df %>% group_by(Type, Delay, Basal, WGR, CellType, Time=ceiling(Last), Top) %>% 
      summarize(Area=mean(Area), .groups = "drop")
  }
  df <- do.call(rbind, files)%>% mutate(Delay = fct_reorder(Delay, as.numeric(Delay))) %>% 
    group_by(CellType, Time, Delay, Basal, WGR, Top, Type) %>% summarize(M=mean(Area), SD=sd(Area)/sqrt(n()), n())
  df %>% filter(Basal == "0.01" & WGR == "1.0" & CellType==12 & Type=="None") %>% ggplot() + 
             geom_line(aes(Time, M, color=factor(Top)), linewidth=2) + 
             geom_errorbar(aes(Time, ymin=M-SD, ymax=M+SD), color="gray")  + 
      facet_wrap(~Delay) +     theme(text=element_text(size=32))
}



{
#dir <- "/home/marco/trabajo/Models/RootModel/"
setwd(dir)
df <- read_csv("output.0.01_None_1_10.0_.brassino", col_names = F, show_col_types = F)
colnames(df) <- c("Brassino", "CellType", "Mother", "Last", "Top", "Area", "GR", "Signal", "Auxin")
group_by(df, CellType, Time=ceiling(Last), Top) %>% filter(CellType>5 & !between(CellType, 10, 11)) %>% 
    summarize(M=mean(Area), SD=sd(Area)/sqrt(n())) %>% ggplot() + 
  geom_line(aes(Time, M, color=factor(Top)), linewidth=2) + geom_errorbar(aes(Time, ymin=M-SD, ymax=M+SD), color="gray") + 
  facet_wrap(~CellType)# + ylim(c(0,0.05))
}
{
  system("grep \"Root length\" _out | cut -f 3 -d \" \" > _b")
  b <- scan("_b")
  plot.ts(diff(b))
}

{
system("grep ^Bras _out > _a")
a <- read_csv("_a", col_names=F) %>% mutate(Type=factor(X5), X=ceiling(X4), CellType=X2) %>% filter(!CellType %in% c(0:5, 10:11, 14:15))
z <- group_by(a, Type, X, CellType) %>% summarize(Y=median(X6), SD=2*mad(X6)/sqrt(n()), N=n())
ggplot(z %>% filter(X<20)) + geom_line(aes(X, Y, col=Type), linewidth=2) + 
    geom_errorbar(aes(x=X, ymin=Y-SD, ymax=Y+SD), width=0.5, color="gray")+ facet_wrap(~CellType) #+ ylim(c(0,0.03))
}

