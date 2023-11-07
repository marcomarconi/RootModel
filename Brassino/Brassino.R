library(tidyverse)
library(TTR)
library(ggthemes)
theme_set(theme_bw())

#for f in `ls output*_`; do grep "Root length [^ ]* Meristem length [^ ]* Time [0-9]*" $f  -o | sed 's/Root length \([0-9\.]*\) Meristem length \([0-9\.]*\) Time \([0-9]*\)/\3,\1,\2/' | uniq | grep -v nan > $f.out; done


dir <- "/home/marco/trabajo/Models/RootModel/Brassino/Brassino_Pin_4_eponly/"
setwd(dir)
files <- list()
for(f in list.files(".","*.out")) {
  df <- read_csv(f, show_col_types = FALSE, col_names = FALSE) 
  values <- sub("output.", "", f) %>% sub("_.out", "", .) %>% gsub("_", " ",.) %>% strsplit(., " ") %>% unlist
  df$PIN <- values[1] %>% factor()
  df$Type <- values[2] %>% factor()
  df$Delay <- values[3]
  df$Auxin <- values[4]  %>% factor()
  colnames(df) <- c("Time", "Root", "Meristem", "PIN", "Type", "Delay", "Auxin")
  df$GR <- EMA(c(0, diff((df$Root))), 5)
  files[[f]] <- df
}

df <- do.call(rbind, files)
ggplot(df %>% filter(PIN=="1.0" & Delay=="20" & Auxin=="100"  )) + geom_line(aes(Time, GR, color=Type), linewidth=3) + scale_color_colorblind()
