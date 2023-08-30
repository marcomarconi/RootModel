library(tidyverse)
library(TTR)
theme_set(theme_bw())

#for f in `ls output*_`; do grep "root length [^ ]* meristem_length [^ ]* time [0-9]*" $f  -o | sed 's/root length \([0-9\.]*\) meristem_length \([0-9\.]*\) time \([0-9]*\)/\3,\1,\2/' | uniq > $f.out; done


dir <- "/home/marco/trabajo/Models/RootModel/Brassino/Brassino2/"
setwd(dir)
files <- list()
for(f in list.files(".", pattern = ".*0\\.2.*False.*out")) {
  df <- read_csv(f) %>% head(-10)
  colnames(df) <- c("Time", "Root", "Meristem")
  df$GR <- EMA(c(0, diff((df$Root))), 10)
  files[[f ]] <- df
}

n <- c("All", "Vascular", "Pericycle", "Endodermis", "Cortex", "Epidermis", "Epidermis/Cortex", "None") # no
full_df <- Reduce(function(...) full_join(..., by = "Time"), files) %>% arrange(Time) 
GR <- full_df[, grep("Time|GR", colnames(full_df))]
colnames(GR) <- c("Time", n)
matplot(GR[,-1]  , type="o")
first <- GR %>% melt(id.vars = c("Time"), variable.name = "Tissue") %>% mutate(Measure="GR") 
Root <- full_df[, grep("Time|Root", colnames(full_df))]
colnames(Root) <- c("Time", n)
matplot(Root[,1], (Root[,-1] - 0) / 1, type="o")
second <- Root %>% melt(id.vars = c("Time"), variable.name = "Tissue")  %>% mutate(Measure="Root") 
Mer <- full_df[, grep("Time|Meristem", colnames(full_df))]
colnames(Mer) <- c("Time", n)
matplot(Mer[,1], Mer[,-1], type="o")
third <- Mer %>% melt(id.vars = c("Time"), variable.name = "Tissue")  %>% mutate(Measure="Meristem") 
