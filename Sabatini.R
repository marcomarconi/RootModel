library(tidyverse)
library(TTR)
theme_set(theme_bw())

#for f in `ls output*_`; do grep "root length [^ ]* meristem_length [^ ]* time [0-9]*" $f  -o | sed 's/root length \([0-9\.]*\) meristem_length \([0-9\.]*\) time \([0-9]*\)/\3,\1,\2/' | uniq > $f.out; done


dir <- "/home/marco/trabajo/Models/RootModel/Sabatini/Sabatini_QuasExp_TurgorElongation_Tissue_NoRemesh_2hours///"
setwd(dir)
files <- list()
for(f in c("output._0.1_0.80_All_.out",
           "output._1.0_0.80_Vascular_.out",
           "output._1.0_0.80_Pericycle_.out",
           "output._1.0_0.80_Endodermis_.out",
           "output._1.0_0.80_Cortex_.out",
           "output._1.0_0.80_Epidermis_.out",
           "output._1.0_0.80_EpidermisCortex_.out",
           "output._1.0_0.80_None_.out"
           )) {
  df <- read_csv(f)
  colnames(df) <- c("Time", "Root", "Meristem")
  df$GR <- EMA(c(0, diff((df$Root))), 1)
  files[[f %>% sub("output.*_([A-Z].*)_\\.out", "\\1", .)]] <- df
}

n <- c("All", "Vascular", "Pericycle", "Endodermis", "Cortex", "Epidermis", "Epidermis/Cortex", "None")
full_df <- Reduce(function(...) full_join(..., by = "Time"), files) %>% arrange(Time) %>% na.omit
GR <- full_df[, grep("Time|GR", colnames(full_df))]
colnames(GR) <- c("Time", n)
matplot(GR[,-1]  , type="o")
first <- GR %>% melt(id.vars = c("Time"), variable.name = "Tissue") %>% mutate(Measure="GR") 
Root <- full_df[, grep("Time|Root", colnames(full_df))]
colnames(Root) <- c("Time", n)
m <- rowMeans(Root[,-1]) 
s <- apply(Root[,-1],1,sd) 
matplot((Root[,-1] - 0) / 1, type="o")
second <- Root %>% melt(id.vars = c("Time"), variable.name = "Tissue")  %>% mutate(Measure="Root") 
Mer <- full_df[, grep("Time|Meristem", colnames(full_df))]
colnames(Mer) <- c("Time", n)
matplot(Mer[,-1], type="o")
third <- Mer %>% melt(id.vars = c("Time"), variable.name = "Tissue")  %>% mutate(Measure="Meristem") 
final_df <- rbind(first, second, third)
filter(final_df, Measure=="GR" & Time > 100 & Time > 100) %>% group_by(Tissue) %>% summarise(M=mean(value), S=2*sd(value)) %>% 
  ggplot() + geom_bar(aes(Tissue, M), stat = "identity", fill="purple") + geom_errorbar(aes(Tissue, ymin=M-S, ymax=M+S), width=0.25) + theme(text = element_text(size=28), axis.title.x = element_blank(), axis.text.x = element_text( hjust = 1, angle = 45)) + ylab("Growth Rate")
filter(final_df, (Measure=="Meristem") & Time > 100) %>% group_by(Tissue, Measure) %>% summarise(value=last(value)) %>% 
  ggplot() + geom_bar(aes(Tissue, value), position = "dodge", stat = "identity") + theme(text = element_text(size=28), axis.title.x = element_blank(), axis.text.x = element_text( hjust = 1, angle = 45)) + ylab("Relative Size") 
