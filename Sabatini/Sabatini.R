library(tidyverse)
library(TTR)
library(reshape2)
theme_set(theme_bw())

# NOTES: 
# the  model runs without auxin, we set walls EK to 0.20, quasimodo relax the walls even further
# the trick is in the Turgor Pressure non-Meristem Reduction option, it looks that 0.80 gives good results. We need to reduce the elongation zone turgor pressure otherwise it overcome the meristem and we see no difference between mutants
# peridioc remesh seems necessary, a remeshiing of 40 seems ok

#for f in `ls output*_`; do grep -i "root length [^ ]* meristem length [^ ]* time [0-9]*" $f  -o | sed 's/root length \([0-9\.]*\) meristem_length \([0-9\.]*\) time \([0-9]*\)/\3,\1,\2/' | uniq > $f.out; done


dir <- "/home/marco/trabajo/Models/RootModel/Sabatini/Sabatini_WT_background_yesremesh/"
dir <- "/home/marco/trabajo/Models/RootModel/Sabatini/Sabatini_QuasExp_TurgorElongation_Tissue_YesRemesh_2hours_LRC100_4K/"

setwd(dir)
files <- list()
for(f in c(
           # "output._0.1_0.80_All_.out",
           # "output._1.0_0.80_Vascular_.out",
           # "output._1.0_0.80_Pericycle_.out",
           # "output._1.0_0.80_Endodermis_.out",
           # "output._1.0_0.80_Cortex_.out",
           # "output._1.0_0.80_Epidermis_.out",
           # "output._0.5_0.80_EpidermisCortex_.out",
           # "output._1.0_0.80_None_.out"
    "output._0.5_0.5_Meristem_.out",
    "output._0.5_0.5_VascularEZ_.out",
    "output._0.5_0.5_VascularMeristem_.out",
    "output._0.5_0.5_EpidermisCortexEZ_.out",
    "output._0.5_0.5_EpidermisCortexMeristem_.out",
    "output._0.5_0.5_EpidermisEZ_.out",
    "output._0.5_0.5_EpidermisMeristem_.out",
    "output._0.5_0.5_None_.out",
    "output._0.0_0.5_None_.out"
    
           )) {
  print(f)    
  df <- read_csv(f, show_col_types = F) %>% head(-10)
  colnames(df) <- c("Time", "Root", "Meristem")
  df$GR <- EMA(c(NA, diff((df$Root))), 1)
  #files[[f %>% sub("output.*_([A-Z].*)_\\.out", "\\1", .)]] <- df
  files[[f]] <- df
}

#n <- c("All", "Vascular", "Pericycle", "Endodermis", "Cortex", "Epidermis", "Epidermis/Cortex", "None")
n <- c("Meristem", "VascularEZ", "VascularMeristem", "EpidermisCortexEZ", "EpidermisCortexMeristem",        "EpidermisEZ", "EpidermisMeristem", "WT", "Mutant")
full_df <- Reduce(function(...) full_join(..., by = "Time"), files) %>% arrange(Time) 
GR <- full_df[, grep("Time|GR", colnames(full_df))]
colnames(GR) <- c("Time", n)
matplot(GR[,-1]  , type="o")
first <- GR %>% melt(id.vars = c("Time"), variable.name = "Tissue") %>% mutate(Measure="GR") 
Root <- full_df[, grep("Time|Root", colnames(full_df))]
colnames(Root) <- c("Time", n)
m <- rowMeans(Root[,-1]) 
s <- apply(Root[,-1],1,sd) 
matplot(Root[,1], (Root[,-1] - 0) / 1, type="o")
second <- Root %>% melt(id.vars = c("Time"), variable.name = "Tissue")  %>% mutate(Measure="Root") 
Mer <- full_df[, grep("Time|Meristem", colnames(full_df))]
colnames(Mer) <- c("Time", n)
matplot(Mer[,1], Mer[,-1], type="o")
third <- Mer %>% melt(id.vars = c("Time"), variable.name = "Tissue")  %>% mutate(Measure="Meristem") 
final_df <- rbind(first, second, third)

filter(final_df, Measure=="GR" & Time < 1000) %>% group_by(Tissue) %>% summarise(M=mean(value, na.rm=T), S=1*sd(value, na.rm=T)) %>% 
  ggplot() + geom_bar(aes(Tissue, M), stat = "identity", fill="purple") + geom_errorbar(aes(Tissue, ymin=M-S, ymax=M+S), width=0.25) + 
    theme(text = element_text(size=28), axis.title.x = element_blank(), axis.text.x = element_text( hjust = 1, angle = 45)) + 
    ylab("Growth Rate") + ggtitle("Root Growth Rate")

filter(final_df, (Measure=="Meristem") & Time < 1000) %>% group_by(Tissue, Measure) %>% summarise(value=last(value %>% na.omit)) %>% 
  ggplot() + geom_bar(aes(Tissue, value), position = "dodge", stat = "identity") + theme(text = element_text(size=28), axis.title.x = element_blank(), axis.text.x = element_text( hjust = 1, angle = 45)) + ylab("Relative Size") 

final_df %>% filter(Measure=="Meristem"& Time > 1000) %>% group_by(Time) %>% summarise(value=value/max(value, na.rm=T), Tissue=Tissue) %>% 
    group_by(Tissue) %>% summarise(M=mean(value, na.rm=T), S=1*sd(value, na.rm=T)) %>% 
    ggplot() + geom_bar(aes(Tissue, M), position = "dodge", stat = "identity", fill="magenta")+ geom_errorbar(aes(Tissue, ymin=M-S, ymax=M+S), width=0.25)  + 
    theme(text = element_text(size=28), axis.title.x = element_blank(), axis.text.x = element_text( hjust = 1, angle = 45)) + 
    ylab("Size Relative to Longest") + ggtitle("Meristem length")

filter(final_df %>% na.omit, (Measure=="Root") & Time ==2000) %>% group_by(Tissue, Measure) %>% summarise(value=last(value )) %>% 
    ggplot() + geom_bar(aes(Tissue, value), position = "dodge", stat = "identity") + theme(text = element_text(size=28), axis.title.x = element_blank(), axis.text.x = element_text( hjust = 1, angle = 45)) + ylab("Relative Size") 
final_df %>% filter(Measure=="Root"& Time < 1000) %>% group_by(Time) %>% summarise(value=value/max(value, na.rm=T), Tissue=Tissue) %>% 
    group_by(Tissue) %>% summarise(M=mean(value, na.rm=T), S=1*sd(value, na.rm=T)) %>% 
    ggplot() + geom_bar(aes(Tissue, M), position = "dodge", stat = "identity", fill="firebrick")+
    geom_errorbar(aes(Tissue, ymin=M-S, ymax=M+S), width=0.25)  + 
    theme(text = element_text(size=32), axis.title.x = element_blank(), axis.text.x = element_text( hjust = 1, angle = 45)) + 
    ylab("Size Relative to Longest") + ggtitle("Root length")
