library(tidyverse)
library(TTR)

#for f in `ls output*_`; do grep "root length [^ ]* meristem_length [^ ]* time [0-9]*" $f  -o | sed 's/root length \([0-9\.]*\) meristem_length \([0-9\.]*\) time \([0-9]*\)/\3,\1,\2/' | uniq > $f.out; done


dir <- "/home/marco/trabajo/Models/RootModel/Sabatini/Sabatini_QuasExp_TurgorElongation_Tissue_2hours/"
setwd(dir)
files <- list()
for(f in c("output._0.1_0.80_All_.out",
           "output._1.0_0.80_Endodermis_.out",
           "output._1.0_0.80_None_.out",
           "output._1.0_0.80_Vascular_.out",
           "output._1.0_0.80_Cortex_.out",
           "output._1.0_0.80_Epidermis_.out",
           "output._1.0_0.80_Pericycle_.out")) {
  df <- read_csv(f)
  colnames(df) <- c("Time", "Root", "Meristem")
  df$GR <- EMA(c(0, diff((df$Root))), 100)
  files[[f %>% sub("output.*_([A-Z].*)_\\.out", "\\1", .)]] <- df
}
full_df <- Reduce(function(...) full_join(..., by = "Time"), files) %>% arrange(Time) %>% na.omit
GR <- full_df[, grep("Time|GR", colnames(full_df))]
colnames(GR) <- c("Time", names(files))
matplot(GR[,-1] , type="o")
Root <- full_df[, grep("Time|Root", colnames(full_df))]
colnames(Root) <- c("Time", names(files))
m <- rowMeans(Root[,-1]) 
s <- apply(Root[,-1],1,sd) 
matplot((Root[,-1] - 0) / 1, type="o")
Mer <- full_df[, grep("Time|Meristem", colnames(full_df))]
colnames(Mer) <- c("Time", names(files))
matplot(Mer[,-1], type="o")
