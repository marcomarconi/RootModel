library(tidyverse)
library(TTR)

#for f in `ls output*_`; do grep "root length [^ ]* time [0-9]*" $f  -o | sed 's/root length \([0-9\.]*\) time \([0-9]*\)/\2,\1/' | uniq > $f.out; done

dir <- "/home/marco/trabajo/Models/RootModel/Sabatini/Sabatini_QuasExp_TurgorElongation_Tissue/"
setwd(dir)
files <- list()
for(f in c("output._0.1_0.80_All_.out", "output._1.0_0.80_Endodermis_.out",  "output._1.0_0.80_None_.out",   "output._1.0_0.80_Vascular_.out",
           "output._1.0_0.80_Cortex_.out",  "output._1.0_0.80_Epidermis_.out",   "output._1.0_0.80_Pericycle_.out")) {
  df <- read_csv(f)
  colnames(df) <- c("Time", "Root", "Meristem")
  df$GR <- EMA(c(0, diff(log(df$Meristem))))
  files[[f %>% sub("output.*_([A-Z].*)_\\.out", "\\1", .)]] <- df
}
full_df <- Reduce(function(...) full_join(..., by = "Time"), files) %>% arrange(Time)# %>% na.omit
full_df <- full_df[, grep("Time|GR", colnames(full_df))]
colnames(full_df) <- c("Time", names(files))
matplot(full_df[,-1] %>% tail(20), type="o")
