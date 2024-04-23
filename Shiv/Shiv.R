library(tidyverse)
library(ggthemes)
library(TTR)
library(zoo)
#for f in `ls output*`; do grep growth $f | cut -f 8,11 -d " " | sed 's/ /\t/g' > _x.$f; done

# Growth rate
{
    dir <- "/home/marco/trabajo/Models/RootModel/Shiv/SHIV_Auxin_on_EK_tissues/"
    setwd(dir)
    files <- list()
    for(f in list.files(".","_x.*")) {
        print(f)
        df <- read_tsv(f, show_col_types = FALSE, col_names = FALSE) 
        if(nrow(df)==0) {
            warning(paste("No lines in", f))
            next
        }
        values <- sub("_x.output.", "", f) %>% sub("__$", "",.) %>% gsub("_", " ",.)  %>% strsplit(., " ") %>% unlist
        df$Auxin <- values[1] %>% as.integer()
        df$K <- values[2] %>% as.numeric()
        df$Tissue <- values[3] %>% as.factor()
        colnames(df) <- c("Time", "GR", "Auxin", "L", "Tissue")
        df$GR_EMA <- EMA(df$GR %>% na.locf(na.rm=F), 30)
        files[[f]] <- df
    }
    df <- do.call(rbind, files) 
}

df %>% filter(Auxin==200 & L == 1.0 & Time > 10) %>% ggplot(aes(Time, GR_EMA, color=Tissue)) + geom_line(size=3) + scale_color_colorblind()


    