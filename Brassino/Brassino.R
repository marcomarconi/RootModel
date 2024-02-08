library(tidyverse)
library(TTR)
library(ggthemes)
library(LaplacesDemon)
theme_set(theme_bw())

#for f in `ls output*_`; do grep "Root length [^ ]* Meristem length [^ ]* Time [0-9]*" $f  -o | sed 's/Root length \([0-9\.]*\) Meristem length \([0-9\.]*\) Time \([0-9]*\)/\3,\1,\2/' | uniq | grep -v nan > $f.out; done

# Growth rate
{
dir <- "/home/marco/trabajo/Models/RootModel/Brassino/Brassino_noAuxin_lowEK_2h_10steps//"
setwd(dir)
files <- list()
for(f in list.files(".","*.out")) {
  df <- read_csv(f, show_col_types = FALSE, col_names = FALSE) 
  if(nrow(df)==0) {
      warning(paste("No lines in", f))
      next
  }
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
levels(df$Type) <- c("Scen. 1", "Scen. 3-LO", "Scen. 2", "Scen. 3-UP"); df$Type <- factor(df$Type, c("Scen. 1", "Scen. 2", "Scen. 3-UP", "Scen. 3-LO")); 
levels(df$Delay) <- c("Delay 0", "Delay 20min", "Delay 1.5h", "Delay 3h", "Delay 6h");
df$Time <- df$Time * 0.03
ggplot(df %>% filter(Time < 40 & Basal == "0.01" & WGR == "1.0" & Delay!="Delay 0")) + 
    geom_line(aes(Time, GR, color=Type), linewidth=3) + 
    scale_color_manual(values = c("#014d64", "orange3", "#6794a7", "#01a2d9")) + 
    theme(text=element_text(size=28), axis.text = element_text(size=18), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.width = unit(x = 1, units = "cm")) + facet_wrap(~Delay) + 
    ylim(c(0.5, 1.6)) + ylab("Root Growth Rate")+ xlab("Time (h)")

#ggsave("~/GR.pdf", width = 12, height = 9)
}




#for f in `ls output*_`; do grep trig $f | cut -f 39 -d " " > $f.div; done
# Cell division
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
    
    levels(df$Type) <- c("Scen. 1", "Scen. 3-LO", "Scen. 2", "Scen. 3-UP"); df$Type <- factor(df$Type, c("Scen. 1", "Scen. 2", "Scen. 3-UP", "Scen. 3-LO")); 
levels(df$Delay) <- c("0", "20min", "1.5h", "3h", "6h");

ggplot(df %>% filter(  Basal == "0.01" & WGR == "1.0" & Delay != "0")) + 
        geom_bar(aes(Delay, Count, fill=Type), color="gray1", stat="identity", position = position_dodge()) +    
        theme(text=element_text(size=28), axis.text.x = element_text(size=20, angle = 45, vjust = 0.5),legend.text = element_text(size = 18), legend.title = element_blank()) + 
            xlab("")+  ylab("Cell Divisions")+   scale_fill_manual(values = c("#014d64", "orange3", "#6794a7", "#01a2d9")) 

}


# Some cartoon plots
{
    curve(invlogit((x+50)*10) * invlogit((-x-30)*0.5) * -1, -100, 10, col="blue", lwd=5, xaxt = "n",yaxt = "n", xlab="", ylab="",bty="n"); 
    curve(invlogit((x+50)*10) * invlogit((-x-30)*0.5) * -1 - 0.03, -100, 10, col="blue", lwd=5, lty=2, xaxt = "n",yaxt = "n", xlab="", ylab="",bty="n", add=T); 
    curve(invlogit((x+45)*1) * invlogit((x+30)*-1) - 1 , -100, 10, col="red", lwd=5, xaxt = "n",yaxt = "n", xlab="", ylab="", add = T); 
    curve(invlogit((x+45)*1) * invlogit((x+30)*-1) - 1 - 0.03, -100, 10, col="red", lwd=5, lty=2, xaxt = "n",yaxt = "n", xlab="", ylab="", add = T); 
}
{
    curve(x*0-1, -100, 10, col="blue", lwd=5, xaxt = "n",yaxt = "n", xlab="", ylab="",bty="n", ylim=c(1,-1)); 
    curve(x*0-1+0.03, -100, 10, col="blue", lwd=5, lty=2, xaxt = "n",yaxt = "n", xlab="", ylab="",bty="n", add=T); 
    curve(x*0+1, -100, 10, col="red", lwd=5, xaxt = "n",yaxt = "n", xlab="", ylab="", add = T); 
    curve(x*0+1 + 0.03, -100, 10, col="red", lwd=5, lty=2, xaxt = "n",yaxt = "n", xlab="", ylab="", add = T); 
}    
{
    curve(-invlogit((x+50)*10) * invlogit((-x-47)*2) * -1 -1, -100, 10, col="blue", lwd=5, xaxt = "n",yaxt = "n", xlab="", ylab="",bty="n", ylim=c(1,-1)); 
    curve((invlogit((x+45)*1) * invlogit((x+30)*-1))*-1 + 0.03, -100, 10, col="red", lwd=5, lty=2, xaxt = "n",yaxt = "n", xlab="", ylab="", add = T);
    curve(x*0 , -100, 10, col="red", lwd=5, xaxt = "n",yaxt = "n", xlab="", ylab="", add = T); 
    curve(invlogit((x+50)*10) * invlogit((-x-30)*0.5) * 1 + 0.03 - 1, -100, 10, col="blue", lwd=5, lty=2, xaxt = "n",yaxt = "n", xlab="", ylab="",bty="n", add=T); 
    
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
<<<<<<< HEAD
  df <- do.call(rbind, files)%>% mutate(Top=factor(Top), Delay = fct_reorder(Delay, as.numeric(Delay))) %>% 
    group_by(CellType, Time, Delay, Basal, WGR, Top, Type) %>% summarize(M=mean(Area), SD=sd(Area)/sqrt(n()))
  df %>% filter(Basal == "0.01" & WGR == "10.0" & Delay==1 & Type=="Upper" & CellType %in% c(6,7,8,9,12,13)) %>% ggplot() + 
             geom_line(aes(Time, M, color=Top), linewidth=2) + 
=======
  df <- do.call(rbind, files)%>% mutate(Delay = fct_reorder(Delay, as.numeric(Delay))) %>% 
    group_by(CellType, Time, Delay, Basal, WGR, Top, Type) %>% summarize(M=mean(Area), SD=sd(Area)/sqrt(n()), n())
  df %>% filter(Basal == "0.01" & WGR == "1.0" & CellType==12 & Type=="None") %>% ggplot() + 
             geom_line(aes(Time, M, color=factor(Top)), linewidth=2) + 
>>>>>>> 5064876b6aff965952a5b8b5d748fe07adda5575
             geom_errorbar(aes(Time, ymin=M-SD, ymax=M+SD), color="gray")  + 
      facet_wrap(~CellType) +     theme(text=element_text(size=32))
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

