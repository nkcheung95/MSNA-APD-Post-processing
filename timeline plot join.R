# Load necessary packages
library(png)
library(ggplot2)
library(grid)
library(gridExtra)
# Select text file
txt_file <- file.choose()
if (nzchar(txt_file)) {
  cat("Text file selected:", txt_file, "\n")
} else {
  cat("No text file selected.\n")
}

# Select PNG file
png_file <- file.choose()
if (nzchar(png_file)) {
  cat("PNG file selected:", png_file, "\n")
} else {
  cat("No PNG file selected.\n")
}

# Read text file
if (nzchar(txt_file)) {
  text_data <- readLines(txt_file)
  assign("text_data", text_data, envir = .GlobalEnv)
}

# Read PNG file
if (nzchar(png_file)) {
  png_data <- readPNG(png_file)
  assign("png_data", png_data, envir = .GlobalEnv)
}
cluster_timeline <- rasterGrob(png_data, interpolate=T)
df <- read.delim(txt_file,sep="\t")
colnames(df)<-c("time","resp_belt","ECG","REBAP","MSNA_raw","MSNA_int")
total_time <- max(df$time)-min(df$time)

resp_plot <- ggplot(df,aes(x=time,y=resp_belt))+
  geom_line()+
  guides(fill = "none")+
  theme_classic()+
  theme(axis.ticks = element_blank(),axis.text = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))+
  ylab("")+
  xlab("")
ECG_plot <- ggplot(df,aes(x=time,y=ECG))+
  geom_line(color="red")+
  guides(fill = "none")+
  theme_classic()+
  theme(axis.ticks = element_blank(),axis.text = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))+
  ylab("")+
  xlab("")
BP_plot <- ggplot(df,aes(x=time,y=REBAP))+
  geom_line(color="blue")+
  guides(fill = "none")+
  theme_classic()+
  theme(axis.ticks = element_blank(),axis.text = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))+
  ylab("")+
  xlab("")        
MSNA_plot <- ggplot(df,aes(x=time,y=MSNA_int))+
  geom_line(color="green")+
  guides(fill = "none")+
  theme_classic()+
  theme(axis.ticks = element_blank(),axis.text = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))+
  ylab("")+
  xlab("") 
raw_plot <- ggplot(df,aes(x=time,y=MSNA_raw))+
  geom_line(color="grey")+
  guides(fill = "none")+
  theme_classic()+
  theme(axis.ticks = element_blank(),axis.text = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))+
  ylab("")+
  xlab("") 
raw_plot
combined_traces <- grid.arrange(resp_plot, ECG_plot,BP_plot,MSNA_plot,raw_plot,ncol=1)
timeline_combined <- grid.arrange(combined_traces,cluster_timeline,ncol=1)
ggsave("timeline_combined.png",timeline_combined,width=30,height=20)
