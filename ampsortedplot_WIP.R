#Combined analysis
#Amplitude sorted cluster plot
#baseline file_choose
#ask for 100% basemine mean
bl_mean_amp <- as.numeric(readline("Enter the mean baseline burst amplitude in V: "))
amp_sort_list <- list()
sort_amp$normal_amp <-(bl_mean_amp/sort_amp$`Burst Amp`)*100
#overall
sort_amp$sort <- as.factor(sort_amp$sort)

burstsort_plot <- ggplot(sort_amp,aes(x=sort,y=normal_amp))+
  geom_bar()

timeline_plot <- grid.arrange(grobs=timeline_list,ncol=1)
timelength <- length(combined_grobs_list)
ggsave("cluster_timeline.png",timeline_plot,path=plots_folder,height=timelength,width=30)