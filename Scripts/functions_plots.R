#### Custom functions

#Function for generating a custom designed violin plot (f1 methods)
violin_plot_custom_hamza <- function(df,param,lab1,lab2) { #only need to add dataframe, parameter, and labels for axes
  p <- ggplot(data = df,aes(x = Tank, y = param, fill = Tank))+
    # scale_fill_manual(values=c("#1F968BFF","#74D055FF", "39568CFF","#440154FF"))+ 
    scale_fill_manual(values=c("#009E73","#E69F00","#56B4E9","#999999"))+#color scheme
    geom_violin(alpha=0.4, position = position_dodge(width = .75),size=0.5,color="black")+ #violin plot to display distribution density
    geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=0.9, alpha = 0.5)+#boxplot to display quantiles
    geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+#scatterplot of individual data points
    theme_pubr(base_family = "")+
    labs(title=lab1,y = lab2)+#labels
    rremove("legend.title")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
    axis.ticks = element_line(size=2,color="black"),axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.92, 0.85))+
    font("xylab",size=13)+font("xy",size=13)+
    font("xy.text", size = 13)+
    font("legend.text",size = 13)+
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank())+
    theme(text=element_text(family="Times New Roman", size=12))
  return(p)
  
}


violin_plot_custom_hamza2 <- function(df,param,lab1,lab2) { #only need to add dataframe, parameter, and labels for axes
  ggplot(data = df,aes(x = Tank, y = param, fill = Tank))+
    # scale_fill_manual(values=c("#1F968BFF","#74D055FF", "39568CFF","#440154FF"))+ #color scheme
  geom_violin(alpha=0.4, position = position_dodge(width = .75),size=0.5,color="black")+ #violin plot to display distribution density
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=0.9, alpha = 0.5)+#boxplot to display quantiles
  geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+#scatterplot of individual data points
  theme_pubr()+
  labs(title=lab1,y = lab2)+#labels
  rremove("legend.title")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        axis.ticks = element_line(size=2,color="black"),
        axis.ticks.length=unit(0.2,"cm"),legend.position = c(0.92, 0.85))+
    font("xylab",size=15)+font("xy",size=15)+font("xy.text", size = 15) +
    font("legend.text",size = 15)+
    theme(legend.position = "none")+
  theme(axis.title.x=element_blank())+
    scale_fill_manual(values = c("#009E73","#E69F00","#56B4E9","#999999"), name = "Tank", 
                      labels = (c("F1C", "F1M", "F1P", "F1T")))
  
}
