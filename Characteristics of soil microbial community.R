setwd("E:/华为家庭存储/电脑办公资料/7-期刊论文/7-长期禁牧对植物-土壤-微生物的影响/图表20251015/20251219")
library(ggplot2)
library(ggpubr)
library(gghalves)
data <-read.csv(file.choose())
data$Group <- factor (data$Group, levels = c("EDG","TGG","SGE","LGE","NDG"))
ordercolors<-c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")

p1<-ggplot(data,aes(Group,complexity.b))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  #geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,0.8),breaks = c(0,0.2,0.4,0.6,0.8))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="complexity.b")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))
p2<-ggplot(data,aes(Group,complexity.f))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  #geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,0.8),breaks = c(0,0.2,0.4,0.6,0.8))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="complexity.f")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))


p3 <- ggplot(data, aes(x = Group, y = AVD.b)) + 
  geom_violin(aes(fill = Group, color = Group), trim = FALSE, alpha = 0.7) +  # 设置小提琴图的透明度
  geom_boxplot(aes(fill = Group), notch = FALSE, width = 0.1, outlier.colour = NA, alpha = 0.7) +  # 设置箱线图的透明度
  #geom_jitter(aes(fill =Group1,color=Group1, position=position_jitter(0.1), size=1,color="white"))+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) + 
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  theme_bw() + theme(text = element_text(size=13))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none",
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size=13, color = "black")) +
  labs(x = " ", y = "Average varition degree") +scale_y_continuous(limits = c(0, 1.0), expand = c(0, 0)) 

p4 <- ggplot(data, aes(x = Group, y = AVD.f)) + 
  geom_violin(aes(fill = Group, color = Group), trim = FALSE, alpha = 0.7) +  # 设置小提琴图的透明度
  geom_boxplot(aes(fill = Group), notch = FALSE, width = 0.1, outlier.colour = NA, alpha = 0.7) +  # 设置箱线图的透明度
  #geom_jitter(aes(fill =Group1,color=Group1, position=position_jitter(0.1), size=1,color="white"))+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) + 
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  theme_bw() + theme(text = element_text(size=13))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none",
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size=13, color = "black")) +
  labs(x = " ", y = "Average varition degree") +scale_y_continuous(limits = c(0, 1.0), expand = c(0, 0)) 
library(ggpubr)
p12<-ggarrange(p1,p2,p10,p11,ncol = 2,nrow = 2)
ggsave('微生物网络-20251219.pdf',p12, width = 8, height = 6)























##云雨图
p10<-ggplot(data = data,
           aes(x=Group, y=AVD.b, fill=Group)) +
  geom_half_violin(side = "l", color=NA, trim=F,alpha=1) +
  geom_half_boxplot(side = "l", errorbar.draw = FALSE, width=0.2, linewidth=0.3,outlier.color = NA) +
  #geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 1.0), expand = c(0, 0)) +
  scale_x_discrete(labels = c("EDG","TGG","SGE","LGE","NDG")) +
  labs(y="AVD",x=NULL) +
  theme_bw() +
  theme(legend.position = " ",
        panel.grid = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size=13, color = "black"))

p11<-ggplot(data = data,
            aes(x=Group, y=AVD.f, fill=Group)) +
  geom_half_violin(side = "r", color=NA, trim=F,alpha=1) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.3,outlier.color = NA) +
  #geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(0, 1.0), expand = c(0, 0)) +
  scale_x_discrete(labels = c("EDG","TGG","SGE","LGE","NDG")) +
  labs(y="AVD",x=NULL) +
  theme_bw() +
  theme(legend.position = " ",
        panel.grid = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size=13, color = "black"))






