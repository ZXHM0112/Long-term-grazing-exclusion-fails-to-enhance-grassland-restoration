

setwd("E:/华为家庭存储/电脑办公资料/7-期刊论文/7-长期禁牧对植物-土壤-微生物的影响/图表20251015/20251219")
library(ggplot2)
data <-read.csv(file.choose())
data$Group <- factor (data$Group, levels = c("EDG","TGG","SGE","LGE","NDG"))
P1<-ggplot(data,aes(Group,AGB))+
  geom_point(aes(color=Group),size=3)+
  geom_errorbar(aes(ymin = AGB-se1, ymax = AGB+se1,color=Group),
                width = 0.2,position = position_dodge(width = 0.8),cex=0.7)+ #添加误差棒
  labs(x=" ",y="Temporal stability of above-ground biomass")+
  theme_bw()+#主题
  theme(panel.grid=element_blank(),
        axis.text.y=element_text(color='black',size=13),
        axis.text.x=element_text(color='black',size=13), #axis.text.x=element_blank()为去掉横坐标
        #axis.ticks.x = element_blank(),
        #legend.text = element_text(color='black',size=12),
        #legend.title = element_text(color='red',size=13),
        legend.position = "none" ,
       strip.background.x = element_rect(fill = "#0081b4", color = "black"))+
  scale_y_continuous(expand = c(0, 0), limit = c(0, 12))+#去除网格线
  scale_color_manual(values=c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P2<-ggplot(data,aes(Group,Asynchrony))+
  geom_point(aes(color=Group),size=3)+
  geom_errorbar(aes(ymin = Asynchrony-se2, ymax = Asynchrony+se2,color=Group),
                width = 0.2,position = position_dodge(width = 0.8),cex=0.7)+ #添加误差棒
  labs(x=" ",y="Species asynchrony")+
  theme_bw()+#主题
  theme(panel.grid=element_blank(),
        axis.text.y=element_text(color='black',size=13),
        axis.text.x=element_text(color='black',size=13), #axis.text.x=element_blank()为去掉横坐标
        #axis.ticks.x = element_blank(),
        #legend.text = element_text(color='black',size=12),
        #legend.title = element_text(color='red',size=13),
        legend.position = "none" ,
        strip.background.x = element_rect(fill = "#0081b4", color = "black"))+
  scale_y_continuous(expand = c(0, 0), limit = c(0, 1.2),breaks = seq(0, 1.2, 0.3))+#去除网格线
  scale_color_manual(values=c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))
ggsave('地上生物量时间稳定性-20251219.pdf',P1, width = 4, height = 3)
ggsave('物种异步性-20251219.pdf',P2, width = 4, height = 3)

library(ggbeeswarm)
data <-read.csv(file.choose())
data$Group <- factor (data$Group, levels = c("EDG","TGG","SGE","LGE","NDG"))
P3<-ggplot(data,aes(Group,MAOC))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,30),breaks = c(0,10,20,30))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="MAOC(g kg-1)")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P4<-ggplot(data,aes(Group,POC))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,50),breaks = c(0,10,20,30,40,50))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="POC(g kg-1)")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

#P5<-ggplot(data,aes(Group,MAOC.POC))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,6),breaks = c(0,1.5,3,4.5,6))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="MAOC:POC")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P5<-ggplot(data,aes(Group,MAOC.SOC))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,100),breaks = c(0,20,40,60,80,100))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="MAOC:SOC")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P6<-ggplot(data,aes(Group,BNC))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,2),breaks = c(0,0.5,1,1.5,2))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="BNC(mg g-1)")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P7<-ggplot(data,aes(Group,FNC))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,10),breaks = c(0,2,4,6,8,10))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="FNC(mg g-1)")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P8<-ggplot(data,aes(Group,Amino ))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,15),breaks = c(0,3,6,9,12,15))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="Amino(mg g-1)")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P9<-ggplot(data,aes(Group,urease ))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,4),breaks = c(0,1,2,3,4))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="Urease(mg g-1h-1)")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P10<-ggplot(data,aes(Group,sucrase ))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,35),breaks = c(0,7,14,21,28,35))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="Sucrase(mg g-1h-1)")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))

P11<-ggplot(data,aes(Group,catalalse ))+
  geom_boxplot(aes(color=Group),width=0.4,size=0.7,outlier.color = NA)+
  geom_beeswarm(aes(color=Group),shape=21,dodge.width=0.8)+
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+
  scale_y_continuous(limits = c(0,2.5),breaks = c(0,0.5,1,1.5,2,2.5))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black",size = 13),
        legend.position = "none",
        axis.ticks = element_line(color="black",linewidth = 1))+
  labs(x=NULL,y="Catalalse(ml g-1)")+
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))
library(ggpubr)
P12<-ggarrange(P1,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,ncol = 3,nrow = 4)
ggsave('土壤性质-20251219.pdf',P12, width = 9, height = 12)
