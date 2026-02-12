
setwd("E:/华为家庭存储/电脑办公资料/7-期刊论文/7-长期禁牧对植物-土壤-微生物的影响/图表20251015/20251021")
mydata <- read.csv(file.choose(), header = T)
library(ggplot2)


data <-read.csv(file.choose(),row.names = 1)
data$Group <- factor (data$Group, levels = c("EDG","TGG","STG","LTG","NDG"))
p1<-ggplot(data, aes(x = Time, y = AGB)) +
  geom_line(aes(color = Group), linewidth =1) + # 绘制折线
  geom_point(aes(color = Group), size =3) + # 添加数据点
  geom_errorbar(aes(ymin = AGB - se1, ymax = AGB + se1),#设置误差线
                width = 0.1,linewidth = 0.2) + # 误差线横杆宽度
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+ # 自定义颜色
  theme_bw() + theme(text = element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none" ) +
  labs(x = " ", y = "AGB (g m-2)")  # 设置x轴和y轴标签
p1<-p1+ylim(100,500)

p2<-ggplot(data, aes(x = Time, y = BGB_0_30)) +
  geom_line(aes(color = Group), linewidth =1) + # 绘制折线
  geom_point(aes(color = Group), size =3) + # 添加数据点
  geom_errorbar(aes(ymin = BGB_0_30 - se2, ymax = BGB_0_30 + se2),#设置误差线
                width = 0.1,linewidth = 0.2) + # 误差线横杆宽度
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+ # 自定义颜色
  theme_bw() + theme(text = element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none" ) +
  labs(x = " ", y = "BGB (g m-2)")  # 设置x轴和y轴标签
p2<-p2+ylim(0,10000)


p3<-ggplot(data, aes(x = Time, y = SOC)) +
  geom_line(aes(color = Group), linewidth =1) + # 绘制折线
  geom_point(aes(color = Group), size =3) + # 添加数据点
  geom_errorbar(aes(ymin = SOC - se3, ymax = SOC + se3),#设置误差线
                width = 0.1,linewidth = 0.2) + # 误差线横杆宽度
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+ # 自定义颜色
  theme_bw() + theme(text = element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none" ) +
  labs(x = " ", y = "SOC (g kg-1)")  # 设置x轴和y轴标签
p3<-p3+ylim(20,100)


p4<-ggplot(data, aes(x = Time, y = Richness.B)) +
  geom_line(aes(color = Group), linewidth =1) + # 绘制折线
  geom_point(aes(color = Group), size =3) + # 添加数据点
  geom_errorbar(aes(ymin = Richness.B - se4, ymax = Richness.B + se4),#设置误差线
                width = 0.1,linewidth = 0.2) + # 误差线横杆宽度
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+ # 自定义颜色
  theme_bw() + theme(text = element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none" ) +
  labs(x = " ", y = "Bacterial richness")  # 设置x轴和y轴标签
p4<-p4+ylim(1000,4000)


p5<-ggplot(data, aes(x = Time, y = Richness.F)) +
  geom_line(aes(color = Group), linewidth =1) + # 绘制折线
  geom_point(aes(color = Group), size =3) + # 添加数据点
  geom_errorbar(aes(ymin = Richness.F - se6, ymax = Richness.F + se6),#设置误差线
                width = 0.1,linewidth = 0.2) + # 误差线横杆宽度
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b"))+ # 自定义颜色
  theme_bw() + theme(text = element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none" ) +
  labs(x = " ", y = "Fungal richness")  # 设置x轴和y轴标签
p5<-p5+ylim(500,1500)

library(ggpubr)
p6<-ggarrange(p1,p2,p3,p4,p5,ncol = 2,nrow = 3)
ggsave('年变化趋势图-20251219.pdf',p6, width = 12, height = 12)


###########平均值箱线图
mydata <-read.csv(file.choose(),row.names = 1)
mydata$Group <- factor (mydata$Group, levels = c("EDG","TGG","STG","LTG","NDG"))
p7 <- ggplot(mydata, aes(x = Group, y = AGB)) + 
  geom_violin(aes(fill = Group, color = Group), trim = FALSE, alpha = 0.8) +  # 小提琴图
  geom_boxplot(aes(fill = Group), notch = FALSE, width = 0.2, outlier.colour = NA, alpha = 0.8) +  # 箱线图
  #geom_jitter(aes(color = Group), position = position_jitter(0.1), size = 0.7, alpha = 0.8,color = "white") +  # 抖动点
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) + 
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) +
  theme_bw() + theme(text = element_text(size=16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none") +
  labs(x = " ", y = "AGB (g m-2)")  # 设置x轴和y轴标签
p7<-p7+ylim(0,600)


p8 <- ggplot(mydata, aes(x = Group, y = BGB_0_30)) + 
  geom_violin(aes(fill = Group, color = Group), trim = FALSE, alpha = 0.8) +  # 小提琴图
  geom_boxplot(aes(fill = Group), notch = FALSE, width = 0.2, outlier.colour = NA, alpha = 0.8) +  # 箱线图
  #geom_jitter(aes(color = Group), position = position_jitter(0.1), size = 0.7, alpha = 0.8,color = "white") +  # 抖动点
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) + 
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) +
  theme_bw() + theme(text = element_text(size=16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none") +
  labs(x = " ", y = "BGB (g m-2)")  # 设置x轴和y轴标签
p8<-p8+ylim(0,8000)

p9 <- ggplot(mydata, aes(x = Group, y = SOC)) + 
  geom_violin(aes(fill = Group, color = Group), trim = FALSE, alpha = 0.8) +  # 小提琴图
  geom_boxplot(aes(fill = Group), notch = FALSE, width = 0.2, outlier.colour = NA, alpha = 0.8) +  # 箱线图
  #geom_jitter(aes(color = Group), position = position_jitter(0.1), size = 0.7, alpha = 0.8,color = "white") +  # 抖动点
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) + 
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) +
  theme_bw() + theme(text = element_text(size=16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none") +
  labs(x = " ", y = "BGB (g kg-1)")  # 设置x轴和y轴标签
p9<-p9+ylim(0,125)

p10 <- ggplot(mydata, aes(x = Group, y = Richness.B)) + 
  geom_violin(aes(fill = Group, color = Group), trim = FALSE, alpha = 0.8) +  # 小提琴图
  geom_boxplot(aes(fill = Group), notch = FALSE, width = 0.2, outlier.colour = NA, alpha = 0.8) +  # 箱线图
 # geom_jitter(aes(color = Group), position = position_jitter(0.1), size = 0.7, alpha = 0.8,color = "white") +  # 抖动点
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) + 
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) +
  theme_bw() + theme(text = element_text(size=16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none") +
  labs(x = " ", y = "Bacterial richness")  # 设置x轴和y轴标签
p10<-p10+ylim(1000,4000)


# 去掉异常值（基于 IQR）
Q1 <- quantile(mydata$Richness.F, 0.25)
Q3 <- quantile(mydata$Richness.F, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
# 过滤数据
data_cleaned <- mydata[mydata$Richness.F >= lower_bound & mydata$Richness.F <= upper_bound, ]

p11 <- ggplot(data_cleaned, aes(x = Group, y = Richness.F)) + 
  geom_violin(aes(fill = Group, color = Group), trim = FALSE, alpha = 0.8) +  # 小提琴图
  geom_boxplot(aes(fill = Group), notch = FALSE, width = 0.2, outlier.colour = NA, alpha = 0.8) +  # 箱线图
 #geom_jitter(aes(color = Group), position = position_jitter(0.1), size = 0.7, alpha = 0.8,color = "white") +  # 抖动点
  scale_fill_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) + 
  scale_color_manual(values = c("#999990", "#2fc5ee", "#2bd188", "#bad50d","#ecaf0b")) +
  theme_bw() + theme(text = element_text(size=16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.position = "none") +
  labs(x = " ", y = "Fungal richness")  # 设置x轴和y轴标签
p11<-p11+ylim(0,1500)

library(ggpubr)
p12<-ggarrange(p7,p8,p9,p10,p11,ncol = 2,nrow = 3)
ggsave('不同处理平均值-20251219.pdf',p12, width = 9, height = 12)
