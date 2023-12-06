#Code "Precious metals in bottom ashes from municipal waste incineration"
#PLOS ONE submission 
#Cite: ""

Sys.setlocale("LC_ALL", "English")

#data imported from working directory (data presented in Table 1, and met_szl.csv)
library(readr)
library(dplyr)
met_szl<-read_delim(met_szl.csv, col_names=TRUE, delim = ";", escape_double = FALSE, trim_ws = TRUE)

#censored data transformation
library(NADA)
Pd_to_ROS<-data.frame(Pd=met_szl$Pd, Pd_PPO=c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,
              TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
              FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
              TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE,
              FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))

write.csv(Pd_to_ROS, "Pd_to_ROS.csv")

Pd_pop<-ros(Pd_to_ROS$Pd, Pd_to_ROS$Pd_PPO, forwardT="log", reverseT="exp")
Pd_pop['modeled']
Pd_pop$censored
Pd_pop$obs
write.csv(Pd_pop, "Pd_pop.csv")

#number of censored data for Pd=14
loc<-1:14
#The order of replacing censored data with values calculated from ROS
s_p<-sample(loc, 14, replace=FALSE)
#Pd variable was modified in Pd_to_ROS.csv file and only column Pd was save as Pd_z_ROS
Pd_z_ROS<-read_delim(Pd_z_ROS.csv, col_names=TRUE)
met_szl_f<-data.frame(`Sample number`= 1:52, Ag=met_szl$Ag, Au=met_szl$Au, Pd=Pd_zROS$Pd, Pt=met_szl$Pt)

#descriptive statistics
library(e1071) 
library(ggplot2)
library(tidyr)
library(forecast)
library(dplyr)

summary(met_szl_f)
sd(met_szl_f$Ag)
sd(met_szl_f$Au)
sd(met_szl_f$Pd)
sd(met_szl_f$Pt)
skewness(met_szl_f$Ag)
skewness(met_szl_f$Au)
skewness(met_szl_f$Pd)
skewness(met_szl_f$Pt)

ggplot(met_szl_f, aes(x=Ag))+
  geom_histogram(bins=8)+
  theme_minimal()+
  xlab("Ag [ppb]")

ggplot(met_szl_f, aes(x=Au))+
  geom_histogram(bins=8)+
  theme_minimal()+
  xlab("Au [ppb]")

ggplot(met_szl_f, aes(x=Pd))+
  geom_histogram(bins=8)+
  theme_minimal()+
  xlab("Pd [ppb]")

ggplot(met_szl_f, aes(x=Pt))+
  geom_histogram(bins=8)+
  theme_minimal()+
  xlab("Pt [ppb]")

met_szl_f_longer<-pivot_longer(met_szl_f, !Sample.number, names_to = "Metals", values_to = "content")

ggplot(met_szl_f_longer, aes(x=Sample.number, y=content, fill=Metals))+
  geom_col()+
  scale_y_continuous(trans='log10')+
  theme_minimal()+
  scale_fill_manual(values=c("#C0C0C0","#FFD700","#5b0a91", "#14903F" ))+
  ylab("Metals [ppb]")+
  xlab("Sample number")

value<-ifelse(Pd_to_ROS$Pd_PPO==TRUE, "ROS", "Real")
value1<-c(rep("Real", 16), "ROS", rep("Real", 35))
met_szl_f_r<-cbind(met_szl_f, value)
met_szl_f_r<-cbind(met_szl_f_r, value1)
  
ggplot(met_szl_f_r, aes(x=Sample.number, y=Pd, fill=value))+
  geom_col()+
  theme_minimal()+
  scale_fill_manual(values=c("#3C3C3B","#C0C0C0"))+
  theme(legend.position=c(0.1, 0.9))+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))+
  ylab("Pd [ppb]")
  xlab("Sample number")

ggplot(met_szl_f_r, aes(x=Sample.number, y=Ag, fill=value1))+
  geom_col()+
  theme_minimal()+
  scale_fill_manual(values=c("#3C3C3B","#C0C0C0"))+
  theme(legend.position=c(0.1, 0.9))+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE), limits=c(NA, 1e6))+
  guides(fill = guide_legend(title = "value"))+
  ylab("Ag [ppb]")+
  xlab("Sample number")

ggplot(met_szl_f_r, aes(x=Sample.number, y=Au))+
  geom_col(fill="#3C3C3B")+
  theme_minimal()+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))+
  ylab("Au [ppb]")+
  xlab("Sample number")

ggplot(met_szl_f_r, aes(x=Sample.number, y=Pt))+
  geom_col(fill="#3C3C3B")+
  theme_minimal()+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))+
  ylab("Pt [ppb]")+
  xlab("Sample number")

ggAcf(met_szl_f$Ag, lag.max =20)+
  theme_minimal()+
  ggtitle("Ag")+
  theme(plot.title = element_text(hjust=0.5))

ggAcf(met_szl_f$Au, lag.max =20)+
  theme_minimal()+
  ggtitle("Au")+
  theme(plot.title = element_text(hjust=0.5))

ggAcf(met_szl_f$Pd, lag.max =20)+
  theme_minimal()+
  ggtitle("Pd")+
  theme(plot.title = element_text(hjust=0.5))

ggAcf(met_szl_f$Pt, lag.max =20)+
  theme_minimal()+
  ggtitle("Pt")+
  theme(plot.title = element_text(hjust=0.5))

ggPacf(met_szl_f$Ag, lag.max =20)+
  theme_minimal()+
  ggtitle("Ag")+
  theme(plot.title = element_text(hjust=0.5))

ggPacf(met_szl_f$Au, lag.max =20)+
  theme_minimal()+
  ggtitle("Au")+
  theme(plot.title = element_text(hjust=0.5))

ggPacf(met_szl_f$Pd, lag.max =20)+
  theme_minimal()+
  ggtitle("Pd")+
  theme(plot.title = element_text(hjust=0.5))

ggPacf(met_szl_f$Pt, lag.max =20)+
  theme_minimal()+
  ggtitle("Pt")+
  theme(plot.title = element_text(hjust=0.5))

#seasonal plots
met_szl_f_sez<-met_szl_f%>%
  mutate(season=ifelse(Sample.number<=9 | Sample.number>=49 ,'winter', 
                       ifelse(Sample.number>9 & Sample.number<=22, 'spring',
                              ifelse(Sample.number>22 & Sample.number<=35, 'summer','fall'))))

met_szl_f_sez$season<-factor(met_szl_f_sez$season, c("spring", "summer", "fall", "winter"))

ggplot(met_szl_f_sez, aes(season, Ag, color=season))+
  geom_boxplot(show.legend = FALSE)+
  theme_minimal()+
  scale_color_manual(values=c("#14903F","#FFD700","#5b0a91", "#C0C0C0"))+
  ylab("Ag [ppb]")+
  xlab("")+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))

ggplot(met_szl_f_sez, aes(season, Au, color=season))+
  geom_boxplot(show.legend = FALSE)+
  theme_minimal()+
  scale_color_manual(values=c("#14903F","#FFD700","#5b0a91", "#C0C0C0" ))+
  ylab("Au [ppb]")+
  xlab("")+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))

ggplot(met_szl_f_sez, aes(season, Pd, color=season))+
  geom_boxplot(show.legend = FALSE)+
  theme_minimal()+
  scale_color_manual(values=c("#14903F","#FFD700","#5b0a91", "#C0C0C0" ))+
  ylab("Pd [ppb]")+
  xlab("")+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))

ggplot(met_szl_f_sez, aes(season, Pt, color=season))+
  geom_boxplot(show.legend = FALSE)+
  theme_minimal()+
  scale_color_manual(values=c("#14903F","#FFD700","#5b0a91", "#C0C0C0" ))+
  ylab("Pt [ppb]")+
  xlab("")+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))

#ANOVA
one_way_Ag<-aov(Ag~season, data=met_szl_f_sez)
summary(one_way_Ag)
one_way_Au<-aov(Au~season, data=met_szl_f_sez)
summary(one_way_Au)
one_way_Pd<-aov(Pd~season, data=met_szl_f_sez)
summary(one_way_Pd)
one_way_Pt<-aov(Pt~season, data=met_szl_f_sez)
summary(one_way_Pt)

#correlations
library(corrplot)
kor_p<-round(cor(met_szl_f[2:5]),2)
corrplot(kor_p, method="color", type="upper", tl.col="black", addCoef.col = 'black',diag = FALSE, tl.srt=0,  col=colorRampPalette(c("blue","white","red"))(100),cl.lim=c(0,1), number.cex = 2, tl.cex= 1.5, cl.cex = 0.8)
testRes<- cor.mtest(met_szl_f[2:5], conf.level = 0.95)
kor_s<-round(cor(met_szl_f[2:5], method="spearman"),2)
corrplot(kor_s, method="color", type="upper", tl.col="black", addCoef.col = 'black',diag = FALSE, tl.srt=0,  col=colorRampPalette(c("blue","white","red"))(100),cl.lim=c(0,1), number.cex = 2, tl.cex= 1.5, cl.cex = 0.8)

ggplot(met_szl, aes(x=Ag, y=Au))+
  geom_point()+
  theme_minimal()+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))+
  ylab("Au [ppb]")+
  xlab("Ag [ppb]")

ggplot(met_szl, aes(x=Ag, y=Pd))+
  geom_point()+
  theme_minimal()+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))+
  ylab("Pd [ppb]")+
  xlab("Ag [ppb]")

ggplot(met_szl, aes(x=Ag, y=Pt))+
  geom_point()+
  theme_minimal()+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))+
  ylab("Pt [ppb]")+
  xlab("Ag [ppb]")

ggplot(met_szl, aes(x=Au, y=Pt))+
  geom_point()+
  theme_minimal()+
  scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = TRUE))+
  ylab("Pt [ppb]")+
  xlab("Au [ppb]")

# ADF test
library(tseries)
library(aTSA)
adf.test(met_sz_f$Ag)
adf.test(met_sz_f$Au)
adf.test(met_sz_f$Pd)
adf.test(met_sz_f$Pt)

adf.test(met_sz_f$Ag)
adf.test(met_sz_f$Au)
adf.test(met_sz_f$APd)
adf.test(met_sz_f$Pt)

#Rosner test
library(EnvStats)
rosnerTest(met_szl_f$Ag, k=10, alpha=0.05)
rosnerTest(met_sz_f$Au, k=9, alpha=0.05)
rosnerTest(met_sz_f$Pd, k=4, alpha=0.05)
rosnerTest(met_sz_f$Pt, k=9, alpha=0.05)

# Student t- test
##Hinwil
t.test(met_sz_f$Ag, alternative = "two.sided", mu = 5300)
t.test(met_sz_f$Ag, alternative = "less", mu = 5300)
t.test(met_sz_f$Ag, alternative = "greater", mu = 4240)

t.test(met_sz_f$Au, alternative = "two.sided", mu = 400)
t.test(met_sz_f$Au, alternative = "less", mu = 400)

t.test(met_sz_f$Pt, alternative = "two.sided", mu = 59)
t.test(met_sz_f$Pt, alternative = "less", mu = 59)

t.test(met_sz_f$Ag, alternative = "two.sided", mu = 7.5)
t.test(met_sz_f$Ag, alternative = "greater", mu = 7.5)

t.test(met_sz_f$Au, alternative = "two.sided", mu = 0.04)
t.test(met_sz_f$Au, alternative = "greater", mu = 0.04)

t.test(met_sz_f$Pt, alternative = "two.sided", mu = 0.05)
t.test(met_sz_f$Pt, alternative = "greater", mu = 0.05)

t.test(met_sz_f$Pd, alternative = "two.sided", mu = 1.5)
t.test(met_sz_f$Pd, alternative = "greater", mu = 1.5)

#ICA
library(fastICA)
library(stringr)

ica_model <- fastICA(met_szl_f[,2:5], 4, alg.typ = "parallel", fun = "logcosh", alpha = 1, method = "C", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)

plot(ica_model$X %*% ica_model$K, main="ICA components")
plot(ica_model$S)

cor(ica_model$S)
weight_matrix <- data.frame(t(ica_model$A))
names(weight_matrix) <- str_glue("IC{seq(weight_matrix)}")

dist_matrix <- dist(as.matrix(weight_matrix))
clust <- hclust(dist_matrix)
plot(clust)


plot(ica_model$S[,1], col="red", main = "Estimated signals", xlab = "Time", ylab = "Amplitude")
lines(ica_model$S[,2], col="blue") 
lines(ica_model$S[,3], col="green") 
lines(ica_model$S[,4], col="yellow") 
mtext(" 1C- red, 2C blue,  3C green, 4C yellow")

ica<-data.frame(factor<-c(1,2,3,4), eigenvalue<-c(1.48, 0.95, 0.94, 0.57), label=c("36.67%", "24.65%", "24.58%", "14.10%"))

ggplot(ica, aes(factor, eigenvalue, label= label))+
  geom_point(size=1.5)+
  geom_line()+
  ylim(0, 1.7)+
  xlim(1, 4.2)+
  geom_text(hjust=0.3, vjust=-0.7, size=3.5)+
  ylab("Eigenvalue")+
  xlab("Component number")+
  theme_minimal()

ica_weight_matrix <- data.frame(t(scale(ica_model$A))) |>
  rename_with(~ str_glue("IC{seq(.)}")) |>
  mutate(variable = names(met_szl_f[2:5])) |>
  pivot_longer(
    cols = starts_with("IC"),
    names_to = "ic", values_to = "loading"
  )
ica_loading_plot <- ggplot(
  ica_weight_matrix,
  aes(x = ic, y = variable, fill = loading)
) +
  geom_tile() +
  labs(
    title = "ICA loadings",
    x = NULL,
    y = NULL
  ) +
  scico::scale_fill_scico(palette = "cork", limits = c(-3, 3)) +
  coord_fixed(ratio = 1 / 2)

print(ica_loading_plot)
