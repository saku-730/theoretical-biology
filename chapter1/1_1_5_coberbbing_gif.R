## 1_1_5_spideranime.R made by kero

library(gganimate)
library(tidyverse)
library(gifski)
rm(list=objects())

# setting
N0 <- 1 #N0 t=0での個体数
R <- 2.5 #R 増殖率の値
generation <- 10 #世代数
K <- 100 #K 環境収容力


# equation
logistic <- function(Nt){
  Nt1 <- Nt+R*(1-Nt/K)*Nt
  return(Nt1)
}

N_t <- N0

Nt1curve <- list()

# 曲線
for (N in seq(0,1.5*K,1)){ 
  Nt1 <- logistic(N)
  Nt1curve <- append(Nt1curve,Nt1)
}

plot_data <- tibble(Ncurve=as.numeric(Nt1curve),
                    Nt = seq(0,1.5*K,1))

max_y <- max(unlist(Nt1curve))

# 軌跡
spider_plot <- list(0)

for (t in seq(1,generation,1)){
  N_t1 <-  logistic(N_t)
  N_t <- N_t1
  spider_plot <-append(spider_plot,N_t1)
  spider_plot <-append(spider_plot,N_t1)
}
spider_plot <- spider_plot[-2*10-1]
spider_plot_sub <- spider_plot[-1]
spider_plot_sub <- head(spider_plot_sub,-1)
spider_plot_sub<-c(N0,spider_plot_sub)
spider_plot_sub<-c(N0,spider_plot_sub)

max_x<- max(unlist(spider_plot))


footprint <- tibble(y=as.numeric(spider_plot),x=as.numeric(spider_plot_sub),
                    time=seq(1,generation*2,1))

fig1 <- ggplot()+
  geom_path(data = plot_data,aes(x=Nt,y=Nt,color ="Nt+1 = Nt"))+
  geom_path(data = plot_data,aes(x=Nt,y=Ncurve,color =""))+
  geom_path(data = footprint,
            arrow=arrow(angle=30,type = "closed",length = unit(0.15,"inches")),
            aes(x=x,y=y,color ="個体数の軌跡"))+
  labs(title = "1.1.5 図2 蜘蛛の巣図法アニメーション",x = "Nt", y= "Nt+1",color="")+ 
  scale_x_continuous(limits = c(0,1.1*max_x))+ # x軸範囲
  scale_y_continuous(limits = c(-1,1.1*max_y))+ # y軸範囲
  theme(plot.title = element_text(hjust = 0.5))+ #図のタイトルを中央にする
  transition_reveal(along=time)


animate(fig1,width = 650,height = 650)
anim_save("1_1_5_Fanime.gif")
