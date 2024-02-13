library(tidyverse)
rm(list=objects())

# setting
N01 <- 25
N02 <- 150
N03 <- 5
R02 <- 0.2
R05 <- 0.5
generation <- 300
K <- 100

Result <- data_frame(
  ge = 0,
  N2R_05 = N02,
  N1R_02 = N01,
  N3R_05 = N03,
)

C <- function(N0){
  C <- (K-N0)/N0
  return(C)
}

# equation
for (t in 1:generation){
  t <- t/10   #なめらかにするために細かく値をとっている
  N3R_5 <- K/(1+C(N03)*exp(R05*(-1)*t)) 
  N1R_2 <- K/(1+C(N01)*exp(R02*(-1)*t)) 
  N2R_5 <- K/(1+C(N02)*exp(R05*(-1)*t))
  Result[nrow(Result)+1,] <- list(t,N2R_5,N1R_2,N3R_5)
}

figure <- ggplot(Result,aes(ge))+
  geom_path(aes(y=N2R_05,color ="r=0.5,N0=150"))+
  geom_path(aes(y=N1R_02,color ="r=0.2,N0=25"))+
  geom_path(aes(y=N3R_05,color ="r=0.5,N0=5"))+
  labs(title = "1.1.4 図2 ロジスティック増殖",x = "時間", y= "個体数")+
  theme(plot.title = element_text(hjust = 0.5))


figure