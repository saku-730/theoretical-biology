library(tidyverse)
rm(list=objects())

# setting
R01 <- 0.1
R02 <- 0.2
R05 <- 0.5
population <- 100
K <- 100

Result <- data_frame(
  po = 0,
  R_01 = 0,
  R_02 = 0,
  R_05 = 0,
)

# equation
for (n in 1:population){
  R_01 <- R01*(1-n/K)*n
  R_02 <- R02*(1-n/K)*n
  R_05 <- R05*(1-n/K)*n
  Result[nrow(Result)+1,] <- list(n,R_01,R_02,R_05)
}

figure <- ggplot(Result,aes(po))+
  geom_path(aes(y=R_01,color ="r=0.1"))+
  geom_path(aes(y=R_02,color ="r=0.2"))+
  geom_path(aes(y=R_05,color ="r=0.5"))+
  labs(title = "1.1.4 図1 dn/dtと個体数の比較(K=100)",x = "個体数", y= "dn/dt")+
  theme(plot.title = element_text(hjust = 0.5))


figure