## 1_1_8.R made by saku

library(tidyverse)
rm(list=objects())

# setting
N0 <- 5
R01 <- 0.1
R02 <- 0.2
R03 <- 0.3
R05 <- 0.5
generation <- 10

Result <- data_frame(
  ge = 0,
  R_01 = N0,
  R_02 = N0,
  R_03 = N0,
  R_05 = N0,
)

# equation


exp(r0*a*(1-exp(-t/a)))


for (t in 1:generation){
  NR_1 <- (R01+1)^t * N0
  NR_2 <- (R02+1)^t * N0
  NR_3 <- (R03+1)^t * N0
  NR_5 <- (R05+1)^t * N0
  Result[nrow(Result)+1,] <- list(t,NR_1,NR_2,NR_3,NR_5)
}

figure <- ggplot(Result,aes(ge))+
  geom_point(aes(y = R_01,color ="R_01"))+
  geom_path(aes(y=R_01,color ="R_01"))+
  geom_point(aes(y = R_02,color = "R_02"))+
  geom_path(aes(y=R_02,color ="R_02"))+
  geom_point(aes(y = R_03,color ="R_03"))+
  geom_path(aes(y=R_03,color ="R_03"))+
  geom_point(aes(y = R_05,color ="R_05"))+
  geom_point(aes(y = R_05,color ="R_05"))+
  geom_path(aes(y=R_05,color ="R_05"))+
  labs(title = "1.1.1 図1 一種の個体群動態(差分方程式)",x = "世代", y= "個体数")+
  theme(plot.title = element_text(hjust = 0.5))


figure