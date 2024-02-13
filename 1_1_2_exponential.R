library(tidyverse)
rm(list=objects())

# setting
N0 <- 5
R01 <- 0.1
R02 <- 0.2
R03 <- 0.3
R05 <- 0.5
generation <- 100

Result <- data_frame(
  ge = 0,
  R_01 = N0,
  R_02 = N0,
  R_03 = N0,
  R_05 = N0,
)

# equation
for (t in 1:generation){
  t <- t/10   #なめらかにするために細かく値をとっている
  NR_1 <- (R01+1)^t * N0
  NR_2 <- (R02+1)^t * N0
  NR_3 <- (R03+1)^t * N0
  NR_5 <- (R05+1)^t * N0
  Result[nrow(Result)+1,] <- list(t,NR_1,NR_2,NR_3,NR_5)
}

figure <- ggplot(Result,aes(ge))+
  geom_path(aes(y=R_01,color ="r_01"))+
  geom_path(aes(y=R_02,color ="r_02"))+
  geom_path(aes(y=R_03,color ="r_03"))+
  geom_path(aes(y=R_05,color ="r_05"))+
  labs(title = "1.1.2 図1 指数関数的増殖",x = "時間", y= "個体数")+
  theme(plot.title = element_text(hjust = 0.5))
  

figure