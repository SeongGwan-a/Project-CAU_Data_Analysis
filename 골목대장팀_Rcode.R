### [ 분석및 예측]-----------------------------------------------------------------------------------------


############################################데이터 불러오기#################################################

setwd('C:/Users/sy/sy_golmok/')
df = read.csv("df_real.csv")


#필요한 패키지 불러오기
install.packages("psych")
install.packages("dplyr")
install.packages("car")

library(psych)
library(dplyr)
library(car)


#factor로 변환
str(df$time)
df$time = as.factor(df$time)
df$location = as.factor(df$location)
df$qu = as.factor(df$qu)

#시간순으로 index 열 추가
index=c()
for (i in 0:9){
  j = nrow(df[df$area==i,])/30
  n = rep(1:30, each =j)
  index = c(index,n)}
df[,"index"] = index

################################################스케일링####################################################


df_scale = NULL

for (n in 1:30){
  temp = filter(df, name==unique(name)[n])
  for (i in 6:8) {
    temp[,i] = (temp[,i] - mean(temp[,i]))/sd(temp[,i])}
  df_scale = rbind(df_scale,temp)
}


attach(df_scale)



## [[3-1-1]] 방송전 매출액에 대한 regression fitting ------------------------------------------------------


## 순서는 
## 1. 각 상권별로 방송이전 데이터 (time==0) 추출해 회귀선적합
## 2. '(1)의 회귀직선에서의 예측값' 및 '방송직후 실제 매출액과 (1)의 회귀직선에서의 예측값의 차이' 도출 


############################################해방촌 회귀분석#################################################


#신흥시장 데이터만 (name=="신흥시장")
df_sinheung = filter(df_scale, name=="신흥시장")


df_sinheung_0=filter(df_sinheung , time==0) # 방송이전 데이터 추출해 회귀선 적합
lm_sin <- lm(amount ~ index, df_sinheung_0)
summary(lm_sin)


# 방송직후 실제 매출액과 위의 회귀에 위한 예측값의 차이
df_sinheung $amount[df_sinheung $index == 22]-predict(lm_sin , newdata = data.frame (time=as.factor(0), index=22), interval="confidence")
predict(lm_sin , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",level=0.95) #예측값
                                                     


png(filename="해방촌 신흥시장 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

par(mfcol=c(1,1))
plot(df_sinheung$index, df_sinheung$amount, xlab="분기", ylab ="매출액")
title("해방촌 신흥시장 분기 매출액")
abline(lm_sin, col="red")
abline(v=22, col="black")
abline(h=-0.2115262, col="brown", lty=2)
abline(h=-0.09992091, col="brown", lty=2)

dev.off()


############################################성수동 회귀분석#################################################


# 성수동의 상원길 데이터만 (name=="상원길")
df_sangwon = filter(df_scale, name=="상원길")

df_sangwon_0=filter(df_sangwon, time==0)
lm_sang <- lm(amount ~ index, df_sangwon_0)
summary(lm_sang)

df_sangwon$amount[df_sangwon $index == 22]-predict(lm_sang , newdata = data.frame ( index=22), interval="confidence")
predict(lm_sang , newdata = data.frame ( index=22), interval="confidence")


                                            

png(filename="성수동 골목 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_sangwon$index, df_sangwon$amount, xlab="분기", ylab ="매출액", ylim=c(-2.1,1.6))
title("성수동 골목 분기 매출액")
abline(h=0.8601193, col="brown", lty=2)
abline(h=1.539987, col="brown", lty=2)
abline(lm_sang, col="red")
abline(v=22, col="black")

dev.off()


############################################성내동 회귀분석#################################################


# 성내동 천호대로162길 데이터만 (name=="천호대로162길")
df_chunho = filter(df_scale, name=="천호대로162길")

df_chunho_0=filter(df_chunho, time==0)
lm_ch <- lm(amount ~ index, df_chunho_0)
summary(lm_ch)

df_chunho$amount[df_chunho $index == 22]-predict(lm_ch , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
                                                level=0.95)
df_chunho$amount[df_chunho $index == 22]-predict(lm_ch , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
                                                 level=0.90)

predict(lm_ch , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
        level=0.95)



png(filename="성내동 만화거리 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_chunho$index, df_chunho$amount, xlab="분기", ylab ="매출액")
title("성내동 만화거리 분기 매출액")
abline(h=0.4137966, col="brown", lty=2)
abline(h=1.507424, col="brown", lty=2)
abline(lm_ch ,col="red")
abline(v=22, col="black")

dev.off()


################################################홍은동 회귀분석#############################################


# 홍은동 데이터만 (area=3)
# 상권 하나 있음

df_hongeun = filter(df_scale, area==3)

df_hongeun_0=filter(df_hongeun, time==0)
lm_hong <- lm(amount ~ index, df_hongeun_0)
summary(lm_hong)


df_hongeun$amount[df_hongeun $index == 24]-predict(lm_hong , newdata = data.frame (time=as.factor(0), index=24), interval="confidence",
                                                   level=0.95)
df_hongeun$amount[df_hongeun $index == 24]-predict(lm_hong , newdata = data.frame (time=as.factor(0), index=24), interval="confidence",
                                                  level=0.999)
predict(lm_hong , newdata = data.frame (time=as.factor(0), index=24), interval="confidence",
        level=0.95)



png(filename="홍은동 포방터시장 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_hongeun$index, df_hongeun$amount, xlab="분기", ylab ="매출액")
title("홍은동 포방터시장 분기 매출액")
abline(h=-0.04710324, col="brown", lty=2)
abline(h=0.4531464, col="brown", lty=2)
abline(lm_hong ,col="red")
abline(v=24, col="black")

dev.off()


#############################################청파동 회귀분석################################################



# 청파동 청파로47길길 데이터만 (name=="청파로47길")
df_chungpa_only = filter(df_scale, name=="청파로47길")
df_chungpa_only_0=filter(df_chungpa_only, time==0)

# 청파동 대학상권의 분기적 특성을 고려해, 방송직후 분기인 1분기에 대해서만 회귀
df_chungpa_only_1 = filter(df_chungpa_only_0, qu==1)
lm_cp_q <- lm(amount ~ index, df_chungpa_only_1)
summary(lm_cp_q)


df_chungpa_only$amount[df_chungpa_only$index == 25]-predict(lm_cp_q , newdata = data.frame (time=as.factor(0), index=25), interval="confidence",
                                                           level=0.95)
predict(lm_cp_q , newdata = data.frame (time=as.factor(0), index=25), interval="confidence",
        level=0.95)



#1분기
png(filename="청파동 하숙골목 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_chungpa_only$index, df_chungpa_only$amount, xlab="분기", ylab ="매출액")
title("청파동 하숙골목 분기 매출액")
abline(h=-0.821531, col="brown", lty=2)
abline(h=0.3319076, col="brown", lty=2)
abline(lm_cp_q ,col="red")
abline(v=25, col="black")

dev.off()


#############################################회기동 회귀분석################################################
############################################################################################################


# 회기동
# 경희대로4길 데이터만 (name=="경희대로4길")
df_kyung= filter(df_scale, name=="경희대로4길")
df_kyung_only_0=filter(df_kyung, time==0)
df_kyung_only_1 = filter(df_kyung_only_0, qu==1)
lm_ky_q <- lm(amount ~ index, df_kyung_only_1)
summary(lm_ky_q)

df_kyung$amount[df_kyung$index == 25]-predict(lm_ky_q  , newdata = data.frame (time=as.factor(0), index=25), interval="confidence")
df_kyung$amount[df_kyung$index == 25]-predict(lm_ky_q  , newdata = data.frame (time=as.factor(0), index=25), interval="confidence", level = 0.90)

predict(lm_ky_q  , newdata = data.frame (time=as.factor(0), index=25), interval="confidence", level = 0.95)



# 1분기
png(filename="회기동 벽화골목 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_kyung$index, df_kyung$amount, xlab="분기", ylab ="매출액")
title("회기동 벽화골목 분기 매출액")
abline(lm_ky_q ,col="red")
abline(v=25, col="black")
abline(h=-0.2804858, col="brown", lty=2)
abline(h=1.558297, col="brown", lty=2)

dev.off()


#############################################둔촌동 회귀분석################################################

# 둔촌동
# 양재대로96길 데이터만 (name=="양재대로96길")
df_yang= filter(df_scale, name=="양재대로96길")
df_yang_only_0=filter(df_yang, time==0)
lm_yang_q <- lm(amount ~ index, df_yang_only_0)
summary(lm_yang_q)

df_yang$amount[df_yang$index == 28]-predict(lm_yang_q , newdata = data.frame (time=as.factor(0), index=28), interval="confidence", level=0.90)
predict(lm_yang_q , newdata = data.frame (time=as.factor(0), index=25), interval="confidence", level=0.95)



png(filename="둔촌동 골목 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_yang$index, df_yang$amount, xlab="분기", ylab ="매출액")
title("둔촌동 골목 분기 매출액")
abline(lm_yang_q ,col="red")
abline(v=28, col="black")
abline(h=0.2544361, col="brown", lty=2)
abline(h=1.331388, col="brown", lty=2)

dev.off()


#######################################정릉동 회귀분석######################################################

# 상권 두개의 합

# 정릉로26길 (name=="정릉로26길")
df_26= filter(df_scale, name=="정릉로26길")
df_26_0 = filter(df_jn, time==0)

# 정릉아리랑시장 (name=="정릉아리랑시장")
df_jn= filter(df_scale, name=="정릉아리랑시장")
df_jn_0 = filter(df_jn, time==0)

# 두개 평균
df_jn_avg = (df_26[6:7] + df_jn[6:7]) / 2
df_jn_avg = cbind(df_jn[2:5],df_jn_avg,df_jn[9:12])
df_jn_avg_0 = filter(df_jn_avg, time==0)


df_jn_avg_0=filter(df_jn, time==0)
lm_jn_q <- lm(amount ~ index, df_jn_avg_0)
summary(lm_yang_q)

df_jn_avg$amount[df_jn_avg$index == 28]-predict(lm_jn_q , newdata = data.frame (time=as.factor(0), index=28), interval="confidence", level=0.95)
predict(lm_jn_q , newdata = data.frame (time=as.factor(0), index=28), interval="confidence", level=0.95)


png(filename="정릉동 아리랑시장 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_jn$index, df_jn$amount, xlab="분기", ylab ="매출액", ylim=c(-1.9,1.9))
title("정릉동 아리랑시장 분기 매출액")
abline(lm_jn_q ,col="red")
abline(v=28, col="black")
abline(h=0.8249876, col="brown", lty=2)
abline(h=1.879505, col="brown", lty=2)

dev.off()


##############################################홍제동 회귀분석##############################################

# 홍제동
# 세무서길 데이터만 (name=="세무서길")
df_hj= filter(df_scale, name=="세무서길")
df_hj_only_0=filter(df_hj, time==0)
lm_hj_q <- lm(amount ~ index, df_hj_only_0)
summary(lm_hj_q)


df_hj$amount[df_hj$index == 29]-predict(lm_hj_q  , newdata = data.frame (time=as.factor(0), index=29), interval="confidence",
                                       level=0.95)

predict(lm_hj_q  , newdata = data.frame (time=as.factor(0), index=29), interval="confidence",
        level=0.95)



png(filename="홍제동 문화촌 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_hj$index, df_hj$amount, xlab="분기", ylab ="매출액")
title("홍제동 문화촌 분기 매출액")
abline(lm_hj_q ,col="red")
abline(v=29, col="black")
abline(h=0.4139335, col="brown", lty=2)
abline(h=1.740107, col="brown", lty=2)

dev.off()


#############################################공릉동 도깨비시장 회귀분석##############################################


# 공릉동도깨비시장 데이터만 (name=="공릉동도깨비시장")
df_gn= filter(df_scale, name=="공릉동도깨비시장")
df_gn_only_0=filter(df_gn, time==0)
lm_gn_q <- lm(amount ~ index, df_gn_only_0)
summary(lm_gn_q )

df_gn$amount[df_gn$index == 29]-predict(lm_gn_q   , newdata = data.frame (time=as.factor(0), index=29), interval="confidence", level=0.999)
df_gn$amount[df_gn$index == 29]-predict(lm_gn_q   , newdata = data.frame (time=as.factor(0), index=29), interval="confidence")
predict(lm_gn_q   , newdata = data.frame (time=as.factor(0), index=29), interval="confidence")


png(filename="공릉동 분기 매출액.png",width=600,height=600,unit="px",bg="transparent",res=100)

par(mfcol=c(1,1))
plot(df_gn$index, df_gn$amount, xlab="분기", ylab ="매출액")
title("공릉동 골목 분기 매출액")
abline(lm_gn_q ,col="red")
abline(v=29, col="black")
abline(h=-0.5853879, col="brown", lty=2)
abline(h=0.762243, col="brown", lty=2)

dev.off()







## [[ 3-1-4~ 매출건수 ]] 방송전 매출액에 대한 regression fitting ------------------------------------------------------


# 상권별 골목 상권 하나씩 회귀 --------------------------------------------------------

# 해방촌 신흥시장 ----------------------------------------------------------------
#신흥시장 데이터만 (name=="신흥시장")
df_sinheung = filter(df_scale, name=="신흥시장")
df_sinheung_0=filter(df_sinheung , time==0)
lm_sin <- lm(count ~ index, df_sinheung_0)
summary(lm_sin)
df_sinheung $count[df_sinheung $index == 22]-predict(lm_sin , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
                                                     level=0.95)
df_sinheung $count[df_sinheung $index == 22]-predict(lm_sin , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
                                                     level=0.999)

predict(lm_sin , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
        level=0.95)


png(filename="해방촌 신흥시장 분기 매출건수.png",width=600,height=600,unit="px",bg="transparent",res=100)

par(mfcol=c(1,1))
plot(df_sinheung$index, df_sinheung$count, xlab="분기", ylab ="매출건수")
title("해방촌 신흥시장 분기 매출 건수")
abline(lm_sin, col="red")
abline(v=22, col="black")
abline(h=-0.06655577, col="brown", lty=2)
abline(h=0.4683661, col="brown", lty=2)

dev.off()



# 성수동 상원길 -----------------------------------------------------------------
#상원길 데이터만 (name=="상원길")
df_sangwon = filter(df_scale, name=="상원길")
df_sangwon_0=filter(df_sangwon, time==0)
lm_sang <- lm(count ~ index, df_sangwon_0)
summary(lm_sang)

df_sangwon$count[df_sangwon $index == 22]-predict(lm_sang , newdata = data.frame ( index=22), interval="confidence",
                                                  level=0.95)
df_sangwon$count[df_sangwon $index == 22]-predict(lm_sang , newdata = data.frame ( index=22), interval="confidence",
                                                  level=0.999)
predict(lm_sang , newdata = data.frame ( index=22), interval="confidence",
        level=0.95)



png(filename="성수동 골목 분기 거래건수.png",width=600,height=600,unit="px",bg="transparent",res=100)


plot(df_sangwon$index, df_sangwon$count, xlab="분기", ylab ="매출건수",ylim=c(-2.1,2.3))
title("성수동 골목 분기 거래건수")
abline(lm_sang,col="red")
abline(v=22, col="black")
abline(h=1.237492, col="brown", lty=2)
abline(h=2.257451, col="brown", lty=2)

dev.off()




# 성내동 천호대로 162길 -----------------------------------------------------------
#천호대로162길 데이터만 (name=="천호대로162길")
df_chunho = filter(df_scale, name=="천호대로162길")
df_chunho_0=filter(df_chunho, time==0)
lm_ch <- lm(count ~ index, df_chunho_0)
summary(lm_ch)


df_chunho$count[df_chunho $index == 22]-predict(lm_ch , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
                                                level=0.95)
df_chunho$count[df_chunho $index == 22]-predict(lm_ch , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
                                                level=0.90)
predict(lm_ch , newdata = data.frame (time=as.factor(0), index=22), interval="confidence",
        level=0.95)



png(filename="성내동 만화거리 분기 매출 건수.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_chunho$index, df_chunho$count, xlab="분기", ylab ="매출건수")
title("성내동 만화거리 분기 매출건수")
abline(lm_ch ,col="red")
abline(v=22, col="black")
abline(h=0.2080006, col="brown", lty=2)
abline(h=1.414599, col="brown", lty=2)

dev.off()



# 홍은동 ---------------------------------------------------------------------
#홍은동 데이터만 (area=3)
# 상권 하나 
df_hongeun = filter(df_scale, area==3)
df_hongeun_0=filter(df_hongeun, time==0)
lm_hong <- lm(count ~ index, df_hongeun_0)
summary(lm_hong)


df_hongeun$count[df_hongeun $index == 24]-predict(lm_hong , newdata = data.frame (time=as.factor(0), index=24), interval="confidence",
                                                  level=0.95)
df_hongeun$count[df_hongeun $index == 24]-predict(lm_hong , newdata = data.frame (time=as.factor(0), index=24), interval="confidence",
                                                  level=0.999)
predict(lm_hong , newdata = data.frame (time=as.factor(0), index=24), interval="confidence",
        level=0.95)


png(filename="홍은동 포방터시장 분기 매출건수.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_hongeun$index, df_hongeun$count, xlab="분기", ylab ="매출건수")
title("홍은동 포방터시장 분기 매출건수")
abline(lm_hong ,col="red")
abline(v=24, col="black")
abline(h=-0.03223051, col="brown", lty=2)
abline(h=0.3518306, col="brown", lty=2)

dev.off()



# 청파동 청파로 47길 -------------------------------------------------------------
# 청파로47길길 데이터만 (name=="청파로47길")
df_chungpa_only = filter(df_scale, name=="청파로47길")
df_chungpa_only_0=filter(df_chungpa_only, time==0)
df_chungpa_only_1 = filter(df_chungpa_only_0, qu==1)
lm_cp_q <- lm(count ~ index, df_chungpa_only_1)
summary(lm_cp_q)

df_chungpa_only$count[df_chungpa_only$index == 25]-predict(lm_cp_q , newdata = data.frame (time=as.factor(0), index=25), interval="confidence",
                                                           level=0.95)
df_chungpa_only$count[df_chungpa_only$index == 25]-predict(lm_cp_q , newdata = data.frame (time=as.factor(0), index=25), interval="confidence",
                                                           level=0.90)
predict(lm_cp_q , newdata = data.frame (time=as.factor(0), index=25), interval="confidence",
        level=0.95)


png(filename="청파동 하숙골목 분기 매출건수.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_chungpa_only$index, df_chungpa_only$count, xlab="분기", ylab ="매출건수")
title("청파동 하숙골목 분기 매출건수")
abline(lm_cp_q ,col="red")
abline(v=25, col="black")
abline(h=-0.5998888, col="brown", lty=2)
abline(h=0.2973717, col="brown", lty=2)

dev.off()



# 회기동 경희대로 4길 -------------------------------------------------------------
# 경희대로4길 데이터만 (name=="경희대로4길")
df_kyung= filter(df_scale, name=="경희대로4길")
df_kyung_only_0=filter(df_kyung, time==0)
df_kyung_only_1 = filter(df_kyung_only_0, qu==1)
lm_ky_q <- lm(count ~ index, df_kyung_only_1)
summary(lm_ky_q)

df_kyung$count[df_kyung$index == 25]-predict(lm_ky_q  , newdata = data.frame (time=as.factor(0), index=25), interval="confidence")
df_kyung$count[df_kyung$index == 25]-predict(lm_ky_q  , newdata = data.frame (time=as.factor(0), index=25), interval="confidence", level = 0.90)
predict(lm_ky_q  , newdata = data.frame (time=as.factor(0), index=25), interval="confidence", level = 0.95)



png(filename="회기동 벽화골목 분기 매출건수.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_kyung$index, df_kyung$count, xlab="분기", ylab ="매출건수", ylim=c(-2,2))
title("회기동 벽화골목 분기 매출건수")
abline(lm_ky_q ,col="red")
abline(v=25, col="black")
abline(h=-0.5583137, col="brown", lty=2)
abline(h=1.965807, col="brown", lty=2)

dev.off()




# 둔촌동 양재대로 96길 ------------------------------------------------------------
# 양재대로96길 데이터만 (name=="양재대로96길")
df_yang= filter(df_scale, name=="양재대로96길")
df_yang_only_0=filter(df_yang, time==0)
lm_yang_q <- lm(count ~ index, df_yang_only_0)
summary(lm_yang_q)

df_yang$count[df_yang$index == 28]-predict(lm_yang_q , newdata = data.frame (time=as.factor(0), index=28), interval="confidence", level=0.95)
df_yang$count[df_yang$index == 28]-predict(lm_yang_q , newdata = data.frame (time=as.factor(0), index=28), interval="confidence", level=0.99)
predict(lm_yang_q , newdata = data.frame (time=as.factor(0), index=28), interval="confidence", level=0.95)



png(filename="둔촌동 골목 분기 매출건수.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_yang$index, df_yang$count, xlab="분기", ylab ="매출건수", ylim = c(-1.5, 1.9))
title("둔촌동 골목 분기 매출건수")
abline(lm_yang_q ,col="red")
abline(v=28, col="black")
abline(h=1.03825, col="brown", lty=2)
abline(h=1.873044, col="brown", lty=2)

dev.off()




# 정릉동 아리랑 시장 --------------------------------------------------------------
# 정릉로26길 (name=="정릉로26길")
df_26= filter(df_scale, name=="정릉로26길")
df_26_0 = filter(df_jn, time==0)

# 정릉아리랑시장 (name=="정릉아리랑시장")
df_jn= filter(df_scale, name=="정릉아리랑시장")
df_jn_0 = filter(df_jn, time==0)

# 두개 평균
df_jn_avg = (df_26[6:7] + df_jn[6:7]) / 2
df_jn_avg = cbind(df_jn[2:5],df_jn_avg,df_jn[9:13])
df_jn_avg_0 = filter(df_jn_avg, time==0)


df_jn_avg_0=filter(df_jn, time==0)
lm_jn_q <- lm(count ~ index, df_jn_avg_0)
summary(lm_yang_q)


df_jn$count[df_jn$index == 28]-predict(lm_jn_q  , newdata = data.frame (time=as.factor(0), index=28), interval="confidence", level=0.90)
predict(lm_jn_q  , newdata = data.frame (time=as.factor(0), index=28), interval="confidence", level=0.95)


png(filename="정릉동 아리랑시장 분기 매출건수png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_jn$index, df_jn$count, xlab="분기", ylab ="매출건수")
title("정릉동 아리랑시장 분기 매출건수")
abline(lm_jn_q ,col="red")
abline(v=28, col="black")
abline(h=0.8182656, col="brown", lty=2)
abline(h=1.568987, col="brown", lty=2)

dev.off()



# 홍제동 세무서길 ----------------------------------------------------------------
# 세무서길 데이터만 (name=="세무서길장")
df_hj= filter(df_scale, name=="세무서길")
df_hj_only_0=filter(df_hj, time==0)
lm_hj_q <- lm(count ~ index, df_hj_only_0)
summary(lm_hj_q)


df_hj$count[df_hj$index == 29]-predict(lm_hj_q  , newdata = data.frame (time=as.factor(0), index=29), interval="confidence",
                                       level=0.95)
df_hj$count[df_hj$index == 29]-predict(lm_hj_q  , newdata = data.frame (time=as.factor(0), index=29), interval="confidence",
                                       level=0.999)
predict(lm_hj_q  , newdata = data.frame (time=as.factor(0), index=29), interval="confidence",
        level=0.95)



png(filename="홍제동 문화촌 분기 매출건수.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_hj$index, df_hj$count, xlab="분기", ylab ="매출건수")
title("홍제동 문화촌 분기 매출건수")
abline(lm_hj_q ,col="red")
abline(v=29, col="black")
abline(h=-0.2458745, col="brown", lty=2)
abline(h=1.339349, col="brown", lty=2)


dev.off()



# 공릉동 도깨비 시장 --------------------------------------------------------------
# 공릉동도깨비시장 데이터만 (name=="공릉동도깨비시장")
df_gn= filter(df_scale, name=="공릉동도깨비시장")
df_gn_only_0=filter(df_gn, time==0)
lm_gn_q <- lm(count ~ index, df_gn_only_0)
summary(lm_gn_q )


df_gn$count[df_gn$index == 29]-predict(lm_gn_q   , newdata = data.frame (time=as.factor(0), index=29), interval="confidence")
predict(lm_gn_q   , newdata = data.frame (time=as.factor(0), index=29), interval="confidence", level=0.95)



png(filename="공릉동 분기 매출건수.png",width=600,height=600,unit="px",bg="transparent",res=100)

plot(df_gn$index, df_gn$count, xlab="분기", ylab ="매출건수")
title("공릉동 골목 분기 매출건수")
abline(lm_gn_q ,col="red")
abline(v=29, col="black")
abline(h=0.3613896, col="brown", lty=2)
abline(h=1.402962, col="brown", lty=2)

dev.off()









##########################상권별 방송영향 저장##############################

Diff_amt = c(0.8666071, -0.2936026, -0.4571717, 1.712077, 0.9482728,
             0.4598449, -0.5921831, -0.6387149, -0.7975756, 1.118589)

Diff_count = c(0.4375315,-1.036008,-0.2646097,0.5666578,
               -0.01326623, -0.3580703, -0.7769143, 0.10757,
               -1.522345, -0.06255092)


pos_Diff_amt = c(0.8666071, 1.712077, 0.9482728,  0.4598449, 1.118589)
t.test(pos_Diff_amt, mu=0, alternative="greater")

neg_Diff_amt = c(-0.2936026, -0.4571717,  -0.5921831, -0.6387149, -0.7975756)
t.test(nneg_Diff_amt, mu=0, alternative="less")



pos_Diff_co = c(0.4375315, 0.5666578, 0.10757)
t.test(pos_Diff_co, mu=0, alternative = "greater")
neg_Diff_co = c(-1.036008, -0.2646097, -0.01326623, -0.3580703, -0.3768446, -1.522345, -0.06255092)
t.test(neg_Diff_co, mu=0, alternative = "less" )
  
  
  
  
#######################방송영향의 독립변수 저장############################3
  rating = c(5.04, 5.18, 5.825, 7.8, 9.2, 8.9, 5.54, 5.575, 7.225, 8)

senti = c(0.4830954169797145,0.3993677555321391,0.5105633802816901,0.46075663466967814,
          0.41306405806248025, 0.3801470588235294, 0.3959731543624161, 0.4189189189189189,
          0.3249745848864792, 0.462)


######################데이터프레임 생성############################3
analy = cbind(name, area_df, Diff_amt, Diff_count, rating, senti)


########################분석############################33
library(leaps)
library(mlbench)

plot(senti, Diff_amt)
lm_amt = lm(Diff_amt~senti+rating)
lm_amt_st = step(lm_amt, direction="both")

lm_amt_sub = regsubsets(Diff_amt~senti+rating, data=analy)
summary(lm_amt_sub)
summary(lm_amt_sub)$bic
summary(lm_amt_sub)$adjr2

lm_count_sub = regsubsets(Diff_count~senti+rating, data=analy)
summary(lm_count_sub)
summary(lm_count_sub)$bic
summary(lm_count_sub)$adjr2



lm_amt = lm(Diff_amt~senti+rating)
predict(lm_amt, newdata = data.frame(senti=0.4552238805970149, rating=5), interval="confidence")

lm_count = lm(Diff_count~senti+rating)
predict(lm_count, newdata = data.frame(senti=0.4552238805970149, rating=5), interval="confidence")


################################상도동 데이터 준비#################################

sangdo = read.csv("D:/공모전_골목대장/제출용/sangdo_m.csv")
index=c(rep(1:30, each=2))
sangdo[,"index"] = index

#상권 두개 평균으로 합치기
sangdo_mean <- sangdo %>%
  group_by(stdr_yy_cd, stdr_qu_cd) %>%
  summarize(amount = mean(thsmon_selng_amt), count = mean(thsmon_selng_co), index = mean(index))

#스케일링
temp = ( sangdo_mean[,3] - apply(sangdo_mean, 2, mean)[3] ) / apply(sangdo_mean, 2, sd)[3]
temp2 = ( sangdo_mean[,4] - apply(sangdo_mean, 2, mean)[4] ) / apply(sangdo_mean, 2, mean)[4]

sangdo_scale = sangdo_mean
sangdo_scale[,3] = temp
sangdo_scale[,4] = temp2


################################상도동 데이터 분석#################################
lm_sangdo_1 <- lm(amount ~ index, sangdo_mean)

lm_sangdo_2 <- lm(count ~ index, sangdo_mean)

predict(lm_sangdo_1, newdata = data.frame(index=33), interval="confidence")
predict(lm_sangdo_2, newdata = data.frame(index=33), interval="confidence")


##################################스케일링 풀어주기###############################
apply(sangdo_mean, 2, sd)[3]
apply(sangdo_mean, 2, mean)[3]

#(회귀예측값 + 방송영향 Lower/Upper Bound) * 상도 데이터 표준편차 + 상도 데이터 평균
(1.489816 - 0.9243901) * apply(sangdo_mean, 2, sd)[3] + apply(sangdo_mean, 2, mean)[3]
(1.489816 + 0.5798688) * apply(sangdo_mean, 2, sd)[3] + apply(sangdo_mean, 2, mean)[3]

(0.5044116 - 0.778334) * apply(sangdo_mean, 2, sd)[4] + apply(sangdo_mean, 2, mean)[4]
(0.5044116 - 0.3023701) * apply(sangdo_mean, 2, sd)[4] + apply(sangdo_mean, 2, mean)[4]



