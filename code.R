#### Part 1: Preparations: package loading, crawling and data cleaning ####
### 1.1 Loading packages
## 1.1.1 For webscrapping
library(rvest)
library(xml2)
library(bitops)
library(readxl)
library(RCurl)
library(dplyr)
library(stringr)
library(XML)
library(lubridate)
library(rpart)

## 1.1.2 Data vitualization
library(ggplot2)
library("DT")
library(reshape2)
library(RColorBrewer)
library(scales)
library(showtextdb)
library(sysfonts)
library(showtext)
library(grid)
library(Cairo)
library(XML)
library(lattice)
library(openair)

### 1.1.3 Modeling
# - Time series
library(forecast) 

# - Bayes
library(bsts) 

# - Neural network
library(neuralnet) 
library(nnetpredint)
library(gbm) 

# - LSTM
## Download
devtools::install_github("rstudio/keras")
library(keras)
install_keras()
devtools::install_github("rstudio/tensorflow")
library(keras)
library(tensorflow)

# - GARCH
library(rugarch)

### 2. Data input and cleaning
## 2.1 Web scrapping
# Build website series
year = 2016:2018
month = sprintf("%02d", 1:12)
url = paste("https://www.aqistudy.cn/historydata/daydata.php?city=北京&month=", 
            expand.grid(year, month)$Var1, 
            expand.grid(year, month)$Var2, sep="")

# test
test = read_html(url[1], encoding="utf-8")%>%html_table(., header=TRUE, trim=TRUE);tbls<-tbls[[1]]

# web scrapping
data = data.frame()
vest= function(m){
  data = read_html(m, encoding="utf-8")%>%html_table(., header=TRUE, trim=TRUE)
  data = table[[1]]
}
for (i in url){
  Sys.sleep(3)
  data = rbind(data, vest(i))
}

# Examine the data
dim(data) #[1] 1095 11, 日期少了一天(2016年是闰年) 需要数据清洗

# Storing the data
save(data, file="./beijing_aqi_2016-2018.xlsx")

## 2.2 Clean the data
plt = read_excel("./beijing_aqi_2016-2018.xlsx", sheet=1) #plt = pollution
for(i in 2:36){
  plt1 = read_excel("北京空气污染2016-2018.xlsx", sheet=i)
  plt = rbind(plt, plt1)
}

names(plt)[c(1, 3, 9)] = c("date", "Level", "O3")
dim(plt) #plt数据有9行，1095个数据（365天*3）
attributes(plt)$names
plt$Year = year(plt$date) # 数据末尾增加年份以便分年度可视化

# Clean the data type
clean = function(plt){
  plt$Year = as.integer(plt$Year)
  plt$date = as.Date(plt$date)
  plt$AQI = as.integer(plt$AQI)
  plt$PM2.5 = as.integer(plt$PM2.5)
  plt$PM10 = as.integer(plt$PM10)
  plt$SO2 = as.integer(plt$SO2)
  plt$CO = as.integer(plt$CO)
  plt$NO2 = as.integer(plt$NO2)
  plt$O3 = as.integer(plt$O3)
  plt$Level = as.factor(plt$Level)
  return(plt)
}
plt = clean(plt)

# handle missing value
# 2016年4月16日的数据缺失
plt[106:107, ]
plt = add_row(plt, date = "2016-04-16", 
                   AQI = mean(plt[106:107, ]$AQI), 
                   Level = "轻度污染" , 
                   PM2.5 = mean(plt[106:107, ]$PM2.5), 
                   PM10 = mean(plt[106:107, ]$PM10), 
                   SO2 = mean(plt[106:107, ]$SO2), 
                   CO = mean(plt[106:107, ]$CO), 
                   NO2 = mean(plt[106:107, ]$NO2), 
                   O3 = mean(plt[106:107, ]$O3), 
                   Year = 2016, 
                   .before = 107)
plt[106:108, ]
plt = clean(plt)

# Insert average
getMid = function(date, level, year, former, latter){
  return(c( date, 
            mean(c(former$AQI, latter$AQI)), 
            level, 
            mean(c(former$PM2.5, latter$PM2.5)), 
            mean(c(former$PM10, latter$PM10 )), 
            mean(c(former$SO2, latter$SO2)), 
            mean(c(former$CO, latter$CO)), 
            mean(c(former$NO2, latter$NO2)), 
            mean(c(former$O3, latter$O3)), 
            year))
}
which(plt$AQI == 0)#250 818 856 877 879 有五个数据出现问题

# 2016年9月6日，使用这两天前后两日的平均值填补缺失值
plt[249:251, ]
plt[250, ] = getMid("2016-09-06", "良", 2016, plt[249, ], plt[251, ])
plt[249:251, ]
plt = clean(plt)

# 对其他四个点做同样处理
plt[817:819, ]
plt[818, ] = getMid("2018-03-28", "中度污染", 2018, plt[817, ], plt[819, ])
plt = clean(plt)
plt[817:819, ]

plt[855:857, ]
plt[856, ] = getMid("2018-05-05", "轻度污染", 2018, plt[855, ], plt[857, ])
plt = clean(plt)
plt[855:857, ]

plt[876:878, ]
plt[877, ] = getMid("2018-05-26", "轻度污染", 2018, plt[876, ], plt[878, ])
plt = clean(plt)
plt[876:878, ]

plt[878:880, ]
plt[879, ] = getMid("2018-05-28", "良", 2018, plt[878, ], plt[880, ])
plt = clean(plt)
plt[878:880, ]

# 出于横向对比需要，并基于便利性考虑，去掉2016年2月29日的数据
idx <- which(plt$date == "2016-2-29")
plt = plt[-c(idx), ];
plt
Origin_data = plt

### Part 2 Visulization
## 2.1 Annual Calendar Thermal Map of AQI in Beijing from 2016 to 2018
breaks = c(0, 50, 100, 150, 200, 300, 501)
label = c("excellent", "good", "Mild pollution", "moderate pollution", "heavy pollution ", "serious pollution")
calendarPlot(plt, pollutant="AQI", breaks=breaks, labels=label, w.shift=2, year=2016)
calendarPlot(plt, pollutant="AQI", breaks=breaks, labels=label, w.shift=2, year=2017)
calendarPlot(plt, pollutant="AQI", breaks=breaks, labels=label, w.shift=2, year=2018)

## 2.2 Cross-sectional monthly and quarterly charts from 2016 to 2018
# 提取本张图所需变量
plt2 = plt[c("date", "AQI", "Year", "Level")]
# 画图时保证数据均匀分布的预处理
plt2_2018 = plt2[which(plt2$Year==2018), ]
Max_2018_index = 365*2 + which(plt2_2018$AQI == max(plt2_2018$AQI))
plt2[Max_2018_index, ]$AQI=303

# 构造日、月和季度的标签，转换年为Factor
plt2$ID = rep(seq(from=1, to=365), 3)
plt2$Month = month(plt2$date)
plt2$Quarter = quarter(plt2$date)
plt2$Year = factor(plt2$Year, order=T)

# 圆环的角度设计
circlemonth = seq(15, 345, length=12)
circlebj = rep(c(-circlemonth[1:3], rev(circlemonth[1:3])), 2)
circlequarter = seq(45, 315, length=4)
circleqd = rep(c(-circlequarter[1], circlequarter[1]), 2)

# 每个数据的长度
plt2$Monthdata = -5

# 把不同年份的数据，用不同的距离区分开，由近到远为2016，2017和2018
plt2$Asst = 5
plt2$Asst[plt2$Year==2017] = 10
plt2$Asst[plt2$Year==2018] = 15

# A为双月，B为单月
plt2$Monthjo = ifelse(plt2$Month%%2==0, "A", "B")
# A为2016年双月，B为2016年单月
plt2A = plt2[plt2$Year==2016 & plt2$Monthjo=="A", ]
plt2B = plt2[plt2$Year==2016 & plt2$Monthjo=="B", ]
plt2$Quarterdata = 20
# C为2016年1～3和7～9月 D为2016年4～6 10～12
plt2C = plt2%>%filter(plt2$Year==2016)%>%filter(Quarter %in% c(1, 3)) 
plt2D = plt2%>%filter(plt2$Year==2016)%>%filter(Quarter %in% c(2, 4)) 

# 将数值型的AQI指数根据污染指数标准进行分割：
plt2$FADD = cut(plt2$AQI, breaks=c(0, 50, 100, 150, 200, 300, 501), labels=c("0~50", "51~100", "101~150", "151~200", "201~300", "301~500"), order=T)

# Plotting
CairoPNG(file="ECOCircle.png", width=1488, height=996, dpi=100)
showtext.begin()
ggplot()+
  # 最外层的季度标签
  geom_bar(data=plt2A, aes(x=ID, y=Monthdata), stat="identity", width=1, fill="#ECEDD1", col="#ECEDD1")+
  geom_bar(data=plt2B, aes(x=ID, y=Monthdata), stat="identity", width=1, fill="#DFE0B1", col="#DFE0B1")+
  geom_bar(data=plt2C, aes(x=ID, y=Quarterdata), stat="identity", width=1, fill="#BDBDBD", col="#BDBDBD")+
  geom_bar(data=plt2D, aes(x=ID, y=Quarterdata), stat="identity", width=1, fill="#D4D2D3", col="#D4D2D3")+
  
  # 每年的数据用不同的长度区分为不同层，然后再进行极坐标转换
  geom_bar(data=plt2[plt2$Year==2018, ], aes(x=ID, y=Asst, fill=FADD), stat="identity", width=1)+
  geom_bar(data=plt2[plt2$Year==2017, ], aes(x=ID, y=Asst, fill=FADD), stat="identity", width=1)+
  geom_bar(data=plt2[plt2$Year==2016, ], aes(x=ID, y=Asst, fill=FADD), stat="identity", width=1)+
  scale_fill_brewer(palette="YlOrRd", type="seq", direction=1, guide=guide_legend(reverse=T))+
  coord_polar(theta="x")+
  ylim(-20, 20)+
  guides(colour=guide_legend(reverse=TRUE))+
  geom_text(data=NULL, aes(x=circlemonth, y=-2.5, label=paste0(1:12, "月"), angle=circlebj), family="myfont", size=7, hjust=0.5, vjust=.5)+
  geom_text(data=NULL, aes(x=circlequarter, y=17.5, label=paste0(c("一", "二", "三", "四"), "季度"), angle=circleqd), family="myfont", size=7)+
  annotate("text", x=0, y=-15, label="北京", size=25, hjust=.5, vjust=1, family="myfont") +    
  labs(title="北京市2016~2018空气质量AQI水平对比", subtitle="由里到外分别为2016、2017、2018", x="", y="", fill="")+
  theme(
    text=element_text(family="myfont"), 
    axis.text=element_blank(), 
    axis.title=element_blank(), 
    axis.ticks=element_blank(), 
    panel.background=element_blank(), 
    panel.grid=element_blank(), 
    panel.border=element_blank(), 
    legend.key.size=unit(1.2, 'cm'), 
    legend.key.height=unit(1, 'cm'), 
    legend.text.align=1, 
    legend.position="right", legend.justification=c(1, 0), 
    legend.text=element_text(size=20, face="bold"), 
    plot.background=element_blank(), 
    plot.title=element_text(size=50, lineheight=1.5), 
    plot.subtitle=element_text(size=35, lineheight=1.5), 
    plot.caption=element_text(size=25, hjust=0, lineheight=1.2), 
    plot.margin=unit(c(.5, .5, .5, .5), "lines") )
showtext_end()
dev.off()

## 3. Timeseries plot
# 原数据时间序列折线图
setEPS()
postscript("原数据时间序列折线图.eps")
ggplot(plt, aes(date, AQI)) +  
  geom_line(color="#003399")+ 
  ggtitle("Time Series")+
  theme(plot.title = element_text(face="bold", hjust = 0.5))+
  coord_fixed(ratio = 1)
dev.off()

# 周和月的时间序列图
plt$AQI_ma = ma(plt$AQI, order=7)
plt$AQI_ma30 = ma(plt$AQI, order=30)
setEPS()
postscript("日、月、月均时间序列折线图.eps")
ggplot() +
  geom_line(data = plt, aes(x = date, y = AQI, colour = "Daily AQI")) +
  geom_line(data = plt, aes(x = date, y = AQI_ma, colour = "Weekly Moving Average"))  +
  geom_line(data = plt, aes(x = date, y = AQI_ma30, colour = "Monthly Moving Average"))  + 
  ggtitle("Weekly and Monthly Smoothed Time Series")+
  theme(plot.title = element_text(face="bold", hjust = 0.5))+
  ylab('AQI')+
  coord_fixed(ratio = 1)
dev.off()

#### Part II: model analysis, prediction and effect evaluation ####
### 1 Differential integrated moving average autoregression (ARIMA) ###
## 1.1 Data detection
# ADF stability test: data is stable
adf.test(plt$AQI, alternative = "stationary") #p value is 0.01, reject the original hypothesis of data instability, and consider the original data to be stab

# 将时间序列拆解
AQI_month =  ts(plt$AQI, frequency=30) 

# 用平滑的方式计算时间序列中的季节性成分
decomp =  stl(AQI_month, s.window="periodic") 
plot(decomp, main="Time Series Decomposing")

# 调整原始数据，减去季节性成分
deseasonal_cnt =  seasadj(decomp) 
# 这一波可以看到数据forecast(fit, h=14)

## 1.2 Use ACF and PACF to decide p and q
ggAcf(plt$AQI)+
  ggtitle("ACF Plot")+
  theme(plot.title = element_text(face="bold", hjust = 0.5))
ggPacf(plt$AQI)+
  ggtitle("PACF Plot")+
  theme(plot.title = element_text(face="bold", hjust = 0.5))
# Based on plot, we could use AR(2)or AR(3).

## 1.3 Modelling# 取Test setn = length(plt$AQI)
# test set
n = length(plt$AQI)
test_len = 14 #预测两周的AQI值
train_len = n - test
train_ARIMA = plt[-((n-test_len+1):n), ]

# models
## auto
fit1 = auto.arima(train_ARIMA$AQI, seasonal=T, stepwise=T)
fit1

## based on PACF and ACF plot
fit2 = Arima(train_ARIMA$AQI, order = c(3, 0, 0))
fit2
fit3 = Arima(train_ARIMA$AQI, order = c(2, 0, 0))
fit3

# 对数据按月、季做移动平均处理后再考虑季节性因素
AQI_month =  ts(plt$AQI, frequency=30)
fit4 = auto.arima(AQI_month, seasonal = T, trace = T)
fit4

AQI_season =  ts(plt$AQI, frequency=90)
fit5 = auto.arima(AQI_season, seasonal = T, trace = T)
fit5

# 对比上述五个模型的AIC值和Log likelihood，选择fit1模型

## 4. model comparing
# Prediction Result
frcst = forecast(fit, h=test_len)
frcst %>% autoplot(include=700)+
  ggtitle("Forecast from fit: ARIMA(5, 0, 3)")+
  theme(plot.title = element_text(face="bold", hjust = 0.5))+ ylab('AQI')+xlab('')

# Compare to ture value
Vs_ARIMA = data.frame( "Date" = plt$date, 
                       "Actual" = plt$AQI, 
                       "Fitted" = c(frcst$fitted[1:(n-test_len)], frcst$mean[1:test_len]))

## 5. Model Assesment
# MAPE (mean absolute percentage error)
MAPE = function(mape){
  mean(abs(mape$Actual - mape$Fitted) / mape$Fitted)}
MSE = function(mse){
  sum(mse$Actual - mse$Fitted)^2 / (nrow(mse)-1)}
mape_ARIMA = MAPE(Vs_ARIMA)
mse_ARIMA = MSE(Vs_ARIMA)

# 95% CI
posterior.interval <- cbind.data.frame(
  frcst$lower[1:test_len, 2], 
  frcst$upper[1:test_len, 2], 
  plt$date[(n+1-test_len):n])
names(posterior.interval) <- c("LL", "UL", "Date")

VS_ARIMA <- left_join(Vs_ARIMA, posterior.interval, by="Date") #直接将置信区间加入data.frame的右侧

# Result
ggplot(data=VS_ARIMA, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-12-18")), linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("  ARIMA(5, 3, 0):      Accurate = ", 100 - round(100*mape_ARIMA, 2), "%", 
                 " MAPE = ", round(100*mape_ARIMA, 2), "%", 
                 " MSE = ", round(mse_ARIMA, 2))) +
  theme(axis.text.x=element_text(angle = 0, hjust = 0.5), 
        plot.title = element_text(face="bold", hjust = 0.4))
ggplot(data=VS_ARIMA[(1095-3*test_len):1095, ], aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-12-18")), linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("  ARIMA(5, 3, 0):      Accurate = ", 100 - round(100*mape_ARIMA, 2), "%", 
                 " MAPE = ", round(100*mape_ARIMA, 2), "%", 
                 " MSE = ", round(mse_ARIMA, 2))) +
  theme(axis.text.x=element_text(angle = 0, hjust = 0.5), 
        plot.title = element_text(face="bold", hjust = 0.4))+
  scale_x_date(date_breaks = "1 month")

# Residule test
checkresiduals(fit) # 展示了有季节性因素的影响
Box.test(fit$residuals) # 0.8457>0.05，接受原假设，认为残差为白噪声，说明建模成功

### 2. Bayes model
## 2.1 Data prepare
ss <- AddLocalLinearTrend(list(), plt$AQI)
ss <- AddSeasonal(ss, plt$AQI, nseasons = 3)
bsts.model <- bsts(plt$AQI, state.specification = ss, niter = 30, ping=0)
burn <- SuggestBurn(0.05, bsts.model)

## 2.2 Predict
horizon = 14
p <- predict.bsts(bsts.model, horizon = 14, burn = burn, quantiles = c(.025, .975))

## 2.3 Result test
d2 <- data.frame(
  c(as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:length(p$mean)), ])+plt$AQI, 
               p$mean)), plt$AQI, plt$date)
names(d2) <- c("Fitted", "Actual", "Date")

mape_Bayesian = MAPE(d2)
mse_Bayesian = MSE(d2)

# CI
posterior.interval <- cbind.data.frame(
  p$interval[1, ], 
  p$interval[2, ], 
  plt$date[(1095+1-length(p$interval[1, ])):1095])
names(posterior.interval) <- c("LL", "UL", "Date")

d3 <- left_join(d2, posterior.interval, by="Date") #直接将置信区间加入data.frame的右侧

# prediction plot
ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-12-18")), linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("  Bayesian:      Accurate = ", 100 - round(100*mape_Bayesian, 2), "%", 
                 " MAPE = ", round(100*mape_Bayesian, 2), "%", 
                 " MSE = ", round(mse_Bayesian, 2))) +
  theme(axis.text.x=element_text(angle = 0, hjust = 0.5), 
        plot.title = element_text(face="bold", hjust = 0.4))

# partial predcition plot
ggplot(data=d3[(1095-3*15):1095, ], aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-12-18")), linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("  Bayesian:      Accurate = ", 100 - round(100*mape_Bayesian, 2), "%", 
                 " MAPE = ", round(100*mape_Bayesian, 2), "%", 
                 " MSE = ", round(mse_Bayesian, 2))) +
  theme(axis.text.x=element_text(angle = 0, hjust = 0.5), 
        plot.title = element_text(face="bold", hjust = 0.4))+
  scale_x_date(date_breaks = "1 month")

### 3. neural network ###
## 3.1 数据归一化处理
plt_n = plt
str(plt_n)
plt_n = na.omit(plt_n)

mu = mean(plt_n$AQI)
sd = sd(plt_n$AQI)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
plt_n$date[is.na(plt_n$date)] <- getmode(plt_n$date)
plt_n$AQI = (plt_n$AQI-mean(plt_n$AQI))/sd(plt_n$AQI)
plt_n$PM2.5= (plt_n$PM2.5-mean(plt_n$PM2.5))/sd(plt_n$PM2.5)
plt_n$PM10= (plt_n$PM10-mean(plt_n$PM10))/sd(plt_n$PM10)
plt_n$SO2= (plt_n$SO2-mean(plt_n$SO2))/sd(plt_n$SO2)
plt_n$CO= (plt_n$CO-mean(plt_n$CO))/sd(plt_n$CO)
plt_n$NO2= (plt_n$NO2-mean(plt_n$NO2))/sd(plt_n$NO2)
plt_n$O3= (plt_n$O3-mean(plt_n$O3))/sd(plt_n$O3)

n = dim(plt_n)[1]

## 3.2 训练集和测试集选择
data_x = plt_n[1:n, c('PM2.5', 'PM10', 'SO2', 'CO', 'NO2', 'O3')]
data_y = plt_n[1:n, c('date', 'AQI')]
train_y = data_y[1:round(n*0.7), c('date', 'AQI')]
train = data_x[1:round(n*0.7), c('PM2.5', 'PM10', 'SO2', 'CO', 'NO2', 'O3')]
train$AQI = train_y$AQI
test_x = data_x[(round(n*0.7)+1):n, c('PM2.5', 'PM10', 'SO2', 'CO', 'NO2', 'O3')]
test_y = data_y[(round(n*0.7)+1):n, c('date', 'AQI')]
test_len_nn = n - length("train$AQI")

## 3.3 参数搜索
# 搜索最佳的第一层节点数(取第二层为3)
max_layer = 8
rmse = rep(0, max_layer)
thresh = c(1:length(rmse))
name = names(train)
f = as.formula(paste("AQI~", paste(name[!name %in% c("AQI")], collapse = " + ")))

result = data.frame()
max_iter = 10
for(iter in 1:max_iter){
  print(paste0("第", iter, "次迭代"))
  for(i in 1:length(rmse)){
    print(thresh[i])
    nn = neuralnet::neuralnet(f, data=as.data.frame(train), hidden=c(thresh[i], 3), 
                               linear.output=TRUE, threshold=0.1, algorithm = "rprop+", stepmax=1e6)
    pr.nn = (neuralnet::compute(nn, test_x)$net.result) * sd + mu
    test.r = test_y$AQI*sd + mu
    MSE.nn = sum((test.r - pr.nn)^2) / (nrow(test_y)-1)
    print(sqrt(MSE.nn))
    rmse[i] = sqrt(MSE.nn)
  }
  result = rbind(result,rmse)
}

# 以迭代10次为例：
m = data.frame()
for(j in 1:max_layer){
  for(i in 1:max_iter){
  m = rbind(m,c(j,result[i,j]))}
}
line = data.frame()
for(j in 1:max_layer){
  line = rbind(line,c(j,mean(result[,j])))
}
plot(m, main = "First Layer", ylab="Average RMSE of 10 Iterations", xlab="")  #图形中可以看出 第以层的最佳节点数可以在236中进一步取
lines(line,col = "red")

# 搜索最佳的第二层节点数(取第一层为6)
max_layer = 5
rmse = rep(0, max_layer)
thresh = c(1:length(rmse))
result = data.frame()
max_iter = 10
for(iter in 1:max_iter){
  print(paste0("第",iter,"次迭代"))
  for(i in 1:length(rmse)){
    print(thresh[i])
    nn = neuralnet::neuralnet(f, data=as.data.frame(train), hidden=c(6, thresh[i]), 
                               linear.output=TRUE, threshold=0.1, algorithm = "rprop+", stepmax=1e6)
    pr.nn = (neuralnet::compute(nn, test_x)$net.result) * sd + mu
    test.r = test_y$AQI*sd+mu
    MSE.nn = sum((test.r - pr.nn)^2) / (nrow(test_y)-1)
    print(sqrt(MSE.nn))
    rmse[i] = sqrt(MSE.nn)
  }
  result = rbind(result,rmse)
}

# 以迭代10次为例：
m = data.frame()
for(j in 1:max_layer){
  for(i in 1:max_iter){
    m = rbind(m, c(j,result[i, j]))}
}
line = data.frame()
for(j in 1:max_layer){
  line = rbind(line, c(j, mean(result[, j])))
}

plot(m, main = "Second Layer", ylab="Average RMSE of 10 Iterations", xlab="")  #图形中可以看出 第一层的最佳节点数可以在123中进一步取
lines(line, col = "red")

## 3.4 Model and reslt

# model
network = neuralnet::neuralnet(f, data=as.data.frame(train), hidden=c(6, 3), 
                           linear.output=TRUE, threshold=0.1, algorithm = "rprop+", stepmax=1e6)
CairoPNG(file="ECOCie.png", dpi=100)
plot(network, main="Neutral Network")
dev.off()
# predict
str(network)
network$response
nn_predict = compute(network, test_x)

# Model assesment
Vs_Network = data.frame('Date' = plt$date, 
                        "Actual" = plt$AQI, 
                        "Fitted" = c(network$response * sd(plt$AQI) + mean(plt$AQI), nn_predict$net.result * sd(plt$AQI)+mean(plt$AQI)))
mape_nn = MAPE(Vs_Network)
mse_nn = MSE(Vs_Network)

# Prediction
ggplot(data=Vs_Network, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-02-10")), linetype=2) + 
  #geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) + 置信区间可以使用nnetpredint包构造，此处省略
  ggtitle(paste0(" Neutral Network:      Accurate = ", 100 - round(100*mape_nn, 2), "%", 
                 " MAPE = ", round(100*mape_nn, 2), "%", 
                 " MSE = ", round(mse_nn, 2))) +
  theme(plot.title = element_text(face="bold", hjust = 0.5))

###  4 Long and short term memory model LSTM in recurrent neural network###
## 4.1 data processing
# Differencing
Series = plt$AQI
plt
diffed = diff(Series, differences = 1)

# lagging
k=1
lag_transform = function(x, k){           
  lagged = c(rep(NA, k), x[1:(length(x)-k)])       
  DF = as.data.frame(cbind(lagged, x))       
  colnames(DF) = c( paste0('x-', k), 'x')       
  DF[is.na(DF)] = 0       
  return(DF)
}
supervised = lag_transform(diffed, k)
head(supervised)

# Divide dataset
test_len_lstm = 180
n = length(plt$AQI)-k
test = supervised[(n-test_len_lstm+1):n, ]
train = supervised[1:(n-test_len_lstm), ]
train_len_lstm = nrow(train)

# Normalize

scale_data = function(train, test, feature_range = c(0, 1)){
  # Input two groups of data and normalization range, and output the list composed of two groups of data after normalization processing
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x)) / (max(x) - min(x)))
  std_test  = ((test - min(x)) / (max(x) - min(x)))
  scaled_train = std_train * (fr_max - fr_min) + fr_min
  scaled_test  = std_test  * (fr_max - fr_min) + fr_min
  return( list(scaled_train = as.vector(scaled_train), 
               scaled_test = as.vector(scaled_test), 
               scaler = c(min = min(x), max = max(x))) )
}

#  Normalize data from - 1 to 1
Scaled = scale_data(train, test, c(-1, 1))
y_train = Scaled$scaled_train[, 2]
y_test = Scaled$scaled_test[, 2]
x_train = Scaled$scaled_train[, 1]
x_test = Scaled$scaled_test[, 1]

invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  # Enter two sets of data and the range of normalisation
  # Scaled: data that has been normalized
  # Scaler: an array composed of the maximum value and the minimum value of the original data during normalization

  min = scaler[1]   
  max = scaler[2]   
  t = length(scaled)   
  mins = feature_range[1]   
  maxs = feature_range[2]   
  inverted_dfs = numeric(t)
  # return the normalized data vector to the original length
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min     
    inverted_dfs[i] = rawValues   
  }
  return(inverted_dfs) 
}

# Convert input data to 3 dimentins
dim(x_train) = c(length(x_train), 1, 1)
X_shape2 = dim(x_train)[2] 
X_shape3 = dim(x_train)[3] 
batch_size = 1 # must be a common factor of both the train and test samples 
units = 1 # can adjust this, in model tuninig phase

## 4.2 Model
model = keras_model_sequential()
model %>% layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful = T) %>% layer_dense(units = 1)
model %>% compile(loss = 'mean_squared_error', optimizer = 'adam', metrics = c('mean_absolute_error'))
summary(model)

## 4.3 Learning processure
Epochs = 50
batch_size = 1
Learning_process = model %>% fit(
  x_train, y_train, 
  epochs = Epochs, batch_size = batch_size, 
  validation_split = 0.01
)
plot(Learning_process)  #经过50次学习之后，损失率降到0.025%水平

## 4. Prediction and model assesment
L = length(x_test)
dim(x_test) = c(length(x_test), 1, 1)
scaler = Scaled$scaler
predictions = numeric(L) 

for(i in 1:L){
  X = x_test[i]      
  dim(X) = c(1, 1, 1)
  yhat = model %>% predict(X, batch_size=batch_size)
  # invert scaling      
  yhat = invert_scaling(yhat, scaler, c(-1, 1))
  # invert differencing      
  yhat  = yhat + Series[(train_len_lstm+i)]      
  predictions[i] = yhat 
}
predictions

# result evaluation
Vs_LSTM = data.frame('Date' = plt$date[-1095], 'Actual' = plt$AQI[-1095], 
                     'Fitted' = c(rep(NA, train_len_lstm), predictions))
mape_lstm = MAPE(Vs_LSTM[(1095-test_len_lstm ):(1095-k), ])
mse_lstm = MSE(Vs_LSTM[(1095-test_len_lstm ):(1095-k), ])
ggplot(data=Vs_LSTM[366:1094, ], aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2018-07-04")), linetype=2) + 
  #geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) + 置信区间可以使用nnetpredint包构造，此处省略
  ggtitle(paste0(" LSTM:      Accurate = ", 100 - round(100*mape_lstm, 2), "%", 
                 " MAPE = ", round(100*mape_lstm, 2), "%", 
                 " MSE = ", round(mse_lstm, 2))) +
  theme(plot.title = element_text(face="bold", hjust = 0.5))


### GARCH and Auto-regressive

## GARCH
spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL, 
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 
                   
                   mean.model     = list(armaOrder = c(1, 1), 
                                         external.regressors = NULL, 
                                         distribution.model = "norm", 
                                         start.pars = list(), 
                                         fixed.pars = list()))

garch <- ugarchfit(spec = spec, data = plt, solver.control = list(trace=0))
garch_fixed_spec

## simulate one path
T <- 1095
set.seed(42)
path_garch <- ugarchpath(garch_fixed_spec, n.sim = T)
str(path_garch@path$seriesSim)

## loop
estim_coeffs_vs_T <- error_coeffs_vs_T <- NULL
T_sweep <- ceiling(seq(1000, T, length.out = 7))
for (T_ in T_sweep) {
  garch_fit <- ugarchfit(spec = garch_spec, data = synth_log_returns[1:T_])
  error_coeffs_vs_T <- rbind(error_coeffs_vs_T, abs((coef(garch_fit) - true_params)/true_params))
  estim_coeffs_vs_T <- rbind(estim_coeffs_vs_T, coef(garch_fit))
}
rownames(error_coeffs_vs_T) <- rownames(estim_coeffs_vs_T) <- paste("T =", T_sweep)

## plots
matplot(T_sweep, 100*error_coeffs_vs_T, 
        main = "Relative error in estimated GARCH coefficients", 
        xlab = "T", ylab = "error (%)",
        type = "b", pch = 20, col = rainbow(5), ylim=c(-10,250))
legend("topright", inset = 0.01, legend = colnames(error_coeffs_vs_T), pch = 20, col = rainbow(5))

# forecast log-returns along the whole out-of-sample
garch_fore <- ugarchforecast(plt, n.ahead = 1, n.roll = out_of_sample-1)
forecast_log_returns <- xts(garch_fore@forecast$seriesFor[1, ], dates_out_of_sample)
forecast_volatility <- xts(garch_fore@forecast$sigmaFor[1, ], dates_out_of_sample)

# plot of log-returns
plot(cbind("fitted"   = fitted(plt),
           "forecast" = forecast_log_returns,
           "original" = synth_log_returns), 
     col = c("blue", "red", "black"), lwd = c(0.5, 0.5, 2),
     main = "Forecast of synthetic log-returns", legend.loc = "topleft")