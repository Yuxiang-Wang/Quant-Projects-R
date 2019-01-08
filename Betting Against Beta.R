library(BatchGetSymbols)
library(zoo)
first.date <- Sys.Date()-365*10
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
print(df.SP500$tickers)
tickers <- df.SP500$tickers

freq.data <- 'monthly'
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date, freq.data = freq.data)

tempprice = l.out$df.tickers
price= tempprice[,c("ticker","ref.date","volume","ret.adjusted.prices")]

######################################

FFmonthly <- read.csv("D:/F-F_Research_Data_Factors.csv")
FF = FFmonthly[FFmonthly$X>200810&FFmonthly$X<201612,]
price = price[price$ref.date>'2008-11-15'&price$ref.date<'2016-12-31',]

FF[,"lagMktRP"]=lag(FF$Mkt.RF,1)
FF[,"lagSMB"]=lag(FF$SMB,1)
FF[,"lagHML"]=lag(FF$HML,1)
FF[,"lagRF"]=lag(FF$RF,1)
FFlag = FF[,c("X","lagMktRP","lagSMB","lagHML","lagRF")][-1,]

df <- data.frame(ticker=character(),
                 alpha=double(),
                 beta=double())

######################################

width=36
n=97
k=1   ############ alpha beta
for( i in tickers){  # i="MMM"
  tryCatch({
  regdata = price[price$ticker==i,]
  
  for(j in width:n){  #j=12  # j=34 #j=117
    rollreg = regdata[(j-width+1):j,]
    rollFFlag = FFlag[(j-width+1):j,]
    RP = rollreg$ret.adjusted.prices*100 - rollFFlag$lagRF
    reg = lm(RP ~ rollFFlag$lagMktRP+rollFFlag$lagSMB+rollFFlag$lagHML) 
    temp = data.frame(i,date=rollreg$ref.date[width],alpha = reg$coefficients[1], beta = reg$coefficients[2])
    df = rbind(df,temp)
    }

  cat(k,i,"done","\n")
  k=k+1
  },error=function(e){
    cat("ERROR: ",i,"\n")
    })
}

##########################

date = df[df$i=='MMM',]$date
df.beta = df[order(df$date,df$beta),]
df.alpha = df[order(df$date,df$alpha),]

library(plyr)
count =count(df, "date")

require(psych)
summary.beta = describeBy(df.beta$beta, df.beta$date, mat=T)

reg = lm(alpha~ beta,data = df.beta)
summary(reg)
plot(df.beta$beta,df.beta$alpha)
abline(reg)

weightbeta = df.beta
weightbeta[,"weight"]=NA
for(i in date){  # i="2011-11-01"
  tempbeta = df.beta[df.beta$date==i,]
  tempbeta$rank = seq.int(nrow(tempbeta))
  tempbeta$weight = -2/sum((abs(tempbeta$rank - mean(tempbeta$rank)))) * (tempbeta$rank - mean(tempbeta$rank))
  for(j in tempbeta$i){  # j='NFLX'
    weightbeta$weight[(weightbeta$i==j)&(weightbeta$date==i)]=tempbeta$weight[tempbeta$i==j]
  }
}

weightreturn = merge(weightbeta,price,by.x=c("i","date"),by.y=c("ticker","ref.date"))
weightreturn = weightreturn[order(weightreturn$date,weightreturn$beta),]

lagweightreturn = weightreturn
lagweightreturn = lagweightreturn[order(lagweightreturn$i,lagweightreturn$date),]
lagweightreturn$lag.weight = NA
lagweightreturn$lag.weight <- c(NA, lagweightreturn$weight[-nrow(lagweightreturn)])
lagweightreturn$lag.weight[which(!duplicated(lagweightreturn$i))] <- NA
lagweightreturn = lagweightreturn[order(lagweightreturn$date,lagweightreturn$beta),]

############# position
for(i in date){   # i="2011-11-01"
  tempbeta = lagweightreturn[lagweightreturn$date==i,]
  templong = sum(tempbeta$beta[tempbeta$weight>0]*tempbeta$weight[tempbeta$weight>0])
  tempshort = sum(tempbeta$beta[tempbeta$weight<0]*tempbeta$weight[tempbeta$weight<0])
  longmulty = -tempshort/templong
  lagweightreturn$position[(lagweightreturn$date==i)&(lagweightreturn$weight>0)]=longmulty/(longmulty-1)
  lagweightreturn$position[(lagweightreturn$date==i)&(lagweightreturn$weight<0)]=1/(longmulty-1)
  lagweightreturn$position[(lagweightreturn$date==i)&(lagweightreturn$weight==0)]=0
}

##############################

lagweightreturn = lagweightreturn[order(lagweightreturn$i,lagweightreturn$date),]
lagweightreturn$lag.position = NA
lagweightreturn$lag.position <- c(NA, lagweightreturn$position[-nrow(lagweightreturn)])
lagweightreturn$lag.position[which(!duplicated(lagweightreturn$i))] <- NA

tempreturn = na.omit(lagweightreturn)
portreturn <- data.frame(date=double(),
                 return=double())

for( i in date[-1]){  # i="2011-12-01"  i=15523
  tempbeta = tempreturn[tempreturn$date==i,]
  return = sum(tempbeta$lag.position*tempbeta$lag.weight*tempbeta$ret.adjusted.prices)
  #print(i)
  temp = data.frame(date=i, return = return)
  portreturn = rbind(portreturn,temp)
}

# return represent the profit of last month.
# 2011-12-01 represent return during 2011-11
sum(portreturn$return)
portreturn$r=portreturn$return+1
prod(portreturn$r)
prod(portreturn$r)^(12/61)
summary(portreturn)
portreturn$up[portreturn$return>0]=1
portreturn$up[portreturn$return<=0]=0
sum(portreturn$up)
nrow(portreturn)
SRtrain=mean(portreturn$return)/sqrt(var(portreturn$return))

FFport = FFmonthly[FFmonthly$X>=201112&FFmonthly$X<=201612,]
RP = portreturn$return*100-FFport$RF
reg = lm(RP ~ FFport$Mkt.RF+FFport$SMB+FFport$HML)
summary(reg)
nrow(FFport)
nrow(portreturn)
FFport$Mkt=(FFport$Mkt.RF+FFport$RF)/100
SRMKT.train=mean(FFport$Mkt)/sqrt(var(FFport$Mkt))

lagweightreturn = lagweightreturn[order(lagweightreturn$date,lagweightreturn$beta),]

write.csv(portreturn, file = "D:/return.csv")

######################################## 2017 performance
FFtest = FFmonthly[FFmonthly$X>201310&FFmonthly$X<201712,]  # lag factors
nrow(FFtest)

pricetest= tempprice[tempprice$ref.date>'2013-11-30'&tempprice$ref.date<'2017-12-31',]

dftest <- data.frame(ticker=character(),
                     alpha=double(),
                     beta=double())

width=36
n=49
k=1   ############ alpha beta
for( i in tickers){  # i="A"
  tryCatch({
    regdata = pricetest[pricetest$ticker==i,]
    # nrow(regdata)
    for(j in width:n){  # j=36
      rollreg = regdata[(j-width+1):j,]
      rollFFlag = FFtest[(j-width+1):j,]
      RP = rollreg$ret.adjusted.prices*100 - rollFFlag$RF
      reg = lm(RP ~ rollFFlag$Mkt.RF+rollFFlag$SMB+rollFFlag$HML) ####### wait  ####### whether significant
      temp = data.frame(i,date=rollreg$ref.date[width],alpha = reg$coefficients[1], beta = reg$coefficients[2])
      dftest = rbind(dftest,temp)
    }
    
    cat(k,i,"done","\n")
    k=k+1
  },error=function(e){
    cat("ERROR: ",i,"\n")
  })
}

date = dftest[dftest$i=='MMM',]$date
dftest.beta = dftest[order(dftest$date,dftest$beta),]

require(psych)
summary.beta.test = describeBy(dftest.beta$beta, dftest.beta$date, mat=T)

reg = lm(alpha~ beta,data = dftest.beta)
summary(reg)
plot(dftest.beta$beta,dftest.beta$alpha)
abline(reg)

weightbeta.test = dftest.beta
weightbeta.test[,"weight"]=NA
for(i in date){  # i="2016-11-01"
  tempbeta.test = dftest.beta[dftest.beta$date==i,]
  tempbeta.test$rank = seq.int(nrow(tempbeta.test))
  tempbeta.test$weight = -2/sum((abs(tempbeta.test$rank - mean(tempbeta.test$rank)))) * (tempbeta.test$rank - mean(tempbeta.test$rank))
  for(j in tempbeta.test$i){  # j='BHF'
    weightbeta.test$weight[(weightbeta.test$i==j)&(weightbeta.test$date==i)]=tempbeta.test$weight[tempbeta.test$i==j]
  }
}

weightreturn.test = merge(weightbeta.test,pricetest,by.x=c("i","date"),by.y=c("ticker","ref.date"))
weightreturn.test = weightreturn.test[order(weightreturn.test$date,weightreturn.test$beta),]

lagweightreturn.test = weightreturn.test
lagweightreturn.test = lagweightreturn.test[order(lagweightreturn.test$i,lagweightreturn.test$date),]
lagweightreturn.test$lag.weight = NA
lagweightreturn.test$lag.weight <- c(NA, lagweightreturn.test$weight[-nrow(lagweightreturn.test)])
lagweightreturn.test$lag.weight[which(!duplicated(lagweightreturn.test$i))] <- NA
lagweightreturn.test = lagweightreturn.test[order(lagweightreturn.test$date,lagweightreturn.test$beta),]

############# position
for(i in date){   # i="2016-11-01"
  tempbeta.test = lagweightreturn.test[lagweightreturn.test$date==i,]
  templong.test = sum(tempbeta.test$beta[tempbeta.test$weight>0]*tempbeta.test$weight[tempbeta.test$weight>0])
  tempshort.test = sum(tempbeta.test$beta[tempbeta.test$weight<0]*tempbeta.test$weight[tempbeta.test$weight<0])
  longmulty.test = -tempshort.test/templong.test
  lagweightreturn.test$position[(lagweightreturn.test$date==i)&(lagweightreturn.test$weight>0)]=longmulty.test/(longmulty.test-1)
  lagweightreturn.test$position[(lagweightreturn.test$date==i)&(lagweightreturn.test$weight<0)]=1/(longmulty.test-1)
  lagweightreturn.test$position[(lagweightreturn.test$date==i)&(lagweightreturn.test$weight==0)]=0
}

lagweightreturn.test = lagweightreturn.test[order(lagweightreturn.test$i,lagweightreturn.test$date),]
lagweightreturn.test$lag.position = NA
lagweightreturn.test$lag.position <- c(NA, lagweightreturn.test$position[-nrow(lagweightreturn.test)])
lagweightreturn.test$lag.position[which(!duplicated(lagweightreturn.test$i))] <- NA

tempreturn.test = na.omit(lagweightreturn.test)
portreturn.test <- data.frame(date=double(),
                         return=double())

for( i in date[-(1:2)]){  # i="2011-12-01"  i=15523
  tempbeta = tempreturn.test[tempreturn.test$date==i,]
  return = sum(tempbeta$lag.position*tempbeta$lag.weight*tempbeta$ret.adjusted.prices)
  #print(i)
  temp = data.frame(date=i, return = return)
  portreturn.test = rbind(portreturn.test,temp)
}

# return represent the profit of last month.
# 2011-12-01 represent return during 2011-11
sum(portreturn.test$return)
portreturn.test$r=portreturn.test$return+1
prod(portreturn.test$r)
portreturn.test$up[portreturn.test$return>0]=1
portreturn.test$up[portreturn.test$return<=0]=0
sum(portreturn.test$up)
nrow(portreturn)
SRtest = mean(portreturn.test$return)/sqrt(var(portreturn.test$return))

write.csv(portreturn.test,file = "D:/turereturn.csv")

FFsr = FFmonthly[FFmonthly$X>201611&FFmonthly$X<201712,]
FFsr$Mkt = (FFsr$Mkt.RF+FFsr$RF)/100
SRMKTtest = mean(FFsr$Mkt.RF+FFsr$RF)/sqrt(var(FFsr$Mkt.RF+FFsr$RF))
SRMKTtest = mean(FFmonthly$Mkt.RF+FFmonthly$RF)/sqrt(var(FFmonthly$Mkt.RF+FFmonthly$RF))

##########################################

nrow(portreturn)

date[-1]
# Fama-French 5 factors, volumn, ted spread, lag
FF5<- read.csv("D:/data/F-F_Research_Data_5_Factors.csv")
MOM<- read.csv("D:/data/F-F_Momentum_Factor.csv")
library(readxl)
TED <- read_excel("D:/data/TED Spread revised.xls")
volatility <- read_excel("D:/data/volatility.xlsx")

FF5train=FF5[FF5$X>201107&FF5$X<201611,]
MOMtrain=MOM[MOM$date>201107&MOM$date<201611,]
Volatrain=volatility[volatility$date<'2016-11-01',]
TED=na.omit(TED)
TEDtrain = TED[TED$observation_date>'2011-07-31'&TED$observation_date<'2016-12-01',]
TEDtrain$year=substr(TEDtrain$observation_date,1,4)
TEDtrain$month=substr(TEDtrain$observation_date,6,7)

TEDmonth=data.frame(year=double(),month=double(),ted=double(),stdted=double())
year = TEDtrain$year[!duplicated(TEDtrain$year)]
month = TEDtrain[order(TEDtrain$year,TEDtrain$month),]$month[!duplicated(TEDtrain$month)][order(month)]
for( i in year){
  for( j in month){   #i=year[1] j=month[1]
    tempTED=TEDtrain[TEDtrain$year==i&TEDtrain$month==j,]
    meanTED=mean(tempTED$TEDRATE)
    std=sqrt(var(tempTED$TEDRATE))
    tempt=data.frame(year=i,month=j,ted=meanTED,stdted=std)
    TEDmonth=rbind(TEDmonth,tempt)
  }
}
TEDmonth=na.omit(TEDmonth)
TEDmonth=TEDmonth[c(-1,-2,-3),]
nrow(TEDmonth)

cvtrain <- data.frame(date=MOM$date[MOM$date>201110&MOM$date<201612],
                 return=portreturn$return*100,
                 lag.Mkt.RF=FF5train$Mkt.RF[-(1:2)],
                 lag.SMB=FF5train$SMB[-(1:2)],
                 lag.HML=FF5train$HML[-(1:2)],
                 lag.RMW=FF5train$RMW[-(1:2)],
                 lag.CMA=FF5train$CMA[-(1:2)],
                 lag.RF=FF5train$RF[-(1:2)],
                 lag.MOM=MOMtrain$Mom[-(1:2)],
                 lag.volaCBOE=volatility$CboeVolatility[(1:61)],
                 lag.volaCBOESP100=volatility$`CboeS&P100Volatility`[(1:61)],
                 lag.TED=TEDmonth$ted,
                 lag.stdTED=TEDmonth$stdted)

################# test
FF5test=FF5[FF5$X>201610&FF5$X<201711,]
MOMtest=MOM[MOM$date>201610&MOM$date<201711,]
Volatest=volatility[volatility$date>'2016-10-31'&volatility$date<'2017-11-01',]

TEDtest = TED[TED$observation_date>'2016-10-31'&TED$observation_date<'2017-11-01',]
TEDtest$year=substr(TEDtest$observation_date,1,4)
TEDtest$month=substr(TEDtest$observation_date,6,7)

TEDmonth.test=data.frame(year=double(),month=double(),ted=double(),stdted=double())
year = TEDtest$year[!duplicated(TEDtest$year)]

for( i in year){
  for( j in month){   #i=year[1] j=month[1]
    tempTED=TEDtest[TEDtest$year==i&TEDtest$month==j,]
    meanTED=mean(tempTED$TEDRATE)
    std=sqrt(var(tempTED$TEDRATE))
    tempt=data.frame(year=i,month=j,ted=meanTED,stdted=std)
    TEDmonth.test=rbind(TEDmonth.test,tempt)
  }
}
TEDmonth.test=na.omit(TEDmonth.test)
nrow(TEDmonth.test)

cvtest <- data.frame(date=MOM$date[MOM$date>201611&MOM$date<201710],
                      return=(portreturn.test$return*100)[1:10],
                      lag.Mkt.RF=FF5test$Mkt.RF[1:10],
                      lag.SMB=FF5test$SMB[1:10],
                      lag.HML=FF5test$HML[1:10],
                      lag.RMW=FF5test$RMW[1:10],
                      lag.CMA=FF5test$CMA[1:10],
                      lag.RF=FF5test$RF[1:10],
                      lag.MOM=MOMtest$Mom[1:10],
                      lag.volaCBOE=Volatest$CboeVolatility[1:10],
                      lag.volaCBOESP100=Volatest$`CboeS&P100Volatility`[1:10],
                      lag.TED=TEDmonth.test$ted[-1],
                      lag.stdTED=TEDmonth.test$stdted[-1]
                     )


x.train=model.matrix(return~.,cvtrain[,-1])[,-1] 
y.train=cvtrain[,'return']
y.train2=cvtrain[,'return']-cvtrain$lag.RF
x.test=model.matrix(return~.,cvtest[,-1])[,-1] 
y.test=cvtest[,'return']
library(glmnet)
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x.train,y.train,alpha=1,lambda=grid)
lasso.mod2=glmnet(x.train,y.train2,alpha=1,lambda=grid)

plot(lasso.mod)
plot(lasso.mod2)

set.seed(1)
cv.out=cv.glmnet(x.train,y.train,alpha=1) 
cv.out2=cv.glmnet(x.train,y.train2,alpha=1) 
plot(cv.out)
plot(cv.out2)
bestlam=cv.out$lambda.min

lasso.coef=predict(cv.out,type="coefficients",s=bestlam)
lasso.coef=predict(cv.out2,type="coefficients",s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]

lasso.pred=predict(lasso.mod,s=bestlam,newx=x.test)
smse=sqrt(mean((lasso.pred-y.test)^2))

mean(portreturn.test$return*100)

lmreg=lm(return~lag.Mkt.RF+lag.HML+lag.RMW+lag.CMA+lag.MOM+lag.volaCBOE+lag.TED+lag.stdTED,data=cvtrain)
summary(lmreg)
lmreg=lm(return~lag.volaCBOE+lag.TED,data=cvtrain)
summary(lmreg)

############################################## sharp ratio, whole time period
portall=rbind(portreturn,portreturn.test)
mktall=rbind(FFport[,-(6:9)],FFsr)
srportall=mean(portall$return)/sqrt(var(portall$return))
nrow(portall)
srmktall=mean(mktall$Mkt)/sqrt(var(mktall$Mkt))



