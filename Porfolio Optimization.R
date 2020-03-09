# OR-538 FINAL PROJECT

# LIBRARIES Installation
install.packages("crypto") # Main library for crypto currencies
install.packages("quantmod") 
install.packages("corrplot") 
install.packages("quadprog") 
install.packages("ggplot2") 
install.packages("ggrepel") 



# Library Loading 
library(crypto) # Main library for crypto currencies
library(quantmod) # For daily return funtion
library(corrplot) # for correlation plot
library(quadprog) # for sove.qp()
library(ggplot2)
library(ggrepel) # to make texts away from each other
library(MASS)
library(fGarch)


#coin <- crypto_history( start_date = 20161001,
#                        end_date = 20190920, sleep = 1, coin ='BTC')

#coin <- crypto_history( start_date = 20191001,
#                        end_date = 20190920,  sleep = NULL)

??crypto

# START of PART
#***********************************************************************************************************
#***********************************************************************************************************
# Preparing the dataset
#***********************************************************************************************************
#***********************************************************************************************************

coinBTC <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='BTC')

coinLTC <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='LTC')

coinPPC <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='PPC')

coinNMC <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='NMC')

coinDOGE <- crypto_history( start_date = 20161001,
                            end_date = 20191001, sleep = 1, coin ='DOGE')

coinGRC <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='GRC')

coinXPM <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='XPM')

coinXRP <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='XRP')

coinNXT <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='NXT')

coinAUR <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='AUR')

coinDASH <- crypto_history( start_date = 20161001,
                            end_date = 20191001, sleep = 1, coin ='DASH')

coinNEO <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='NEO')

coinXMR <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='XMR')

coinXEM <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='XEM')

coinPOT <- crypto_history( start_date = 20161001,
                           end_date = 20191001, sleep = 1, coin ='POT')


# converting dataframe in to time series


#stocks <- xts(coinBTC[,-4], order.by=as.Date(coinBTC[,4], "%Y/%m/%d"))

stocks <- xts(coinBTC[,5:13], order.by=as.Date(coinBTC[,4], "%Y/%m/%d")) # only add numeric columns

stocks$BTC <- stocks$close
stocks$LTC <- coinLTC$close
stocks$PPC <- coinPPC$close
stocks$NMC <- coinNMC$close
stocks$DOGE <- coinDOGE$close
stocks$GRC <- coinGRC$close
stocks$XPM <- coinXPM$close
stocks$XRP <- coinXRP$close
stocks$NXT <- coinNXT$close
stocks$AUR <- coinAUR$close
stocks$DASG <- coinDASH$close
stocks$NEO <- coinNEO$close
stocks$XMR <- coinXMR$close
stocks$XEM <- coinXEM$close
stocks$POT <- coinPOT$close


stocks <- stocks[,-(1:9)]

crypto <- stocks # has price

rm(stocks)

dailycrypto <- dailyReturn(crypto$BTC) # created the dataframe


dailycrypto$BTC <- dailyReturn(crypto$BTC)
dailycrypto$LTC <- dailyReturn(crypto$LTC)
dailycrypto$PPC <- dailyReturn(crypto$PPC)
dailycrypto$NMC <- dailyReturn(crypto$NMC)
dailycrypto$DOGE <- dailyReturn(crypto$DOGE)
dailycrypto$GRC <- dailyReturn(crypto$GRC)
dailycrypto$XPM <- dailyReturn(crypto$XPM)
dailycrypto$XRP <- dailyReturn(crypto$XRP)
dailycrypto$NXT <- dailyReturn(crypto$NXT)
dailycrypto$AUR <- dailyReturn(crypto$AUR)
dailycrypto$DASG <- dailyReturn(crypto$DASG)
dailycrypto$NEO <- dailyReturn(crypto$NEO)
dailycrypto$XMR <- dailyReturn(crypto$XMR)
dailycrypto$XEM <- dailyReturn(crypto$XEM)
dailycrypto$POT <- dailyReturn(crypto$POT)

dailycrypto <- dailycrypto[,-1] # has returns

cryptoclose <- crypto
dailycryptoreturns <- dailycrypto

rm(crypto,dailycrypto)

#converting time series into dataframes

cryptoclosedf <- data.frame(as.matrix(cryptoclose), date=as.Date(as.yearmon(time(cryptoclose))))
dailycryptoreturnsdf <- data.frame(as.matrix(dailycryptoreturns), date=as.Date(as.yearmon(time(dailycryptoreturns))))

cryptoclosedf <- data.frame(as.matrix(cryptoclose))
dailycryptoreturnsdf <- data.frame(as.matrix(dailycryptoreturns))


# use the following command to set working directory in your PC
setwd("D:/MASON/Fall 2019/OR-538/Project/R-Code")

write.csv(cryptoclosedf,'cryptoclosedf.csv')
write.csv(dailycryptoreturnsdf,'dailycryptoreturnsdf.csv')



##################
# NOTE 
# if you have csv file run the following code 
##################

# cryptoclosedf <- read.csv("D:/MASON/Fall 2019/OR-538/Project/R-Code/cryptoclosedf.csv")
# dailycryptoreturnsdf <- read.csv("D:/MASON/Fall 2019/OR-538/Project/R-Code/dailycryptoreturnsdf.csv")

# set the path accordingly
# don't delete them just create another variable like path3 to add you directory and replace in read.csv()

# for User1
path1 = "cryptoclosedf.csv"
path2 = "dailycryptoreturnsdf.csv"

# for User2
#path3 =
#path4 = 

# for User3
#path5 =
#path6 =

cryptoclosedf <- read.csv(path1)
dailycryptoreturnsdf <- read.csv(path2)


cryptoclose <- xts(cryptoclosedf[,-1], order.by=as.Date(cryptoclosedf[,1], "%Y-%m-%d")) # only add numeric columns
dailycryptoreturns <- xts(dailycryptoreturnsdf[,-1], order.by=as.Date(dailycryptoreturnsdf[,1], "%Y-%m-%d")) # only add numeric columns

rm(path1,path2)

#getwd()
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# Preparing the dataset
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# END of PART


# START of PART
#***********************************************************************************************************
#***********************************************************************************************************
# Correlation Plot for the Cryptos
#***********************************************************************************************************
#***********************************************************************************************************

#library(corrplot) # for correlation plot


corrplot(cor(dailycryptoreturns), order = "AOE", type = "upper", tl.pos = "d", tl.cex = 0.8, tl.col = "red")
corrplot(cor(dailycryptoreturns), add = TRUE, type = "lower", method = "number", order = "AOE",
         col = "black", diag = FALSE, tl.pos = "n", cl.pos = "n", number.cex = 0.8)

#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# Correlation Plot for the Cryptos
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# END of PART



# START of PART
#***********************************************************************************************************
#***********************************************************************************************************
# Working on Optimization (8 MONTHS) 5% RISK
#***********************************************************************************************************
#***********************************************************************************************************

cryptoclosedf[1:2,-1]

n <- length(cryptoclosedf$BTC)

cryptoclosedf[731:1096,]

cryptoclosedf[1096,]


# Using the below values for setting time period 
s <- 851
n <- 1096

returns = 100 * (cryptoclosedf[(s+1):n,c(-1,-2)] / cryptoclosedf[s:(n-1),c(-1,-2)] - 1)

pairs(returns)

mean(returns$BTC)

mean_vect = colMeans(returns)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

l <- length(mean_vect)

Amat = cbind(rep(1,l), mean_vect, diag(1,nrow=l))  # set the constraints matrix


muP = seq(min(mean_vect)+.02,max(mean_vect)-.02,length=10)


muP = seq(-0.22, 4.25, length=300)  # set of 300 possible target values 
# for the expect portfolio return; note that due to the possibilities of short sale, the return could be 
# lower than the minimum return of the individual asset in the portfolio
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=l) # storage for portfolio weights


for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i], rep(0,l))  # constraint thresholds: no short selling, min weights are zero
  # In this optimization problem, no linear term, only quadratic term in the cost function
  result = solve.QP(Dmat = 2*cov_mat, dvec=rep(0,l), Amat=Amat, bvec=bvec, meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}



plot(sdP,muP,type="l",xlim=c(1,20),ylim=c(-1,2),lty=3)  #  plot 

mufree = 1.73/365 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=2) # show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier

#text(sd_vect[1],mean_vect[1],"BTC",cex=1.5)
#text(sd_vect[2],mean_vect[2],"LTC",cex=1.5)
#text(sd_vect[3],mean_vect[3],"UTX",cex=1.5)
#text(sd_vect[4],mean_vect[4],"CAT",cex=1.5)
#text(sd_vect[5],mean_vect[5],"MRK",cex=1.5)
#text(sd_vect[6],mean_vect[6],"IBM",cex=1.5)


for(j in 1:l)
{
  text(sd_vect[j],mean_vect[j],colnames(cryptoclosedf[j+2]),cex=0.5)
}


#lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=3) # show line of optimal portfolios

#sdP[match(5, trunc(sdP))]
#muP[match(5, trunc(sdP))]

#lines(c(5,sdP[match(5, trunc(sdP))]),c(-1,muP[match(5, trunc(sdP))]),lwd=4,lty=3) # show line of optimal portfolios

model <- lm(c(mufree,muP[ind])~c(0,sdP[ind])) # show line of optimal portfolios

#model <- lm(c(4,10)~c(2,8)) # show line of optimal portfolios

risk=5
model$coefficients[2]*risk + model$coefficients[1] # RETURN on % risk

#lines(c(5,5),c(-1,(model$coefficients[2]*5 + model$coefficients[1])))

#abline(coef = coef(model), v= risk, h= (model$coefficients[2]*risk + model$coefficients[1]))

# Creating chart using GGPLOT


mufree = 1.73/365 # input value of risk-free interest rate
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
ind3 = (muP > muP[ind2])


ggplotdf <- as.data.frame(sdP)


ggplotdf <- cbind(ggplotdf,muP)

chk <- data.frame(x1=c(0,sdP[ind]), y1=c(mufree,muP[ind]))  

for(j in 1:l)
{
  if(j == 1)
  {
    dfforlable = data.frame(a = sd_vect[j],b = mean_vect[j],c = colnames(cryptoclosedf[j+2]))
  }
  else
  {
    x = data.frame(a = sd_vect[j],b = mean_vect[j],c = colnames(cryptoclosedf[j+2]))
    dfforlable =  rbind(dfforlable,x)
  }
}

hline = model$coefficients[2]*risk + model$coefficients[1]


ggplot(ggplotdf) + geom_point(aes(sdP,muP)) + geom_point(aes(0,mufree), colour = 'red', size = 4) + geom_line(data = chk, aes(x1,y1), linetype =2, size = 1.5) +
  
  geom_point(aes(sdP[ind],muP[ind]),colour = 'blue', shape = 19, size = 4) + geom_text_repel(data = dfforlable, aes(a,b,label = c)) +
  geom_point(aes(sdP[ind2],muP[ind2]), colour = 'green', size =5) +
  geom_vline(xintercept = risk, size =1.5, colour = 'orange', linetype =3) + geom_hline(yintercept = hline, size =1.5,colour = 'orange', linetype =3)

colnames(returns)

#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# Working on Optimization (8 Months) 5% RISK
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# END of PART



# START of PART
#***********************************************************************************************************
#***********************************************************************************************************
# Working on Optimization (REMOVING AUR)
#***********************************************************************************************************
#***********************************************************************************************************

cryptoclosedf[1:2,-1]

n <- length(cryptoclosedf$BTC)

cryptoclosedf[731:1096,]

cryptoclosedf[1096,]


# Using the below values for setting time period 
s <- 851
n <- 1096

returns = 100 * (cryptoclosedf[(s+1):n,c(-1,-2,-11)] / cryptoclosedf[s:(n-1),c(-1,-2,-11)] - 1)

pairs(returns)

mean(returns$BTC)

mean_vect = colMeans(returns)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

l <- length(mean_vect)

Amat = cbind(rep(1,l), mean_vect, diag(1,nrow=l))  # set the constraints matrix


muP = seq(min(mean_vect)+.02,max(mean_vect)-.02,length=10)


muP = seq(-0.22, 0.62, length=300)  # set of 300 possible target values 
# for the expect portfolio return; note that due to the possibilities of short sale, the return could be 
# lower than the minimum return of the individual asset in the portfolio
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=l) # storage for portfolio weights


for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i], rep(0,l))  # constraint thresholds: no short selling, min weights are zero
  # In this optimization problem, no linear term, only quadratic term in the cost function
  result = solve.QP(Dmat = 2*cov_mat, dvec=rep(0,l), Amat=Amat, bvec=bvec, meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}



plot(sdP,muP,type="l",xlim=c(1,20),ylim=c(-1,2),lty=3)  #  plot 

mufree = 1.73/365 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=2) # show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier

#text(sd_vect[1],mean_vect[1],"BTC",cex=1.5)
#text(sd_vect[2],mean_vect[2],"LTC",cex=1.5)
#text(sd_vect[3],mean_vect[3],"UTX",cex=1.5)
#text(sd_vect[4],mean_vect[4],"CAT",cex=1.5)
#text(sd_vect[5],mean_vect[5],"MRK",cex=1.5)
#text(sd_vect[6],mean_vect[6],"IBM",cex=1.5)


for(j in 1:l)
{
  text(sd_vect[j],mean_vect[j],colnames(cryptoclosedf[j+2]),cex=0.5)
}


#lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=3) # show line of optimal portfolios

#sdP[match(5, trunc(sdP))]
#muP[match(5, trunc(sdP))]

#lines(c(5,sdP[match(5, trunc(sdP))]),c(-1,muP[match(5, trunc(sdP))]),lwd=4,lty=3) # show line of optimal portfolios

model <- lm(c(mufree,muP[ind])~c(0,sdP[ind])) # show line of optimal portfolios

#model <- lm(c(4,10)~c(2,8)) # show line of optimal portfolios

risk=5
model$coefficients[2]*risk + model$coefficients[1] # RETURN on % risk

#lines(c(5,5),c(-1,(model$coefficients[2]*5 + model$coefficients[1])))

#abline(coef = coef(model), v= risk, h= (model$coefficients[2]*risk + model$coefficients[1]))

# Creating chart using GGPLOT


mufree = 1.73/365 # input value of risk-free interest rate
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
ind3 = (muP > muP[ind2])


ggplotdf <- as.data.frame(sdP)


ggplotdf <- cbind(ggplotdf,muP)

chk <- data.frame(x1=c(0,sdP[ind]), y1=c(mufree,muP[ind]))  

for(j in 1:l)
{
  if(j == 1)
  {
    dfforlable = data.frame(a = sd_vect[j],b = mean_vect[j],c = colnames(returns[j]))
  }
  else
  {
    x = data.frame(a = sd_vect[j],b = mean_vect[j],c = colnames(returns[j]))
    dfforlable =  rbind(dfforlable,x)
  }
}

hline = model$coefficients[2]*risk + model$coefficients[1]


ggplot(ggplotdf) + geom_point(aes(sdP,muP)) + geom_point(aes(0,mufree), colour = 'red', size = 4) + geom_line(data = chk, aes(x1,y1), linetype =2, size = 1.5) +
  
  geom_point(aes(sdP[ind],muP[ind]),colour = 'blue', shape = 19, size = 4) + geom_text_repel(data = dfforlable, aes(a,b,label = c)) +
  geom_point(aes(sdP[ind2],muP[ind2]), colour = 'green', size =5) +
  geom_vline(xintercept = risk, size =1.5, colour = 'orange', linetype =3) + geom_hline(yintercept = hline, size =1.5,colour = 'orange', linetype =3)



#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# Working on Optimization (REMOVING AUR)
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# END of PART

# START of PART
#***********************************************************************************************************
#***********************************************************************************************************
# Working on Optimization (8 MONTHS) 1% RETURN
#***********************************************************************************************************
#***********************************************************************************************************

cryptoclosedf[1:2,-1]

n <- length(cryptoclosedf$BTC)

cryptoclosedf[731:1096,]

cryptoclosedf[1096,]


# Using the below values for setting time period 
s <- 851
n <- 1096

returns = 100 * (cryptoclosedf[(s+1):n,c(-1,-2)] / cryptoclosedf[s:(n-1),c(-1,-2)] - 1)

pairs(returns)

mean(returns$BTC)

mean_vect = colMeans(returns)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

l <- length(mean_vect)

Amat = cbind(rep(1,l), mean_vect, diag(1,nrow=l))  # set the constraints matrix


muP = seq(min(mean_vect)+.02,max(mean_vect)-.02,length=10)


muP = seq(-0.22, 4.25, length=300)  # set of 300 possible target values 
# for the expect portfolio return; note that due to the possibilities of short sale, the return could be 
# lower than the minimum return of the individual asset in the portfolio
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=l) # storage for portfolio weights


for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i], rep(0,l))  # constraint thresholds: no short selling, min weights are zero
  # In this optimization problem, no linear term, only quadratic term in the cost function
  result = solve.QP(Dmat = 2*cov_mat, dvec=rep(0,l), Amat=Amat, bvec=bvec, meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}



plot(sdP,muP,type="l",xlim=c(1,20),ylim=c(-1,2),lty=3)  #  plot 

mufree = 1.73/365 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=2) # show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier

#text(sd_vect[1],mean_vect[1],"BTC",cex=1.5)
#text(sd_vect[2],mean_vect[2],"LTC",cex=1.5)
#text(sd_vect[3],mean_vect[3],"UTX",cex=1.5)
#text(sd_vect[4],mean_vect[4],"CAT",cex=1.5)
#text(sd_vect[5],mean_vect[5],"MRK",cex=1.5)
#text(sd_vect[6],mean_vect[6],"IBM",cex=1.5)


for(j in 1:l)
{
  text(sd_vect[j],mean_vect[j],colnames(cryptoclosedf[j+2]),cex=0.5)
}


#lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=3) # show line of optimal portfolios

#sdP[match(5, trunc(sdP))]
#muP[match(5, trunc(sdP))]

#lines(c(5,sdP[match(5, trunc(sdP))]),c(-1,muP[match(5, trunc(sdP))]),lwd=4,lty=3) # show line of optimal portfolios

model <- lm(c(mufree,muP[ind])~c(0,sdP[ind])) # show line of optimal portfolios

#model <- lm(c(4,10)~c(2,8)) # show line of optimal portfolios

risk=5
model$coefficients[2]*risk + model$coefficients[1] # RETURN on % risk

#lines(c(5,5),c(-1,(model$coefficients[2]*5 + model$coefficients[1])))

#abline(coef = coef(model), v= risk, h= (model$coefficients[2]*risk + model$coefficients[1]))

# Creating chart using GGPLOT


mufree = 1.73/365 # input value of risk-free interest rate
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
ind3 = (muP > muP[ind2])


ggplotdf <- as.data.frame(sdP)


ggplotdf <- cbind(ggplotdf,muP)

chk <- data.frame(x1=c(0,sdP[ind]), y1=c(mufree,muP[ind]))  

for(j in 1:l)
{
  if(j == 1)
  {
    dfforlable = data.frame(a = sd_vect[j],b = mean_vect[j],c = colnames(cryptoclosedf[j+2]))
  }
  else
  {
    x = data.frame(a = sd_vect[j],b = mean_vect[j],c = colnames(cryptoclosedf[j+2]))
    dfforlable =  rbind(dfforlable,x)
  }
}

hline = model$coefficients[2]*risk + model$coefficients[1]

hline = 1

vline = (1 - model$coefficients[1]) / model$coefficients[2]

ggplot(ggplotdf) + geom_point(aes(sdP,muP)) + geom_point(aes(0,mufree), colour = 'red', size = 4) + geom_line(data = chk, aes(x1,y1), linetype =2, size = 1.5) +
  
  geom_point(aes(sdP[ind],muP[ind]),colour = 'blue', shape = 19, size = 4) + geom_text_repel(data = dfforlable, aes(a,b,label = c)) +
  geom_point(aes(sdP[ind2],muP[ind2]), colour = 'green', size =5) +
  geom_vline(xintercept = vline, size =1.5, colour = 'orange', linetype =3) + geom_hline(yintercept = hline, size =1.5,colour = 'orange', linetype =3)



#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# Working on Optimization (8 Months) 1% RETURN
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# END of PART


# START of PART
#***********************************************************************************************************
#***********************************************************************************************************
# Working on Optimization WHOLE
#***********************************************************************************************************
#***********************************************************************************************************

cryptoclosedf[1:2,-1]

n <- length(cryptoclosedf$BTC)

cryptoclosedf[731:1096,]

cryptoclosedf[1096,]


# Using the below values for setting time period 
s <- 1
n <- 1096

returns = 100 * (cryptoclosedf[(s+1):n,c(-1,-2)] / cryptoclosedf[s:(n-1),c(-1,-2)] - 1)

pairs(returns)

mean(returns$BTC)

mean_vect = colMeans(returns)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

l <- length(mean_vect)

Amat = cbind(rep(1,l), mean_vect, diag(1,nrow=l))  # set the constraints matrix


muP = seq(min(mean_vect)+.02,max(mean_vect)-.02,length=10)


muP = seq(0.28, 1.25, length=300)  # set of 300 possible target values 
# for the expect portfolio return; note that due to the possibilities of short sale, the return could be 
# lower than the minimum return of the individual asset in the portfolio
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=l) # storage for portfolio weights


for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i], rep(0,l))  # constraint thresholds: no short selling, min weights are zero
  # In this optimization problem, no linear term, only quadratic term in the cost function
  result = solve.QP(Dmat = 2*cov_mat, dvec=rep(0,l), Amat=Amat, bvec=bvec, meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}



plot(sdP,muP,type="l",xlim=c(1,20),ylim=c(-1,2),lty=3)  #  plot 

mufree = 1.73/365 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=2) # show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier

#text(sd_vect[1],mean_vect[1],"BTC",cex=1.5)
#text(sd_vect[2],mean_vect[2],"LTC",cex=1.5)
#text(sd_vect[3],mean_vect[3],"UTX",cex=1.5)
#text(sd_vect[4],mean_vect[4],"CAT",cex=1.5)
#text(sd_vect[5],mean_vect[5],"MRK",cex=1.5)
#text(sd_vect[6],mean_vect[6],"IBM",cex=1.5)


for(j in 1:l)
{
  text(sd_vect[j],mean_vect[j],colnames(cryptoclosedf[j+2]),cex=0.5)
}


#lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=3) # show line of optimal portfolios

#sdP[match(5, trunc(sdP))]
#muP[match(5, trunc(sdP))]

#lines(c(5,sdP[match(5, trunc(sdP))]),c(-1,muP[match(5, trunc(sdP))]),lwd=4,lty=3) # show line of optimal portfolios

model <- lm(c(mufree,muP[ind])~c(0,sdP[ind])) # show line of optimal portfolios

#model <- lm(c(4,10)~c(2,8)) # show line of optimal portfolios

risk=6
model$coefficients[2]*risk + model$coefficients[1] # RETURN on % risk

#lines(c(5,5),c(-1,(model$coefficients[2]*5 + model$coefficients[1])))

#abline(coef = coef(model), v= risk, h= (model$coefficients[2]*risk + model$coefficients[1]))

# Creating chart using GGPLOT


mufree = 1.73/365 # input value of risk-free interest rate
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
ind3 = (muP > muP[ind2])


ggplotdf <- as.data.frame(sdP)


ggplotdf <- cbind(ggplotdf,muP)

chk <- data.frame(x1=c(0,sdP[ind]), y1=c(mufree,muP[ind]))  

for(j in 1:l)
{
  if(j == 1)
  {
    dfforlable = data.frame(a = sd_vect[j],b = mean_vect[j],c = colnames(cryptoclosedf[j+2]))
  }
  else
  {
    x = data.frame(a = sd_vect[j],b = mean_vect[j],c = colnames(cryptoclosedf[j+2]))
    dfforlable =  rbind(dfforlable,x)
  }
}

hline = model$coefficients[2]*risk + model$coefficients[1]

#hline = 1

#vline = (1 - model$coefficients[1]) / model$coefficients[2]

ggplot(ggplotdf) + geom_point(aes(sdP,muP)) + geom_point(aes(0,mufree), colour = 'red', size = 4) + geom_line(data = chk, aes(x1,y1), linetype =2, size = 1.5) +
  
  geom_point(aes(sdP[ind],muP[ind]),colour = 'blue', shape = 19, size = 4) + geom_text_repel(data = dfforlable, aes(a,b,label = c)) +
  geom_point(aes(sdP[ind2],muP[ind2]), colour = 'green', size =5) +
  geom_vline(xintercept = risk, size =1.5, colour = 'orange', linetype =3) + geom_hline(yintercept = hline, size =1.5,colour = 'orange', linetype =3)



#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# Working on Optimization WHOLE
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# END of PART


# START of PART
#***********************************************************************************************************
#***********************************************************************************************************
# Working Value at Risk (VaR) 
#***********************************************************************************************************
#***********************************************************************************************************

#confidence file


cryptoclosedf[1:2,-1]

n <- length(cryptoclosedf$BTC)

cryptoclosedf[731:1096,]

cryptoclosedf[1096,]


# Using the below values for setting time period 
s <- 851
n <- 1096

returns = 100 * (cryptoclosedf[(s+1):n,c(-1,-2)] / cryptoclosedf[s:(n-1),c(-1,-2)] - 1)

#pairs(returns)

mean(returns$BTC)

mean_vect = colMeans(returns)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

l <- length(mean_vect)

Amat = cbind(rep(1,l), mean_vect, diag(1,nrow=l))  # set the constraints matrix


muP = seq(min(mean_vect)+.02,max(mean_vect)-.02,length=10)


muP = seq(-0.22, 4.25, length=300)  # set of 300 possible target values 
# for the expect portfolio return; note that due to the possibilities of short sale, the return could be 
# lower than the minimum return of the individual asset in the portfolio
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=l) # storage for portfolio weights


for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i], rep(0,l))  # constraint thresholds: no short selling, min weights are zero
  # In this optimization problem, no linear term, only quadratic term in the cost function
  result = solve.QP(Dmat = 2*cov_mat, dvec=rep(0,l), Amat=Amat, bvec=bvec, meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}



plot(sdP,muP,type="l",xlim=c(1,20),ylim=c(-1,2),lty=3)  #  plot 

mufree = 1.73/365 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
W <- weights[ind,] # Find tangency portfolio
lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=2) # show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier

#text(sd_vect[1],mean_vect[1],"BTC",cex=1.5)
#text(sd_vect[2],mean_vect[2],"LTC",cex=1.5)
#text(sd_vect[3],mean_vect[3],"UTX",cex=1.5)
#text(sd_vect[4],mean_vect[4],"CAT",cex=1.5)
#text(sd_vect[5],mean_vect[5],"MRK",cex=1.5)
#text(sd_vect[6],mean_vect[6],"IBM",cex=1.5)


for(j in 1:l)
{
  text(sd_vect[j],mean_vect[j],colnames(cryptoclosedf[j+2]),cex=0.5)
}


#lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=3) # show line of optimal portfolios

#sdP[match(5, trunc(sdP))]
#muP[match(5, trunc(sdP))]

#lines(c(5,sdP[match(5, trunc(sdP))]),c(-1,muP[match(5, trunc(sdP))]),lwd=4,lty=3) # show line of optimal portfolios

model <- lm(c(mufree,muP[ind])~c(0,sdP[ind])) # show line of optimal portfolios

#model <- lm(c(4,10)~c(2,8)) # show line of optimal portfolios

risk=5
model$coefficients[2]*risk + model$coefficients[1] # RETURN on % risk

#lines(c(5,5),c(-1,(model$coefficients[2]*5 + model$coefficients[1])))

abline(coef = coef(model), v= risk, h= (model$coefficients[2]*risk + model$coefficients[1]))

# Creating chart using GGPLOT


mufree = 1.73/365 # input value of risk-free interest rate
sharpe =( muP-mufree)/sdP # compute Sharpe ratios

ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
ind3 = (muP > muP[ind2])


# Start point
PR <- rep(0, length(returns[,1]))
for (i in 1:length(returns[,1])) 
{
  PR[i] = sum((returns[i,1] * W[1]))/100
}

# hist(PR,30, probability = TRUE, ylim = c(0.0, .1), xlim = c(-20, 60))
# Returns Distribution
ggplot(data = as.data.frame(PR), aes(x = PR, y = ..density..)) + geom_histogram(fill = "blue", color = "black") +
  labs(x = "Portfolio Returns", y = "Probability", title = "Density of Returns") +
  xlim(-10, 20) + scale_x_continuous(labels = scales::percent)

# fit Distribution
fitt <- fitdistr(PR, "t")
param <- as.numeric(fitt$estimate)
mean <- param[1]
lambda <- param[2]
df <- param[3]
sd <- lambda* sqrt((df)/(df - 2))
alpha <- .05

qalpha <- qt(alpha, df = df)
VaR_par <- - 1000000*(mean + qalpha * lambda)
VaR_par
qalpha1 <-  qstd(alpha, mean = mean, sd = sd, nu = df)

q <- as.numeric(quantile(PR, alpha))
VaRq <- -1000000 * q
VaRq

ggplot(data = as.data.frame(PR), aes(x = PR, y = ..density..)) + geom_histogram(fill = "blue", color = "black") +
  labs(x = "Portfolio Returns", y = "Probability", title = "Density of Returns at 95% Confidence, 245 Trading Days") +
  # xlim(-.10, .20) +
  geom_density(color = "orange", size = 1.5) +
  geom_vline(xintercept = qalpha1, color = "red", size = 1) +
  geom_vline(xintercept = q, color = "red4", size = 1) +
  theme_light() +
  annotate(geom = "text", x = -.06, y = -.15, label = "VaR ($43,282)", color = "black") +
  annotate(geom = "text", x = -.025, y = -.15, label = "VaR ($37,982)", color = "black") +
  scale_x_continuous(labels = scales::percent)



#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# Working Value at Risk (VaR)
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# END of PART