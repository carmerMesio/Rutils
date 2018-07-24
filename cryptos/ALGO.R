
#--------------------- VOLUME CALCULATION -----------------------

MeanVolume <- function(lastVol, antVolumes ){
  
  meanVolumes = mean (antVolumes); #5 minute previous 96 Volumes. (8h)
  
  if (lastVol >= 3*meanVolumes) {
    
    if (lastVol >= 50*meanVolumes) { 
      
      return (1)
    } else {
      
      if (lastVol >= 20*meanVolumes) { 
        
        return (0.8)
      } else {
        
        if (lastVol >= 10*meanVolumes) { 
          
          return (0.6)
        } else {
          
          if (lastVol >= 5*meanVolumes) { 
            
            return (0.4)
          } else {
            
            if (lastVol >= 3*meanVolumes) { 
              
              return (0.2)
            }
          }
        }
      }
    }
  } else {
    
    return (0)
  }
  
}

#******************** VOLUME TEST
lastValTest = 600
y <- rnorm(96, mean=100, sd=20)
volumeIndicator = MeanVolume(lastValTest, y)
y <- append(lastValTest, y, after = length(y))
barplot(rev(y), main="Vol Distribution", xlab="Number of Vols")
volumeIndicator #OK
#******************** END TEST

#--------------------- VARIANCE COEFICIENT CALCULATION -----------------------

sdCompute <- function(lastVol, antVolumes ){
  
  maxVol = max (antVolumes [1:90]); #We don't care about last 30 minutes.
  
  if (maxVol > 0.65*lastVol) { #We refuse higher values of 0.65*lastVol.
    
    return (-1) #Then we reject the entry.
  }
  
  sdVol = sd (antVolumes); #We calculate standard deviaton.
  meanVol = mean (antVolumes); #We calculate mean again.
  coef = floor((sdVol/meanVol)*100); #We delete decimals.
  
  if (coef > 30) { #We accept 30% scattering
    
    return (0) #We do not ponderate, but we accept. 
  }else {
    
    if (coef > 15) { 
      
      return (0.2) 
    }else {
      
      if (coef > 10) { 
        
        return (0.4)
      }else {
        
        if (coef > 7) { 
          
          return (0.6) 
        }else {
          
          if (coef > 4) { 
            
            return (0.8)
          }else {
            
            return (1)
          }
        }
      }
    }
  }
}

#******************** VARIANCE TEST
lastValTest = 300
y <- rnorm(96, mean=100, sd=30)
varIndicator = sdCompute(lastValTest, y)
y <- append(lastValTest, y, after = length(y))
varIndicator #OK
barplot(rev(y), main="Vol Distribution", xlab="Number of Vols")
#******************** END TEST

#--------------------- MARKET DIRECTION DETECTION -----------------------

buyOrSell <- function(lastCandleClose, lastCandleOpen, prevCandleCloses ){
  
  if (lastCandleClose > lastCandleOpen) { #Entonces compra.
    
    auxBuy = 1
  } else {
    
    if (lastCandleClose < lastCandleOpen) { #Entonces vende.
      
      auxBuy = 0
    } else {
    
      return (-1) #Ni se compra ni se vende
    }
  }
  
  if (auxBuy == 1 && (lastCandleClose > max(prevCandleCloses)) ) {
    
    return (1) #Compra
  } else {
    
    if (auxBuy == 0 && (lastCandleClose < min(prevCandleCloses)) ) {
      
      return (0) #Vende
    } else {
      
      return (-1) #Ni se compra ni se vende
    }
  }
}

#******************** DIRECTION TEST
lOpen <- 20
lClose <- 30
y <- floor(rnorm(15, mean=20, sd=4))

buy = buyOrSell(lClose, lOpen, y)
y <- append(lClose, y, after = length(y))
barplot(rev(y), main="Vol Distribution", xlab="Number of Vols")
buy
#******************** END TEST

#--------------------- MARKETCAP VALUE -----------------------

marketCapValue <- function(marketcap){

  if (marketcap > 750000000) { 
    
    return (1)
  }else {
    
    if (marketcap > 250000000) { 
      
      return (2) 
    }else {
      
      if (marketcap > 125000000) { 
        
        return (3) 
      }else {
        
        if (marketcap > 50000000) { 
          
          return (4) 
        }else {
            
            return (5) 
        }
      }
    }
  }
}

#******************** MKAP TEST
mkap <- 40000000
mc <- marketCapValue(mkap)
mc
mkap <- 65000000
mc <- marketCapValue(mkap)
mc
mkap <- 130000000
mc <- marketCapValue(mkap)
mc
mkap <- 280000000
mc <- marketCapValue(mkap)
mc
mkap <- 790000000
mc <- marketCapValue(mkap)
mc
#******************** END TEST

#--------------------- ORDER BOOK -----------------------

getOrderBook <- function(buy, totalBid, totalAsk){
  
  if (buy == 1 && (totalBid/totalAsk >= 10) ) {
    
    return (0.8)
  } else {
    
    if (buy == 1 && (totalBid/totalAsk >= 5) ) {
      
      return (0.6)
    } else {
      
      if (buy == 1 && (totalBid/totalAsk >= 2) ) {
        
        return (0.4)
      } else {
        
        if (buy == 1 && (totalBid > totalAsk) ){
          
          return (0.2)
        }
      }
    }
  }
    
  if (buy == 0 && (totalAsk/totalBid >= 10) ) {
    
    return (0.8)
  } else {
    
    if (buy == 0 && (totalAsk/totalBid >= 5) ) {
      
      return (0.6)
    } else {
      
      if (buy == 0 && (totalAsk/totalBid >= 2) ) {
        
        return (0.4)
      } else {
        
        if (buy == 0 && (totalAsk > totalBid) ){
          
          return (0.2)
        }
      }
    }
  }  
      
  return (0)
}

#******************** ORDERBOOK TEST
bid <- 120
ask <- 20
buy <- 1
final <- getOrderBook(buy, bid, ask)
final
#******************** END TEST

#--------------------- SHORT TERM BITCOIN TREND (1 WEEK) -----------------------

shortTermTrendBtc  <- function(midCloseBtc){
  
  n = 1:length(midCloseBtc)
  y = lm(midCloseBtc ~ n)
  m = coef(y)[2]
  
  if (m > 0.01) {
    
    return (1) #Alcista
  } else {
    
    if(m < -0.01) {
    
      return (2) #Bajista
    } else {
    
        return (3) #Lateral
    }
  }
}

#******************** BTC TREND TEST
x<-cumsum(rnorm(2016,0,1))
n <- 1:2016 
y <- lm(x ~ n)
m <- coef(y)
m
plot.ts(x)
r <- shortTermTrendBtc(x)
r
abline(coef(y)[1], coef(y)[2])
#******************** END TEST

#--------------------- RISK CALCULATION -----------------------

riskMatrixCalculation <- function(marketCap, directionBTC, buy, varIndicator, volumeIndicator, strength, totalCap, available, pricesBefore) {

  #MarketCap        1, 2, 3, 4, 5             -> The higher, the higher risk                    ++
  #directionBTC     1, 2, 3                   -> 1 - bullish, 2 - bearish, 3 - neutral          +
  #buy              1, 2                      -> 1 buy, 0 sell                                  +
  #varIndicator     0, 0.2, 0.4, 0.6, 0.8, 1  -> The higher, The higher ponderation (1 is best) ++
  #volumeIndicator  0.2, 0.4, 0.6, 0.8, 1     -> The higher, The higher ponderation (1 is best) +++

  sdeviation <- sd(pricesBefore) #We get sdeviation of last 20 values to set the Stop Loss.
  
  qualityIndicator <- 0.8*volumeIndicator + 0.2*varIndicator # (1 is best)
  
  if ( (directionBTC == 1 && buy == 1) || (directionBTC == 2 && buy == 0)) {
    
    plus <- 0.1
  } else if ((directionBTC == 3 && buy == 1) || (directionBTC == 3 && buy == 0) ){
    
    plus <- 0
  } else {
    
    plus <- -0.1
  }
  
  finalPond <- qualityIndicator + plus + strength #(higher is 1.9, lower is 0.284)
  
  if (marketCap == 5) { #Initially, we will assume a higher risk with risky assets but we will move it quickly when we get benefits.
    
    if (finalPond > 1.8) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.6) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.3) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.7) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.4) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    }
    
  } else if (marketCap == 4) {
    
    if (finalPond > 1.8) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.6) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.3) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.7) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.4) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    }
    
  } else if (marketCap == 3) {
    
    if (finalPond > 1.8) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.6) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.3) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.7) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.4) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    }
    
  } else if (marketCap == 2) {
    
    if (finalPond > 1.8) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.6) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.3) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.7) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.4) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    }

  } else if (marketCap == 1) {
    
    if (finalPond > 1.8) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.6) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1.3) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 1) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.7) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else if (finalPond > 0.4) {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    } else {
      
      amount<- min(totalCap*0.15, available); stopPrice <- sdeviation
    }
    
  }
  
  res <- list(amount,stopPrice, buy)
  return (res)
}

#--------------------------------------------VOID MAIN----------------------------------------------

mainVPOCAlgorithm  <- function(){

  #-------------------VARIABLES FROM API--------------------
  lastVol <- 600
  antVolumes <- rnorm(96, mean=100, sd=20)
  lastOpen <- 20
  lastClose <- 30
  mkap <- 130000000
  prevCandleSticks <- floor(rnorm(15, mean=20, sd=4))
  dataBTC <- cumsum(rnorm(2016,0,1))
  actualBid <- 120
  actualAsk <- 20
  totalCap <- 10000 #Total amount of money
  available <- 1700 #Amount of money available to invest
  lastPrice <- 17.75
  pricesBefore <- (rnorm(20, mean=12, sd=2))
  
  time <- 1
  #-------------------END VARIABLES--------------------
  
  volumeIndicator <- MeanVolume(lastVol, antVolumes)

  if (volumeIndicator != 0) {
    varIndicator <- sdCompute(lastVol, antVolumes)
    
    if (varIndicator != -1) {
      buy <- buyOrSell(lastClose, lastOpen, prevCandleSticks)
      
      if (buy != -1) {
        
        totalBid <- 0
        totalAsk <- 0
        
        while (time < 2) { #We have to change 2 to 10
          Sys.sleep(1)
          totalBid = actualBid + totalBid
          totalAsk = actualAsk + totalAsk
          time <- time + 1
          
        }
        
        ok <- getOrderBook(buy, totalBid, totalAsk)
        
        if (ok != 0) {
          marketCap <- marketCapValue(mkap)
          directionBTC <- shortTermTrendBtc(dataBTC)
          
          quantityAndStopPriceDir <- riskMatrixCalculation(marketCap, directionBTC, buy, varIndicator, volumeIndicator, ok, totalCap, available, pricesBefore)
          
          return (quantityAndStopPriceDir) #This is a list[amount of money, stopPrice]. We do not return the stock price at which we buy because we buy at the market price in this moment.
          
        } else {
          
          return (-1)
        }
        
      } else {
        
        return (-1)
      }
      
    } else {
      
      return (-1) #We reject the entry
    }
    
  } else {
    
    return (-1) #We reject the entry
  }
}

mainVPOCAlgorithm()









