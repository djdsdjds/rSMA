#warning 
library(quantstrat)
library(quantmod)
library(knitr)
#test every combination of fast/slow sma up to 200
X<-list()
aux<-1
for(i in 2:100){
  for (j in 1:(i-1)){
    X[[aux]]=c(j,i)
    aux=aux+1
  }
}
X1<-as.data.frame(X)

# Settings
for (i in 1:4950) {
  

sym <- "GOLD"
crcy <- "USD"
from <- "2008-01-01"
nSlowSMA <- X1[2,i]
nFastSMA <-X1[1,i]
initEq <- 0
order.qty<- 1

strat.name <- "sma crossover"
portofolio.name <- "my portfolio"
account.name <- "my account"

currency(crcy)
Sys.setenv(TZ = "UTC")
getSymbols(sym, from = from)
stock(sym, crcy, multiplier = 1)

rm.strat(strat.name)
rm.strat(portofolio.name)
rm.strat(account.name)
initDate <- as.Date(from) - 1  # any date before the first in the timeseries

initPortf(portofolio.name, symbols = c(sym), initDate = initDate, currency = crcy)
initAcct(account.name, portfolios = portofolio.name, initDate = initDate, currency = crcy, initEq = initEq)
initOrders(portofolio.name, initDate = initDate)
strategy(strat.name, store = TRUE)

slowSMA.label <- paste0("SMA", nSlowSMA)
fastSMA.label <- paste0("SMA", nFastSMA)

add.indicator(strat.name, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n = nSlowSMA), label = slowSMA.label)
add.indicator(strat.name, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n = nFastSMA), label = fastSMA.label)

# Fast SMA vs slow SMA crossing
add.signal(strat.name, name = "sigCrossover",
           arguments = list(columns = c(fastSMA.label, slowSMA.label), relationship = "gt"),
           label = "fast.crossed.above.slow")
add.signal(strat.name, name = "sigCrossover",
           arguments = list(columns = c(fastSMA.label, slowSMA.label), relationship = "lt"),
           label = "fast.crossed.below.slow")

# Long
# Enter when fastsma>slowsma
add.rule(strat.name, name = "ruleSignal",
         arguments = list(sigcol = "fast.crossed.above.slow", sigval = TRUE, ordertype = "market", orderside = "long",
                          replace = FALSE, prefer = "Open", orderqty = order.qty, atrMod = "X"),
         type = "enter", path.dep = TRUE, label = "enterLong.safe")

# Exit when fastsma<slowsma
add.rule(strat.name, name = "ruleSignal",
         arguments = list(sigcol = "fast.crossed.below.slow", sigval = TRUE, ordertype = "market", orderside = "long",
                          replace = FALSE, prefer = "Open", orderqty = "all", atrMod = "X"),
         type = "exit", path.dep = TRUE, label = "exitLong.on.close.crossing.fast")


# Short
#Enter when fastsma<slowsma 
add.rule(strat.name, name = "ruleSignal",
        arguments = list(sigcol = "fast.crossed.below.slow", sigval = TRUE, ordertype = "market", orderside = "short",
                          replace = FALSE, prefer = "Open", orderqty = -order.qty, atrMod = "X"),
         type = "enter", path.dep = TRUE, label = "enterShort.safe")
#Exit when fastsma>slowsma
add.rule(strat.name, name = "ruleSignal",arguments = list(sigcol = "fast.crossed.above.slow", sigval = TRUE, ordertype = "market", orderside = "short",
                          replace = FALSE, prefer = "Open", orderqty = "all", atrMod = "X"),
         type = "exit", path.dep = TRUE, label = "exitShort.on.close.crossing.fast")
applyStrategy(strategy = strat.name, portfolios = portofolio.name)
        
updatePortf(portofolio.name)
updateAcct(account.name)
updateEndEq(account.name)


#chart.Posn(portofolio.name, Symbol = sym,
 #          TA = "add_SMA(n = 50, col = 'red'); add_SMA(n = 200, col = 'blue')")

tstats <- tradeStats(portofolio.name)
kable(t(tstats))

X1[4,i]<-tstats$Ann.Sharpe
X1[3,i]<-tstats$Net.Trading.PL
}
X2<-t(X1)
max(X1[3,])
max(X1[4,])
X2[which(X2[,3] == max(X2[,3])), ]

X2[which(X2[,4] == max(X2[,4])), ]
plot(X2[,1]/X2[,2],X2[,3])
