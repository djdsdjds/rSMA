library(quantstrat)
library(quantmod)
library(knitr)
sym <- "GOLD"
crcy <- "USD"
from <- "2008-01-01"
to<- "2018-09-24"
nSlowSMA <-50
nFastSMA <-10
initEq <- 1000
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
initDate <- as.Date(from) - 1 

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
  
