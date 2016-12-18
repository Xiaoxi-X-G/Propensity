rm(list = ls())

# Ref: http://snowplowanalytics.com/guides/recipes/catalog-analytics/market-basket-analysis-identifying-products-that-sell-well-together.html
# Ref: https://select-statistics.co.uk/blog/market-basket-analysis-understanding-customer-behaviour/
require(arules)
require(arulesViz)

RScriptPath<-"C:/gxx/r/project/MarketBasket"

Column.type <- c("factor",
                 "factor",
                 "factor",
                 "factor")
DataRaw <- read.csv(paste(RScriptPath, "/groceries.csv",sep=''),
                    na.strings = c("", "NA"),
                    colClasses = Column.type)



summary(DataRaw)

####visu check #####
# The item data are grouped into 4, and the presented. 
# As most of transaction may not fall into all 4 groups, there are lots of NULL
# Check read.csv to na.strings = c("", "NA") to get ride of ""
barplot(table(DataRaw$citrus.fruit), main="fruit")
barplot(table(DataRaw$semi.finished.bread), main = "bread") 
barplot(table(DataRaw$margarine), main = "margarine")
barplot(table(DataRaw$ready.soups), main = "soup")




#### check to transactions & Visu check ####
Data.list <- apply(DataRaw, MARGIN = 1, function(x) paste(x[!is.na(x)], sep = ","))
names(Data.list) <- paste("Tr", c(1:nrow(DataRaw)), sep="")
Data.txn <- as(Data.list, "transactions")
summary(Data.txn)


itemFrequencyPlot(Data.txn, topN = 25)


#### Apply Apriori to find Rules ####
# support = Prob(lhs & rhs): percentage of both lhs&rhs against all transactions
# confidence = Prob(rhs | lhs): percentage of rhs providing lhs hapeening (conditional prob)
# life = confidence / prob(rhs): Normalize confidence over chances of rhs, as a high confidence may be due to high numer of rhs
# life = Prob(lhs & rhs)/ Prob(rhs) / prob(rhs): lift = 1 implies independce of lhs and rhs
Basket.rules <- apriori(Data.txn, 
                        parameter = list(sup = 0.001, conf = 0.1, target = "rules"))

TopRules <- head(sort(Basket.rules, by = "lift"), n =20)
inspect(TopRules)
plot(Basket.rules)
#The most interesting rules reside on the support/confidence border which can be clearly seen in this plot.

plot(TopRules, method = "graph", interactive = T, control=list(type="items",main=""))
