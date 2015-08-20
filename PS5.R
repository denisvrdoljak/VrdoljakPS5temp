library("ggplot2")
library("pastecs")
library("psych")
library("Hmisc")
library("ggm")
library("ggplot2")
library("polycor")
#library("WSR")

cl <- function(fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

draft <- read.csv("C:/Users/26760/Desktop/PS5/PS5data.csv")
View(draft)

#1.A
summary(lm(income ~ years_education,data=draft))

#1.C
draft$draft_high <- (draft$draft_number < 81)

lm.highdraft.onedu <- lm(years_education ~ draft_high, data=draft)
cl(lm.highdraft.onedu, draft$draft_number)

#1.D

lm.highdraft.onincome <- lm(income ~ as.numeric(draft_high), data=draft)
cl(lm.highdraft.onincome, draft$draft_number)

#1.E
cl(lm.highdraft.onincome,draft$draft_number)[2,1]/ cl(lm.highdraft.onedu,draft$draft_number)[2,1]

#1.G
