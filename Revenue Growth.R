#Data: https://www.statista.com/statistics/607716/worldwide-artificial-intelligence-market-revenues/
rev <- c(10.1,14.7,22.6,34.9,51.3,70.9,94.4,126)
year <- 2018:2025-2018
AI <- data.frame(rev,year)
m1 <- glm(rev~year, data = AI)
m1$deviance
fitted <- m1$fitted.values
plot(year, rev)
lines(year, fitted)

subyear <- 1:5
m2 <- glm(rev~year, data = AI[subyear,],family = Gamma(link = "log"))
m2$deviance
fitted <- m2$fitted.values
plot(year[subyear], rev[subyear])
lines(year[subyear], fitted)

newyears <- seq(6,7,1)
newdf <- data.frame(year = newyears)
newP1 <- predict(m1, newdata = newdf, type = "response")
newP2 <- predict(m2, newdata = newdf, type = "link",se.fit = TRUE)
newP2 <- predict(m2, newdata = newdf, type = "response")

alpha <- 0.01
newd <- t(data.matrix(data.frame(intce = rep(1,length(newyears)),year = newyears)))
ci_para <- confint(m2,level = 1-alpha)
ratio <- 0.15*0.1
lwr <- family(m2)$linkinv(ci_para[,1] %*% newd)*ratio
upr <- family(m2)$linkinv(ci_para[,2] %*% newd)*ratio
lwr;upr
#chosen 0.10 + 0.05: 100%
#chosen 0.10 + 0.10: 0%
#chosen 0.15 + 0.05: 100%
#chosen 0.15 + 0.05: 0%
