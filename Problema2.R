mis_dades<-iris

y <- mis_dades$Sepal.Length
x <- mis_dades$Petal.Length
plot(x,y)

xbar <- mean(x)
ybar <- mean(y)

m <- sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)

b <- ybar-(m*xbar)

m*1.5+b

mod <- lm(y~x) # regressio lineal de xy

ypredict <- predict(mod, data.frame(x=x)) #fer la prediccio de la regressio lineal segons la base de dades x (valors de x)

plot(x,y, pch=16, col='lightblue')
lines(x,ypredict) #linea de regressio lineal

Rsqr <- sum((ypredict-ybar)^2)/sum((y-ybar)^2) # R=coeficient de correlacio

summary(mod) #mes informacio sobre mod (Multiple R-squared:   0.76)

R <- sqrt(Rsqr)

cor.test(x,y)
