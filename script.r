setwd("~/Downloads/pie2/desembre/questionari-glm")
library(car)
library(nlme)
dd <- read.csv2("ah.csv")

head(dd)
sp(H~Days, boxplot=F, smooth=F, data=dd)

# ------------------------
# Gaussian (Normal) amb link = sqrt
# ------------------------

# Ap. 1
summary(m1 <- glm(H~Days,
                  family=gaussian(link="sqrt"),
                  data=dd))

plot(dd$Days, predict(m1, type="response"), type="l")
with(dd, points(Days, H, col="red", pch="+"))

# Ap. 2
plot(rstandard(m1, type="pearson"))
abline(h=c(-2,0,2))
which(abs(rstandard(m1)) > 2)

# Ap. 3
FDays <- as.factor(dd$Days)
m1f <- glm(H ~ Days + FDays,
           family = gaussian(link="sqrt"),
           data = dd)

print(anova(m1, m1f, test="F"))

# Ap. 4
print(leveneTest(resid(m1, type="pearson")~FDays))

# Ap. 5
customDays <- data.frame(Days=c(0, 105, 150))
pred <- predict(m1, customDays, se.fit = T, type="response")

mu_1 <- pred$fit
phi_1 <- summary(m1)$dispersion
var_gaussian <- phi_1*1 # normal: V(mu) = 1

print(cbind(mu = mu_1,
            var = var_gaussian,
            se = pred$se.fit))


# ------------------------
# Gamma amb link = log
# ------------------------

# Ap. 1
summary(m2 <- glm(H ~ Days,
          family=Gamma(link="log"),
          data=dd))

plot(dd$Days, predict(m2, type="response"), type="l")
with(dd, points(Days, H, col="red", pch="+"))

# Ap. 2
plot(rstandard(m2), ylim = c(4,-4))
abline(h=c(-3,0,3))
which(abs(rstandard(m2)) > 3)

# Ap. 3
FDays <- as.factor(dd$Days)
m2f <-glm(H ~ Days + FDays,
          family=Gamma(link="log"),
          data=dd)

anova(m2, m2f, test="F")

# Ap. 4
print(leveneTest(resid(m2, type="pearson")~FDays))

# Ap. 5
customDays <- data.frame(Days=c(0, 105, 150))
pred <- predict(m2, customDays, se.fit = T, type="response")

mu_2 <- pred$fit
phi_2 <- summary(m2)$dispersion
var_gamma <- phi_2*mu_2*mu_2 # Gamma: V(mu) = mu*mu

print(cbind(mu = mu_2,
            var = var_gamma,
            se = pred$se.fit))

# ------------------------
# Response with variance function = mu and link = log (~ Poisson)
# ------------------------

# Ap. 1
summary(m3 <- glm(H ~ Days,
                  family=quasi(variance="mu",link="log"),
                  data=dd))

plot(dd$Days, predict(m3, type="response"), type="l")
with(dd, points(Days, H, col="red", pch="+"))

# Ap. 2
plot(rstandard(m3))
abline(h=c(-2,0,2))
which(abs(rstandard(m3)) > 2)

# Ap. 3
FDays <- as.factor(dd$Days)
m3f <-glm(H ~ Days + FDays,
          family=quasi(link="log", variance="mu"),
          data=dd)

anova(m3, m3f, test="F")

# Ap. 4
print(leveneTest(resid(m2, type="pearson")~FDays))

# Ap. 5
customDays <- data.frame(Days=c(0, 105, 150))
pred <- predict(m3, customDays, se.fit = T, type="response")

mu3 <- pred$fit
phi3 <- summary(m3)$dispersion
var_poisson <- phi3*mu3 # poisson: V(mu) = mu

print(cbind(Var = var_poisson,
            Days = customDays,
            se = pred$se.fit))

# Ap. 6
# question 17
res.dev <- c()
null.dev <- c()

res.dev[1] <- sum(summary(m1)$deviance.resid^2)
null.dev[1] <- summary(m1)$null.deviance

res.dev[2] <- sum(summary(m2)$deviance.resid^2)
null.dev[2] <- summary(m2)$null.deviance

res.dev[3] <- sum(summary(m3)$deviance.resid^2)
null.dev[3] <- summary(m3)$null.deviance

cbind(mod = c(1,2,3),
      res = res.dev,
      null = null.dev,
      ratio = res.dev/null.dev)

# question 18
leveneTest(resid(m1, type="pearson")~FDays)
leveneTest(resid(m2, type="pearson")~FDays)
leveneTest(resid(m3, type="pearson")~FDays)

# questions 19 and 20
mu <- c()
phi <- c()
var <- c()

customDays <- data.frame(Days=c(0, 150))

for (day in customDays$Days) {
  mu[1] <- predict(m1, data.frame(Days=c(day)), type="response")
  mu[2] <- predict(m2, data.frame(Days=c(day)), type="response")
  mu[3] <- predict(m3, data.frame(Days=c(day)), type="response")
  
  phi[1] <- summary(m1)$dispersion
  phi[2] <- summary(m2)$dispersion
  phi[3] <- summary(m3)$dispersion
  
  var[1] <- phi[1]*1 # Normal
  var[2] <- phi[2]*mu[2]*mu[2] # Gamma
  var[3] <- phi[3]*mu[3] # Poisson
  
  print(cbind(mu = mu,
              disp = phi,
              var = var))
}
  
# plots
plot(dd$Days, predict(m1, type="response"), type="l", col="blue")
lines(dd$Days, predict(m2, type="response"), type="l", col="red", lty=1)
lines(dd$Days, predict(m3, type="response"), type="l", col="green", lty=4)
