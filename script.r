setwd("~/Downloads/pie2/desembre/")
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

# Ap. 2
plot(m1, which=1)
plot(rstandard(m1, type="pearson"))
abline(h=c(-2,0,2))
which(abs(rstandard(m1)) > 2)

# Ap. 3
FDays <- as.factor(dd$Days)
m1f <- glm(H ~ Days + FDays,
           family = gaussian(link="sqrt"),
           data = dd)

print(anova(m1, m1f, test="F"))
print(anova(m1f, test="F"))

# Ap. 4
print(leveneTest(resid(m1, type="pearson")~FDays))

# Ap. 5
customDays <- data.frame(Days=c(0, 105, 150))
pred <- predict(m1, customDays, se.fit = T, type="response")
print(cbind(mu = pred$fit,
            se = pred$se.fit))

# ------------------------
# Gamma amb link = log
# ------------------------

# Ap. 1
summary(m2 <- glm(H ~ Days,
          family=Gamma(link="log"),
          data=dd))

plot(predict(m2, ty="response"))
with(dd, points(Days, H, col="red"))

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
print(cbind(mu = pred$fit,
            se = pred$se.fit,
            sd = summary(m2)$sigma))

# ------------------------
# Response with variance function = mu and link = log (~ Poisson)
# ------------------------

# Ap. 1
summary(m3 <- glm(H ~ Days,
                  family=quasi(variance="mu",link="log"),
                  data=dd))

# Ap. 2
plot(rstandard(m3))
abline(h=c(-2,0,2))
which(abs(rstandard(m3)) > 2)

# Ap. 3

# Ap. 4

# Ap. 5
customDays <- data.frame(Days=c(0, 105, 150))
pred <- predict(m3, customDays, se.fit = T, type="response")
print(cbind(mu = pred$fit,
            se = pred$se.fit))
