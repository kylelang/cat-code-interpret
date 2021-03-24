### Title:    Explore Weirdness with Dummy Codes
### Author:   Kyle M. Lang
### Created:  2021-03-21
### Modified: 2021-03-24

rm(list = ls(all = TRUE))

library(lsmeans)

###--------------------------------------------------------------------------###
###--Balanced Groups---------------------------------------------------------###
###--------------------------------------------------------------------------###

## Single dummy code, deterministic outcome:
n    <- 100000
beta <- matrix(runif(3))

x1 <- factor(rep(c(0, 1), (n / 2)), labels = c("foo", "bar"))

X <- model.matrix(~ x1)

y <- X %*% matrix(beta[-3, ])

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - tab["foo"]

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Single dummy code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - tab["foo"]

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Independent dummy codes, deterministic outcome:
x2 <- factor(rep(c(0, 1), each = (n / 2)), labels = c("bob", "bill"))

table(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - tab["foo", "bob"]

mm <- summary(emmeans(fit, "x1", at = list(x2 = "bob")))$emmean

coef(fit)[1] - mm[1]


###--------------------------------------------------------------------------###

## Independent dummy codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - tab["foo", "bob"]

mm <- summary(emmeans(fit, "x1", at = list(x2 = "bob")))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Correlated dummy codes, deterministic outcome:
x1 <- x2 <- rep(c(0, 1), each = (n / 2))

x2[1 : (n / 10)]           <- 1
x2[(n - (n / 10) + 1) : n] <- 0

tab <- table(x1, x2)
colSums(tab)
rowSums(tab)

chisq.test(x1, x2)

x1 <- factor(x1, labels = c("foo", "bar"))
x2 <- factor(x2, labels = c("bob", "bill"))

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - tab["foo", "bob"]

mm <- summary(emmeans(fit, "x1", at = list(x2 = "bob")))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Correlated dummy codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - tab["foo", "bob"]

mm <- summary(emmeans(fit, "x1", at = list(x2 = "bob")))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###
###--Unbalanced Groups-------------------------------------------------------###
###--------------------------------------------------------------------------###

## Single dummy code, deterministic outcome:
n <- 99999

x1 <- factor(rep(c(0, 1, 1), (n / 3)), labels = c("foo", "bar"))

table(x1)

X <- model.matrix(~ x1)

y <- X %*% matrix(beta[-3, ])

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - tab["foo"]

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Single dummy code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - tab["foo"]

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Independent dummy codes, deterministic outcome:
n <- 100000

x1 <- factor(rbinom(n, 1, 0.7), labels = c("foo", "bar"))
x2 <- factor(rbinom(n, 1, 0.7), labels = c("bob", "bill"))

table(x1, x2)
chisq.test(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - tab["foo", "bob"]

mm <- summary(emmeans(fit, "x1", at = list(x2 = "bob")))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Independent dummy codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - tab["foo", "bob"]

mm <- summary(emmeans(fit, "x1", at = list(x2 = "bob")))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Correlated dummy codes, deterministic outcome:
x1 <- x2 <- rep(c(0, 1), each = (n / 2))

x2[1 : (n / 5)]            <- 1
x2[(n - (n / 10) + 1) : n] <- 0

tab <- table(x1, x2)
colSums(tab)
rowSums(tab)

chisq.test(x1, x2)

x1 <- factor(x1, labels = c("foo", "bar"))
x2 <- factor(x2, labels = c("bob", "bill"))

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - tab["foo", "bob"]

mm <- summary(emmeans(fit, "x1", at = list(x2 = "bob")))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Correlated dummy codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - tab["foo", "bob"]

mm <- summary(emmeans(fit, "x1", at = list(x2 = "bob")))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

### SUMMARY: -With both balanced and unbalanced groups, the only issues arises
###           with multiple codes and noisy data.
###          - All intercepts match the marginal means calculated via emmeans.
