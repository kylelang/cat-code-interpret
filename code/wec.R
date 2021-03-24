### Title:    Explore Weirdness with Weighted Effects Codes
### Author:   Kyle M. Lang
### Created:  2021-03-21
### Modified: 2021-03-24

rm(list = ls(all = TRUE))

library(wec)
library(emmeans)

###--------------------------------------------------------------------------###
###--Balanced Groups---------------------------------------------------------###
###--------------------------------------------------------------------------###

## Single effects code, deterministic outcome:
n    <- 100000
beta <- matrix(runif(3))

x1 <- factor(rep(c(0, 1), (n / 2)), labels = c("foo", "bar"))

contrasts(x1) <- contr.wec(x1, "foo")

X <- model.matrix(~ x1)

y <- X %*% matrix(beta[-3, ])

fit <- lm(y ~ x1)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Single effects code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Independent effects codes, deterministic outcome:
x2 <- factor(rep(c(0, 1), each = (n / 2)), labels = c("bob", "bill"))

contrasts(x2) <- contr.wec(x2, "bob")

table(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Independent effects codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Correlated effects codes, deterministic outcome:
x1 <- x2 <- rep(c(0, 1), each = (n / 2))

x2[1 : (n / 10)]           <- 1
x2[(n - (n / 10) + 1) : n] <- 0

tab <- table(x1, x2)
colSums(tab)
rowSums(tab)

chisq.test(x1, x2)

x1 <- factor(x1, labels = c("foo", "bar"))
x2 <- factor(x2, labels = c("bob", "bill"))

contrasts(x1) <- contr.wec(x1, "foo")
contrasts(x2) <- contr.wec(x2, "bob")

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Correlated effects codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)


###--------------------------------------------------------------------------###
###--Unbalanced Groups-------------------------------------------------------###
###--------------------------------------------------------------------------###

## Single effects code, deterministic outcome:
n <- 99999

x1 <- factor(rep(c(0, 1, 1), (n / 3)), labels = c("foo", "bar"))

table(x1)

contrasts(x1) <- contr.wec(x1, "foo")

X <- model.matrix(~ x1)

y <- X %*% matrix(beta[-3, ])

fit <- lm(y ~ x1)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Single effects code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Independent effects codes, deterministic outcome:
n <- 100000

x1 <- factor(rbinom(n, 1, 0.7), labels = c("foo", "bar"))
x2 <- factor(rbinom(n, 1, 0.7), labels = c("bob", "bill"))

contrasts(x1) <- contr.wec(x1, "foo")
contrasts(x2) <- contr.wec(x2, "bob")

table(x1, x2)
chisq.test(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Independent effects codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Correlated effects codes, deterministic outcome:
x1 <- x2 <- rep(c(0, 1), each = (n / 2))

x2[1 : (n / 5)]            <- 1
x2[(n - (n / 10) + 1) : n] <- 0

tab <- table(x1, x2)
colSums(tab)
rowSums(tab)

chisq.test(x1, x2)

x1 <- factor(x1, labels = c("foo", "bar"))
x2 <- factor(x2, labels = c("bob", "bill"))

contrasts(x1) <- contr.wec(x1, "foo")
contrasts(x2) <- contr.wec(x2, "bob")

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

## Correlated effects codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y)

## Compute the marginal means and frequency weights:
mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean
w  <- table(x1) / length(x1)

coef(fit)[1] - crossprod(mm, w)

###--------------------------------------------------------------------------###

### SUMMARY: - Weighted effects codes give good intercept estimates in all
###            situations.
###          - The intercepts always match their respective marginal means as
###            estimated via the emmeans package.
