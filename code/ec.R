### Title:    Explore Weirdness with Unweighted Effects Codes
### Author:   Kyle M. Lang
### Created:  2021-03-21
### Modified: 2021-03-24

rm(list = ls(all = TRUE))

library(emmeans)

## Define a function to automatically fix EC names:
fixEcNames <- function(x) {
    tmp                    <- contrasts(x)
    colnames(contrasts(x)) <- rownames(tmp)[rowSums(tmp) > 0]
    x
}


###--------------------------------------------------------------------------###
###--Balanced Groups---------------------------------------------------------###
###--------------------------------------------------------------------------###

## Single effects code, deterministic outcome:
n    <- 100000
beta <- matrix(runif(3))

x1 <- factor(rep(c(0, 1), (n / 2)), labels = c("foo", "bar"))

contrasts(x1) <- contr.sum(levels(x1))

x1 <- fixEcNames(x1)

X <- model.matrix(~ x1)

y <- X %*% matrix(beta[-3, ])

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Single effects code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Independent effects codes, deterministic outcome:
x2 <- factor(rep(c(0, 1), each = (n / 2)), labels = c("bob", "bill"))

contrasts(x2) <- contr.sum(levels(x2))

x2 <- fixEcNames(x2)

table(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Independent effects codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

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

contrasts(x1) <- contr.sum(levels(x1))
contrasts(x2) <- contr.sum(levels(x2))

x1 <- fixEcNames(x1)
x2 <- fixEcNames(x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Correlated effects codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)


###--------------------------------------------------------------------------###
###--Unbalanced Groups-------------------------------------------------------###
###--------------------------------------------------------------------------###

## Single effects code, deterministic outcome:
n <- 99999

x1 <- factor(rep(c(0, 1, 1), (n / 3)), labels = c("foo", "bar"))

table(x1)

contrasts(x1) <- contr.sum(levels(x1))

x1 <- fixEcNames(x1)

X <- model.matrix(~ x1)

y <- X %*% matrix(beta[-3, ])

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Single effects code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Independent effects codes, deterministic outcome:
n <- 100000

x1 <- factor(rbinom(n, 1, 0.7), labels = c("foo", "bar"))
x2 <- factor(rbinom(n, 1, 0.7), labels = c("bob", "bill"))

contrasts(x1) <- contr.sum(levels(x1))
contrasts(x2) <- contr.sum(levels(x2))

x1 <- fixEcNames(x1)
x2 <- fixEcNames(x2)

table(x1, x2)
chisq.test(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Independent effects codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

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

contrasts(x1) <- contr.sum(levels(x1))
contrasts(x2) <- contr.sum(levels(x2))

x1 <- fixEcNames(x1)
x2 <- fixEcNames(x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Correlated effects codes, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab)

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

### SUMMARY: - With balanced groups, the only issues arrise with correlated
###            codes and noisy data. When groups are unbalanced, noisy data is
###            problematic with independent and correlated codes.
###          - The intercept estimates always match the appropriate marginal
###            means estimated with emmeans.
