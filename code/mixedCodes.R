### Title:    Explore Weirdness with Mixed Codes
### Author:   Kyle M. Lang
### Created:  2021-03-21
### Modified: 2021-03-24

rm(list = ls(all = TRUE))

library(wec)
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

## Dummy & EC: Independent codes, deterministic outcome:
n    <- 100000
beta <- matrix(runif(3))

x1 <- factor(rep(c(0, 1), (n / 2)), labels = c("foo", "bar"))
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

coef(fit)[1] - mean(tab["foo", ])

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & EC: Independent codes, noisy outcome:
y <- X %*% beta + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["foo", ])

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & EC: Correlated codes, deterministic outcome:
x1 <- x2 <- rep(c(0, 1), each = (n / 2))

x2[1 : (n / 10)]           <- 1
x2[(n - (n / 10) + 1) : n] <- 0

tab <- table(x1, x2)
colSums(tab)
rowSums(tab)

chisq.test(x1, x2)

x1 <- factor(x1, labels = c("foo", "bar"))
x2 <- factor(x2, labels = c("bob", "bill"))

contrasts(x2) <- contr.sum(levels(x2))

x2 <- fixEcNames(x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["foo", ])

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & EC: Correlated codes, noisy outcome:
y <- X %*% beta + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["foo", ])

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###
###--------------------------------------------------------------------------###

## Dummy & WEC: Independent codes, deterministic outcome:
x1 <- factor(rep(c(0, 1), (n / 2)), labels = c("foo", "bar"))
x2 <- factor(rep(c(0, 1), each = (n / 2)), labels = c("bob", "bill"))

contrasts(x2) <- contr.wec(x2, "bob")

table(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y[x1 == "foo"])

mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & WEC: Independent codes, noisy outcome:
y <- X %*% beta + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y[x1 == "foo"])

mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & WEC: Correlated codes, deterministic outcome:
x1 <- x2 <- rep(c(0, 1), each = (n / 2))

x2[1 : (n / 10)]           <- 1
x2[(n - (n / 10) + 1) : n] <- 0

tab <- table(x1, x2)
colSums(tab)
rowSums(tab)

chisq.test(x1, x2)

x1 <- factor(x1, labels = c("foo", "bar"))
x2 <- factor(x2, labels = c("bob", "bill"))

contrasts(x2) <- contr.wec(x2, "bob")

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y[x1 == "foo"])

mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & WEC: Correlated codes, noisy outcome:
y <- X %*% beta + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y[x1 == "foo"])

mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean

coef(fit)[1] - mm[1]


###--------------------------------------------------------------------------###
###--Unbalanced Groups-------------------------------------------------------###
###--------------------------------------------------------------------------###

## Dummy & EC: Independent codes, deterministic outcome:
x1 <- factor(rbinom(n, 1, 0.7), labels = c("foo", "bar"))
x2 <- factor(rbinom(n, 1, 0.7), labels = c("bob", "bill"))

contrasts(x2) <- contr.sum(levels(x2))

x2 <- fixEcNames(x2)

table(x1, x2)
chisq.test(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["foo", ])

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & EC: Independent codes, noisy outcome:
y <- X %*% beta + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["foo", ])

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & EC: Correlated codes, deterministic outcome:
x1 <- x2 <- rep(c(0, 1), each = (n / 2))

x2[1 : (n / 5)]            <- 1
x2[(n - (n / 10) + 1) : n] <- 0

tab <- table(x1, x2)
colSums(tab)
rowSums(tab)

chisq.test(x1, x2)

x1 <- factor(x1, labels = c("foo", "bar"))
x2 <- factor(x2, labels = c("bob", "bill"))

contrasts(x2) <- contr.sum(levels(x2))

x2 <- fixEcNames(x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["foo", ])

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & EC: Correlated codes, noisy outcome:
y <- X %*% beta + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["foo", ])

mm <- summary(emmeans(fit, "x1"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###
###--------------------------------------------------------------------------###

## Dummy & WEC: Independent codes, deterministic outcome:
x1 <- factor(rbinom(n, 1, 0.7), labels = c("foo", "bar"))
x2 <- factor(rbinom(n, 1, 0.7), labels = c("bob", "bill"))

contrasts(x2) <- contr.wec(x2, "bob")

table(x1, x2)
chisq.test(x1, x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y[x1 == "foo"])

mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & WEC: Independent codes, noisy outcome:
y <- X %*% beta + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y[x1 == "foo"])

mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & WEC: Correlated codes, deterministic outcome:
x1 <- x2 <- rep(c(0, 1), each = (n / 2))

x2[1 : (n / 5)]            <- 1
x2[(n - (n / 10) + 1) : n] <- 0

tab <- table(x1, x2)
colSums(tab)
rowSums(tab)

chisq.test(x1, x2)

x1 <- factor(x1, labels = c("foo", "bar"))
x2 <- factor(x2, labels = c("bob", "bill"))

contrasts(x2) <- contr.wec(x2, "bob")

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y[x1 == "foo"])

mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Dummy & WEC: Correlated codes, noisy outcome:
y <- X %*% beta + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

coef(fit)[1] - mean(y[x1 == "foo"])

mm <- summary(emmeans(fit, "x1", weights = "proportional"))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###
