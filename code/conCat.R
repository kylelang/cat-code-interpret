### Title:    Explore Weirdness when Mixing Continuous Predictors with
###           Categorical Codes
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

### Single continous predictor, deterministic data:
n    <- 100000
beta <- matrix(runif(3))

x1 <- sample(-10 : 10, n, TRUE)

y <- beta[1] + x1

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - tab["0"]
mm <- summary(emmeans(fit, "x1", at = list(x1 = 0)))$emmean

coef(fit)[1] - mm

###--------------------------------------------------------------------------###

### Single continous predictor, noisy data:
n <- 100000

x1 <- sample(-10 : 10, n, TRUE)

y <- beta[1] + beta[2] * x1 + rnorm(n, 0, 10)

fit <- lm(y ~ x1)
summary(fit)

tab <- tapply(y, x1, mean)
tab

coef(fit)[1] - tab["0"]

mm <- summary(emmeans(fit, "x1", at = list(x1 = 0)))$emmean

coef(fit)[1] - mm


###--------------------------------------------------------------------------###
###--Balanced Groups---------------------------------------------------------###
###--------------------------------------------------------------------------###

## Independent dummy code, deterministic outcome:
x2 <- factor(rep(c(0, 1), each = (n / 2)), labels = c("bob", "bill"))

summary(glm(x2 ~ x1, family = binomial))

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["0", "bob"])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Independent dummy code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["0", "bob"])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Independent unweighted effects code, deterministic outcome:
contrasts(x2) <- contr.sum(levels(x2))
x2            <- fixEcNames(x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["0", ])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Independent unweighted effects code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["0", ])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Independent weighted effects code, deterministic outcome:
contrasts(x2) <- contr.wec(x2, "bob")

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(y[x1 == 0])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean
w  <- table(x2) / length(x2)

coef(fit)[1] - crossprod(w, mm)

###--------------------------------------------------------------------------###

## Independent weighted effects code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(y[x1 == 0])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean
w  <- table(x2) / length(x2)

coef(fit)[1] - crossprod(w, mm)


###--------------------------------------------------------------------------###
###--Unbalanced Groups-------------------------------------------------------###
###--------------------------------------------------------------------------###

## Independent dummy code, deterministic outcome:
n <- 99999

x1 <- x1[1 : n]
x2 <- factor(rep(c(0, 1, 1), each = (n / 3)), labels = c("bob", "bill"))

summary(glm(x2 ~ x1, family = binomial))

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["0", "bob"])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Independent dummy code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["0", "bob"])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean

coef(fit)[1] - mm[1]

###--------------------------------------------------------------------------###

## Independent unweighted effects code, deterministic outcome:
contrasts(x2) <- contr.sum(levels(x2))
x2            <- fixEcNames(x2)

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["0", ])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Independent unweighted effects code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(tab["0", ])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean

coef(fit)[1] - mean(mm)

###--------------------------------------------------------------------------###

## Independent weighted effects code, deterministic outcome:
contrasts(x2) <- contr.wec(x2, "bob")

X <- model.matrix(~ x1 + x2)

y <- X %*% beta

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(y[x1 == 0])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean
w  <- table(x2) / length(x2)

coef(fit)[1] - crossprod(w, mm)

###--------------------------------------------------------------------------###

## Independent weighted effects code, noisy outcome:
y <- y + rnorm(n, 0, 10)

fit <- lm(y ~ x1 + x2)
summary(fit)

tab <- tapply(y, list(x1, x2), mean)
tab

coef(fit)[1] - mean(y[x1 == 0])

mm <- summary(emmeans(fit, "x2", at = list(x1 = 0)))$emmean
w  <- table(x2) / length(x2)

coef(fit)[1] - crossprod(w, mm)

###--------------------------------------------------------------------------###
