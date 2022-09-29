### Title:    Explore Weirdness with Mixed Codes Intercepts (and more)
### Author:   Edoardo Costantini
### Created:  2022-09-27
### Modified: 2022-09-29

# Preparing the environment ---------------------------------------------------

    # Load packages
    library(dplyr)
    library(wec)

    # Define the seed
    set.seed(1234)

    # Define a sample size
    n <- 1e3

    # Generate predictors
    X1 <- sample(0:1, n, replace = T) # binary
    X2 <- sample(0:2, n, replace = T) # three-categories
    X1X2 <- X1 * X2                   # interaction terms

    # Obtain y with no true interaction
    y <- 7 + X1 + X2 + 0 * X1X2 + rnorm(n)

    # Put everything in a dataset
    cafe <- data.frame(
        enjoyment = y,
        coffee = factor(X1, labels = c(".robusta", ".arabica")),
        style = factor(X2, labels = c(".moka", ".french", ".pour"))
    )

# Dummy codes -----------------------------------------------------------------

    # Before getting into the details of the effects coding, note these important 
    # points:
    # - any time a random sample is involved, quantities of interest are never 
    #   exactly 0.
    # - when we specify a model, we are simplifying reality by forcing some of 
    #   the relationships to be exactly 0
    # - for example, if I say y ~ x1 + x2, I'm implicitly saying that, for example,
    #   the coefficients of x1^2 and of x1*x2 are exactly 0!
    #   However, in the sample these will probably not be 0.
    #   Consider for example the data generated above.

    # First, estimate cell means
    group_means <- matrix(c(
        mean(y[X1 == 0 & X2 == 0]),
        mean(y[X1 == 0 & X2 == 1]),
        mean(y[X1 == 0 & X2 == 2]),
        mean(y[X1 == 1 & X2 == 0]),
        mean(y[X1 == 1 & X2 == 1]),
        mean(y[X1 == 1 & X2 == 2])
    ), ncol = 2, byrow = F, dimnames = list(
        c("moka", "french", "pour"),
        c("robusta", "arabica")
    ))

    # Then estimate the effects of X1
    group_means <- cbind(
        group_means,
        coffee_effects = group_means[, "arabica"] - group_means[, "robusta"]
    )

    # Estimate interaction effect between X1 and X2 (some interaction is here! Just by chance)
    diff(group_means[, 3])

    # As you can see, the interaction effect between coffee and style is not 0! 

    # Estimate main and interaction effects with linear regressions
    lmout1 <- lm(enjoyment ~ coffee + style, data = cafe)
    lmout2 <- lm(enjoyment ~ coffee * style, data = cafe)

    # Model that assumes no interaction
    round(lmout1$coefficients, 5)

    # Model that does not assume there is no interaction
    round(lmout2$coefficients, 5)

    # Interaction effects
    group_means[2, 3] - group_means[1, 3]
    group_means[3, 3] - group_means[1, 3]

    # Explain the difference: lmout1 is assuming that X11:X21 and X11:X22 are exactly 0
    # which is never the case, just by chance. So the model with no interaction is 
    # slightly wrong for the data. The model with the interaction is describing every 
    # relationship happening in the data. In a sense, we are modelling the data perfectly.

# Cell means codes ------------------------------------------------------------

# > Simple linear regression --------------------------------------------------

    # Fit model 
    slm_cm <- lm(enjoyment ~ - 1 + style, data = cafe)

    # Compare coefficient estimates with group means
    data.frame(
        slm_cm = slm_cm$coefficients[1:3],
        manual = c(
            mean(cafe[cafe$style == ".moka", "enjoyment"]),
            mean(cafe[cafe$style == ".french", "enjoyment"]),
            mean(cafe[cafe$style == ".pour", "enjoyment"])
        )
    )

# > Multiple linear regression ------------------------------------------------

    # Fit model
    mlm_cm <- lm(
        enjoyment ~ -1 + style + coffee,  # no interaction
        data = cafe
    )
    mlm_cm_int <- lm(
        enjoyment ~ -1 + style * coffee,  # with interaction
        data = cafe
    )
    
    # Compare coefficients estiamtes with group means
    data.frame(
        mlm_cm = mlm_cm$coefficients[1:3],
        mlm_cm_int = mlm_cm_int$coefficients[1:3],
        manual = group_means[, "robusta"]
    )

# Unweighted effects codes ----------------------------------------------------

    # Check sample sizes are not the same
    table(cafe$style)

    # Change coding style to unweighted effects code
    cafe$style.ec <- cafe$style
    contrasts(cafe$style.ec) <- contr.sum(levels(cafe$style.ec))

    # Improve names
    colnames(contrasts(cafe$style.ec)) <- c(".moka", ".french")

    # Check desired coding is applied
    contrasts(cafe$style.ec)

# > Simple linear regression --------------------------------------------------

    # Fit model 
    slm_ec <- lm(enjoyment ~ style.ec, data = cafe)

    # Compute the manual unweighted group mean / mean of the group means
    uwgm <- mean(c(
        mean(cafe[cafe$style == ".moka", "enjoyment"]),
        mean(cafe[cafe$style == ".french", "enjoyment"]),
        mean(cafe[cafe$style == ".pour", "enjoyment"])
    ))

    # Compare coefficients estimates with group means
    c(
        slm_cm = slm_ec$coefficients[[1]],
        manual = uwgm,
        diff = round(slm_ec$coefficients[[1]] - uwgm, 5)
    )

# > Multiple linear regression ------------------------------------------------

    # Fit model
    mlm_ec <- lm(enjoyment ~ coffee + style.ec, data = cafe)$coefficients # no interaction
    mlm_ec_int <- lm(enjoyment ~ coffee * style.ec, data = cafe)$coefficients # with interaction

    # Compute the mean of the group means in the robusta subsample
    mean(group_means[, "robusta"])

    # Compare
    c(
        mlm = mlm_ec[[1]],
        mlm_int = mlm_ec_int[[1]],
        manual = mean(group_means[, "robusta"])
    )

# Weighted effects codes ----------------------------------------------------

    # Change coding style to unweighted effects code
    cafe$style.wec <- cafe$style
    contrasts(cafe$style.wec) <- contr.wec(cafe$style.wec, omitted = ".moka")
    
    # Check desired coding is applied
    contrasts(cafe$style.wec)

    # Check the meanings of the ratios
    table(cafe$style)[-1] / table(cafe$style)[".moka"]
    
# > Simple linear regression --------------------------------------------------

    # Fit model
    slm_w <- lm(enjoyment ~ style.wec, data = cafe)$coefficients

    # compute the group means
    style_means <- cafe %>%
        group_by(style) %>%
        summarize(mean = mean(enjoyment))

    # compute the weights for the means
    weights <- table(cafe$style) / nrow(cafe)

    # multiply the group means by their weights and sum (obtain weighted mean of the group means)
    sum(style_means$mean * weights)

    # Grand mean of Y
    mean(cafe$enjoyment)

    # Compare results
    c(
        lm = slm_w[[1]],
        wgm = sum(style_means$mean * weights),
        gm = mean(cafe$enjoyment)
    )

# > Multiple linear regression ------------------------------------------------

    # Fit model
    mlm_w <- lm(enjoyment ~ coffee + style.wec, data = cafe)$coefficients # no interaction
    mlm_w_int <- lm(enjoyment ~ coffee * style.wec, data = cafe)$coefficients # with interaction

    # Work in the reference group
    cafe_robusta <- cafe %>%
        filter(coffee == ".robusta")

    # compute the group means, in the reference group of coffee
    style_means <- cafe_robusta %>% 
        group_by(style) %>%
        summarize(mean = mean(enjoyment))

    # compute the weights for the means based on the full sample size, not the robusta subset!
    weights <- table(cafe$style) / nrow(cafe)

    # compute the weights for the means based on the robusta subset
    weights_r <- table(cafe_robusta$style) / nrow(cafe_robusta)

    # Why should we we use the weights based on the full sample size and not based on the robusta
    # subset?
    # Because the contrasts don't change if we add more predictors!

    contrasts(cafe$style.wec)[1, ]

    # is still 

    table(cafe$style)[-1] / table(cafe$style)[".moka"]

    # even if we add predictors!

    # multiply the group means by their weights and sum (obtain the weighted mean of the group means)
    wgm <- sum(style_means$mean * weights)

    # check what would happen if we use weights_r
    wgm_r <- sum(style_means$mean * weights_r)

    # Compare results
    c(
        lm = mlm_w[[1]],
        lm_int = mlm_w_int[[1]],
        wgm = wgm,
        gm = mean(cafe_robusta$enjoyment), # not the same anymore!
        wgm_r = wgm_r
    )

    # Fit linear regression to robusta subsample
    # New robusta weighted effect
    cafe_robusta$style.wec.r <- cafe_robusta$style
    contrasts(cafe_robusta$style.wec.r) <- contr.wec(cafe_robusta$style.wec.r, omitted = ".moka")

    # Fit model
    slm_w_r <- lm(enjoyment ~ style.wec.r, data = cafe_robusta)$coefficients
    slm_w_r[1]

    # Check the meanings of the ratios
    table(cafe$style)[-1] / table(cafe$style)[".moka"]

    # Compare intercept with subsample mean
    c(
        lm = mlm_w[[1]],
        lm_int = mlm_w_int[[1]],
        wgm = wgm,
        gm = mean(cafe_robusta$enjoyment), # not the same anymore!
        wgm_r = wgm_r
    )