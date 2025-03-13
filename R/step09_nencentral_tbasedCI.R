#' @title Function Title
#' @description Function description
#' @param x Parameter description
#' @return Return value description
#' @export
#' @import tidyr ggplot2 multcompView dplyr readxl tibble stringr readr rstatix


# --------------------------------------------
# Load Necessary Libraries and Set Working Directory
# --------------------------------------------
library(tidyr)
library(ggplot2)
library(multcompView)
library(dplyr)
library(readxl)
library(tibble)
library(stringr)
library(readr)
library(rstatix)

############################################################
## STEP 9: Noncentral t-based CIs for Effect Sizes
## Using the formula:
##   λ = d * sqrt(n_tilde / 2)
##   CI_d = [ (t_L / λ) * d,  (t_U / λ) * d ]
##
## Where t_L, t_U are the (0.025, 0.975) quantiles
## of the noncentral t distribution with df = n1 + n2 - 2
## and noncentrality = λ.
############################################################

step09 <- function(final_cohens_d_results){

    # Suppose 'final_cohens_d_results' has columns:
    #  - n1, n2: group sizes
    #  - Hedges_g (or your effect size 'd')
    #  - n_tilde, already computed, if available
    #     or we can recompute here
    #  - v = n1 + n2 - 2 (degrees of freedom)
    #
    # We'll produce columns: CI_lower, CI_upper (noncentral t approach).

    alpha_level <- 0.05  # 95% CI => alpha = 0.05

    final_cohens_d_results <- final_cohens_d_results %>%
        rowwise() %>%
        mutate(
            # 1) Harmonic mean, if not already computed
            n_tilde = if (!("n_tilde" %in% names(.))) {
                (2 * n1 * n2) / (n1 + n2)
            } else {
                n_tilde
            },

            # 2) Noncentrality parameter:
            #    Here we use Hedges_g as the "d" (or use 'Cohens_d' if you prefer).
            lambda = Hedges_g * sqrt(n_tilde / 2),

            # 3) Degrees of freedom
            df = n1 + n2 - 2,

            # 4) Quantiles from the NONCENTRAL t distribution
            #    lower limit at 0.025
            #    upper limit at 0.975
            #    We set 'lower.tail=TRUE' so 0.025 is the left tail cutoff,
            #    using noncentral param = lambda.
            t_L = qt(alpha_level / 2, df = df, ncp = lambda, lower.tail = TRUE),
            t_U = qt(1 - alpha_level / 2, df = df, ncp = lambda, lower.tail = TRUE),

            # 5) CI lower/upper
            #    Formula: CI_d = [ (t_L / λ)*d,  (t_U / λ)*d ]
            #    Here d = Hedges_g
            CI_lower_nct = (t_L / lambda) * Hedges_g,
            CI_upper_nct = (t_U / lambda) * Hedges_g
        ) %>%
        ungroup()

    cat("\n>>> Noncentral t-based Confidence Intervals (Approx) <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, PlateNutrient1, PlateNutrient2, Hedges_g, CI_lower_nct, CI_upper_nct))


    return(final_cohens_d_results %>%
               select(Trait, PlateNutrient1, PlateNutrient2, Hedges_g, CI_lower_nct, CI_upper_nct))
    }
