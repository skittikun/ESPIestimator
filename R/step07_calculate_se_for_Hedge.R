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

##########################################################
## STEP 7: Calculate the Standard Error for Hedges' g
## Using the formula:
## SE = sqrt( (v/(v-2)) * (2 / n_tilde) * ( 1 + g^2 * (n_tilde/2) - g^2 / (J(v))^2 ) )
##
## where:
##  - v = n1 + n2 - 2
##  - g = Hedges' g (in place of the unknown true effect size δ)
##  - n_tilde is the harmonic mean of n1 and n2, i.e. 2*n1*n2 / (n1 + n2)
##  - J(v) = Correction_Factor
##########################################################

step07 <- function(largest_diff_cross_plate_nutr){

    final_cohens_d_results <- final_cohens_d_results %>%
        mutate(
            # 1) Harmonic mean of the two group sizes
            n_tilde = (2 * n1 * n2) / (n1 + n2),

            # 2) Standard Error of Hedges' g
            #    Using g^2 in place of δ^2,
            #    and J(v) = Correction_Factor.
            SE_Hedges_g = sqrt(
                (v / (v - 2)) *
                    (2 / n_tilde) *
                    (
                        1 +
                            (Hedges_g^2 * (n_tilde / 2)) -  # term: δ^2 * n_tilde / 2
                            (Hedges_g^2 / (Correction_Factor^2))  # term: δ^2 / (J(v))^2
                    )
            )
        )

    cat("\n>>> Standard Error for Hedges' g Calculated <<<\n")
    print(final_cohens_d_results)



    return(final_cohens_d_results)
    }
