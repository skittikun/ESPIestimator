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
## STEP 5: Apply Small Sample Correction Factor
##         to Convert Cohen's d -> Hedges' g
##         Using v = (n1 + n2 - 2)
##########################################################

step05 <- function(largest_diff_cross_plate_nutr){

    # We assume final_cohens_d_results has columns:
    # n1, n2, Cohens_d, etc.

    final_cohens_d_results <- final_cohens_d_results %>%
        mutate(
            # 1) Degrees of freedom (standard definition for Hedges' g)
            v = n1 + n2 - 2,

            # 2) Correction factor: J(v) = 1 - 3 / (4v - 1)
            Correction_Factor = 1 - 3 / (4 * v - 1),

            # 3) Hedges' g = d * J(v)
            Hedges_g = Cohens_d * Correction_Factor
        )

    cat("\n>>> Small Sample Correction Applied: Hedges' g Calculated <<<\n")
    print(final_cohens_d_results)

    # --- NEW STEP: Print the Correction Factors ---
    cat("\n>>> Correction Factors (J(v)) for Each Row <<<\n")

    # If you'd like a simple vector of just the correction factors:
    print(final_cohens_d_results$Correction_Factor)

    # Or, for more context, a table with key columns:
    cat("\nTable of Correction Factors with Key Columns:\n")
    print(
        final_cohens_d_results %>%
            dplyr::select(Trait, PlateNutrient1, PlateNutrient2, v, Correction_Factor)
    )


    return(final_cohens_d_results %>%
               dplyr::select(Trait, PlateNutrient1, PlateNutrient2, v, Correction_Factor))
    }
