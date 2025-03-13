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

# --------------------------------------------
# STEP 16 Compile and Print All Statistical Results
# --------------------------------------------

step16 <- function(final_cohens_d_results){

    cat("\n============================================\n")
    cat("SUMMARY OF STATISTICAL ANALYSIS\n")
    cat("============================================\n")

    # 1. Print the largest plate differences across traits
    cat("\n>>> Largest Plate Differences Across Traits <<<\n")
    print(largest_diff_across_traits)

    # 2. Print the largest cross-plate nutrient differences
    cat("\n>>> Largest Cross-Plate Nutrient Differences <<<\n")
    print(largest_diff_cross_plate_nutr)

    # 3. Print Cohen's d results
    cat("\n>>> Cohen's d Results for Each Trait <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, PlateNutrient1, PlateNutrient2, Cohens_d))

    # 4. Print Hedges' g results
    cat("\n>>> Hedges' g Results with Correction Factor <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, PlateNutrient1, PlateNutrient2, Hedges_g, SE_Hedges_g))

    # 5. Print Euclidean distance of environmental conditions
    cat("\n>>> Euclidean Distance of Environmental Conditions <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, PlateNutrient1, PlateNutrient2, EuclDist))

    # 6. Print ESPI (Effect Size Per Impact)
    cat("\n>>> ESPI (Hedges' g / Euclidean Distance) <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, PlateNutrient1, PlateNutrient2, ESPI))

    # 7. Print Confidence Intervals for Hedges' g
    cat("\n>>> Confidence Intervals (95%) for Hedges' g <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, PlateNutrient1, PlateNutrient2, Hedges_g, CI_lower_nct, CI_upper_nct))

    # 8. Print Confidence Intervals for ESPI
    cat("\n>>> Confidence Intervals (95%) for ESPI <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, PlateNutrient1, PlateNutrient2, ESPI, ESPI_lower, ESPI_upper))

    cat("\n>>> Final Statistical Compilation Complete <<<\n")
    return(plot_espi_ci)
    }
