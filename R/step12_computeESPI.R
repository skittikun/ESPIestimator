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
## STEP 12: Compute ESPI = Hedges' g / Euclidean Distance
##       And Visualize
##########################################################

step12 <- function(final_cohens_d_results){

    final_cohens_d_results <- final_cohens_d_results %>%
        mutate(
            # 1. Compute ESPI
            ESPI = ifelse(
                EuclDist == 0 | is.na(EuclDist),
                NA_real_,  # Avoid dividing by zero
                Hedges_g / EuclDist
            )
        )

    # 2. Print the updated data
    cat("\n>>> ESPI Computed (Hedges' g / EuclDist) <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, PlateNutrient1, PlateNutrient2, Hedges_g, EuclDist, ESPI))



    return(final_cohens_d_results %>%
               select(Trait, PlateNutrient1, PlateNutrient2, Hedges_g, EuclDist, ESPI))
    }
