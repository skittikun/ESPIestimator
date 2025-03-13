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

############################################
## STEP 14 Add CI to ESPI
############################################

step14 <- function(final_cohens_d_results){

    final_cohens_d_results <- final_cohens_d_results %>%
        mutate(
            # 1. ESPI if not already computed
            ESPI = ifelse(
                is.na(EuclDist) | EuclDist == 0,
                NA_real_,
                Hedges_g / EuclDist
            ),

            # 2. Confidence Interval for ESPI (assuming Distance = constant)
            #    If you have columns CI_lower_nct, CI_upper_nct for Hedges_g:
            ESPI_lower = ifelse(
                is.na(EuclDist) | EuclDist == 0,
                NA_real_,
                CI_lower_nct / EuclDist
            ),
            ESPI_upper = ifelse(
                is.na(EuclDist) | EuclDist == 0,
                NA_real_,
                CI_upper_nct / EuclDist
            )
        )

    # Print to inspect
    cat("\n>>> ESPI with CI (Naive Approach) <<<\n")
    print(final_cohens_d_results %>%
              select(Trait, Pair, Hedges_g, CI_lower_nct, CI_upper_nct, EuclDist, ESPI, ESPI_lower, ESPI_upper))



    return(final_cohens_d_results %>%
               select(Trait, Pair, Hedges_g, CI_lower_nct, CI_upper_nct, EuclDist, ESPI, ESPI_lower, ESPI_upper))
    }
