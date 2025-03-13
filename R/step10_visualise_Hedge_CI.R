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
## STEP 10 VISUALIZE HEDGES' G WITH CONFIDENCE INTERVALS
##########################################################

step10 <- function(final_cohens_d_results){


    plot_hedges_g_ci <- ggplot(final_cohens_d_results, aes(x = Pair, y = Hedges_g, color = Trait)) +
        # Plot each effect size as a point
        geom_point(position = position_dodge(width = 0.5), size = 3) +

        # Error bars for the CI
        geom_errorbar(
            aes(ymin = CI_lower_nct, ymax = CI_upper_nct),
            width = 0.2,
            position = position_dodge(width = 0.5)
        ) +

        labs(
            title = "Effect Sizes (Hedges’ g) with Noncentral-t Confidence Intervals",
            x = "Plate-Nutrient Pair",
            y = "Hedges’ g (95% CI)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    print(plot_hedges_g_ci)


    return(plot_hedges_g_ci)
    }
