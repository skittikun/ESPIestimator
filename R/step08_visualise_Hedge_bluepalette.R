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
## STEP8: Visualize Hedges' g + SE (All Traits in One Plot),
##       Using a Blues Palette
##########################################################

step08 <- function(final_cohens_d_results){

    # 1. Ensure we have a 'Pair' column for labeling on the x-axis
    final_cohens_d_results <- final_cohens_d_results %>%
        mutate(
            Pair = paste(PlateNutrient1, "vs", PlateNutrient2)
        )

    # 2. Create a grouped bar chart (one panel),
    #    x = Pair, fill/group by Trait, y = Hedges_g
    plot_hedges_g_se <- ggplot(final_cohens_d_results,
                               aes(x = Pair, y = Hedges_g, fill = Trait)) +
        # a) Bars for Hedges' g, grouped (dodged) by Trait
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +

        # b) Error bars showing Hedges' g ± SE_Hedges_g
        geom_errorbar(
            aes(ymin = Hedges_g - SE_Hedges_g, ymax = Hedges_g + SE_Hedges_g),
            width = 0.2,
            position = position_dodge(width = 0.7)
        ) +

        # c) Assign a Brewer palette of blues
        scale_fill_brewer(palette = "Blues") +

        # d) Labels & theme
        labs(
            title = "Hedges' g with Standard Error (All Traits in One Plot)",
            x = "Plate-Nutrient Pair",
            y = "Hedges' g ± SE"
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)  # tilt x labels for readability
            # legend.position = "none"  # uncomment if you prefer hiding the legend
        )

    cat("\n>>> Plot: Hedges' g with SE (Grouped by Trait, Blues Palette) <<<\n")
    print(plot_hedges_g_se)



    return(plot_hedges_g_se)
    }
