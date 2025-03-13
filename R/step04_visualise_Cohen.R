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
## STEP 4: Visualization of Cohen's d
##########################################################

step04 <- function(largest_diff_cross_plate_nutr){


    # 1. Create a 'Pair' column for labeling on the x-axis
    final_cohens_d_results <- final_cohens_d_results %>%
        mutate(
            Pair = paste(PlateNutrient1, "vs", PlateNutrient2, sep = " ")
        )

    # 2. Build a basic bar chart with facets by Trait
    plot_cohens_d <- ggplot(final_cohens_d_results, aes(x = Pair, y = Cohens_d, fill = Trait)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        labs(
            title = "Cohen's d for maximum trait change",
            x = "Plate-Nutrient Pair",
            y = "Cohen's d"
        ) +
        # Assign a Brewer palette for distinct blue shades
        scale_fill_brewer(palette = "Blues") +

        # Put each trait in its own facet for clarity
        facet_wrap(~ Trait, scales = "free_x", nrow = 1) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),  # rotate labels for readability
            legend.position = "none"                            # remove legend if you like
        )

    # 3. Print (or return) the plot
    print(plot_cohens_d)

    return(plot_cohens_d)
    }
