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
## STEP 13 Visualize ESPI on a Single Plot
## (No Faceting by Trait)
############################################

step13 <- function(final_cohens_d_results){

    library(ggplot2)

    plot_espi <- ggplot(final_cohens_d_results, aes(x = Pair, y = ESPI, fill = Trait)) +
        # Use grouped (dodged) bars so multiple traits can appear side by side
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +

        # Add the Blues palette for discrete traits
        scale_fill_brewer(palette = "Blues") +

        labs(
            title = "ESPI (Hedges' g / Euclidean Distance)",
            x = "Plate-Nutrient Pair",
            y = "ESPI (unitless)"
        ) +

        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)  # tilt labels for readability
            # legend.position = "none"  # uncomment if you want to hide the legend
        )

    print(plot_espi)




    return(plot_espi)
    }
