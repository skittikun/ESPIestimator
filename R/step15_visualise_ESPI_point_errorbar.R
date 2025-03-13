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

###########################################################
## STEP 15 Visualize: ESPI as a Point + Error Bar
###########################################################

step15 <- function(final_cohens_d_results){

    plot_espi_ci <- ggplot(final_cohens_d_results, aes(x = Pair, y = ESPI, color = Trait)) +
        # 1. Points for ESPI
        geom_point(position = position_dodge(width = 0.5), size = 3) +

        # 2. Error bars for ESPI CI
        geom_errorbar(
            aes(ymin = ESPI_lower, ymax = ESPI_upper),
            width = 0.2,
            position = position_dodge(width = 0.5)
        ) +

        labs(
            title = "ESPI with CI (Hedges’ g / Distance, CI from g Only)",
            x = "Plate-Nutrient Pair",
            y = "ESPI ± CI"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    print(plot_espi_ci)
    return(plot_espi_ci)
    }
