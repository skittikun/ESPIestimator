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
## STEP 6: Visualize Hedges' g
############################################

step06 <- function(largest_diff_cross_plate_nutr){

    library(dplyr)
    library(ggplot2)

    # 1. Ensure 'Pair' is created
    final_cohens_d_results <- final_cohens_d_results %>%
        mutate(
            # Create the Pair label
            Pair = paste(PlateNutrient1, "vs", PlateNutrient2, sep = " "),

            # Convert Pair to a factor with levels in the original row order
            # (unique() retains the first occurrence of each Pair in the order encountered)
            Pair = factor(Pair, levels = unique(Pair))
        )

    # 2. Single-panel bar chart with a blue gradient based on Hedges' g
    #    and keep the bars in the data's original order.
    plot_hedges_g <- ggplot(final_cohens_d_results,
                            aes(x = Pair, y = Hedges_g, fill = Hedges_g)) +
        # a) Bars for Hedges' g
        geom_col(width = 0.7) +

        # b) Gradient from lightblue to darkblue
        scale_fill_gradient(low = "lightblue", high = "darkblue") +

        # c) Labels & theme
        labs(
            title = "Hedges' g per trait",
            x = "Plate-Nutrient Pair",
            y = "Hedges' g"
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
            # legend.position = "none"  # uncomment if you prefer no legend
        )

    cat("\n>>> Plot: Hedges' g in Original Data Order (Blue Gradient) <<<\n")
    print(plot_hedges_g)


    return(plot_hedges_g)
    }
