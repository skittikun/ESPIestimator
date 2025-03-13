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
## STEP 11: Calculate Euclidean Distance of Environmental
##       Conditions for Each Pair of Plate/Nutrient
##       and Visualize with Hedges’ g
##########################################################

step11 <- function(final_cohens_d_results){

    # --------------------------------------------------------
    # 1. Read the key.xlsx file
    # --------------------------------------------------------
    key_data <- read_excel("key.xlsx")  # Adjust path as needed

    # key_data has columns:
    #  Plate, Nutrient, Temp, Light, Carbon, Nitrogen, Phosphate

    # --------------------------------------------------------
    # 2. Normalize Each Environmental Column to [0,1]
    #    (Min-Max scaling done independently per column)
    # --------------------------------------------------------
    normalize_min_max <- function(x) {
        (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }

    key_data_norm <- key_data %>%
        mutate(
            Temp_norm      = normalize_min_max(Temp),
            Light_norm     = normalize_min_max(Light),
            Carbon_norm    = normalize_min_max(Carbon),
            Nitrogen_norm  = normalize_min_max(Nitrogen),
            Phosphate_norm = normalize_min_max(Phosphate)
        )

    # --------------------------------------------------------
    # 3. Define a helper function to get the normalized
    #    environmental vector for a given (Plate, Nutrient)
    # --------------------------------------------------------
    get_env_vector <- function(plate, nutrient, df_norm) {
        # Subset for matching plate & nutrient
        row_match <- df_norm %>%
            filter(Plate == plate, Nutrient == as.numeric(nutrient))

        # If no match or multiple matches, handle gracefully
        if (nrow(row_match) != 1) {
            warning(paste("Could not find unique row for Plate =", plate,
                          "Nutrient =", nutrient, "; returning NA vector."))
            return(rep(NA, 5))
        }

        # Return vector of normalized environment columns
        c(
            row_match$Temp_norm,
            row_match$Light_norm,
            row_match$Carbon_norm,
            row_match$Nitrogen_norm,
            row_match$Phosphate_norm
        )
    }

    # --------------------------------------------------------
    # 4. Compute Euclidean Distance for Each Row in
    #    final_cohens_d_results
    # --------------------------------------------------------
    # We assume final_cohens_d_results has columns like:
    #  PlateNutrient1, PlateNutrient2, Hedges_g, etc.
    # e.g. "fvfm_P3_6" => trait + "_" + Plate + "_" + Nutrient
    # If your columns differ, adapt accordingly.

    # For safety, we store new results in final_cohens_d_results
    # If you have not created that data frame yet, do so or adapt code.

    final_cohens_d_results <- final_cohens_d_results %>%
        rowwise() %>%
        mutate(
            # Parse Plate, Nutrient from the PlateNutrient1 string
            # Example: "fvfm_P3_6" => we might split by "_" and take the last two parts
            # If your naming scheme is consistent, you can do:
            PN1_parts = str_split(PlateNutrient1, "_", simplify = TRUE),
            # The last part is the Nutrient, the second to last is Plate (with trait removed).
            # Example: PN1_parts = c("fvfm", "P3", "6")

            Plate1    = PN1_parts[,2],
            Nutr1_str = PN1_parts[,3],

            # Parse Plate, Nutrient from PlateNutrient2
            PN2_parts = str_split(PlateNutrient2, "_", simplify = TRUE),
            Plate2    = PN2_parts[,2],
            Nutr2_str = PN2_parts[,3],

            # Now get the environment vectors from the normalized key
            Env_vec1 = list(get_env_vector(Plate1, Nutr1_str, key_data_norm)),
            Env_vec2 = list(get_env_vector(Plate2, Nutr2_str, key_data_norm)),

            # Euclidean distance:
            EuclDist = sqrt( sum( (unlist(Env_vec1) - unlist(Env_vec2))^2 ) )
        ) %>%
        ungroup() %>%
        select(-PN1_parts, -PN2_parts)  # remove helper columns

    # --------------------------------------------------------
    # 5. Visualize Each Pair’s Hedges’ g vs EuclDist
    # --------------------------------------------------------
    # A straightforward approach is a scatter plot with, e.g.,
    # x = Euclidean Distance, y = Hedges’ g

    ##########################################################
    ## Visualize Hedges' g vs. EuclDist, Labeling Each Point
    ##########################################################


    # Example: Label points by Trait
    plot_dist_vs_g <- ggplot(final_cohens_d_results,
                             aes(x = EuclDist, y = Hedges_g)) +
        # 1. Scatter points
        geom_point(size = 3, alpha = 0.7, color = "blue") +

        # 2. Label each point with the Trait
        #    (Adjust vjust/hjust or use geom_text_repel for better readability)
        geom_text(aes(label = Trait),
                  nudge_y = 2.5,  # move text slightly above the point
                  size = 3,
                  color = "black") +

        # 3. Optional: Add a smooth regression line (method="lm") or remove it entirely
        geom_smooth(method = "", se = FALSE, color = "red") +

        # 4. Labels & theme
        labs(
            title = "Relationship Between Environmental Distance and Effect Size",
            x = "Euclidean Distance (Normalized Environmental Conditions)",
            y = "Hedges' g"
        ) +
        theme_minimal()

    cat("\n>>> Plot: EuclDist vs. Hedges' g (Points Labeled by Trait) <<<\n")
    print(plot_dist_vs_g)


    return(plot_dist_vs_g)
    }
