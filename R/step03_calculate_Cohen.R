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
## STEP 3: Calculate Cohen's d for Each Cross-Plate
##         Nutrient Pair Using Pooled SD
##########################################################

step03 <- function(largest_diff_cross_plate_nutr){

    # We'll store the final Cohen's d calculations here:
    final_cohens_d_results <- data.frame()

    # Loop over each row in 'largest_diff_cross_plate_nutr'
    for (i in seq_len(nrow(largest_diff_cross_plate_nutr))) {

        # Extract relevant info
        the_trait         <- largest_diff_cross_plate_nutr$Trait[i]
        plate_nutrient_1  <- largest_diff_cross_plate_nutr$PlateNutrient1[i]
        plate_nutrient_2  <- largest_diff_cross_plate_nutr$PlateNutrient2[i]

        cat("\n=========================================================\n")
        cat("Computing Cohen's d for:", the_trait, "\n")
        cat("Pair:", plate_nutrient_1, "vs", plate_nutrient_2, "\n")
        cat("=========================================================\n")

        # 1. Pivot the data for THIS trait so we have columns:
        #    (Plate, Nutrient, Value). Same approach as before.

        desired_cols <- names(data)[startsWith(names(data), paste0(the_trait, "_"))]

        df_pivoted <- data %>%
            select(Nutrient, all_of(desired_cols)) %>%
            tidyr::pivot_longer(
                cols      = all_of(desired_cols),
                names_to  = "Plate",    # e.g. "fvfm_P3"
                values_to = "Value"
            ) %>%
            filter(!is.na(Value))  # remove missing data

        # 2. We now have a label like "fvfm_P3_6" for PlateNutrient. We need to split that label
        #    into (Plate, Nutrient) or otherwise match it. Previously, we created a new column
        #    PlateNutrient = paste(Plate, Nutrient, sep="_").
        #    Let's do that again and then filter for the two pairs.

        # Create the combined label in df_pivoted
        # (Plate is e.g. "fvfm_P3", Nutrient is e.g. 6)
        df_pivoted <- df_pivoted %>%
            mutate(PlateNutrient = paste(Plate, Nutrient, sep = "_"))

        # Filter to just the two PlateNutrient combos
        pair_data <- df_pivoted %>%
            filter(PlateNutrient %in% c(plate_nutrient_1, plate_nutrient_2))

        # 3. Calculate group stats for each PlateNutrient
        #    (mean, sd, n). We'll separate them into two data frames or do a group_by approach.

        group_stats <- pair_data %>%
            group_by(PlateNutrient) %>%
            summarize(
                Mean = mean(Value, na.rm = TRUE),
                SD   = sd(Value, na.rm = TRUE),
                n    = n()
            ) %>%
            ungroup()

        # Extract row for group 1
        stats1 <- group_stats[group_stats$PlateNutrient == plate_nutrient_1, ]
        # Extract row for group 2
        stats2 <- group_stats[group_stats$PlateNutrient == plate_nutrient_2, ]

        # 4. If one of them is missing or no data, skip
        if (nrow(stats1) == 0 || nrow(stats2) == 0) {
            warning(paste("No data found for one of the groups:",
                          plate_nutrient_1, "or", plate_nutrient_2))
            next
        }

        # 5. Compute the pooled SD
        n1 <- stats1$n
        n2 <- stats2$n
        sd1 <- stats1$SD
        sd2 <- stats2$SD
        mean1 <- stats1$Mean
        mean2 <- stats2$Mean

        pooled_sd <- sqrt(((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2) / (n1 + n2 - 2))

        # 6. Cohen's d
        d_value <- abs(mean1 - mean2) / pooled_sd

        cat("Group1:", plate_nutrient_1, "n=", n1, "Mean=", mean1, "SD=", sd1, "\n")
        cat("Group2:", plate_nutrient_2, "n=", n2, "Mean=", mean2, "SD=", sd2, "\n")
        cat("Pooled SD:", pooled_sd, "\n")
        cat("Cohen's d:", d_value, "\n")

        # 7. Store results
        final_cohens_d_results <- rbind(
            final_cohens_d_results,
            data.frame(
                Trait             = the_trait,
                PlateNutrient1    = plate_nutrient_1,
                PlateNutrient2    = plate_nutrient_2,
                n1                = n1,
                n2                = n2,
                Mean1             = mean1,
                Mean2             = mean2,
                SD1               = sd1,
                SD2               = sd2,
                Pooled_SD         = pooled_sd,
                Cohens_d          = d_value
            )
        )
    }

    cat("\n>>> Cohen's d Computation Complete <<<\n")
    cat("Below is the final table with sample sizes, means, SDs, and Cohen's d.\n")
    print(final_cohens_d_results)

    return(final_cohens_d_results)
    }
