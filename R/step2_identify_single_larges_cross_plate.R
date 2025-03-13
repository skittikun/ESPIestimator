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

##################################################
## STEP 2: For Each Trait’s Top Plate Pair (Step 1),
##         Identify the Single Largest Cross-Plate
##         Nutrient Pair.
##################################################

step2 <- function(largest_diff_across_traits){# This is where we'll store our final results:
    largest_diff_cross_plate_nutr <- data.frame()

    # Loop over the table that tells us, for each trait,
    # which two plates were "most different" overall in Step 1.
    for (i in seq_len(nrow(largest_diff_across_traits))) {

        # 1. Extract info for this trait and the plate pair
        the_trait    <- largest_diff_across_traits$Trait[i]
        plate1       <- largest_diff_across_traits$Plate1[i]
        plate2       <- largest_diff_across_traits$Plate2[i]

        cat("\n=========================================================\n")
        cat("Analyzing CROSS-Plate x Nutrient differences for trait:", the_trait, "\n")
        cat("Selected plates from Step 1:", plate1, "and", plate2, "\n")
        cat("=========================================================\n")

        # 2. Pivot the data for THIS trait so we have:
        #    columns: (Plate, Nutrient, Value).
        #    Each row is a single measurement.

        # Identify columns that start with the_trait, e.g. "fvfm_"
        desired_cols <- names(data)[startsWith(names(data), paste0(the_trait, "_"))]

        # Pivot to long format
        df_pivoted <- data %>%
            select(Nutrient, all_of(desired_cols)) %>%
            pivot_longer(
                cols      = all_of(desired_cols),
                names_to  = "Plate",    # e.g. "fvfm_P1", "fvfm_P3", etc.
                values_to = "Value"
            ) %>%
            filter(!is.na(Value))  # remove rows with missing data

        # 3. Filter to just the 2 plates found in Step 1
        #    (Remember these are "fvfm_P3" style column names,
        #     so if your Step 1 uses exactly that naming, we match directly.)
        plates_of_interest <- c(plate1, plate2)
        subset_df <- df_pivoted %>%
            filter(Plate %in% plates_of_interest)

        # 4. Create a combined factor: PlateNutrient
        #    e.g. "fvfm_P3" + "_" + "6"
        #    But if you prefer a simpler label, you might strip "fvfm_" first.
        subset_df <- subset_df %>%
            mutate(PlateNutrient = paste(Plate, Nutrient, sep = "_"))

        # Convert to factor
        subset_df$PlateNutrient <- factor(subset_df$PlateNutrient)

        # 5. Fit an ANOVA for Value ~ PlateNutrient
        #    (We do a new assumption check for *this* subset.)
        anova_model <- aov(Value ~ PlateNutrient, data = subset_df)

        # 6. Check assumptions (Shapiro & Bartlett)
        shapiro_test    <- shapiro.test(residuals(anova_model))
        bartlett_test   <- bartlett.test(Value ~ PlateNutrient, data = subset_df)

        cat("Shapiro-Wilk p-value:", shapiro_test$p.value, "\n")
        cat("Bartlett's Test p-value:", bartlett_test$p.value, "\n")

        normality_violated   <- (shapiro_test$p.value < 0.05)
        homogeneity_violated <- (bartlett_test$p.value < 0.05)

        # 7. Decide which post-hoc test to use
        if (!normality_violated && !homogeneity_violated) {
            cat(">>> Assumptions Met: Using Classic ANOVA + Tukey\n")

            # a) Tukey
            tukey_res <- TukeyHSD(anova_model, "PlateNutrient")
            # Convert to a data frame
            tukey_df <- as.data.frame(tukey_res$PlateNutrient) %>%
                rownames_to_column(var = "Comparison") %>%
                mutate(
                    group1 = sub("^(.*)-.*$", "\\1", Comparison),
                    group2 = sub(".*-(.*)$", "\\1", Comparison),
                    AbsDiff = abs(diff)
                ) %>%
                arrange(desc(AbsDiff))

            # b) Take the row with the largest difference
            largest_row <- tukey_df[1, ]

            # c) Store in final results
            largest_diff_cross_plate_nutr <- rbind(
                largest_diff_cross_plate_nutr,
                data.frame(
                    Trait          = the_trait,
                    Plate1         = plate1,
                    Plate2         = plate2,
                    PlateNutrient1 = largest_row$group1,
                    PlateNutrient2 = largest_row$group2,
                    Mean_Diff      = largest_row$diff,
                    p_value        = largest_row$`p adj`
                )
            )

            cat("\n>>> Largest Cross-Plate Nutrient Pair for", the_trait, "\n")
            print(largest_row)

        } else {
            cat(">>> Assumptions Violated: Using Welch ANOVA + Games-Howell\n")

            # a) Games-Howell
            library(rstatix)
            gh_res <- subset_df %>% games_howell_test(Value ~ PlateNutrient)
            gh_res <- gh_res %>%
                mutate(AbsDiff = abs(estimate)) %>%
                arrange(desc(AbsDiff))

            largest_row <- gh_res[1, ]

            # b) Store in final results
            largest_diff_cross_plate_nutr <- rbind(
                largest_diff_cross_plate_nutr,
                data.frame(
                    Trait          = the_trait,
                    Plate1         = plate1,
                    Plate2         = plate2,
                    PlateNutrient1 = largest_row$group1,
                    PlateNutrient2 = largest_row$group2,
                    Mean_Diff      = largest_row$estimate,
                    p_value        = largest_row$p.adj
                )
            )

            cat("\n>>> Largest Cross-Plate Nutrient Pair for", the_trait, "\n")
            print(largest_row)
        }
    }

    cat("\n>>> Cross-Plate*Nutrient Analysis Complete <<<\n")
    cat("Below are the single pairs that produce the greatest difference.\n")
    print(largest_diff_cross_plate_nutr)

    return(largest_diff_cross_plate_nutr)
    }
