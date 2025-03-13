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

# Update path as needed

#setwd("/Users/163726/Library/CloudStorage/OneDrive-UTS/C3_Projects/C3_ABB/ESPI_Lili/")

# --------------------------------------------
# Load the Data
# --------------------------------------------

step01 <- function(excelIN){
    #data <- read_excel("Final values_NPell.xlsx")
    data <- read_excel(excelIN)

    # --------------------------------------------
    # Define Traits
    # --------------------------------------------
    traits <- c("fvfm", "etr", "npq", "qe", "slow")

    # We'll store the final "largest difference" from each trait here
    largest_diff_across_traits <- data.frame()

    # --------------------------------------------
    # STEP 1: Main Analysis Loop
    # --------------------------------------------
    for (trait in traits) {
        cat("\n============================================\n")
        cat("Analyzing Trait:", trait, "\n")
        cat("============================================\n")

        # 1. Gather columns named like trait_P1, trait_P2, etc.
        #    to get a single "Value" column + "Plate".
        desired_cols <- names(data)[startsWith(names(data), paste0(trait, "_"))]

        # If no matching columns found, skip
        if (length(desired_cols) == 0) {
            cat("No columns found for trait:", trait, "- skipping.\n")
            next
        }

        # Pivot from wide to long
        df <- data %>%
            select(all_of(desired_cols)) %>%
            pivot_longer(
                cols      = everything(),
                names_to  = "Plate",       # e.g. "fvfm_P1"
                values_to = "Value"
            ) %>%
            filter(!is.na(Value))

        # Convert Plate to a factor
        df$Plate <- factor(df$Plate)

        # 2. Fit classical ANOVA: Value ~ Plate
        anova_model <- aov(Value ~ Plate, data = df)

        # 3. Check Normality of Residuals (Shapiro-Wilk)
        shapiro_test <- shapiro.test(residuals(anova_model))

        # 4. Check Homogeneity of Variances (Bartlett’s Test)
        bartlett_test <- bartlett.test(Value ~ Plate, data = df)

        cat("Shapiro-Wilk p-value:", shapiro_test$p.value, "\n")
        cat("Bartlett's Test p-value:", bartlett_test$p.value, "\n")

        normality_violated   <- (shapiro_test$p.value < 0.05)
        homogeneity_violated <- (bartlett_test$p.value < 0.05)

        # 5. Decide which post-hoc method to use
        if (!normality_violated && !homogeneity_violated) {
            cat(">>> Assumptions Met: Using Classic ANOVA + Tukey\n")

            # a) ANOVA summary
            anova_summary <- summary(anova_model)
            print(anova_summary)

            # b) Post-hoc: Tukey HSD
            tukey_res <- TukeyHSD(anova_model, "Plate")
            # Convert to data frame for easy manipulation
            tukey_df <- as.data.frame(tukey_res$Plate) %>%
                rownames_to_column(var = "Comparison") %>%
                mutate(
                    group1 = sub("^(.*)-.*$", "\\1", Comparison),
                    group2 = sub(".*-(.*)$", "\\1", Comparison),
                    AbsDiff = abs(diff)   # absolute mean difference
                )

            # c) Pick the pair of plates with the greatest difference (in absolute value)
            #    You could also filter by p adj < 0.05 if you only want significant differences:
            #    filter(`p adj` < 0.05)
            tukey_df <- tukey_df %>%
                arrange(desc(AbsDiff))

            largest_row <- tukey_df[1, ]
            cat("\n** Largest Plate Difference (Tukey) for", trait, " **\n")
            print(largest_row)

            # d) Store in final results data frame
            #    We'll store group1, group2, difference, p-adj, etc.
            largest_diff_across_traits <- rbind(
                largest_diff_across_traits,
                data.frame(
                    Trait         = trait,
                    Method        = "Classic ANOVA + Tukey",
                    Plate1        = largest_row$group1,
                    Plate2        = largest_row$group2,
                    Mean_Diff     = largest_row$diff,
                    p_value       = largest_row$`p adj`
                )
            )

        } else {
            cat(">>> Assumptions Violated: Using Welch's ANOVA + Games-Howell\n")

            # a) Welch’s ANOVA
            welch_res <- oneway.test(Value ~ Plate, data = df, var.equal = FALSE)
            print(welch_res)

            # b) Games-Howell Post-hoc
            gh_res <- df %>% games_howell_test(Value ~ Plate)

            # c) Create an absolute difference column
            gh_res <- gh_res %>%
                mutate(AbsDiff = abs(estimate)) %>%
                arrange(desc(AbsDiff))

            # d) Pick the pair with the greatest difference
            largest_row <- gh_res[1, ]
            cat("\n** Largest Plate Difference (Games-Howell) for", trait, " **\n")
            print(largest_row)

            # e) Store in final results data frame
            largest_diff_across_traits <- rbind(
                largest_diff_across_traits,
                data.frame(
                    Trait         = trait,
                    Method        = "Welch ANOVA + Games-Howell",
                    Plate1        = largest_row$group1,
                    Plate2        = largest_row$group2,
                    Mean_Diff     = largest_row$estimate,
                    p_value       = largest_row$p.adj
                )
            )
        }
    }

    cat("\n>>> Analysis Complete <<<\n")
    cat("Below are the pairs of plates with the largest difference (absolute value) for each trait.\n")
    print(largest_diff_across_traits)
}
