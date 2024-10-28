# Kobayashi Maru

#Install and load packages.
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("funModeling")
install.packages("car")
install.packages("geepack")
install.packages("ggplot2")
install.packages("emmeans")
install.packages("multcompView")
library(dplyr)
library(ggplot2)
library(funModeling)
library(car)
library(tidyr)
library(geepack)
library(ggplot2)
library(emmeans)
library(multcompView)

# Import data.
raw_data <- read.csv("study2_n1.csv")
working <- raw_data

#' Explore pre-processed data.
str(working)
summary(working)
status(working)

# Remove irrelevant variables.
working <- working[which(working$COHORT == "A"),]
working <- working %>%
  select(AGE, COHORT, DXSTAGE, T1CHEMO, T1DIABET, T2DIAB, T3DIAB, T1EQTOT,
         T2EQTOT, T3EQTOT, T1KIDNEY, T2KIDNEY, T3KIDNEY, T1RP, t2rp, t3rp,
         T1RT, t2rt, t3rt, T1SPBONE, T2SPBONE, T3SPBONE, T1VIAGRA, T2VIAGRA,
         T3VIAGRA, ptid, t1ADT, t2ADT, t3ADT, t1arthritis, t2arthritis, 
         t3arthritis, t1heart, T2HEART, T3HEART, t1sporg, T2SPORG, T3SPORG)

# Change blank spaces and character "na"s to NAs.
working[working == ""] <- NA
working[working == "na"] <- NA

# Recode factors.
working <- working %>% 
  mutate(across(c("DXSTAGE", "T1CHEMO", "T1DIABET", "T2DIAB", "T3DIAB",
                  "T1KIDNEY", "T2KIDNEY", "T3KIDNEY", "T1RP", "t2rp", "t3rp", 
                  "T1RT", "t2rt", "t3rt", "T1SPBONE", "T2SPBONE", "T3SPBONE",
                  "T1VIAGRA", "T2VIAGRA", "T3VIAGRA", "t1ADT", "t2ADT",
                  "t3ADT", "t1arthritis", "t2arthritis", "t3arthritis",
                  "t1heart", "T2HEART", "T3HEART", "t1sporg", "T2SPORG",
                  "T3SPORG"), as.factor))

# DXSTAGE variable needs cleaning, then re-factorization.
working <- working %>%
  mutate(DXSTAGE = case_when(
    grepl("^T1", DXSTAGE) ~ "T1",
    grepl("^T2", DXSTAGE) ~ "T2",
    grepl("^T3", DXSTAGE) ~ "T3",
    grepl("^T4", DXSTAGE) ~ "T4",
    TRUE ~ DXSTAGE
  ))
working$DXSTAGE <- factor(working$DXSTAGE)

#' EDA: Create histogram for each numerical variable and bar chart for each
#' categorical variable in data set.
for (var in names(working)) {
  if (is.numeric(working[[var]]) && var != "ptid") {
    histogram <- ggplot(working, aes(x = .data[[var]])) +
      geom_histogram(fill = "skyblue") +
      labs(title = var, x = var, y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 18))
    print(histogram)
  } else if (is.factor(working[[var]])) {
    barchart <- ggplot(working, aes(x = .data[[var]])) +
      geom_bar(fill = "salmon") +
      labs(title = var, x = var, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(, hjust = 0.5, size = 18))
    print(barchart)
  }
}

# Planning out analysis.
# Checking for discrepancies. Diabetes, present to absent? YES.
which(working$T1DIABET == "2" & working$T2DIAB == "0")
which(working$T1DIABET == "2" & working$T2DIAB == "1")
which(working$T1DIABET == "2" & working$T2DIAB == "3")
which(working$T1DIABET == "2" & working$T3DIAB == "0")
which(working$T1DIABET == "2" & working$T3DIAB == "1")
which(working$T1DIABET == "2" & working$T3DIAB == "3")
#' Some patients reported yes to diabetes and then no later on, though 
#' "past, now now" would have been the correct response.
#' Due to this, consider only history of comorbidities, etc. Also for 
#' simplicity.

# Prostatectomy, present to absent? Nope.
which(working$T1RP == "1" & working$t2rp == "0")
which(working$T1RP == "1" & working$t3rp == "0")
#' Same pattern for radiation and hormone therapy. Don't need to worry 
#' about patients coming off treatment.

# Prostatectomy, absent to present? HIT.
which(working$T1RP == "0" & working$t2rp == "1")
which(working$T1RP == "0" & working$t3rp == "1")
which(working$t2rp == "0" & working$t3rp == "1")
#' Some patients started prostatectomy between T1 and T2 (measured at T2).
#' NOT at T3.
#' Some patients started radiation therapy between T1 and T2 (measured at 
#' T2).
#' ALSO, some patients started radiation therapy between T2 and T3 
#' (measured at T3).
#' Some patients started hormone therapy between T1 and T2 (measured at 
#' T2).
#' ALSO, some patients started hormone therapy between T2 and T3 (measured 
#' at T3).

#' Create data frame with discrepancies removed. Check sample size.
working_pruned <- working %>%
  filter(!((working$T1RP == "0" & working$t3rp == "1") |
             (working$T1RT == "0" & working$t3rt == "1") |
             (working$t1ADT == "0" & working$t3ADT == "1")
  ))
working_pruned
# Pruned version has only 23 observations. Proceed with ITT instead.

#' Create treatment variable, based on treatment at T2. Then remove 
#' unnecessary treatment information.
working <- working %>%
  mutate(
    treatment = case_when(
      t2rp == "1" & t2rt == "0" ~ "RP",           
      t2rp == "0" & t2rt == "1" ~ "RT",           
      t2rp == "1" & t2rt == "1" ~ "RP+RT",           
      t2rp == "0" & t2rt == "0" ~ "None",      
      TRUE ~ "Other")
  )
working <- working %>%
  select(-T1RT, - t2rt, -t3rt, -T1RP, -t2rp, -t3rp)

#' Expand data into long format by splitting EQ5D data into time points 
#' and EQ5D score. Retain baseline EQ5D data in a new variable.
working$baseline_EQ5D <- working$T1EQTOT
working <- working %>%
  pivot_longer(cols = c(T1EQTOT, T2EQTOT, T3EQTOT), names_to = "time", 
               values_to = "EQ5D") %>%
  mutate(time = case_when(
    time == "T1EQTOT" ~ "T1",
    time == "T2EQTOT" ~ "T2",
    time == "T3EQTOT" ~ "T3")
  ) %>%
  mutate(across(c(treatment, time), as.factor))

# Remove irrelevant variables for analysis.
working <- working %>%
  select(ptid, AGE, DXSTAGE, treatment, T1CHEMO, T1DIABET, T1KIDNEY, time, 
         t1arthritis, T1SPBONE, T1VIAGRA, t1ADT, t1heart, t1sporg, EQ5D,
         baseline_EQ5D)

# Assessing normality and normality of residuals.
shapiro.test(working$EQ5D[working$time == "T1"])
shapiro.test(log(working$EQ5D[working$time == "T1"] + min(working$EQ5D) + 1))
shapiro.test(sqrt(working$EQ5D[working$time == "T1"] + min(working$EQ5D) + 1))
qqPlot(working$EQ5D[working$time == "T1"])
qqPlot(log(working$EQ5D[working$time == "T1"] + min(working$EQ5D) + 1))
qqPlot(sqrt(working$EQ5D[working$time == "T1"] + min(working$EQ5D) + 1))
#' EQ5D is not normal at any/all time points, even with transformations.
#' Same thing with EQ5D residuals. Let's use non-parametric analysis.

#' Assess collinearity by creating linear model, then removing colinear 
#' variables:
status(working)
# Remove T1CHEMO since it only has one level.
col_model <- lm(EQ5D ~ AGE + DXSTAGE + T1DIABET + T1KIDNEY + time +
                  treatment + T1SPBONE + T1VIAGRA + t1ADT + t1arthritis +
                  t1heart + t1sporg + baseline_EQ5D, data = working)
alias(col_model)
# Remove t1arthritis due to perfect multicollinearity.
# Also remove T1VIAGRA and t1heart due to literature.
col_model <- lm(EQ5D ~ AGE + DXSTAGE + T1DIABET + T1KIDNEY + time +
                  treatment + T1SPBONE + t1ADT + t1sporg + baseline_EQ5D, 
                data = working)
vif(col_model)
# Looks good! Let's remove them from the data frame too.
working <- working %>%
  select(-t1arthritis, -T1CHEMO, -T1VIAGRA, -t1heart)

# Proceed with GEE with covariates.
gee_model <- geeglm(EQ5D ~ treatment * time + baseline_EQ5D + AGE + 
                      DXSTAGE + T1DIABET + T1KIDNEY + T1SPBONE + t1ADT + 
                      t1sporg, id = ptid, data = working, 
                    family = Gamma(link = "log"), corstr = "ar1")
# Shift EQ5D values to make them positive.
working$EQ5D_shifted <- working$EQ5D - min(working$EQ5D) + 1
working$baseline_EQ5D_shifted <- working$baseline_EQ5D - min(working$EQ5D) + 1
gee_model_1 <- geeglm(EQ5D_shifted ~ treatment * time + baseline_EQ5D + 
                      AGE + T1DIABET + T1KIDNEY + T1SPBONE + t1ADT + 
                        t1sporg, id = ptid, data = working, 
                      family = Gamma(link = "log"), corstr = "ar1")
?geeglm
summary(gee_model_1)
#' Post-hoc test to examine differences in means w/ respect to treatment 
#' and time.
emm_1 <- emmeans(gee_model_1, ~ treatment | time)
pairwise_comparisons_1 <- contrast(emm_1, method = "pairwise", adjust = "Bonferroni")
summary(pairwise_comparisons_1)
#' Had to remove DXSTAGE for this model. Will try another model that 
#' removes observations with DXSTAGE as NA.

working_extra <- working[!is.na(working$DXSTAGE),]
gee_model_2 <- geeglm(EQ5D_shifted ~ treatment * time + baseline_EQ5D + 
                        AGE + T1DIABET + T1KIDNEY + T1SPBONE + 
                        DXSTAGE + t1ADT + t1sporg, id = ptid, 
                      data = working_extra, family = Gamma(link = "log"), 
                      corstr = "ar1")
summary(gee_model_2)
# Repeat post-hoc test.
emm_2 <- emmeans(gee_model_2, ~ treatment | time)
pairwise_comparisons_2 <- contrast(emm_2, method = "pairwise", adjust = "Bonferroni")
summary(pairwise_comparisons_2)
#' Again, no significant differences between treatments at the different 
#' time points.

# Compare QICs. 
QIC(gee_model_1)
QIC(gee_model_2)
# Model 2 is better. Use its summary statistics for interpretation.
summary(gee_model_2)
summary(pairwise_comparisons_2)

# Plot relationship. First create data frame with necessary data.
mean_EQ5D_data <- working %>%
  group_by(treatment, time) %>%
  dplyr::summarize(
    mean_EQ5D = mean(EQ5D, na.rm = TRUE),
    se_EQ5D = sd(EQ5D, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )
# Add significance letters according to emmeans output.
mean_EQ5D_data <- mean_EQ5D_data %>%
  mutate(
    sig_letter = case_when(
      treatment == "None" & time == "T1" ~ "a",
      treatment == "RP" & time == "T1" ~ "a",
      treatment == "RP+RT" & time == "T1" ~ "b",
      treatment == "RT" & time == "T1" ~ "a",
      treatment == "None" & time == "T2" ~ "a",
      treatment == "RP" & time == "T2" ~ "a",
      treatment == "RP+RT" & time == "T2" ~ "b",
      treatment == "RT" & time == "T2" ~ "a",
      treatment == "None" & time == "T3" ~ "ab",
      treatment == "RP" & time == "T3" ~ "a",
      treatment == "RP+RT" & time == "T3" ~ "b",
      treatment == "RT" & time == "T3" ~ "a",
      TRUE ~ "")
    )
# Plot.
ggplot(mean_EQ5D_data, aes(x = time, y = mean_EQ5D, color = treatment, 
                           group = treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = sig_letter),
            vjust = -0.5,
            size = 5,
            position = position_dodge(width = 0.3),
            show.legend = FALSE) +
  labs(title = "Mean EQ5D Score by Treatment Type Over Time",
       x = "Timepoint",
       y = "Mean EQ5D Score",
       color = "Treatment Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
# Plot w/ error bars. No significance letters.
ggplot(mean_EQ5D_data, aes(x = time, y = mean_EQ5D, color = treatment, 
                           group = treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_EQ5D - se_EQ5D, ymax = mean_EQ5D + se_EQ5D), 
                width = 0.2,
                linewidth = 0.7) +
  labs(title = "Mean EQ5D Score by Treatment Type Over Time",
       x = "Timepoint",
       y = "Mean EQ5D Score",
       color = "Treatment Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

summary(working$T1DIABET)
summary(working$T1KIDNEY)

working$EQ5D[which(working$time == "T3" & working$treatment == "RP+RT")]
summary(working$treatment)

ggplot(mean_EQ5D_data, aes(x = time, y = mean_EQ5D, color = treatment, group = treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = sig_letter),              # Only map `label` here
            vjust = -0.5,                         # Position letters above points
            size = 5,
            show.legend = FALSE) +                           # Size of the letters
  labs(title = "Mean EQ5D Score by Treatment Type Over Time",
       x = "Timepoint",
       y = "Mean EQ5D Score",
       color = "Treatment Type") +                # Legend for treatment only
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
