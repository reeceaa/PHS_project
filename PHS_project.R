# trying some spss stuff

# install readspss
options(repos = c(
  +     janmarvin = 'https://janmarvin.r-universe.dev',
  +     CRAN = 'https://cloud.r-project.org'))
install.packages('readspss')

# load in and clean data
library(readspss)
library(ggplot2)
library(dplyr)

data <- read.sav("casurvey_uva_vcu_combined_2.sav")
data_clean = subset(data, select = -c(CancerLotOfEffort, CancerFrustrated, APLKIND, DrTalkLungTest, 
                                      HOWLONG, hadhpvtst, lastpap, everhadcancer, hpvvac, hpvvacrec, lookrecordonline, ecc, hadmam, cancerconcernedquality, 
                                      cancerlot, cancerfrust, cancerqual, cancerfat, toomany, urbanrural, app, mam_acs, 
                                      mam_usptf, paphpv_none, hpv_test, pap_hpv, pap_up2date, preventnot) ) 

# Recode 1 and 2 -> 1 = agree
# 3 and 4 -> 0 = disagree

data_recode <- data_clean %>%
  mutate(PreventNotPossible = case_when(
    PreventNotPossible %in% c(1, 2) ~ 1,
    PreventNotPossible %in% c(3, 4) ~ 0,
    TRUE ~ PreventNotPossible
  ))


# Controls
# primsource : no coverage = 0, coverage = 1 (1-7 = 1, 8 = 0)
data_recode <- data_recode %>%
  mutate(PrimSource = case_when(
    PrimSource %in% c(1, 2, 3, 4, 5, 6, 7) ~ 1,
    PrimSource %in% c(8) ~ 0,
    TRUE ~ PrimSource
  ))

# education: <= 12yrs = 0, >12 years = 1 (1-3 = 0, 4-7 = 1)
data_recode <- data_recode %>%
  mutate(Education = case_when(
    Education %in% c(4, 5, 6, 7) ~ 1,
    Education %in% c(1, 2, 3) ~ 0,
    TRUE ~ Education
  ))

# income ranges: < = $49,999 = 0, >$49,999 = 1 (1-5 = 0, 6-9 =1, 10, 11 = NA)
data_recode <- data_recode %>%
  mutate(incomeranges = case_when(
    incomeranges %in% c(6, 7, 8, 9) ~ 1,
    incomeranges %in% c(1, 2, 3, 4, 5) ~ 0,
    FALSE ~ incomeranges
  ))


# female (male/female) 1 = female, 0 = male

# Chi Square for ID and DV
# Create a data frame from the main data set.
main_variables = data.frame(data_recode$PreventNotPossible,data_recode$col_up2date)
colnames(main_variables) <- c('PreventNotPossible', 'col_up2date')

# Create a contingency table        
contingency_table <- table(main_variables$PreventNotPossible, main_variables$col_up2date)
# top row = col up to date (0 = no, 1 = yes)
# left column = prevent not (0 = disagree, 1 = agree)


# Chi-squared test
chi_test <- chisq.test(contingency_table)
print(chi_test)


# Prepare data for the bar plot
# Summarize the count of each combination
plot_data <- main_variables %>%
  group_by(PreventNotPossible, col_up2date) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(
    PreventNotPossible = factor(PreventNotPossible, levels = c(0, 1), labels = c("Disagree", "Agree")),
    col_up2date = factor(col_up2date, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Create the bar plot
ggplot(plot_data, aes(x = col_up2date, y = count, fill = PreventNotPossible)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Disagree" = "steelblue", "Agree" = "lightblue")) +
  labs(
    title = "Thoughts on Cancer Prevention vs. Colonoscopy Status",
    x = "Up to Date on Colonoscopy?",
    y = "Count",
    fill = "Not much prevent cancer?"
  ) +
  theme_minimal()
