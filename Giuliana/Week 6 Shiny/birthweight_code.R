library(ggformula)
library(ggplot2)
library(RColorBrewer)

data(birthwt, package = "MASS")

# Reference: Venables, W. N. and Ripley, B. D. (2002) 
# Modern Applied Statistics with S. Fourth edition. Springer.

birth <- birthwt %>%
  mutate(
    smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
    race = factor(race, labels = c("White", "Afican American", "Other")),
    low = factor(low, labels = c("Low weight", "Normal weight")),
    ht = factor(ht, labels = c("No hypertension", "Hypertension")),
    ui = factor(ui, labels = c("No uterine irritability", "Uterine irritability"))
    ) %>%
  set_variable_labels(
    low = 'birth weight less than 2.5 kg',
    age = 'mother age in years',
    lwt = 'mother weight in pounds at last menstrual period',
    race = 'mother race',
    smoke = 'smoking status during pregnancy',
    ptl = 'number of previous premature labours',
    ht = 'history of hypertension',
    ui = 'presence of uterine irritability',
    ftv = 'number of physician visits during the first trimester',
    bwt = 'birth weight (g)',
  )

ggplot(birth, aes(x = lwt, y = low, color = low, fill = low)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~ui, scales = "free",shrink=FALSE) +
  theme_classic() +
  scale_color_brewer(palette="Accent") +
  scale_fill_brewer(palette="Accent") +
  theme(legend.position="none")

ggplot(birth, aes(x = race, y = bwt, color = ui)) +
  geom_violin() +
  geom_jitter(alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5), linetype="dashed") +
  theme_classic() +
  scale_color_brewer(palette="Accent")

ggplot(birth, aes(x = lwt, y = bwt, color = race)) +
  geom_point() +
  geom_smooth(aes(group = race), method = "lm", se = FALSE) +
  theme_classic() +
  scale_color_brewer(palette="Accent")
