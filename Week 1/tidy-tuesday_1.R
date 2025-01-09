# Tidy Tuesday 2025 Code
# Week 1: Bring Your Own Data

# setup 

install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)

# import data 

# Note: data manually downloaded from the National Cancer Institute's
# Surveillance, Epidemiology, and End Results (SEER) Program webpage
# link: https://seer.cancer.gov/
# mild data cleaning performed in Excel to remove metadata

# read and clean data

lung_cancer <- read_csv("data/lung-cancer-data.csv")
prostate_cancer <- read_csv("data/prostate-cancer-data.csv")
breast_cancer <- read_csv("data/breast-cancer-data.csv")
colorectal_cancer <- read_csv("data/colorectal-cancer-data.csv")

lung_clean <- clean_names(lung_cancer)
prostate_clean <- clean_names(prostate_cancer)
breast_clean <- clean_names(breast_cancer)
colorectal_clean <- clean_names(colorectal_cancer)

# tidy data and standardize columns

lung_clean <- lung_clean %>% mutate(cancer_type = "Lung")
prostate_clean <- prostate_clean %>% mutate(cancer_type = "Prostate") %>%
  rename(male_rate_per_100_000 = rate_per_100_000, male_modeled_rate_trend_line = modeled_rate_trend_line) %>%
  mutate(female_rate_per_100_000 = NA, female_modeled_rate_trend_line = NA)
breast_clean <- breast_clean %>% mutate(cancer_type = "Breast")
colorectal_clean <- colorectal_clean %>% mutate(cancer_type = "Colorectal")

full_data <- bind_rows(lung_clean, prostate_clean, breast_clean, colorectal_clean)
view(full_data)

full_data_clean <- full_data %>% pivot_longer(cols = c(female_rate_per_100_000, male_rate_per_100_000), names_to = "sex", 
                                              values_to = "rates_per_100_000") %>%
  select(-c(female_modeled_rate_trend_line, 
            male_modeled_rate_trend_line)) %>%
  mutate(sex = recode(sex, "female_rate_per_100_000" = "Female", "male_rate_per_100_000" = "Male"))

# data viz prep

cbPalette <- c("#D55E00", "#56B4E9")

# final output

ggplot(full_data_clean, mapping = aes(x = year_of_diagnosis, y = rates_per_100_000)) +
  geom_line(full_data_clean, mapping = aes(color = sex)) +
  geom_point(size = 0.2, alpha = 0.8) +
  facet_wrap(~factor(cancer_type, c("Breast", "Prostate", "Lung", "Colorectal"))) + 
  scale_x_continuous(name = "Year of Diagnosis", breaks = seq(1975, 2021, by = 5)) +
  
  scale_y_continuous(name = "Rate per 100,000", breaks = seq(0, 250, by = 25)) +
  scale_color_manual(values = cbPalette) +
  geom_vline(xintercept = 2020, linetype = 2, color = "black", alpha = 0.5) +
  labs(title = "Delay-Adjusted Incidence Rates for the Four Most Common Cancer Types in the U.S. (1975-2021)", 
       caption = "Source: National Cancer Institute \nSEER Program \n \n *Note: The COVID-19 epidemic impacted \n the reliability of figures for 2020-2021",
       color = "Sex") +
  annotate("label",  x = 2015, y = 190, label = "COVID-19*") +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) 

ggsave("us-cancer-incidence.png")

# alternative figure based on changes made in response to Bluesky feedback

cbPalette2 <- c("#CC79A7", "#D55E00", "#000000", "#0072B2")

full_data_clean$sex <- factor(full_data_clean, levels = c("Female", "Male"))

ggplot(full_data_clean, mapping = aes(x = year_of_diagnosis, y = rates_per_100_000, color = cancer_type)) +
  geom_line(mapping = aes(linetype = sex)) +
  scale_x_continuous(name = "Year of Diagnosis", breaks = seq(1975, 2021, by = 5)) +
  scale_y_continuous(name = "Rate per 100,000", breaks = seq(0, 250, by = 25)) +
  scale_color_manual(values = cbPalette2) +
  scale_linetype_manual(name = "Sex", values =c("Female" = "solid", "Male" = "dashed")) +
  labs(title = "Delay-Adjusted Incidence Rates for the Four Most Common Cancer Types in the U.S. (1975-2021)", 
       caption = "Source: National Cancer Institute \nSEER Program \n \n *Note: The COVID-19 epidemic impacted \n the reliability of figures for 2020-2021",
       color = "Cancer Type") +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) 

ggsave("us-cancer-incidence-v2.png")
