# Load required libraries
library(tidyverse)
library(ggplot2)
library(htmlwidgets)
library(plotly)
library(forcats)

# Load the dataset
Heart_Disease_Cleaned_Final <- read.csv("Heart_Disease_Cleaned_Final.csv")

# --------------------------------------
# Step 1: Basic Cleaning
# --------------------------------------

# Remove duplicate rows
Heart_Disease_Cleaned_Final <- Heart_Disease_Cleaned %>% distinct()

# Cap BMI values between 18 and 50
Heart_Disease_Cleaned_Final <- Heart_Disease_Cleaned %>%
  mutate(BMI = ifelse(BMI < 18, 18,
                      ifelse(BMI > 50, 50, BMI)))

# --------------------------------------
# Step 2: Age Group Mapping
# --------------------------------------

Heart_Disease_Cleaned_Final <- Heart_Disease_Cleaned %>%
  mutate(Age.group = case_when(
    AgeCategory == "1"  ~ "18-24",
    AgeCategory == "2"  ~ "25-29",
    AgeCategory == "3"  ~ "30-34",
    AgeCategory == "4"  ~ "35-39",
    AgeCategory == "5"  ~ "40-44",
    AgeCategory == "6"  ~ "45-49",
    AgeCategory == "7"  ~ "50-54",
    AgeCategory == "8"  ~ "55-59",
    AgeCategory == "9"  ~ "60-64",
    AgeCategory == "10" ~ "65-69",
    AgeCategory == "11" ~ "70-74",
    AgeCategory == "12" ~ "75-79",
    AgeCategory == "13" ~ "80+",
    TRUE ~ as.character(AgeCategory)
  ))

# --------------------------------------
# Step 3: BMI Category Mapping
# --------------------------------------

Heart_Disease_Cleaned_Final <- Heart_Disease_Cleaned %>%
  mutate(BMI_Category = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 25 ~ "Normal Weight",
    BMI >= 25 & BMI < 30 ~ "Overweight",
    BMI >= 30 & BMI < 35 ~ "Obesity I",
    BMI >= 35 & BMI < 40 ~ "Obesity II",
    BMI >= 40 ~ "Obesity III"
  ))

# --------------------------------------
# Step 4: Income Category Mapping
# --------------------------------------

Heart_Disease_Cleaned_Final <- Heart_Disease_Cleaned %>%
  mutate(Income_Category = case_when(
    Income == "1" ~ "<$10,000",
    Income == "2" ~ "$10,000–$15,000",
    Income == "3" ~ "$15,000–$20,000",
    Income == "4" ~ "$20,000–$25,000",
    Income == "5" ~ "$25,000–$35,000",
    Income == "6" ~ "$35,000–$50,000",
    Income == "7" ~ "$50,000–$75,000",
    Income == "8" ~ ">$75,000",
    TRUE ~ Income
  ))

# --------------------------------------
# Step 5: Education Level Mapping
# --------------------------------------

Heart_Disease_Cleaned_Final <- Heart_Disease_Cleaned %>%
  mutate(Education_Level = case_when(
    Education == "1" ~ "Never attended",
    Education == "2" ~ "Elementary",
    Education == "3" ~ "Some high school",
    Education == "4" ~ "High school graduate",
    Education == "5" ~ "Some college",
    Education == "6" ~ "College graduate",
    TRUE ~ Education
  ))

# --------------------------------------
# Step 6: Save Cleaned Data
# --------------------------------------

# Final cleaned dataset
Heart_Disease_Cleaned_Final <- Heart_Disease_Cleaned

# Optional: Save to CSV
write.csv(Heart_Disease_Cleaned_Final, "Heart_Disease_Cleaned_Final.csv", row.names = FALSE)

## =========================================================================================================

# --------------------------------------
# Step 1: Overview of the dataset
# --------------------------------------

# View column names and a sample of the data
glimpse(Heart_Disease_Cleaned_Final)
summary(Heart_Disease_Cleaned_Final)

# Check class balance
table(Heart_Disease_Cleaned_Final$HeartDiseaseorAttack)

# Proportion of people with and without heart disease
Heart_Disease_Cleaned_Final %>%
  count(HeartDiseaseorAttack) %>%
  mutate(Proportion = round(100 * n / sum(n), 2))

# --------------------------------------
# Step 2: Heart disease rates across categorical variables
# --------------------------------------

# Heart disease by age group
Heart_Disease_Cleaned_Final %>%
  group_by(Age.group) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# Heart disease by BMI category
Heart_Disease_Cleaned_Final %>%
  group_by(BMI_Category) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# Heart disease by smoking status
Heart_Disease_Cleaned_Final %>%
  group_by(Smoker) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# --------------------------------------
# Step 3: Heart disease and physical activity
# --------------------------------------

Heart_Disease_Cleaned_Final %>%
  group_by(PhysActivity) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# Heart disease and alcohol consumption
Heart_Disease_Cleaned_Final %>%
  group_by(HvyAlcoholConsump) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# Heart disease and high blood pressure
Heart_Disease_Cleaned_Final %>%
  group_by(HighBP) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# --------------------------------------
# Step 4: Socioeconomic and Access-to-Care Descriptives
# --------------------------------------

# Heart disease and income
Heart_Disease_Cleaned_Final %>%
  group_by(Income_Category) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# Heart disease and education
Heart_Disease_Cleaned_Final %>%
  group_by(Education_Level) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# Heart disease and inability to afford care
Heart_Disease_Cleaned_Final %>%
  group_by(NoDocbcCost) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

# Heart disease and cholesterol screening
Heart_Disease_Cleaned_Final %>%
  group_by(CholCheck) %>%
  summarise(
    Count = n(),
    HeartDiseaseRate = round(100 * sum(HeartDiseaseorAttack == 1) / n(), 2)
  )

## =========================================================================================================

# Create output folder if it doesn’t exist
if (!dir.exists("plots/descriptive")) {
  dir.create("plots/descriptive", recursive = TRUE)
}

# List of variables to plot
vars_to_plot <- c("Age.group", "BMI_Category",
                  "Smoker", "PhysActivity", "HvyAlcoholConsump", "HighBP",
                  "Income_Category", "Education_Level", "NoDocbcCost", "CholCheck")

# Loop through each variable and create bar plots
for (var in vars_to_plot) {
  p <- Heart_Disease_Cleaned_Final %>%
    ggplot(aes_string(x = var, fill = "factor(HeartDisease)")) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("#8ecae6", "#d7263d"), labels = c("No Disease", "Heart Disease")) +
    labs(
      title = paste("Heart Disease Proportion by", var),
      x = var,
      y = "Proportion",
      fill = "Heart Disease",
      caption = "Alex Teboul on Kaggle"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the plot
  ggsave(filename = paste0("plots/descriptive/", var, "_heart_disease_plot.png"),
         plot = p, width = 8, height = 5)
}