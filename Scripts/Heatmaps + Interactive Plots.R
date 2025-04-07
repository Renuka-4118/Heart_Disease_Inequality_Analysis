library(dplyr)
library(ggplot2)

# Step 1: Create a grouped lifestyle combination column
lifestyle_combined <- Heart_Disease_Cleaned_Final %>%
  mutate(LifestyleCombo = paste(
    Smoker, PhysActivity, HvyAlcoholConsump,
    HighBP, BMI_Category, Age.group, sep = "_"
  ))

# Step 2: Group by the combination and calculate heart disease rate
lifestyle_summary <- lifestyle_combined %>%
  group_by(LifestyleCombo) %>%
  summarise(
    Total = n(),
    HeartDiseaseCount = sum(HeartDiseaseorAttack == 1),
    HeartDiseaseRate = round((HeartDiseaseCount / Total) * 100, 2)
  ) %>%
  ungroup()

# Step 3: Split the combined string into individual columns for heatmap axes
lifestyle_summary <- lifestyle_summary %>%
  separate(LifestyleCombo,
           into = c("Smoker", "PhysActivity", "Alcohol", "HighBP", "BMI_Category", "Age.group"),
           sep = "_", remove = FALSE)

# Step 4: Create a heatmap - pick 2 variables for axes, color by HeartDiseaseRate
ggplot(lifestyle_summary, aes(x = BMI_Category, y = Age.group, fill = HeartDiseaseRate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#fef6e4", high = "#d7263d", name = "Disease Rate (%)") +
  facet_grid(Smoker + PhysActivity ~ Alcohol + HighBP) +
  labs(
    title = "Heart Disease Rate by Lifestyle Combinations",
    x = "BMI Category",
    y = "Age Category",
    caption = "Alex Teboul on Kaggle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

## DIET

# Step 1: Group and summarize heart disease data by dietary lifestyle variables
diet_summary <- Heart_Disease_Cleaned_Final %>%
  group_by(BMI_Category, Fruits, Veggies) %>%
  summarise(
    HeartDiseaseCount = sum(HeartDiseaseorAttack == 1, na.rm = TRUE),
    Total = n(),
    HeartDiseaseRate = round((HeartDiseaseCount / Total) * 100, 2)
  ) %>%
  ungroup()

# Step 2: Create a heatmap
ggplot(diet_summary, aes(x = BMI_Category, y = Fruits, fill = HeartDiseaseRate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#fef6e4", high = "#2166AC", name = "Disease Rate (%)") +
  facet_wrap(~Veggies) +
  labs(
    title = "Heart Disease Rate by BMI, Fruit & Veggie Intake",
    x = "BMI Category",
    y = "Consumes Fruits Daily",
    caption = "Alex Teboul on Kaggle"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

# ACCESS TO CARE

# Step 1: Group and summarize by access variables
access_summary <- Heart_Disease_Cleaned_Final %>%
  group_by(AnyHealthcare, NoDocbcCost, CholCheck) %>%
  summarise(
    HeartDiseaseCount = sum(HeartDiseaseorAttack == 1, na.rm = TRUE),
    Total = n(),
    HeartDiseaseRate = round((HeartDiseaseCount / Total) * 100, 2)
  ) %>%
  ungroup()

# Step 2: Create heatmap
ggplot(access_summary, aes(x = AnyHealthcare, y = NoDocbcCost, fill = HeartDiseaseRate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#e0f7fa", high = "#006064", name = "Disease Rate (%)") +
  facet_wrap(~CholCheck) +
  labs(
    title = "Heart Disease Rate by Access to Care Factors",
    x = "Has Health Coverage",
    y = "Unable to visit Doctor due to cost",
    caption = "Faceted by Cholesterol Check Status, *Alex Teboul on Kaggle*"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

# SOCIOECONOMIC

# Step 1: Group by socioeconomic variables and summarize
socio_summary <- Heart_Disease_Cleaned_Final %>%
  group_by(Education, Income) %>%
  summarise(
    HeartDiseaseCount = sum(HeartDiseaseorAttack == 1, na.rm = TRUE),
    Total = n(),
    HeartDiseaseRate = round((HeartDiseaseCount / Total) * 100, 2)
  ) %>%
  ungroup()

# Step 2: Create heatmap
ggplot(socio_summary, aes(x = factor(Education), y = factor(Income), fill = HeartDiseaseRate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f1f1f1", high = "#67001f", name = "Disease Rate (%)") +
  labs(
    title = "Heart Disease Rate by Education & Income Levels",
    x = "Education Level",
    y = "Income Category",
    caption = "Higher disease rate associated with lower education and income levels, *Alex Teboul on Kaggle*"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 10, face = "bold")
  )


# RSIK x ACCESS TO CARE HEATMAP

# Step 1: Combine lifestyle & diet factors into a risk profile string
Heart_Disease_Cleaned_Final <- Heart_Disease_Cleaned_Final %>%
  mutate(RiskProfile = paste(Smoker, PhysActivity, HvyAlcoholConsump, HighBP, BMI_Category, Fruits, Veggies, sep = "_"))

# Step 2: Summarize data
risk_access_summary <- Heart_Disease_Cleaned_Final %>%
  group_by(RiskProfile) %>%
  summarise(
    Total = n(),
    HeartDiseaseCount = sum(HeartDiseaseorAttack == 1, na.rm = TRUE),
    NoCareDueToCost = sum(NoDocbcCost == 1, na.rm = TRUE),
    Uninsured = sum(AnyHealthcare == 0, na.rm = TRUE),
    LowIncome = sum(Income %in% 1:2, na.rm = TRUE)
  ) %>%
  mutate(
    HeartDiseaseRate = round((HeartDiseaseCount / Total) * 100, 2),
    NoCareRate = round((NoCareDueToCost / Total) * 100, 2),
    UninsuredRate = round((Uninsured / Total) * 100, 2),
    LowIncomeRate = round((LowIncome / Total) * 100, 2)
  ) %>%
  filter(Total > 50)  # Optional: filter rare combos

# Step 3: Separate RiskProfile into components (for optional facetting)
risk_access_summary <- risk_access_summary %>%
  separate(RiskProfile, into = c("Smoker", "PhysActivity", "Alcohol", "HighBP", "BMI_Category", "Fruits", "Veggies"), sep = "_", remove = FALSE)

# Step 4: Create the main heatmap — Heart Disease Rate vs Access to Care Barrier
ggplot(risk_access_summary, aes(x = HeartDiseaseRate, y = NoCareRate)) +
  geom_point(aes(size = Total, color = BMI_Category), alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Barriers to Care Among High-Risk Heart Disease Profiles",
    x = "Heart Disease Rate (%)",
    y = "Unable to See Doctor Due to Cost (%)",
    size = "Group Size",
    color = "BMI Category",
    caption = "Alex Teboul on Kaggle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # plot panel background
    plot.background = element_rect(fill = "white", color = NA),   # entire plot background
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

# Step 5: Define your folder and save all 4 heatmaps
output_folder <- "C:\\Users\\renuk\\Downloads\\Google Data Analytics 1\\Capstone\\Project 1_Final\\Plots\\Summarized plots"

# Save current plot (risk vs access)
ggsave(filename = file.path(output_folder, "Risk_AccessToCare_HeartDisease.png"), width = 10, height = 6)