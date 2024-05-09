library(readxl)
delegates_all <- read_excel("COP28_delegates_all.xlsx")
library(openxlsx)

library(dplyr)
# Load the writexl package
library(writexl)

library(stringr)

#grepl is used to check if a certain word is present in the column (regardless of surroudning words)
#renaming of oil and gas companies 
delegates_all_mutated <- delegates_all %>%
  mutate(new_organization = case_when(
    grepl('nestle', organization, ignore.case = TRUE) ~ 'Nestle',
    grepl('McDonald', organization, ignore.case = TRUE) ~ 'McDonald',
    grepl('Unilever', organization, ignore.case = TRUE) ~ 'Unilever',
    grepl('Mondelez', organization, ignore.case = TRUE) ~ 'Mondelez',
    grepl('Chipotle', organization, ignore.case = TRUE) ~ 'Chipotle',
    grepl('Compass Group', organization, ignore.case = TRUE) ~ 'Compass Group',
    grepl('Kraft Heinz', organization, ignore.case = TRUE) ~ 'Kraft Heinz',
    grepl('Danone', organization, ignore.case = TRUE) ~ 'Danone',
    grepl('ADM', organization, ignore.case = TRUE) ~ 'Archer Daniels Midland (ADM)',
    grepl('McCormick', organization, ignore.case = TRUE) ~ 'McCormick & Company',
    grepl('Bunge', organization, ignore.case = TRUE) ~ 'Bunge',
    grepl('JBS', organization, ignore.case = TRUE) ~ 'JBS',
    grepl('Mengniu', organization, ignore.case = TRUE) ~ 'Mengniu Dairy',
    grepl('Charoen Pokphand', organization, ignore.case = TRUE) ~ 'Charoen Pokphand',
    grepl('BRF', organization, ignore.case = TRUE) ~ 'BRF',
    TRUE ~ organization  # Keep the original value if no condition is met
  ))

#save excel file
COP28_delegates_food_organizations_renamed <- "C:/Users/Nora Escher/OneDrive - Imperial College London/IMPERIAL/COP28/_data/COP28_delegates_food_organizations_renamed.xlsx"
write_xlsx(delegates_all_mutated, COP28_delegates_food_organizations_renamed)
cat("Excel file saved at:", COP28_delegates_food_organizations_renamed, "\n")


#subset for oil and gas companies
company_names <- c("McDonald", "Unilever", "Mondelez", "Chipotle Mexican Grill",
               "Compass Group", "Kraft Heinz", "Danone", "Archer Daniels Midland (ADM)",
               "McCormick & Company", "Bunge", "JBS", "Mengniu Dairy", 
               "Charoen Pokphand", "BRF")


filtered_food <- subset(delegates_all_mutated, new_organization %in% company_names)

# Word to be deleted
administration <- "Admi"
# Deleting rows where the specified word appears in the specified column
filtered_food <- filtered_food[!grepl(administration, filtered_food$organization), ]
# Word to be deleted
administration1 <- "admi"
# Deleting rows where the specified word appears in the specified column
filtered_food <- filtered_food[!grepl(administration1, filtered_food$organization), ]
# Word to be deleted
administration2 <- "ADMI"
# Deleting rows where the specified word appears in the specified column
filtered_food <- filtered_food[!grepl(administration2, filtered_food$organization), ]



#save excel file
COP28_delegates_food_organizations_renamed_filtered <- "C:/Users/Nora Escher/OneDrive - Imperial College London/IMPERIAL/COP28/_data/COP28_delegates_food_organizations_renamed_filtered.xlsx"
write_xlsx(filtered_food, COP28_delegates_food_organizations_renamed_filtered)
cat("Excel file saved at:", COP28_delegates_food_organizations_renamed_filtered, "\n")


