library(readxl)

library(openxlsx)

library(dplyr)
# Load the writexl package
library(writexl)

library(stringr)

#change the file directory
#file_dir <- "C:/Users/Nora Escher/OneDrive - Imperial College London/IMPERIAL/COP28/_data/"
file_dir <- "C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/COP_Analysis/data/delegates"

delegates_all <- read_csv("COP28_delegates_all.csv")
patterns_and_replacements <- read_excel("replacements_ff.xlsx",sheet = "replacement")
patterns_and_replacements <- patterns_and_replacements %>%
  select(Pattern, Replacement)
mismatch <- read_excel("replacements_ff.xlsx",sheet = "mismatch")
mismatch_phase <- unique(mismatch$Phrase)

#grepl is used to check if a certain word is present in the column (regardless of surroudning words)
#renaming of oil and gas companies 

delegates_all_mutated <- delegates_all%>%
  mutate(new_organization = organization)
for (i in 1:nrow(patterns_and_replacements)) {
  pattern <- patterns_and_replacements$Pattern[i]
  replacement <- patterns_and_replacements$Replacement[i]
  
  delegates_all_mutated <- delegates_all_mutated %>%
    mutate(new_organization = ifelse(grepl(pattern, organization, ignore.case = TRUE), replacement, new_organization))
}

#save excel file
#COP28_delegates_ff_organizations_renamed <- file.path(file_dir,"COP28_delegates_ff_organizations_renamed_check.xlsx")
#write_xlsx(delegates_all_mutated, COP28_delegates_ff_organizations_renamed)
#cat("Excel file saved at:", COP28_delegates_ff_organizations_renamed, "\n")

#subset for oil and gas companies
replacement_vector <- patterns_and_replacements$Replacement
company_names <- unique(replacement_vector)
filtered_oil_and_gas <- subset(delegates_all_mutated, new_organization %in% company_names)
remove.list <- paste(mismatch_phase, collapse = '|')
cleaned_oil_and_gas <- filtered_oil_and_gas %>% filter(!grepl(remove.list, organization, ignore.case = TRUE))

#save excel file
COP28_delegates_ff_organizations_renamed_filtered <- file.path(file_dir,"COP28_delegates_ff_organizations_renamed_filtered_check.xlsx")
write_xlsx(cleaned_oil_and_gas, COP28_delegates_ff_organizations_renamed_filtered)
cat("Excel file saved at:", COP28_delegates_ff_organizations_renamed_filtered, "\n")



#for trading associations
delegates_all_mutated1 <- delegates_all_mutated %>%
  mutate(new_nominator = case_when(
    grepl('International Emissions Trading Association', nominator, ignore.case = TRUE) ~ 'IETA',
    grepl('IETA', nominator, ignore.case = TRUE) ~ 'IETA',
    grepl('clean resource innovation network', nominator, ignore.case = TRUE) ~ 'CRIN',
    grepl('CRIN', nominator, ignore.case = TRUE) ~ 'CRIN',
    grepl('federation of indian chambers of commerce and industry', nominator, ignore.case = TRUE) ~ 'FICCI',
    grepl('FICCI', nominator, ignore.case = TRUE) ~ 'FICCI',
    grepl('World Business Council for Sustainable Development', nominator, ignore.case = TRUE) ~ 'WBCSD',
    grepl('WBCSD', nominator, ignore.case = TRUE) ~ 'WBCSD',
    grepl('Business Council for Sustainable Energy', nominator, ignore.case = TRUE) ~ 'BCSE',
    grepl('BCSE', nominator, ignore.case = TRUE) ~ 'BCSE',
    grepl('Carbon Capture and Storage Association', nominator, ignore.case = TRUE) ~ 'CCSA ',
    grepl('CCSA', nominator, ignore.case = TRUE) ~ 'CCSA ',
    grepl('Chamber of Commerce of the United States of America', nominator, ignore.case = TRUE) ~ 'U.S. Chamber of Commerce',
    grepl('U.S. Chamber of Commerce', nominator, ignore.case = TRUE) ~ 'U.S. Chamber of Commerce',
    grepl('US Chamber of Commerce', nominator, ignore.case = TRUE) ~ 'U.S. Chamber of Commerce',
    grepl('International Chamber of Commerce', nominator, ignore.case = TRUE) ~ 'ICC',
    grepl('Carbon Market Institute', nominator, ignore.case = TRUE) ~ 'Carbon Market Institute',
    grepl('BusinessEurope', nominator, ignore.case = TRUE) ~ 'BusinessEurope',
    grepl('Business Europe', nominator, ignore.case = TRUE) ~ 'BusinessEurope',
    # Add more conditions as needed
    TRUE ~ nominator  # Keep the original value if no condition is met
  ))

association_names <- c("IETA","CRIN","FICCI", "WBCSD", "BCSE", "CCSA", "U.S. Chamber of Commerce", "ICC", "Carbon Market Institute", "BusinessEurope")
filtered_associations <- subset(delegates_all_mutated1, new_nominator %in% association_names)


#save excel file
COP28_delegates_ff_nominators_renamed_filtered <- file.path(file_dir,"COP28_delegates_ff_nominators_renamed_filtered.xlsx")
write_xlsx(filtered_associations, COP28_delegates_ff_nominators_renamed_filtered)
cat("Excel file saved at:", COP28_delegates_ff_nominators_renamed_filtered, "\n")

