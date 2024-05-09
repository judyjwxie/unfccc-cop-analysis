library(readxl)
delegates_all <- read_excel("COP28_delegates_all.xlsx")
library(openxlsx)

library(dplyr)
# Load the writexl package
library(writexl)

library(stringr)

# 
#file_dir <- "C:/Users/Nora Escher/OneDrive - Imperial College London/IMPERIAL/COP28/_data/"
file_dir <- "C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/COP_Analysis/data/delegates"


#grepl is used to check if a certain word is present in the column (regardless of surroudning words)
#renaming of oil and gas companies 
delegates_all_mutated <- delegates_all %>%
  mutate(new_organization = case_when(
    grepl('aramco', organization, ignore.case = TRUE) ~ 'Saudi Aramco',
    grepl('exxon', organization, ignore.case = TRUE) ~ 'ExxonMobil',
    grepl('chevron', organization, ignore.case = TRUE) ~ 'Chevron',
    grepl('shell', organization, ignore.case = TRUE) ~ 'Shell',
    grepl('petrochina', organization, ignore.case = TRUE) ~ 'PetroChina',
    grepl('total', organization, ignore.case = TRUE) ~ 'TotalEnergies',
    grepl('BP', organization, ignore.case = TRUE) ~ 'BP',
    grepl('Petrobras', organization, ignore.case = TRUE) ~ 'Petrobras',
    grepl('TAQA', organization, ignore.case = TRUE) ~ 'TAQA',
    grepl('Equinor', organization, ignore.case = TRUE) ~ 'Equinor',
    grepl('Sinopec', organization, ignore.case = TRUE) ~ 'Sinopec',
    grepl('CNOOC', organization, ignore.case = TRUE) ~ 'newname',
    grepl('Enbridge', organization, ignore.case = TRUE) ~ 'Enbridge',
    grepl('Duke Energy', organization, ignore.case = TRUE) ~ 'Duke Energy',
    grepl('EOG Resources', organization, ignore.case = TRUE) ~ 'EOG Resources',
    grepl('ADNOC', organization, ignore.case = TRUE) ~ 'ADNOC',
    grepl('Lukoil', organization, ignore.case = TRUE) ~ 'Lukoil',
    grepl('Novatek', organization, ignore.case = TRUE) ~ 'Novatek',
    grepl('Gazprom', organization, ignore.case = TRUE) ~ 'Gazprom',
    grepl('Williams Companies', organization, ignore.case = TRUE) ~ 'Williams Companies',
    grepl('Suncor', organization, ignore.case = TRUE) ~ 'Suncor Energy',
    grepl('Baker Hughes', organization, ignore.case = TRUE) ~ 'Baker Hughes',
    grepl('Cenovus', organization, ignore.case = TRUE) ~ 'Cenovus Energy',
    grepl('Imperial Oil', organization, ignore.case = TRUE) ~ 'Imperial Oil',
    grepl('ONGC', organization, ignore.case = TRUE) ~ 'ONGC',
    grepl('Neste', organization, ignore.case = TRUE) ~ 'Neste',
    grepl('Ecopetrol', organization, ignore.case = TRUE) ~ 'Ecopetrol',
    grepl('Formosa', organization, ignore.case = TRUE) ~ 'Formosa Petrochemical',
    grepl('Orlen', organization, ignore.case = TRUE) ~ 'PKN Orlen',
    grepl('Pembina', organization, ignore.case = TRUE) ~ 'Pembina Pipeline',
    grepl('Repsol', organization, ignore.case = TRUE) ~ 'Repsol',
    grepl('Aker', organization, ignore.case = TRUE) ~ 'Aker',
    grepl('PTT', organization, ignore.case = TRUE) ~ 'PTT',
    grepl('inpex', organization, ignore.case = TRUE) ~ 'inpex',
    grepl('eqt', organization, ignore.case = TRUE) ~ 'EQT Corporation',
    grepl('Tourmaline', organization, ignore.case = TRUE) ~ 'Tourmaline Oil',
    grepl('OMV', organization, ignore.case = TRUE) ~ 'OMV',
    grepl('Adani', organization, ignore.case = TRUE) ~ 'Adani Group',
    grepl('ENEOS', organization, ignore.case = TRUE) ~ 'ENEOS Holdings',
    grepl('Galp Energia', organization, ignore.case = TRUE) ~ 'Galp Energia',
    grepl('Centrica', organization, ignore.case = TRUE) ~ 'Centrica',
    grepl('SK Innovation', organization, ignore.case = TRUE) ~ 'SK Innovation',
    grepl('Permian Resources', organization, ignore.case = TRUE) ~ 'Permian Resources',
    grepl('Barito', organization, ignore.case = TRUE) ~ 'Barito Pacific',
    grepl('Technip', organization, ignore.case = TRUE) ~ 'TechnipFMC',
    grepl('YPF', organization, ignore.case = TRUE) ~ 'YPF',
    grepl('Edison', organization, ignore.case = TRUE) ~ 'Edison',
    grepl('PetGas', organization, ignore.case = TRUE) ~ 'Petronas Gas',
    grepl('Petronas', organization, ignore.case = TRUE) ~ 'Petronas Gas',
    grepl('S-OIL', organization, ignore.case = TRUE) ~ 'S-OIL',
    grepl('Sasol', organization, ignore.case = TRUE) ~ 'Sasol',
    grepl('Romgaz', organization, ignore.case = TRUE) ~ 'Romgaz',
    grepl('MEG Energy', organization, ignore.case = TRUE) ~ 'MEG Energy',
    grepl('Whitecap', organization, ignore.case = TRUE) ~ 'Whitecap Resources',
    grepl('Strathcona', organization, ignore.case = TRUE) ~ 'Strathcona Resources',
    grepl('ATCO', organization, ignore.case = TRUE) ~ 'ATCO',
    grepl('Motor Oil', organization, ignore.case = TRUE) ~ 'Motor Oil (Hellas) Corinth Refineries',
    grepl('Hellas', organization, ignore.case = TRUE) ~ 'Motor Oil (Hellas) Corinth Refineries',
    grepl('Transportadora de Gas del Sur', organization, ignore.case = TRUE) ~ 'Transportadora de Gas del Sur',
    grepl('Aker', organization, ignore.case = TRUE) ~ 'Aker',
    grepl('Secure Energy Services', organization, ignore.case = TRUE) ~ 'Secure Energy Services',
    grepl('Fluxys', organization, ignore.case = TRUE) ~ 'Fluxys',
    grepl('CIMC', organization, ignore.case = TRUE) ~ 'CIMC Enric',
    grepl('Atlas Energy', organization, ignore.case = TRUE) ~ 'Atlas Energy Solutions',
    grepl('Genesis', organization, ignore.case = TRUE) ~ 'Genesis Energy',
    grepl('NW Natural', organization, ignore.case = TRUE) ~ 'NW Natural',
    grepl('GasLog', organization, ignore.case = TRUE) ~ 'GasLog',
    grepl('Tetra Tech', organization, ignore.case = TRUE) ~ 'Tetra Technologies',
    grepl('Vertex', organization, ignore.case = TRUE) ~ 'Vertex Energy',
    grepl('Eni SPA', organization, ignore.case = TRUE) ~ 'eni S.p.A.',
    grepl('Eni S.P.A.', organization, ignore.case = TRUE) ~ 'eni S.p.A.',
    grepl('EDF ', organization, ignore.case = TRUE) ~ 'EDF',
    grepl('EDF ', organization, ignore.case = TRUE) ~ 'EDF',
    grepl('groupe EDF ', organization, ignore.case = TRUE) ~ 'EDF',
    # Add more conditions as needed
    TRUE ~ organization  # Keep the original value if no condition is met
  ))


#save excel file
COP28_delegates_ff_organizations_renamed <- file.path(file_dir,"COP28_delegates_ff_organizations_renamed.xlsx")
write_xlsx(delegates_all_mutated, COP28_delegates_ff_organizations_renamed)
cat("Excel file saved at:", COP28_delegates_ff_organizations_renamed, "\n")


#subset for oil and gas companies
company_names <- c(
  "Saudi Aramco", "ExxonMobil", "Chevron", "Shell", "PetroChina",
  "TotalEnergies", "BP", "Petrobras", "TAQA", "Equinor", "Sinopec", "CNOOC",
  "Enbridge", "Duke Energy", "EOG Resources", "ADNOC", "Phillips 66",
  "Lukoil", "Novatek", "Gazprom", "Williams Companies", "Suncor Energy",
  "Baker Hughes", "Cenovus Energy", "Imperial Oil", "ONGC", "PTT",
  "Neste", "Ecopetrol", "Formosa Petrochemical", "PKN Orlen",
  "Pembina Pipeline", "Repsol", "Aker", "PTT Exploration and Production",
  "Inpex", "EQT Corporation", "Tourmaline Oil",
  "OMV", "Adani Group", "ADNOC", "PTT",
  "ENEOS Holdings", "Galp Energia", "Centrica", "SK Innovation",
  "Permian Resources", "Barito Pacific", "TechnipFMC", "YPF", "Edison",
  "PetGas (Petronas Gas)", "DCC plc", "China Resources Gas Group",
  "PTT Oil and Retail Business", "S-OIL", "Sasol", "Romgaz", "MEG Energy",
  "Whitecap Resources", "Strathcona Resources", "ATCO",
  "Motor Oil (Hellas) Corinth Refineries", "Transportadora de Gas del Sur",
  "Aker", "Secure Energy Services", "Fluxys Belgium",
  "CIMC Enric", "Atlas Energy Solutions", "Genesis Energy", "NW Natural",
  "GasLog", "Tetra Technologies", "Vertex Energy", "eni S.p.A.", "EDF"
)


filtered_oil_and_gas <- subset(delegates_all_mutated, new_organization %in% company_names)

#save excel file
COP28_delegates_ff_organizations_renamed_filtered <- file.path(file_dir,"COP28_delegates_ff_organizations_renamed_filtered.xlsx")
write_xlsx(filtered_oil_and_gas, COP28_delegates_ff_organizations_renamed_filtered)
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

