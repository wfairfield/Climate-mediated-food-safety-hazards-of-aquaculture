# ============================================================
# Species, commodity, contaminant, bacterial, and geography summaries
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# ------------------------------------------------------------
# 1. Read Excel file
# ------------------------------------------------------------

file_path <- "your file path/Supplementary Information - S2 - Data.xlsx"

output_folder <- dirname(file_path)

species_dat <- read_excel(
  file_path,
  sheet = "Standardized Species"
)

full_data <- read_excel(
  file_path,
  sheet = "Full Data"
)

# ------------------------------------------------------------
# 2. Clean standardized species data and remove rows excluded by production method
# ------------------------------------------------------------

species_clean <- species_dat %>%
  rename(
    study_number = 1,
    production_method = 2,
    standardized_species = 5
  ) %>%
  filter(
    !is.na(study_number),
    !is.na(standardized_species),
    standardized_species != ""
  ) %>%
  mutate(
    study_number = as.character(study_number),
    production_method = str_squish(as.character(production_method)),
    production_method_clean = str_to_lower(production_method),
    standardized_species = str_squish(standardized_species)
  )

# Create audit table of rows excluded because production method was wild-caught or not specified
production_method_excluded <- species_clean %>%
  filter(
    production_method_clean %in% c("wild-caught", "not specified, exclude")
  ) %>%
  select(
    study_number,
    production_method,
    production_method_clean,
    standardized_species
  ) %>%
  arrange(
    standardized_species,
    study_number
  )

# Keep all rows except those marked wild-caught or not specified
species_clean_included <- species_clean %>%
  filter(
    !production_method_clean %in% c("wild-caught", "primarily wild-caught", "not specified, exclude") |
      is.na(production_method_clean)
  ) %>%
  select(
    study_number,
    production_method,
    standardized_species
  )

# Remove duplicate species within each study

study_species_deduped <- species_clean_included %>%
  distinct(study_number, standardized_species)

# ------------------------------------------------------------
# 3. Remove commonly wild-caught-only groups
# ------------------------------------------------------------

wild_caught_only_pattern <- paste(
  c(
    # Squid
    "squid", "loligo", "illex", "todarodes", "dosidicus", "ommastrephes",
    "uroteuthis", "nototodarus", "calamari",
    
    # Octopus/Cuttlefish
    "octopus", "cuttlefish", "\\bsepia\\b", "\\bsepiella\\b", "sepioteuthis", "acanthosepion",
    
    # Tuna/Bonito
    "\\btuna\\b", "thunnus", "skipjack", "katsuwonus", "\\bbonito\\b", "kawakawa", "euthynnus",
    
    # Mackerel
    "mackerel", "scomber", "scomberomorus", "rastrelliger", "trachurus", "horse mackerel",
    
    # Small Pelagic Fish (Sardine/Anchovy/Herring/Smelt)
    "sardine", "anchovy", "herring", "\\bsprat\\b", "\\bsardina\\b", "sardinella",
    "engraulis", "clupea", "hilsa", "\\bshad\\b", "tenualosa", "pilchard",
    "ilisha", "capelin", "clupeonella", "thryssa", "opisthopterus", "\\bsmelt\\b",
    
    # Shark/Ray/Skate
    "\\bshark\\b", "\\bray\\b", "\\bskate\\b", "catshark", "dogfish", "scyliorhinus",
    "squalus", "mustelus", "\\braja\\b", "stingray", "maskray", "whipray", "alopias", "\\bhammerhead\\b",
    
    # Swordfish/Billfish
    "swordfish", "\\bmarlin\\b", "sailfish", "xiphias", "sail fish", "tetrapturus",
    
    # Whitefish excluding cod
    "\\bhake\\b", "merluccius", "pollock", "pollachius", "theragra", "haddock",
    "melanogrammus", "\\bwhiting\\b", "merlangius", "micromesistius", "\\bling\\b",
    "grenadier", "coalfish", "\\bphycis\\b", "mollera", "coryphaenoides", "trisopterus",
    
    # Angler
    "\\bangler\\b","\\banglerfish\\b","\\bmonkfish\\b", "\\bgoosefish\\b",
    
    # Overly ambiguous taxa
    "\\bfish (no species specified)\\b", "\\bshellfish (no species specified)\\b", 
    "\\btempura seafood (no species specified)\\b",
    
    # Other
    "\\bbeltfish\\b", "hairtail", "barracuda", "\\bwhale\\b", 
    "cutlassfish", "frostfish", "scabbardfish", "ribbon fish", "\\bseal\\b", "narwhal",
    "\\brockfish\\b", "parrotfish", "alfonsin", "atlantic stargazer", "\\bbichique\\b", "bombay duck", 
    "\\bescolar\\b", "flyingfish", "goatfish", "sebastes", "lizardfish", "\\bgrunt\\b", 
    "\\bgrunter\\b", "\\bgurnard\\b", "needlefish", "john dory", "\\bluvar\\b", "ocean jacket", 
    "\\boilfish\\b", "orange roughy", "parrotfish", "scorpionfish", "\\bsnoek\\b", "tilefish", 
    "\\bweever\\b", "\\bflathead\\b", "mahi-mahi",
    
    # primarily wild-caught species not captured in commodity groups
    
    "\\bsiniperca kneri\\b", "\\bbigeye\\b", "\\btrumpeters fish\\b",
    "\\bmarlinspike auger\\b", "\\bcommon periwinkle\\b", "\\bcommon barbel\\b", "\\bshaw\\b",
    "\\bbanded leporinus\\b", "\\bbluering angelfish\\b", "\\bbubblefin wrasse\\b", "\\bindian mackerel\\b",
    "\\bpacific yellowtail emperor\\b", "\\bredcoat\\b", "\\bthornfish\\b", "\\bwedgespot damsel\\b",
    "\\bjellyfish\\b", "\\bfalse trevally\\b", "\\bfourfinger threadfin\\b", "\\bmoontail bullseye\\b",
    "\\bsilver silago\\b", "\\bbrown meagre\\b", "\\bpagel\\b", "\\brock goby\\b",
    "\\bsand steenbras\\b", "\\bbogue\\b", "\\btoothed sparus\\b", "\\bwreckfish\\b",
    "\\bfreshwater snail\\b", "\\bbleak\\b", "\\bfrog triton\\b", "\\btuba false fusus\\b",
    "\\brabbitfish\\b", "\\bnamorado sandperch\\b", "\\bindian spiny turbot\\b", "\\bstreaked spinefoot\\b",
    "\\bcommon snook\\b", "\\bbanded dye\\-murex\\b", "\\bchangeable nassa\\b", "\\bgrooved razor shell\\b",
    "\\bleerfish\\b", "\\bmediterranean moray\\b", "\\bpainted comber\\b", "\\brayed trough shell\\b",
    "\\bsalema porgy\\b", "\\bserrano\\b", "\\bsmooth callista\\b", "\\bsurmullet\\b",
    "\\btuberculate cockle\\b", "\\bcoral hind\\b", "\\bmelon seed\\b", "\\bpacific saury\\b",
    "\\bsaury\\b", "\\btopmouth culter\\b", "\\bmoonfish\\b", "\\brandall's threadfin bream\\b",
    "\\bconch\\b", "\\bcaitipa mojarra\\b", "\\bthick lucine\\b", "\\bemperor fish\\b",
    "\\bbluefish\\b", "\\bcrimson jobfish\\b", "\\bmalabar trevally\\b", "\\bminor razor shell\\b",
    "\\bpod razor shell\\b", "\\bblack scraper\\b", "\\bdark smoothhound puffer\\b", "\\bkorean sandlance\\b",
    "\\bjapanese sandfish\\b", "\\bperiwinkle\\b", "\\bponyfish\\b",
    "\\bsilverbiddy\\b", "\\bpatagonian toothfish\\b", "\\bgoose barnacle\\b", "\\bpagre\\b",
    "\\bsilver sides\\b", "\\bspiny top shell\\b", "\\bnile perch\\b", "\\bblackbelly rosefish\\b",
    "\\bflexuous picarel\\b", "\\bgreater forkbeard\\b", "\\bcisco\\b", "\\bdecapodiform cephalopod\\b",
    "\\bseerfish\\b", "\\bkurkufan\\b", "\\bsooly\\b", "\\bpacific ladyfish\\b",
    "\\bpacific fat sleeper\\b", "\\bkingklip\\b", "\\bpike\\b", "\\bhorned turban\\b",
    "\\bbarred barb\\b", "\\bsevenfinger threadfin\\b", "\\blakerda\\b", "\\bshoemaker spinefoot fish\\b",
    "\\basp\\b", "\\bfalse kelpfish\\b", "\\bpacific sand lance\\b",
    "\\bide\\b", "\\bpodust\\b", "\\brudd\\b", "\\bperch (no species specified)\\b", "\\bcomber\\b"
    
  ),
  collapse = "|"
)

# These tags identify species judged to be primarily wild-caught within commodity groups otherwise retained for aquaculture-focused summaries.

species_level_exclusion_pattern <- paste(
  c(
    # Shrimp/Prawn
    "\\batlantic northern shrimp\\b",
    "\\batlantic seabob\\b",
    "\\bbay shrimp\\b",
    "\\bchinese mud shrimp\\b",
    "\\bdeep water shrimp\\b",
    "\\bdeep\\-water rose shrimp\\b",
    "\\bfiddler shrimp\\b",
    "\\bgolden shrimp\\b",
    "\\bgrass prawn\\b",
    "\\bjapanese mantis shrimp\\b",
    "\\bjawla paste shrimp\\b",
    "\\bjinga shrimp\\b",
    "\\bking prawn\\b",
    "\\bmantis shrimp\\b",
    "\\bocean shrimp\\b",
    "\\boriental shrimp\\b",
    "\\bprawn\\b",
    "\\brainbow shrimp\\b",
    "\\bred shrimp\\b",
    "\\bridgeback shrimp\\b",
    "\\bsalad shrimp\\b",
    "\\bspear shrimp\\b",
    "\\btorpedo shrimp\\b",
    "\\bwhiskered velvet shrimp\\b",
    "\\bwhite prawn\\b",
    "\\bwhite shrimp\\b",
    "\\byellow shrimp\\b",
    
    # Crab
    "\\bbrown crab\\b",
    "\\bcommon moon crab\\b",
    "\\beuropean spider crab\\b",
    "\\bjapanese stone crab\\b",
    "\\blight\\-blue soldier crab\\b",
    "\\bmangrove swimming crab\\b",
    "\\borchid crab\\b",
    "\\bpacific stone crab\\b",
    "\\bred king crab\\b",
    "\\bsamoan crab\\b",
    "\\bsand crab\\b",
    "\\bsand swimming crab\\b",
    "\\bsnow crab\\b",
    "\\bspider crab\\b",
    "\\bspotted crab\\b",
    
    # Cod
    "\\bhound needlefish\\b",
    "\\bpacific cod\\b",
    "\\bpoor cod\\b",
    
    # Pomfret
    "\\bchinese pomfret\\b",
    "\\bgolden pomfret\\b",
    "\\bpomfret\\b",
    "\\bsilver pomfret\\b",
    
    # Flatfish
    "\\badriatic sole\\b",
    "\\bbengal tonguesole\\b",
    "\\bbloch's tonguesole\\b",
    "\\bcommon dab\\b",
    "\\bdoublelined tonguesole\\b",
    "\\beuropean plaice\\b",
    "\\bfoureyed sole\\b",
    "\\bgreenland halibut\\b",
    "\\bhalibut\\b",
    "\\bjoyner's tongue\\-sole\\b",
    "\\blargescale tonguesole\\b",
    "\\blemon sole\\b",
    "\\bmegrim\\b",
    "\\btongue sole\\b",
    "\\boriental sole\\b",
    "\\bplaice\\b",
    "\\bred tongue sole\\b",
    "\\bridged\\-eye flounder\\b",
    "\\bshotted halibut\\b",
    "\\bsole\\b",
    "\\bsouthern flounder\\b",
    "\\bthickback sole\\b",
    "\\bwedge sole\\b",
    
    # Croaker/Drum
    "\\bbanded croaker\\b",
    "\\bbelanger's croaker\\b",
    "\\bbig\\-snout croaker\\b",
    "\\bbighead croaker\\b",
    "\\bbronze croaker\\b",
    "\\bcroaker\\b",
    "\\bdistinct croaker\\b",
    "\\blesser tigertooth croaker\\b",
    "\\bpama croaker\\b",
    "\\bpanna croaker\\b",
    "\\breeve's croaker\\b",
    "\\bsin croaker\\b",
    "\\bsinna croaker\\b",
    "\\bsoldier croaker\\b",
    "\\bsouthern kingcroaker\\b",
    "\\btigertooth croaker\\b",
    "\\bwhite mouth croaker\\b",
    "\\byellow croaker\\b",
    
    # Grouper
    "\\bbanded grouper\\b",
    "\\bgrouper\\b",
    "\\bsnowy grouper\\b",
    "\\bspinycheek grouper\\b",
    "\\bstarspotted grouper\\b",
    "\\bwhite grouper\\b",
    
    # Snapper
    "\\bemperor red snapper\\b",
    "\\bhumpback red snapper\\b",
    "\\bred snapper\\b",
    "\\brussell's snapper\\b",
    "\\bsouthern red snapper\\b",
    "\\byellow snapper\\b",
    
    # Jack/Scad
    "\\bbermuda jack\\b",
    "\\bbigeye scad\\b",
    "\\bbigeye trevally\\b",
    "\\bbluefin trevally\\b",
    "\\bindian scad\\b",
    "\\bjapanese scad\\b",
    "\\bocean jacket\\b",
    "\\boxeye scad\\b",
    "\\bredtail scad\\b",
    "\\bshortfin scad\\b",
    "\\bstriped jack\\b",
    "\\btorpedo scad\\b",
    "\\byellowstripe scad\\b",
    "\\byellowtail scad\\b",
    
    # Eel
    "\\bconger eel\\b",
    "\\bconger pike\\b",
    "\\beuropean conger\\b",
    "\\brosy eel goby\\b",
    "\\bsnake\\-eel\\b",
    "\\bpink cusk\\-eel\\b"

  ),
  collapse = "|"
)

# ------------------------------------------------------------
# Excluded taxa outputs
# ------------------------------------------------------------

# 1. Wild-caught-only species excluded after deduplication
wild_caught_species_excluded <- study_species_deduped %>%
  mutate(
    species_lower = str_to_lower(standardized_species),
    exclusion_type = case_when(
      str_detect(species_lower, wild_caught_only_pattern) ~ "Broad wild-caught-only group exclusion",
      str_detect(species_lower, species_level_exclusion_pattern) ~ "Species-level wild-caught exclusion within retained commodity group",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(exclusion_type)
  ) %>%
  select(
    study_number,
    standardized_species,
    exclusion_type
  )

# 2. Combine all excluded study-species records
excluded_taxa_all_records <- bind_rows(
  
  # Excluded based on production method
  production_method_excluded %>%
    mutate(
      exclusion_type = case_when(
        production_method_clean == "wild-caught" ~ "Production method: wild-caught",
        production_method_clean == "not specified, exclude" ~ "Production method: not specified, exclude",
        TRUE ~ "Production method exclusion"
      )
    ) %>%
    select(
      study_number,
      standardized_species,
      exclusion_type
    ),
  
  # Excluded based on broad or species-level wild-caught filters
  wild_caught_species_excluded
) %>%
  distinct(
    study_number,
    standardized_species,
    exclusion_type
  )

# 3. Frequency summary of excluded taxa
excluded_taxa_frequency <- excluded_taxa_all_records %>%
  count(
    standardized_species,
    exclusion_type,
    name = "number_of_studies"
  ) %>%
  arrange(
    desc(number_of_studies),
    standardized_species,
    exclusion_type
  )

# Remove both broad wild-caught-only groups and species-level wild-caught exclusions
study_species_filtered <- study_species_deduped %>%
  mutate(
    species_lower = str_to_lower(standardized_species)
  ) %>%
  filter(
    !str_detect(species_lower, wild_caught_only_pattern),
    !str_detect(species_lower, species_level_exclusion_pattern)
  ) %>%
  select(-species_lower)

# ------------------------------------------------------------
# 4. Assign commodity groups to remaining species
# ------------------------------------------------------------

species_by_commodity <- study_species_filtered %>%
  mutate(
    species_lower = str_to_lower(standardized_species),
    
    commodity_group = case_when(
      
      # Shellfish and invertebrates
      str_detect(species_lower, "shrimp|prawn|penaeus|litopenaeus|fenneropenaeus|metapenaeus|macrobrachium") ~ "Shrimp/Prawn",
      str_detect(species_lower, "\\bmussel\\b|mytilus|\\bperna\\b|modiolus") ~ "Mussel",
      str_detect(species_lower, "\\bclam\\b|cockle|ruditapes|solenidae|venerupis|mercenaria|\\bdonax\\b|anadara|meretrix|cerastoderma|ark shell|\\bvenus\\b|tegillarca|cyclina|\\bmactra\\b|\\bensis\\b|\\bsolen\\b|\\blucina\\b|solenidae|callista|panopea|polymesoda|\\bpinna\\b|scapharca") ~ "Clam/Cockle",
      str_detect(species_lower, "oyster|crassostrea|magallana|ostrea|pinctada") ~ "Oyster",
      str_detect(species_lower, "scallop|\\bpecten\\b|argopecten|patinopecten|chlamys") ~ "Scallop",
      str_detect(species_lower, "\\bcrab\\b|portunus|callinectes|scylla|chionoecetes|erimacrus|eocheir") ~ "Crab",
      str_detect(species_lower, "lobster|crayfish|homarus|panulirus|nephrops|procambarus|cherax|astacus|crawfish") ~ "Lobster/Crayfish",
      str_detect(species_lower, "whelk|abalone|snail|\\bconch\\b|gastropod|haliotis|\\brapana\\b|buccinum|littorina|turbo|nerita|babylonia|\\bmurex\\b|babylon shell|nassarius|\\bearshell\\b|bufonaria|terebra|bolinus|periwinkle|spotted babylon|hemifusus|tympanotonos") ~ "Whelk/Abalone/Other Gastropods",
      str_detect(species_lower, "sea cucumber|sea urchin|holothuria|apostichopus|strongylocentrotus|paracentrotus|echinoderm") ~ "Sea cucumber/Urchin",
      str_detect(species_lower, "seaweed|\\bkelp\\b|algae|\\balga\\b|porphyra|saccharina|laminaria|undaria|\\blettuce\\b|gracilaria") ~ "Seaweed/Algae",
      
      # Finfish and other commodity groups
      str_detect(species_lower, "\\bsalmon\\b|\\bsalmo\\b|oncorhynchus") ~ "Salmon",
      str_detect(species_lower, "\\btrout\\b") ~ "Trout",
      str_detect(species_lower, "\\bcod\\b|\\bgadus\\b") ~ "Cod",
      str_detect(species_lower, "pomfret|\\bpampus\\b|\\bbrama\\b|parastromateus|trachinotus|butterfish|psenopsis") ~ "Pomfret",
      str_detect(species_lower, "\\bcarp\\b|cyprinus|hypophthalmichthys|ctenopharyngodon|crucian|carassius|\\blabeo\\b|\\brohu\\b|\\bcatla\\b|mrigal|leuciscus|spinibarbus|acrossocheilus|alburnus|barbus|gobio|leptobarbus|chondrostoma|rutilus|scardinius") ~ "Cyprinids (Carp/Minnows)",
      str_detect(species_lower, "tilapia|oreochromis|sarotherodon") ~ "Tilapia",
      str_detect(species_lower, "catfish|clarias|pangasius|pangasianodon|ictalurus|silurus|synodontis|pseudobagrus|bagrus|keli") ~ "Catfish",
      str_detect(species_lower, "\\bsole\\b|flounder|halibut|turbot|plaice|flatfish|\\bsolea\\b|paralichthys|hippoglossus|\\bpsetta\\b|scophthalmus|limanda|cynoglossus|pleuronectes|\\bmegrim\\b|paraplagusia|tonguefish") ~ "Flatfish",
      str_detect(species_lower, "seabass|sea bass|\\bbream\\b|seabream|sea bream|dicentrarchus|\\bsparus\\b|\\bpagrus\\b|pagellus|acanthopagrus|\\blates\\b|barramundi|\\bporgy\\b|steenbras|cephalopholis|argyrops|rhabdosargus") ~ "Seabass/Sea Bream",
      str_detect(species_lower, "\\bmullet\\b|\\bmugil\\b|\\bmullus\\b|\\bsurmullet\\b") ~ "Mullet",
      str_detect(species_lower, "croaker|\\bdrum\\b|corvina|larimichthys|pseudosciaena|otolithes|sciaenops|argyrosomus|nibea|isopisthus|sciaena|scianids|macrodon") ~ "Croaker/Drum",
      str_detect(species_lower, "\\bgrouper\\b|epinephelus|mycteroperca") ~ "Grouper",
      str_detect(species_lower, "\\bsnapper\\b|lutjanus") ~ "Snapper",
      str_detect(species_lower, "\\bjack\\b|amberjack|\\bscad\\b|seriola|megalaspis|caranx|\\blichia\\b|\\btrevally\\b") ~ "Jack/Scad",
      str_detect(species_lower, "\\bperch\\b|bluegill|lepomis|siniperca|mandarin fish|walleye|\\bsander\\b|\\bzingel\\b") ~ "Perch/Sunfish",
      str_detect(species_lower, "\\beel\\b|\\banguilla\\b|\\bconger\\b|muraena|\\bmuraenesox\\b|kingklip") ~ "Eel",
      str_detect(species_lower, "snakehead|channa") ~ "Snakehead",
      str_detect(species_lower, "flathead|platycephalus") ~ "Flathead",
      
      TRUE ~ "Other or Not Otherwise Grouped Aquatic Foods"
    )
  ) %>%
  select(-species_lower)

# ------------------------------------------------------------
# 5. Clean Full Data sheet
# ------------------------------------------------------------

full_data_clean <- full_data %>%
  rename(
    study_number = 1,
    contaminant_cell = 2,
    bacteria_cell = 3,
    human_health = 5,
    temperature = 6,
    weather = 7,
    salinity = 8,
    acidification = 9,
    water_pollution = 10,
    region = 14
  ) %>%
  mutate(
    study_number = as.character(study_number),
    contaminant_cell = str_to_lower(str_squish(as.character(contaminant_cell))),
    bacteria_cell = str_to_lower(str_squish(as.character(bacteria_cell))),
    region = str_squish(as.character(region)),
    across(
      c(human_health, temperature, weather, salinity, acidification, water_pollution),
      ~ str_to_upper(str_squish(as.character(.x)))
    )
  )

# ------------------------------------------------------------
# 6. Outputs: Study-level analyses, summaries, and interactions
# ------------------------------------------------------------

# -----------------------------
# Study-level analysis
# -----------------------------

# 1. Study-level commodity groups
# Used for interaction summaries; one row per study x commodity group
commodity_study <- species_by_commodity %>%
  distinct(study_number, commodity_group)

# Used only for the exported CSV; keeps study number and species rows visible
commodity_study_export <- species_by_commodity %>%
  select(
    study_number,
    standardized_species,
    commodity_group
  ) %>%
  arrange(
    study_number,
    standardized_species,
    commodity_group
  )

# 2. Study-level geography
geography_study <- full_data_clean %>%
  filter(!is.na(region), region != "", region != "NA") %>%
  separate_rows(region, sep = ";|,|\\|") %>%
  mutate(region = str_squish(region)) %>%
  filter(region != "") %>%
  distinct(study_number, region)

# 3. Study-level climate hazards
hazard_study <- full_data_clean %>%
  select(
    study_number,
    human_health,
    temperature,
    weather,
    salinity,
    acidification,
    water_pollution
  ) %>%
  pivot_longer(
    cols = c(human_health, temperature, weather, salinity, acidification, water_pollution),
    names_to = "hazard",
    values_to = "present"
  ) %>%
  filter(present == "Y") %>%
  mutate(
    hazard = case_when(
      hazard == "human_health" ~ "Evidence Linking to Human Health/Disease",
      hazard == "temperature" ~ "Temperature",
      hazard == "weather" ~ "Weather",
      hazard == "salinity" ~ "Salinity",
      hazard == "acidification" ~ "Acidification",
      hazard == "water_pollution" ~ "Water Pollution",
      TRUE ~ hazard
    )
  ) %>%
  distinct(study_number, hazard)

# 4. Study-level contaminants
contaminant_patterns <- tibble::tibble(
  contaminant = c(
    "Antimicrobials",
    "Pathogens",
    "Biotoxin",
    "Heavy Metal",
    "Nanoparticle",
    "Microplastics",
    "Organic Contaminant"
  ),
  pattern = c(
    "antimicrobial|antimicrobials",
    "bacteria|virus|parasite|other pathogen|pathogen",
    "biotoxin|biotoxins",
    "heavy metal|heavy metals",
    "nanoparticle|nanoparticles",
    "microplastic|microplastics",
    "organic contaminant|organic contaminants"
  )
)

contaminant_long <- contaminant_patterns %>%
  rowwise() %>%
  do({
    contaminant_name <- .$contaminant
    contaminant_pattern <- .$pattern
    
    full_data_clean %>%
      filter(str_detect(contaminant_cell, contaminant_pattern)) %>%
      mutate(contaminant = contaminant_name)
  }) %>%
  ungroup()

contaminant_study <- contaminant_long %>%
  distinct(study_number, contaminant)

# 5. Study-level bacterial genera
bacteria_patterns <- tibble::tibble(
  bacteria_genus = c(
    "Vibrio", "Escherichia", "Salmonella", "Staphylococcus", "Listeria",
    "Aeromonas", "Pseudomonas", "Bacillus", "Klebsiella", "Clostridium",
    "Streptococcus", "Arcobacter", "Enterobacter", "Enterococcus",
    "Campylobacter", "Citrobacter", "Mycobacterium", "Shigella",
    "Brevundimonas", "Burkholderia", "Carnobacterium", "Cetobacterium",
    "Cronobacter", "Exiguobacterium", "Fusobacterium", "Lactococcus",
    "Lysinibacillus", "Morganella", "Proteus", "Providencia",
    "Psychrobacter", "Pseudoalteromonas", "Stutzerimonas", "Weissella"
  ),
  pattern = c(
    "vibrio", "escherichia|e\\. coli", "salmonella", "staphylococcus", "listeria",
    "aeromonas", "pseudomonas", "bacillus", "klebsiella", "clostridium",
    "streptococcus", "arcobacter", "enterobacter", "enterococcus",
    "campylobacter", "citrobacter", "mycobacterium", "shigella",
    "brevundimonas", "burkholderia", "carnobacterium", "cetobacterium",
    "cronobacter", "exiguobacterium", "fusobacterium", "lactococcus",
    "lysinibacillus", "morganella", "proteus", "providencia",
    "psychrobacter|pschyrobacter", "pseudoalteromonas", "stutzerimonas", "weissella"
  )
)

bacteria_study <- bacteria_patterns %>%
  rowwise() %>%
  do({
    genus_name <- .$bacteria_genus
    genus_pattern <- .$pattern
    
    full_data_clean %>%
      filter(str_detect(bacteria_cell, genus_pattern)) %>%
      mutate(bacteria_genus = genus_name)
  }) %>%
  ungroup() %>%
  distinct(study_number, bacteria_genus)

# -----------------------------
# Summaries
# -----------------------------

# 6. Overall species summary
overall_species_summary <- data.frame(
  metric = c(
    "Total study-species occurrences after duplicate removal and aquaculture-focused exclusions",
    "Number of unique standardized species after aquaculture-focused exclusions"
  ),
  value = c(
    nrow(study_species_filtered),
    n_distinct(study_species_filtered$standardized_species)
  )
)

# 7. Species frequency
species_frequency <- study_species_filtered %>%
  count(standardized_species, name = "number_of_studies") %>%
  arrange(desc(number_of_studies), standardized_species)

# 8. and 9. Exclusion summary table
exclusion_summary <- tibble::tibble(
  exclusion_category = c(
    "Production method: wild-caught",
    "Production method: not specified, exclude",
    "Broad wild-caught-only group",
    "Species-level wild-caught exclusion within retained commodity group"
  ),
  description = c(
    "Rows where the study-specific production method was listed as wild-caught.",
    "Rows where production method was not specified for taxa requiring study-level production review.",
    "Rows matching commodity/species groups considered primarily wild-caught and not aquaculture-relevant.",
    "Rows matching specific taxa judged to be primarily wild-caught within otherwise retained commodity groups."
  ),
  number_of_study_species_rows_excluded = c(
    sum(production_method_excluded$production_method_clean %in% c("wild-caught"), na.rm = TRUE),
    sum(production_method_excluded$production_method_clean == "not specified, exclude", na.rm = TRUE),
    sum(wild_caught_species_excluded$exclusion_type == "Broad wild-caught-only group exclusion", na.rm = TRUE),
    sum(wild_caught_species_excluded$exclusion_type == "Species-level wild-caught exclusion within retained commodity group", na.rm = TRUE)
  )
)

# 10. Commodity group summary
commodity_group_summary <- species_by_commodity %>%
  group_by(commodity_group) %>%
  summarise(
    total_occurrences = n(),
    unique_standardized_species = n_distinct(standardized_species),
    .groups = "drop"
  ) %>%
  arrange(
    commodity_group == "Other or Not Otherwise Grouped Aquatic Foods",
    desc(total_occurrences),
    commodity_group
  )

# 11. Geographic summary
geographic_summary <- geography_study %>%
  count(region, name = "number_of_studies") %>%
  arrange(desc(number_of_studies), region)

# 12. Climate hazard summary
climate_hazard_summary <- hazard_study %>%
  count(hazard, name = "number_of_studies") %>%
  arrange(desc(number_of_studies), hazard)

# 13. Contaminant summary
contaminant_summary <- contaminant_study %>%
  count(contaminant, name = "number_of_studies") %>%
  arrange(desc(number_of_studies), contaminant)

# 14. Bacterial genera summary
bacterial_summary <- bacteria_study %>%
  count(bacteria_genus, name = "number_of_studies") %>%
  arrange(desc(number_of_studies), bacteria_genus)

# -----------------------------
# Interactions
# -----------------------------

# 15. Contaminant x Climate Hazard
contaminant_hazard_interaction_summary <- contaminant_long %>%
  group_by(contaminant) %>%
  summarise(
    number_of_studies = n_distinct(study_number),
    human_health_y = n_distinct(study_number[human_health == "Y"]),
    temperature_y = n_distinct(study_number[temperature == "Y"]),
    weather_y = n_distinct(study_number[weather == "Y"]),
    salinity_y = n_distinct(study_number[salinity == "Y"]),
    acidification_y = n_distinct(study_number[acidification == "Y"]),
    water_pollution_y = n_distinct(study_number[water_pollution == "Y"]),
    .groups = "drop"
  ) %>%
  arrange(desc(number_of_studies), contaminant)

# 16. Commodity group x contaminant interaction summary
commodity_contaminant_summary <- commodity_study %>%
  inner_join(contaminant_study, by = "study_number") %>%
  distinct(study_number, commodity_group, contaminant) %>%
  count(commodity_group, contaminant, name = "number_of_studies") %>%
  arrange(desc(number_of_studies), commodity_group, contaminant)

# 17. Commodity group x geography summary
commodity_geography_summary <- commodity_study %>%
  inner_join(geography_study, by = "study_number") %>%
  distinct(study_number, commodity_group, region) %>%
  count(commodity_group, region, name = "number_of_studies") %>%
  arrange(desc(number_of_studies), commodity_group, region)

# 18. Contaminant x geography summary
contaminant_geography_summary <- contaminant_study %>%
  inner_join(geography_study, by = "study_number") %>%
  distinct(study_number, contaminant, region) %>%
  count(contaminant, region, name = "number_of_studies") %>%
  arrange(desc(number_of_studies), contaminant, region)

# ------------------------------------------------------------
# 7. Export CSV files
# ------------------------------------------------------------

# Study-level analysis

# 1. Study-level commodity groups
write.csv(commodity_study_export, file.path(output_folder, "01_study_level_commodity_groups.csv"), row.names = FALSE)

# 2. Study-level geography
write.csv(geography_study, file.path(output_folder, "02_study_level_geography.csv"), row.names = FALSE)

# 3. Study-level climate hazards
write.csv(hazard_study, file.path(output_folder, "03_study_level_climate_hazards.csv"), row.names = FALSE)

# 4. Study-level contaminants
write.csv(contaminant_study, file.path(output_folder, "04_study_level_contaminants.csv"), row.names = FALSE)

# 5. Study-level bacterial genera
write.csv(bacteria_study, file.path(output_folder, "05_study_level_bacterial_genera.csv"), row.names = FALSE)

# Summaries

# 6. Overall species summary
write.csv(overall_species_summary, file.path(output_folder, "06_overall_species_summary.csv"), row.names = FALSE)

# 7. Species frequency
write.csv(species_frequency, file.path(output_folder, "07_species_frequency.csv"), row.names = FALSE)

# 8. Exclusion summary table
write.csv(exclusion_summary, file.path(output_folder, "08_exclusion_summary.csv"), row.names = FALSE)

# 9. Exclusion taxa frequency
write.csv(excluded_taxa_frequency, file.path(output_folder, "09_excluded_taxa_frequency.csv"), row.names = FALSE)

# 10. Commodity group summary
write.csv(commodity_group_summary, file.path(output_folder, "10_commodity_group_summary.csv"), row.names = FALSE)

# 11. Geographic summary
write.csv(geographic_summary, file.path(output_folder, "11_geographic_summary.csv"), row.names = FALSE)

# 12. Climate hazard summary
write.csv(climate_hazard_summary, file.path(output_folder, "12_climate_hazard_summary.csv"), row.names = FALSE)

# 13. Contaminant summary
write.csv(contaminant_summary, file.path(output_folder, "13_contaminant_summary.csv"), row.names = FALSE)

# 14. Bacterial genera summary
write.csv(bacterial_summary, file.path(output_folder, "14_bacterial_genera_summary.csv"), row.names = FALSE)

# Interactions

# 15. Contaminant x climate hazard summary
write.csv(contaminant_hazard_interaction_summary, file.path(output_folder, "15_contaminant_climate_hazard_summary.csv"), row.names = FALSE)

# 16. Commodity group x contaminant summary
write.csv(commodity_contaminant_summary, file.path(output_folder, "16_commodity_group_contaminant_summary.csv"), row.names = FALSE)

# 17. Commodity group x geography summary
write.csv(commodity_geography_summary, file.path(output_folder, "17_commodity_group_geography_summary.csv"), row.names = FALSE)

# 18. Contaminant x geography summary
write.csv(contaminant_geography_summary, file.path(output_folder, "18_contaminant_geography_summary.csv"), row.names = FALSE)

cat("\nFiles saved here:\n")
print(output_folder)