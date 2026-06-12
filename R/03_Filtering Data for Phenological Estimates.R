# Filtering pollinator observation data before it can be used to estimate phenology.  


############################################################################################################

# Load Packages 
library(dplyr)
library(sf)
library(lubridate)
library(purrr)


# Read in data 
pollinators_grids <- readRDS("Data/pollinators_joined_with_grids_5.rds") #joined grid pollinators data
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson")


############################################################################################################


# Drop the geometry 
pollinators_grids1 <- pollinators_grids %>%
  st_drop_geometry()

# Checking for species rows with no species names 
sum(is.na(pollinators_grids1$species))

# Found quite a few, looking at them more closely: 
pollinators_grids1 %>% 
  filter(is.na(species))

# These observations have genus but not species. I will remove them. 
pollinators_grids_clean <- pollinators_grids1 %>%
  filter(!is.na(species))

# Making sure it worked
sum(is.na(pollinators_grids_clean$species))

# Take out any species that are not pollinators (e.g., moths that don't have mouth parts or simply don't 
# eat as adults) or that we cannot determine to be pollinators due to a lack of information on their adult
# feeding habits in current literature. See manuscript for full details on how this was decided.

unique(pollinators_grids_clean$species) #get list of species: 7,045 species before filtering  
filtered_5 <- pollinators_grids_clean %>%
  filter(species != "Actias luna",
         species != "Eacles imperialis",
         species != "Hyphantria cunea", 
         species != "Hypercompe scribonia",
         species != "Malacosoma americana",
         species != "Malacosoma disstria", 
         species != "Automeris io",
         species != "Promachus rufipes",
         species != "Chrysopilus thoracicus",
         species != "Delphinia picta",
         species != "Harmonia axyridis",
         species != "Polygonia interrogationis",
         species != "Solenopsis invicta", 
         species != "Asterocampa celtis", 
         species != "Cotinis nitida", 
         species != "Acrolophus popeanella", 
         species != "Apatelodes torrefacta", 
         species != "Dyspteris abortivaria", 
         species != "Eubaphe mendica", 
         species != "Eudryas grata", 
         species != "Antheraea polyphemus", 
         species != "Pelidnota punctata", 
         species != "Peridea angulosa", 
         species != "Callosamia angulifera", 
         species != "Paonias myops", 
         species != "Paraeschra georgica", 
         species != "Pachycondyla chinensis", 
         species != "Polygonia comma", 
         species != "Lethe anthedon", 
         species != "Camponotus pennsylvanicus", 
         species != "Anavitrinella pampinaria", 
         species != "Cicindela sexguttata", 
         species != "Hermeuptychia hermes", 
         species != "Popillia japonica", 
         species != "Vespa crabro", 
         species != "Paonias excaecata", 
         species != "Megalopyge crispata", 
         species != "Eusarca confusaria", 
         species != "Dryocampa rubicunda", 
         species != "Ceratomia undulosa", 
         species != "Ceratomia catalpae", 
         species != "Catocala ilia", 
         species != "Nadata gibbosa", 
         species != "Prenolepis imparis", 
         species != "Camponotus castaneus", 
         species != "Spilosoma congrua", 
         species != "Artace cribrarius", 
         species != "Odontotaenius disjunctus", 
         species != "Exomala orientalis", 
         species != "Monocesta coryli", 
         species != "Photinus pyralis", 
         species != "Parapediasia teterellus", 
         species != "Hypena madefactalis", 
         species != "Lucanus capreolus", 
         species != "Clemensia albata", 
         species != "Diabrotica undecimpunctata", 
         species != "Schizura ipomaeae", 
         species != "Amphipyra pyramidoides", 
         species != "Apantesis phalerata", 
         species != "Lophocampa caryae", 
         species != "Euchaetes egle", 
         species != "Chrysochus auratus", 
         species != "Tapinoma sessile", 
         species != "Labidomera clivicollis", 
         species != "Camponotus chromaiodes", 
         species != "Lymantria dispar", 
         species != "Gluphisia septentrionis", 
         species != "Arta statalis", 
         species != "Protodeltote muscosula", 
         species != "Halysidota tessellaris", 
         species != "Hypagyrtis unipunctata",
         species != "Allotria elonympha",
         species != "Phigalia strigataria", 
         species != "Idia americalis", 
         species != "Phosphila miselioides", 
         species != "Polygrammate hebraeicum", 
         species != "Spodoptera ornithogalli", 
         species != "Polistes exclamans", 
         species != "Polistes dominula", #all of the species up until this point were not pollinators, 
         #the rest are taken out due to lack of information about their diets as we can't properly determine
         #whether they are pollinators 
         species != "Limenitis astyanax", 
         species != "Argyria nivalis", 
         species != "Athetis tarda", 
         species != "Besma quercivoraria", 
         species != "Cecrita guttivitta", 
         species != "Epimecis hortaria", 
         species != "Euchlaena amoenaria", 
         species != "Hypsopygia olinalis", 
         species != "Idaea demissaria", 
         species != "Idia aemula", 
         species != "Lascoria ambigualis", 
         species != "Lithacodes fasciola", 
         species != "Nomophila nearctica", 
         species != "Orgyia leucostigma", 
         species != "Palthis asopialis", 
         species != "Promalactis suzukiella", 
         species != "Protoboarmia porcelaria", 
         species != "Apoda biguttata", 
         species != "Callima argenticinctella", 
         species != "Isa textula", 
         species != "Macrurocampa marthesia", 
         species != "Nematocampa resistaria", 
         species != "Palthis angulalis", 
         species != "Pangrapta decoralis", 
         species != "Prolimacodes badia", 
         species != "Pyrausta acrionalis", 
         species != "Haploa clymene", 
         species != "Leuconycta diphteroides", 
         species != "Lithacodia musta", 
         species != "Ogdoconta cinereola", 
         species != "Phaeolita pyramusalis", 
         species != "Tetanolita mynesalis", 
         species != "Tosale oviplagalis", 
         species != "Zanclognatha cruralis", 
         species != "Macaria aemulataria", 
         species != "Platynota idaeusalis", 
         species != "Tetanolita floridana", 
         species != "Acrolophus panamae", 
         species != "Spilosoma virginica", 
         species != "Euptychia cymela", 
         species != "Condylolomia participialis", 
         species != "Feltia herilis", 
         species != "Lacinipolia renigera", 
         species != "Parasa chloris", 
         species != "Synchlora aerata", 
         species != "Campaea perlata", 
         species != "Autographa precationis", 
         species != "Biston betularia", 
         species != "Coelostathma discopunctana", 
         species != "Digrammia ocellinata", 
         species != "Eucopina tocullionana", 
         species != "Euphyia intermediata", 
         species != "Hydrelia inornata", 
         species != "Lacinipolia explicata", 
         species != "Machimia tentoriferella", 
         species != "Macrochilo morbidalis", 
         species != "Maliattha synochitis", 
         species != "Nephelodes minians", 
         species != "Ochropleura implecta", 
         species != "Pseudeustrotia carneola", 
         species != "Cycnia tenera", 
         species != "Haematopis grataria", 
         species != "Rivula propinqualis", 
         species != "Xanthorhoe ferrugata", 
         species != "Paleacrita vernata", 
         species != "Caenurgia chloropha", 
         species != "Patalene olyzonaria", 
         species != "Hypena baltimoralis", 
         species != "Lomographa glomeraria", 
         species != "Lomographa vestaliata", 
         species != "Orthonama obstipata", 
         species != "Pyrausta merrickalis",
         species != "Choristoneura rosaceana", 
         species != "Metalectra richardsi", 
         species != "Palpita magniferalis", 
         species != "Chionodes mediofuscella", 
         species != "Achatia distincta", 
         species != "Acronicta americana", 
         species != "Antepione thisoaria", 
         species != "Apoda y-inversa", 
         species != "Blepharomastix ranalis", 
         species != "Cerma cerintha", 
         species != "Condica vecors", 
         species != "Drepana arcuata", 
         species != "Macaria pustularia", 
         species != "Metalectra discalis", 
         species != "Nemoria bistriaria", 
         species != "Pantographa limata", 
         species != "Parallelia bistriaris", 
         species != "Phaeoura quernaria", 
         species != "Pseudothyatira cymatophoroides", 
         species != "Zale minerea", 
         species != "Agrochola bicolorago", 
         species != "Chlorochlamys chloroleucaria", 
         species != "Clepsis peritana", 
         species != "Ectropis crepuscularia", 
         species != "Elaphria versicolor", 
         species != "Eutrapela clemataria", 
         species != "Galgula partita", 
         species != "Glenoides texanaria", 
         species != "Horisme intestinata", 
         species != "Hypena scabra", 
         species != "Ilexia intractata", 
         species != "Iridopsis defectaria", 
         species != "Iridopsis larvaria", 
         species != "Melanolophia canadaria", 
         species != "Microcrambus elegans", 
         species != "Phoberia atomaris", 
         species != "Zale lunata", 
         species != "Acrolophus plumifrontella", 
         species != "Argyrotaenia quercifoliana", 
         species != "Argyrotaenia velutinana", 
         species != "Bleptina caradrinalis", 
         species != "Crambus agitatellus", 
         species != "Elophila obliteralis", 
         species != "Hypsoropha hormos", 
         species != "Sphecius speciosus", 
         species != "Epipaschia superatalis", 
         species != "Euclea delphinii", 
         species != "Phalaenostola larentioides"
  )


### Filter: only include species with at least ten observations per grid  

# Identify grid-species combos with at least 10 observations
valid_combos <- filtered_5 %>%
  group_by(grid_id, species) %>%
  filter(n() >= 10) %>%
  distinct(grid_id, species)

# Filter pollinators_grids_clean to only include those combos
filtered_5 <- pollinators_grids_clean %>%
  inner_join(valid_combos, by = c("grid_id", "species"))

# Quick fact check
observation_count_check <- filtered_5 %>%
  group_by(grid_id, species) %>%
  summarize(observation_count = n(), .groups = 'drop')

if (all(observation_count_check$observation_count >= 10)) {
  cat("✅ All species have at least 10 observations per grid.\n")
} else {
  cat("⚠️ Some species do not have at least 10 observations.\n")
}

# Let's also make sure the date column is formatted correctly for analysis 
filtered_5$Date <- as.Date(filtered_5$eventDate)

unique(filtered_5$species) #double check new species list: 1,060 species after filtering 

# Look at the taxonomic groups present 
unique(filtered_5$genus)
unique(filtered_5$family)
unique(filtered_5$order)

# How many grids originally versus after filtering:

#Check and summarize. Currently have 126,619 observations 
length(unique(five_km_grids$grid_id))  #24,128 grids spanning eco-region NA24 (no data attached)
length(unique(pollinators_grids$grid_id)) #19,217 grids that have observations in them before filtering
length(unique(filtered_5$grid_id)) #826 grids with observations after filtering


# Export the rds file 
saveRDS(filtered_5, "Data/filtered_5.rds")
