clean_data <- function(dataset) {
  dataset$Species <- ifelse(dataset$Score == '*', paste0(dataset$Genus, "_unclassified"), dataset$Species)

  sort(dataset$Species)

  combine_duplicate_species <- function(x) {
    if (is.numeric(x))
      sum(x)
    else if (any(x[-1] != x[1]))
      paste(x, collapse = "+")
    else x[1]
  }

  de_duped_data <- dataset %>%
    group_by(Phylum, Class, Order, Family, Genus, Species) %>%
    summarise(across(everything(), combine_duplicate_species)) %>%
    relocate(Species, .after = Genus)

  return(de_duped_data)
}

get_common_species <- function(dataset) {
  common_species <- dataset %>%
    rowwise() %>%
    filter(sum(c_across(starts_with('S') & !c(Score, Species))) >= 10)

  return(common_species)
}

get_rare_species <- function(dataset) {
  rare_species <- dataset %>%
    rowwise() %>%
    filter(sum(c_across(starts_with('S') & !c(Score, Species))) < 10)

  return(rare_species)
}

get_top_abundences <- function(dataset) {
  data_with_total_abundance <- dataset %>%
    rowwise() %>%
    mutate(Abundance = sum(c_across(starts_with('S') & !c(Score, Species))))

  top_twenty <- data_with_total_abundance[, c("Genus", "Species", "Abundance")] %>%
    arrange(desc(Abundance)) %>%
    ungroup() %>%
    top_n(20)

  top_twenty$GenusSpecies <- paste(top_twenty$Genus, top_twenty$Species)

  return(top_twenty) # Why is this giving 29 results on rare species?
}

get_top_occurences <- function(dataset) {
  data_with_total_occurence <- dataset %>%
    rowwise() %>%
    mutate(Occurence = sum(across(starts_with('S') & !c(Score, Species)) > 0))

  top_twenty <- data_with_total_occurence[, c("Genus", "Species", "Occurence")] %>%
    arrange(desc(Occurence)) %>%
    ungroup() %>%
    top_n(20)

  top_twenty$GenusSpecies <- paste(top_twenty$Genus, top_twenty$Species)

  return(top_twenty) # Why is this giving 29 results on rare species?
}

get_by_phylum <- function(dataset, phylum) {
  return(dataset[dataset$Phylum == phylum,])
}

get_by_class <- function(dataset, class) {
  return(dataset[dataset$Class == class,])
}

get_by_order <- function(dataset, order) {
  return(dataset[dataset$Order == order,])
}

get_by_family <- function(dataset, family) {
  return(dataset[dataset$Family == family,])
}

get_by_genus <- function(dataset, genus) {
  return(dataset[dataset$Genus == genus,])
}

get_by_species <- function(dataset, species) {
  return(dataset[dataset$Species == species,])
}

get_by_sample_id <- function(dataset, sample_id) {
  data <- dataset[, c('Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species', sample_id)]
  colnames(data) <- sub("S\\d+", "Value", colnames(data))
  data <- data %>%
    rowwise() %>%
    filter(Value > 0) %>%
    arrange(desc(Value))

  return(data)
}

get_top_in_individual_sample <- function(dataset) {
  data <- dataset[, c("Genus", "Species", "Value")] %>%
    arrange(desc(Value)) %>%
    ungroup() %>%
    top_n(20)
  data$GenusSpecies <- paste(data$Genus, data$Species)

  return(data)
}