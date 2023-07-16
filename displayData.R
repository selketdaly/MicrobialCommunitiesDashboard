library(dplyr)

display_top_abundances <- function(data) {
  ggplot(data, aes(reorder(GenusSpecies, Abundance), fill = ifelse(length(data), yes = "yes", no = "no"))) +
    geom_bar(aes(weight = Abundance)) +
    scale_fill_manual(values = c("yes" = "blue", "no" = "grey"), guide = FALSE) +
    coord_flip() +
    ggtitle("Species abundance") +
    xlab("Species") +
    ylab("Total count") +
    theme_bw(base_size = 16)
}

display_top_occurences <- function(data) {
  ggplot(data, aes(reorder(GenusSpecies, Occurence), fill = ifelse(length(data), yes = "yes", no = "no"))) +
    geom_bar(aes(weight = Occurence)) +
    scale_fill_manual(values = c("yes" = "blue", "no" = "grey"), guide = FALSE) +
    coord_flip() +
    ggtitle("Species occurence") +
    xlab("Species") +
    ylab("Total occurences") +
    theme_bw(base_size = 16)
}

display_sample_results <- function(data) {
  ggplot(data, aes(reorder(GenusSpecies, Value), fill = ifelse(length(data), yes = "yes", no = "no"))) +
    geom_bar(aes(weight = Value)) +
    scale_fill_manual(values = c("yes" = "blue", "no" = "grey"), guide = FALSE) +
    coord_flip() +
    ggtitle("Species occurence") +
    xlab("Species") +
    ylab("Total occurences") +
    theme_bw(base_size = 16)
}

update_phylum_filter <- function(session, filters, phylumList) {
  selected <- NULL
  if (!is.null(filters) & !is.null(filters$phylum)) {
    selected <- filters$phylum
  }
  updateCheckboxGroupInput(session, "phylum", choices = sort(unique(phylumList)), selected = selected)
}

update_class_filter <- function(session, filters, classList) {
  selected <- NULL
  if (!is.null(filters) & !is.null(filters$class)) {
    selected <- filters$class
  }
  updateCheckboxGroupInput(session, "class", choices = sort(unique(classList)), selected = selected)
}

update_order_filter <- function(session, filters, orderList) {
  selected <- NULL
  if (!is.null(filters) & !is.null(filters$order)) {
    selected <- filters$order
  }
  updateCheckboxGroupInput(session, "order", choices = sort(unique(orderList)), selected = selected)
}

update_family_filter <- function(session, filters, familyList) {
  selected <- NULL
  if (!is.null(filters) & !is.null(filters$family)) {
    selected <- filters$family
  }
  updateCheckboxGroupInput(session, "family", choices = sort(unique(familyList)), selected = selected)
}

update_genus_filter <- function(session, filters, genusList) {
  selected <- NULL
  if (!is.null(filters) & !is.null(filters$genus)) {
    selected <- filters$genus
  }
  updateCheckboxGroupInput(session, "genus", choices = sort(unique(genusList)), selected = selected)
}

update_species_filter <- function(session, filters, speciesList) {
  selected <- NULL
  if (!is.null(filters) & !is.null(filters$species)) {
    selected <- filters$species
  }
  updateCheckboxGroupInput(session, "species", choices = sort(unique(speciesList)), selected = selected)
}

update_sample_list <- function(session, data) {
  i <- sapply(data, is.numeric)
  samples <- names(data)[i][colSums(data[i]) > 0]

  updateSelectInput(session, "sampleId", choices = sort(unique(samples)), selected = NULL)
}