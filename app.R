library(shiny)
library(ggplot2)
source("extractData.R")
source("transformData.R")
source("displayData.R")

ui <- fluidPage(
  titlePanel("The Microbial Communities Dashboard"),
  sidebarLayout(
    sidebarPanel(fileInput("upload", "Upload a file"),
                 actionButton("applyFilter", "Apply filters"),
                 actionButton("clearFilter", "Clear"),
                 downloadButton('downloadFiltered', "Download data"),
                 checkboxGroupInput("include", "Include in results:", c("Common" = "common", "Rare" = "rare"), selected = c("common", "rare")),
                 checkboxGroupInput("phylum", "Phylum", choices = NULL, selected = NULL),
                 checkboxGroupInput("class", "Class", choices = NULL, selected = NULL),
                 checkboxGroupInput("order", "Order", choices = NULL, selected = NULL),
                 checkboxGroupInput("family", "Family", choices = NULL, selected = NULL),
                 checkboxGroupInput("genus", "Genus", choices = NULL, selected = NULL),
                 checkboxGroupInput("species", "Species", choices = NULL, selected = NULL)),
    mainPanel(tabsetPanel(
      tabPanel("Dataset", fluid = TRUE,
               plotOutput("speciesAbundance"),
               plotOutput("speciesOccurence")),
      tabPanel("Individual results", fluid = TRUE,
               selectInput("sampleId", "Sample ID", choices = NULL),
               downloadButton('downloadSample', "Download sample data"),
               plotOutput("sampleResultsGraph"),
               tableOutput("sampleResults")
      ))
    ),
  )
)

server <- function(input, output, session) {
  filters <- reactiveValues(phylum = NULL, class = NULL, order = NULL, family = NULL, genus = NULL, species = NULL)

  dataset <- reactive({
    req(input$upload)

    if (!is.null(input$include)) {
      data <- clean_data(get_data(input$upload$datapath))
      if (length(input$include) == 1) {
        if (input$include == "common") {
          data <- get_common_species(data)
        } else if (input$include == "rare") {
          data <- get_rare_species(data)
        }
      }

      if (!is.null(filters)) {
        if (!is.null(filters$phylum) & length(filters$phylum) > 0) {
          data <- get_by_phylum(data, filters$phylum)
        }

        if (!is.null(filters$class) & length(filters$class) > 0) {
          data <- get_by_class(data, filters$class)
        }

        if (!is.null(filters$order) & length(filters$order) > 0) {
          data <- get_by_order(data, filters$order)
        }

        if (!is.null(filters$family) & length(filters$family) > 0) {
          data <- get_by_family(data, filters$family)
        }

        if (!is.null(filters$genus) & length(filters$genus) > 0) {
          data <- get_by_genus(data, filters$genus)
        }

        if (!is.null(filters$species) & length(filters$species) > 0) {
          data <- get_by_species(data, filters$species)
        }
      }

      return(data)
    } else {
      return(tibble())
    }
  })

  global <- reactiveValues(toHighlight = rep(FALSE, 20), selectedBar = NULL, topAbundances = NULL, topOccurences = NULL)

  observeEvent(dataset(), {
    global$topAbundances <- get_top_abundences(dataset())
    global$topOccurences <- get_top_occurences(dataset())

    update_phylum_filter(session, filters, dataset()$Phylum)
    update_class_filter(session, filters, dataset()$Class)
    update_order_filter(session, filters, dataset()$Order)
    update_family_filter(session, filters, dataset()$Family)
    update_genus_filter(session, filters, dataset()$Genus)
    update_species_filter(session, filters, dataset()$Species)

    update_sample_list(session, dataset())
  })

  observeEvent(input$applyFilter, {
    filters$phylum <- input$phylum
    filters$class <- input$class
    filters$order <- input$order
    filters$family <- input$family
    filters$genus <- input$genus
    filters$species <- input$species
  })

  observeEvent(input$clearFilter, {
    filters$phylum <- NULL
    filters$class <- NULL
    filters$order <- NULL
    filters$family <- NULL
    filters$genus <- NULL
    filters$species <- NULL
  })

  output$speciesAbundance <- renderPlot({
    req(input$upload)

    display_top_abundances(global$topAbundances)
  }, res = 96)

  output$speciesOccurence <- renderPlot({
    req(input$upload)

    display_top_occurences(global$topOccurences)
  }, res = 96)

  output$sampleResults <- renderTable({
    req(input$upload)
    get_by_sample_id(dataset(), input$sampleId)
  })

  output$sampleResultsGraph <- renderPlot({
    req(input$upload)

    display_sample_results(get_top_in_individual_sample(get_by_sample_id(dataset(), input$sampleId)))
  }, res = 96)


  output$downloadFiltered <- downloadHandler(
    filename = function() { paste("results.csv") },
    content = function(fname) {
      write.csv(dataset(), fname)
    }
  )

  output$downloadSample <- downloadHandler(
    filename = function() { paste(input$sampleId, "results.csv") },
    content = function(fname) {
      write.csv(get_by_sample_id(dataset(), input$sampleId), fname)
    }
  )
}

shinyApp(ui, server)