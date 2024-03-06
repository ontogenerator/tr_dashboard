#----------------------------------------------------------------------------------------------------------------------
# Load libraries
#----------------------------------------------------------------------------------------------------------------------
library(DT)
library(ggbeeswarm)
library(ggvis)
library(ggplot2)
library(haven)
library(plotly)
library(scales)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(assertthat)
library(vroom)

#----------------------------------------------------------------------------------------------------------------------
# load data & functions
#----------------------------------------------------------------------------------------------------------------------

source("impressum.R", encoding="UTF-8")
source("metric_box.R")
source("OA_metrics.R")
source("ODC_metrics.R")
source("preprints_metrics.R")
source("vis_metrics.R")
source("orcid_metrics.R")
source("contribot_metrics.R")
source("ui_elements.R")
source("methods_descriptions.R", encoding = "UTF-8")
source("resources_descriptions.R", encoding = "UTF-8")
source("about_page.R", encoding = "UTF-8")
source("datasets_panel.R")



dashboard_metrics <- read_csv("./data/dashboard_metrics.csv")
# dashboard_metrics <- read_csv("./shiny_app/data/dashboard_metrics.csv")
dashboard_metrics_aggregate <- read_csv("./data/dashboard_metrics_aggregate.csv")
  # dashboard_metrics_aggregate <- read_csv("./shiny_app/data/dashboard_metrics_aggregate.csv") |>

preprints_dataset_shiny <- vroom("./data/preprints_dataset_shiny.csv")


show_dashboard <- function(...) {
  #----------------------------------------------------------------------------------------------------------------------
  # ui
  #----------------------------------------------------------------------------------------------------------------------

  ui <-
    tagList(
      tags$head(tags$script(type="text/javascript", src = "code.js")),
      navbarPage(
        "Charité Metrics Dashboard", theme = shinytheme("flatly"), id = "navbarTabs",
        tabPanel("Start page", value = "tabStart",
                 wellPanel(
                   br(),
                   fluidRow(
                     column(8,
                            h1(style = "margin-left:0cm", strong("Charité Dashboard on Responsible Research"), align = "left"),
                            h4(style = "margin-left:0cm",
                               HTML('Charité has committed itself to establish, promote and maintain a
                              research environment which enhances the robustness of research and
                              the reproducibility of results
                              (<a href="https://www.charite.de/en/charite/about_us/strategic_direction_2030/">Rethinking Health – Charité 2030</a>).')),
                            h4(style = "margin-left:0cm",
                               HTML('This dashboard gives an overview of several metrics of open and responsible
                        research at the Charité (including the Berlin Institute of Health).
                        For a detailed discussion about which metrics to include in Open Science Dashboards see
                        (<a href = "https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001949">Cobey et al. 2023</a>).
                        For more detailed information on the methods used to
                        calculate those metrics, the dataset underlying the metrics, or resources
                        to improve your own research practices, click one of the following buttons on the right.')),
                        h4(style = "margin-left:0cm",
                           "This dashboard is a pilot that is still under development. More metrics will be added in the future."),
                        h4(style = "margin-left:0cm",
                           HTML('For more detailed open access metrics you can visit the
                         <a href="https://medbib-charite.github.io/oa-dashboard/">Charité Open Access Dashboard</a>
                         developed by the Charité Medical Library.')),
                        br()),
                     column(4,
                            hr(),
                            br(),
                            br(),
                            br(),
                            actionButton(style = "color: white; background-color: #aa1c7d;",
                                         'buttonMethods',
                                         'See methods'),
                            actionButton(style = "color: white; background-color: #aa1c7d;",
                                         'buttonResources',
                                         'See resources'),
                            actionButton(style = "color: white; background-color: #aa1c7d;",
                                         'buttonDatasets',
                                         'See dataset'),
                            br())
                   ),
                 ),

                 # generate Open Science & Clinical trial metrics UI dynamically to determine column width during start of the app
                 uiOutput("OpenScience_metrics") |>
                   shinycssloaders::withSpinner(color = "#007265"),

                 uiOutput("Broader_transparency_metrics") |>
                   shinycssloaders::withSpinner(color = "#007265"),

                 uiOutput("Visualizations_metrics") |>
                   shinycssloaders::withSpinner(color = "#007265"),
                 br(),
                 br(),
                 br(),
                 hr(),
                 bsCollapsePanel(strong("Impressum"),
                                 impressum_text,
                                 style = "default"),
                 bsCollapsePanel(strong("Datenschutz"),
                                 datenschutz_text,
                                 style = "default")
        ),

         # Methods and resources are displayed in a fold-out menu due to lack of space in the navbar.
        navbarMenu("Methods/Resources/Data",
                   methods_panel,
                   resources_panel,

                   tabPanel("Datasets", value = "tabDatasets",
                            h1("Datasets"),
                            h4("The following tables contain the datasets underlying the numbers and plots
              shown for the metrics included in this Shiny app."),
              br(),
              bsCollapse(id = "datasetPanels_PublicationDataset",
                         bsCollapsePanel("Publication dataset",
                                         DT::dataTableOutput("data_table_publ"),
                                         style = "default")),
              br(),
              bsCollapse(id = "datasetPanels_PreprintDataset",
                         bsCollapsePanel("Preprint dataset",
                                         DT::dataTableOutput("data_table_preprints"),
                                         style = "default")),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              hr(),
              bsCollapsePanel(strong("Impressum"),
                              impressum_text,
                              style = "default"),
              bsCollapsePanel(strong("Datenschutz"),
                              datenschutz_text,
                              style = "default")
                   )
        ),
        about_page,

        # Javascript code necessary to provide window width as input variable
        # for the dynamic column width setting
        tags$head(tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        ')),
        # Change color of all selectize dropdowns
        tags$head(tags$style(HTML('.selectize-input.full{background: #DCE3E5; border: #DCE3E5;}')))

      )
    )
  #----------------------------------------------------------------------------------------------------------------------
  # server
  #----------------------------------------------------------------------------------------------------------------------

  server <- function(input, output, session)
  {

    # URI routing
    # (see: https://stackoverflow.com/questions/71541259/uri-routing-with-shiny-router-and-navbarpage-in-a-r-shiny-app/71807248?noredirect=1#comment126924825_71807248)
    observeEvent(session$clientData$url_hash, {
      currentHash <- sub("#", "", session$clientData$url_hash)
      if(is.null(input$navbarTabs) || !is.null(currentHash) && currentHash != input$navbarTabs){
        freezeReactiveValue(input, "navbarTabs")
        updateTabsetPanel(session, "navbarTabs", selected = currentHash)
      }
    }, priority = 1)

    observeEvent(input$navbarTabs, {
      currentHash <- sub("#", "", session$clientData$url_hash)
      pushQueryString <- paste0("#", input$navbarTabs)
      if(is.null(currentHash) || currentHash != input$navbarTabs){
        freezeReactiveValue(input, "navbarTabs")
        updateQueryString(pushQueryString, mode = "push", session)
      }
    }, priority = 0)

    RVs <- reactiveValues(total_os = FALSE, total_ct = FALSE,
                          total_bt = FALSE, total_vis = FALSE)

    observe({
      RVs$total_os <- input$checkbox_total_OS
    }) |>
      bindEvent(input$checkbox_total_OS)

    observe({
      RVs$total_bt <- input$checkbox_total_BT
    }) |>
      bindEvent(input$checkbox_total_BT)

    observe({
      RVs$total_vis <- input$checkbox_total_Vis
    }) |>
      bindEvent(input$checkbox_total_Vis)

    preprintsServer("plot_preprints", dashboard_metrics_aggregate, dashboard_metrics, reactive(RVs$total_os), color_palette)
    OAServer("plot_OA", dashboard_metrics, reactive(RVs$total_os), color_palette)
    ODServer("plot_OD", dashboard_metrics, "data", reactive(RVs$total_os), color_palette)
    ODServer("plot_OC", dashboard_metrics, "code", reactive(RVs$total_os), color_palette)
    ODServer("plot_DAS", dashboard_metrics, "das", reactive(RVs$total_os), color_palette)
    visServer("plot_barzooka_problem", dashboard_metrics, "problem", reactive(RVs$total_vis), color_palette)
    visServer("plot_barzooka_inform", dashboard_metrics, "inform", reactive(RVs$total_vis), color_palette)
    orcidServer("plot_orcid_pubs", dashboard_metrics, "pubs", reactive(RVs$total_bt), color_palette)
    contribotServer("plot_contrib", dashboard_metrics, "credit", reactive(RVs$total_bt), color_palette)

    output$OA <-
      renderUI({
        box_value <- get_current_OA(dashboard_metrics)
        box_text <- paste0("of publications were open access in ", dashboard_metrics$year |> max())
        alignment <- "right"

        metricBoxOutput(title = "Open Access",
                        value = box_value,
                        value_text = box_text,
                        plot = OAOutput("plot_OA", height = "300px"),
                        info_id = "infoOA",
                        info_title = "Open Access",
                        info_text = open_access_tooltip,
                        info_alignment = alignment)
      })  |>
      bindEvent(reactive(RVs$total_os))

    output$OD <-
      renderUI({
        box_value <- get_current_OD(dashboard_metrics)
        box_text <- paste0("of screened publications mentioned sharing data openly in ",
                           dashboard_metrics$year |>  max())
        alignment <- "right"
        metricBoxOutput(title = "Any Open Data",
                        value = box_value,
                        value_text = box_text,
                        plot = ODOutput("plot_OD", height = "300px"),
                        info_id = "infoOD",
                        info_title = "Open Data",
                        info_text = open_data_tooltip,
                        info_alignment = alignment)
      })  |>
      bindEvent(reactive(RVs$total_os))

    output$OC <-
      renderUI({
        box_value <- get_current_OC(dashboard_metrics)
        box_text <- paste0("of screened publications mentioned sharing code openly in ",
                           dashboard_metrics$year |>  max())
        alignment <- "right"
        metricBoxOutput(title = "Any Open Code",
                        value = box_value,
                        value_text = box_text,
                        plot = ODOutput("plot_OC", height = "300px"),
                        info_id = "infoOC",
                        info_title = "Open Code",
                        info_text = open_code_tooltip,
                        info_alignment = alignment)
      })  |>
      bindEvent(reactive(RVs$total_os))

    output$DAS <-
      renderUI({
        box_value <- get_current_DAS(dashboard_metrics)
        box_text <- paste0("of screened publications included a Data (DAS) or Code Availability Statement (CAS) in ",
                           dashboard_metrics$year |>  max())
        alignment <- "left"
        metricBoxOutput(title = "Any Data (DAS) or Code Availability Statement (CAS)",
                        value = box_value,
                        value_text = box_text,
                        plot = ODOutput("plot_DAS", height = "300px"),
                        info_id = "infoDAS",
                        info_title = "Data or Code Availability Statements",
                        info_text = das_tooltip,
                        info_alignment = alignment)
      })  |>
      bindEvent(reactive(RVs$total_os))


    output$preprints <-
      renderUI({
        box_value <- get_current_val(dashboard_metrics_aggregate, n_preprints)
        box_text <- paste0("preprints published in ", dashboard_metrics_aggregate$year |> max())

        metricBoxOutput(title = "Preprints",
                        value = box_value,
                        value_text = box_text,
                        plot = preprintsOutput("plot_preprints", height = "300px"),
                        info_id = "infoPreprints",
                        info_title = "Preprints",
                        info_text = preprints_tooltip,
                        info_alignment = "left")
      }) |>
      bindEvent(reactive(RVs$total_os))


    output$vis_problem <-
      renderUI({
        box_value <- get_current_vis(dashboard_metrics, perc_bar)
        box_text <- paste0("of publications from ", dashboard_metrics$year |> max(),
                           " used bar graphs for continuous data")

        metricBoxOutput(title = "Problematic graph types",
                        value = box_value,
                        value_text = box_text,
                        plot = visOutput("plot_barzooka_problem", height = "300px"),
                        info_id = "infoVisProblem",
                        info_title = "Problematic graph types",
                        info_text = vis_problem_tooltip)
      })

    output$vis_inform <-
      renderUI({
        box_value <- get_current_vis(dashboard_metrics, perc_informative)
        box_text <- paste0("of publications from ", dashboard_metrics$year |> max(), " used more informative graph types")

        metricBoxOutput(title = "More informative graph types for continuous data",
                        value = box_value,
                        value_text = box_text,
                        plot = visOutput("plot_barzooka_inform", height = "300px"),
                        info_id = "infoVisInform",
                        info_title = "More informative graph types",
                        info_text = vis_inform_tooltip,
                        info_alignment = "left")
      })

    #  output$orcid_pubs <- renderUI({
    #   box_value <- get_current_orcids_from_pubs(dashboard_metrics)
    #   box_text <- paste0("of publications with a Charité correspondence author", " included at least one ORCID in ",
    #                      dashboard_metrics$year |> max())
    #
    #   metricBoxOutput(title = "ORCIDs in Publications",
    #                   value = box_value,
    #                   value_text = box_text,
    #                   plot = orcidOutput('plot_orcid_pubs', height = "300px"),
    #                   info_id = "infoOrcidPubs",
    #                   info_title = "ORCIDpubs",
    #                   info_text = orcid_pubs_tooltip,
    #                   info_alignment = "right")
    # })

    output$authorship <- renderUI({
      box_value <- get_current_contribot(dashboard_metrics, perc_has_contrib)
      box_text <- paste0("of screened publications had authorship statements in ",
                         dashboard_metrics$year |> max())
      metricBoxOutput(title = "Authorship Statements",
                      value = box_value,
                      value_text = box_text,
                      plot = contribotOutput("plot_contrib", height = "300px"),
                      info_id = "infoAuthorship",
                      info_title = "Authorship",
                      info_text = authorship_tooltip,
                      info_alignment = "left")
    }) |>
      bindEvent(reactive(RVs$total_bt))

    # dynamically determine column width of Open Science metrics at program start
    # four columns if resolution large enough, otherwise two columns
    output$OpenScience_metrics <- renderUI({
      req(input$width)
      if(input$width < 1400) {
        col_width <- 6
        alignment <- "left"
      } else {
        col_width <- 4
        alignment <- "right"
      }

      wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
                h2(strong("Open Science"), align = "left"),
                fluidRow(
                  column(2, checkboxInput("checkbox_total_OS", strong("Show absolute numbers"), value = FALSE)),
                  column(8, h5(strong("Double-click or select rectangular area inside any panel to zoom in")))
                ),
                fluidRow(
                  column(col_width, uiOutput("OA") |>
                           shinycssloaders::withSpinner(color = "#007265")),
                  column(col_width, uiOutput("preprints") |>
                           shinycssloaders::withSpinner(color = "#007265"))
                  ),
                  fluidRow(column(col_width, uiOutput("DAS") |>
                           shinycssloaders::withSpinner(color = "#007265")),
                  column(col_width, uiOutput("OD") |>
                           shinycssloaders::withSpinner(color = "#007265")),
                  column(col_width, uiOutput("OC") |>
                           shinycssloaders::withSpinner(color = "#007265"))
                )
      )
    })



    output$Broader_transparency_metrics <- renderUI({
      req(input$width)
      if(input$width < 1400) {
        col_width <- 6
        alignment <- "left"
        } else {
          col_width <- 4
          alignment <- "right"
        }
      wellPanel(style = "padding-top: 0px; padding-bottom: 0px;",
                h2(strong("Broader Transparency"), align = "left"),
                fluidRow(
                  column(2, checkboxInput("checkbox_total_BT", strong("Show absolute numbers"), value = FALSE)),
                  column(8, h5(strong("Double-click or select rectangular area inside any panel to zoom in")))
                ),
                fluidRow(
                  # column(
                  #   col_width, uiOutput("orcid_pubs") |>
                  #     shinycssloaders::withSpinner(color = "#007265")
                  # ),
                  column(
                    col_width, uiOutput("authorship") |>
                      shinycssloaders::withSpinner(color = "#007265")
                  )
                  )
                )
      })

    output$Visualizations_metrics <- renderUI({

      #always show two tabs in one row for the visualization metrics
      col_width <- 6
      alignment <- "left"

      wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
                h2(strong("Visualizations"),
                   align = "left"),
                fluidRow(
                           column(2, checkboxInput("checkbox_total_Vis", strong("Show absolute numbers"), value = FALSE)),
                           column(8, h5(strong("Double-click or select rectangular area inside any panel to zoom in")))
                         ),
                fluidRow(
                  column(col_width, uiOutput("vis_problem") |>
                           shinycssloaders::withSpinner(color = "#007265")),
                  column(col_width, uiOutput("vis_inform") |>
                           shinycssloaders::withSpinner(color = "#007265"))
                ))
    })

    #actionButton to switch tabs
    observeEvent(input$buttonMethods, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      #updateCollapse(session, "methodsPanels_OpenScience", open = "Preprints")
    })

    observeEvent(input$buttonResources, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabRessources")
    })

    observeEvent(input$buttonDatasets, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabDatasets")
    })



    #tooltip buttons -> methods section
    observeEvent(input$infoOA, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      updateCollapse(session, "methodsPanels_OpenScience", open = "Open Access")
    })

    observeEvent(input$infoOD, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      updateCollapse(session, "methodsPanels_OpenScience", open = "Open Data and Open Code")
    })

    observeEvent(input$infoOC, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      updateCollapse(session, "methodsPanels_OpenScience", open = "Open Data and Open Code")
    })

    observeEvent(input$infoDAS, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      updateCollapse(session, "methodsPanels_OpenScience", open = "Data or Code Availability Statements")
    })

    observeEvent(input$infoPreprints, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      updateCollapse(session, "methodsPanels_OpenScience", open = "Preprints")
    })

    observeEvent(input$infoOrcid, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      updateCollapse(session, "methodsPanels_broader_transparency", open = "ORCID")
    })

    observeEvent(input$infoOrcidPubs, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      updateCollapse(session, "methodsPanels_broader_transparency", open = "ORCIDs in publications")
    })

    observeEvent(input$infoAuthorship, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabMethods")
      updateCollapse(session, "methodsPanels_broader_transparency", open = "Authorship")
    })

    observeEvent(input$infoVisProblem, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabRessources")
    })

    observeEvent(input$infoVisInform, {
      updateTabsetPanel(session, "navbarTabs",
                        selected = "tabRessources")
    })

    #data table to show the underlying datasets
    output$data_table_publ <- DT::renderDataTable({
      make_datatable(dashboard_metrics)
    })

    output$data_table_preprints <- DT::renderDataTable({
      make_datatable(preprints_dataset_shiny)
    })


    color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                       "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
                       "#DCE3E5", "#CF9188")

  }

  shinyApp(ui, server)
}


show_dashboard()

