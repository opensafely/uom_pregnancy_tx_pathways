library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(tidyverse)
library(DT)
library(here)
library(fs)
library(gt)

load(here("sdr", "data", "snomed_code_usage.rda"))
load(here("sdr", "data", "snomed_code_dict.rda"))

used_snomed_concept_ids <- unique(snomed_code_usage$snomed_concept_id)

pregnancy_codelists_paths <- dir_ls(
  here("codelists"),
  glob = "*user-VickiPalin-pregnancy*.csv$"
)

pregnancy_codelists <- pregnancy_codelists_paths |>
  map(read_csv, col_types = list(code = "c")) %>%
  bind_rows(.id = "codelist") |>
  mutate(codelist = str_extract(codelist, "(?<=user-VickiPalin-)\\w+"))


ui <- page_navbar(
  title = "SNOMED TOOL",
  selected = "Code usage",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  tabPanel(
    title = "Code usage",
    grid_container(
      layout = c(
        "code_settings code_usage"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "350px",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "code_settings",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "selected_codelist",
            label = "Select codelist from file:",
            choices = unique(pregnancy_codelists$codelist)
          ),
          # selectizeInput(
          #   inputId = "selected_codes",
          #   label = "Select individual codes from codelist:",
          #   choices = NULL,
          #   selected = NULL,
          #   multiple = TRUE
          # ),
          selectInput(
            inputId = "select_start_date",
            label = "Select start date:",
            choices = unique(snomed_code_usage$start_date),
            selected = min(snomed_code_usage$end_date)
          ),
          selectInput(
            inputId = "select_end_date",
            label = "Select end date:",
            choices = unique(snomed_code_usage$end_date),
            selected = max(snomed_code_usage$end_date)
          )
        )
      ),
      grid_card(
        area = "code_usage",
        card_body(
          grid_container(
            layout = c(
              "area2"
            ),
            col_sizes = c(
              "1fr"
            ),
            gap_size = "10px",
            grid_card(
              area = "area2",
              card_header(tabPanel(title = "My Shiny App")),
              card_body(
                tabsetPanel(
                  tabPanel(title = "Yearly code usage counts",
                    DTOutput(outputId = "tbl_code_usage", width = "100%")
                  ),
                  tabPanel(title = "Yearly code usage trends",
                    # DTOutput(outputId = "tbl_code_usage", width = "100%")
                  ),
                  tabPanel(title = "Selected codelist",
                    DTOutput(outputId = "tbl_codelist", width = "100%"))
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  selected_codelist <- reactive({
    pregnancy_codelists |>
      filter(codelist %in% input$selected_codelist) |>
      select(-codelist)
  })

  # observeEvent(
  #   input$selected_codelist, {
  #   updateSelectizeInput(
  #     session,
  #     'selected_codes',
  #     choices = selected_codelist()$code,
  #     server = TRUE)
  #     }
  #   )

  # selected_codes <- reactive({
  #   if (is.null(input$selected_codes)) {
  #     selected_codelist()$code
  #   } else {
  #     input$selected_codes
  #   }
  # })

  codes_subset <- reactive({
    snomed_code_usage |>
      filter(snomed_concept_id %in% selected_codelist()$code) |>
      filter(start_date >= input$select_start_date & end_date <= input$select_end_date) |>
      left_join(snomed_code_dict) |>
      select(start_date, end_date, snomed_concept_id, usage, description)
  })

  output$n_total_usage <- renderText({
    1 + 1
  })

  output$n_used_codes <- renderText({
    1 + 1
  })

  output$n_unused_codes <- renderText({
    1 + 1
  })

  output$tbl_code_usage <- renderDT(
    DT::datatable(
      codes_subset(),
      colnames = c(
        "Start date" = "start_date",
        "End date" = "end_date",
        "SNOMED Code" = "snomed_concept_id",
        "Description" = "description",
        "Usage" = "usage")
        )
    )

  output$tbl_codelist <- renderDT(
    DT::datatable(
      selected_codelist(),
      colnames = c(
        "SNOMED Code" = "code",
        "Description" = "term")
    )
  )

}

shinyApp(ui, server)
