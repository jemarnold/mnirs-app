## setup =====================================
suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(DT)
    library(tidyr)
    library(scales)
    library(ggplot2)
    library(signal)
    library(mnirs)
    library(thematic)
    library(plotly)
    library(shinyjs)
})

# pak::pak("jemarnold/mnirs@dev")

thematic::thematic_shiny(font = "auto")

options(
    mnirs.verbose = FALSE,
    shiny.maxRequestSize = 50 * 1024^2
)

## sourced automatically from R/ directory:
## R/ui.R           UI tab builders & theme
## R/server_process.R  Process Data tab server
## R/server_extract.R  Extract Intervals tab server
## R/server_kinetics.R Analyse Kinetics tab server
## R/utils.R        input parsing & data helpers
## R/plotly_mnirs.R interactive plot builder

## UI ===========================================================
ui <- page_navbar(
    title = tagList(
        img(
            src = "mnirs-hex.svg",
            height = "50px",
            style = "margin-right: 8px; vertical-align: middle;"
        ),
        "{mnirs} Data Processing"
    ),
    theme = app_theme(),
    header = shinyjs::useShinyjs(),
    process_tab(),
    extract_tab(),
    kinetics_tab(),
    instructions_tab(),
    !!!socials_nav()
)

## server ===========================================
server <- function(input, output, session) {
    shared <- process_server(input, output, session)
    extracted <- extract_server(
        input,
        output,
        session,
        base_data = shared$base_data
    )
    kinetics_server(
        input,
        output,
        session,
        interval_list = extracted$interval_list,
        base_data = shared$base_data
    )
}

shinyApp(ui = ui, server = server)
