#' appointments_telephone UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_appointments_telephone_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      DT::DTOutput(ns("appointments_table"))
    )
  )
}

#' appointments_telephone Server Functions
#'
#' @noRd
mod_appointments_telephone_server <- function(id, dMAppointments){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    styled_appointments_list <- shiny::reactive({
      shiny::validate(
        shiny::need(
          dMAppointments$dM$appointments_filtered_timeR(),
          "No appointments in selected range"
        )
      )
      shiny::req(dMAppointments$dM$clinicians)

      DailyMeasure::datatable_styled(
        dMAppointments$appointments_telephoneR(),
        extensions = c("Buttons", "Scroller", "Responsive", "Select"), # allow selection
        selectAllButton = "selectAll",
        selectNoneButton = "selectNone",
        selection = 'none'
      )
    })

    output$appointments_table <- DT::renderDT({
      styled_appointments_list()
    },
    server = FALSE) # this is to use the 'Select' extension
  })
}

## To be copied in the UI
# mod_appointments_telephone_ui("appointments_telephone_1")

## To be copied in the server
# mod_appointments_telephone_server("appointments_telephone_1")
