#' appointments_plain UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_appointments_plain_ui <- function(id){
  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      DT::DTOutput(ns("appointments_table"))
    )
  )
}

#' appointments_plain Server Functions
#'
#' @noRd
mod_appointments_plain_server <- function(id, dMAppointments){

  shiny::moduleServer( id, function(input, output, session){
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
        dMAppointments$appointmentsR()
      )
    })

    output$appointments_table <- DT::renderDT({
      styled_appointments_list()
    })
  })
}

## To be copied in the UI
# mod_appointments_plain_ui("appointments_plain_1")

## To be copied in the server
# mod_appointments_plain_server("appointments_plain_1")
