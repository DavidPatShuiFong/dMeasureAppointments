# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Interface elements of dMeasureAppointments
#'
#' requires R6 methods from fct_integration.R
#'
#' @include fct_integration.R
#' @include fct_tables.R
NULL

###########################################################

#' item description for left sidebar menu
#'
#' @name shinydashboardmenuItem
#'
#' @return shinydashboard menuItem object
#'
#' @export
shinydashboardmenuItem <- function() {
  x <- list(
    shinydashboard::menuItem(
      "Appointments",
      tabName = "appointments",
      icon = shiny::icon("calendar-check")
    )
  )

  return(x)
}

#' center panel description
#'
#' @name dMeasureShinytabItems
#'
#' @return shinytabItems
#'
#' @export
dMeasureShinytabItems <- function() {
  x <- list(
    shinydashboard::tabItem(
      tabName = "appointments",
      shiny::fluidRow(shiny::column(
        width = 12, align = "center",
        shiny::h2("Appointments")
      )),
      shiny::fluidRow(shiny::column(
        width = 12,
        shiny::div(
          id = "appointments_datatable_wrapper",
          dMeasureAppointments::datatableUI("Appointments_dt")
        )
      ))
    )
  )
  return(x)
}


#' Appointments module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @name datatableUI
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
#'
#' @export
datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      DT::DTOutput(ns("appointments_table"))
    )
  )
}

.reactive(dMeasureAppointments, "printcopy_view", TRUE)

#' Appointments module - server
#'
#' @name datatableServer
#'
#' @param id id
#' @param dMAppointments dMeasureAppointments R6 object
#'
#' @return none
#'
#' @export
datatableServer <- function(id, dMAppointments) {

  shiny::moduleServer(id, function(input, output, session) {
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
