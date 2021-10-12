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
    mod_appointments_plain_ui(ns("appointments_plain"))
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

    mod_appointments_plain_server("appointments_plain", dMAppointments)
  })
}
