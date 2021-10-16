# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Configuration interface elements of dMeasureAppointments
#'
#' requires R6 methods from fct_integration.R
#'
#' @include fct_integration.R


#' Appointments module - configuration tabpanel item
#'
#' @details
#'   This function is called by `DailyMeasure`
#'
#' @return tabPanel
#'
#' @export
dMeasureConfigurationTabPanelItem <- function() {
  x <- list(
    shiny::tabPanel(
      title = "Appointments SMS configuration",
      value = "AppointmentConfiguration",
      shiny::column(
        width = 12,
        dMeasureAppointments::dMeasureConfigurationTabPanelUI(
          "dMeasureAppointments_config"
        )
      )
    )
  )
  return(x)
}

#' Appointments module - configuration panel UI
#'
#' @name dMeasureConfigurationTabPanelUI
#'
#' @details
#'   This function is called by `dMeasureConfigurationTabPanelItem`,
#'   with the same module `id` as `DailyMeasure` will
#'   call `dMeasureConfigurationTabPanel`.
#'
#'   The module `id` is also returned by the integration function
#'   `dMeasureIntegration` when called with parameter `configID`
#'
#' @param id module ID
#'
#' @return shiny user interface element
#'
#' @export
dMeasureConfigurationTabPanelUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "API configuration",
          shiny::fluidRow(
            DTedit::dteditmodUI(ns("join_configuration"))
          )
        ),
        shiny::tabPanel(
          "SMS configuratioin",
          shiny::fluidRow()
        )
      )
  )
}

#' Appointments module - configuration panel server
#'
#' @name dMeasureConfigurationTabPanel
#'
#' @details
#'   This (module) server is called by `DailyMeasure` with the
#'   same module `id` as `dMeasureConfigurationTabPanelItem` will
#'   call `dMeasureConfigurationTabPanelUI`
#'
#'   The module `id` is also returned by the integration function
#'   `dMeasureIntegration` when called with the parameter `configID`
#'
#' @param id as required by Shiny modules
#' @param dMAppointments dMeasureAppointments R6 object
#'
#' @return none
#'
#' @export
dMeasureConfigurationTabPanel <- function(id, dMAppointments) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    join_config.callback.insert <- function(data, row) {

      if (data[row, "api"][[1]] %in% data[-row, ]$api) {
        stop(paste("Can't define the same API twice!"))
      }
      if (data[row, "api"][[1]] == "") {
        stop(paste("API cannot be empty!"))
      }

      newID <- dMAppointments$write_join_config(
        api = data[row, "api"][[1]],
        apikey = data[row, "apikey"][[1]],
        deviceId = data[row, "deviceId"][[1]]
      )
      data <- dMAppointments$join_config %>>% dplyr::collect()
      # read the database back in

      return(data)
    }

    join_config.callback.update <- function(data, olddata, row) {

      tryCatch(
        result <- dMAppointments$update_join_config(
          id = data[row, "id"][[1]],
          api = data[row, "api"][[1]],
          # name might have been changed!
          apikey = data[row, "apikey"][[1]],
          deviceId = data[row, "deviceId"][[1]]
        ),
        error = function(e) stop(e)
      )

      data <- dMAppointments$join_config %>>% dplyr::collect()
      # read the database back in

      return(data)
    }

    join_config.callback.delete <- function(data, row) {
      tryCatch(
        dMAppointments$remove_join_config(data[row, "api"][[1]]),
        error = function(e) stop(e)
      )
      data <- dMAppointments$join_config %>>% dplyr::collect()
      # read the database back in

      return(data)
    }

    shiny::observeEvent(
      dMAppointments$join_configR(),
      ignoreNULL = TRUE, once = TRUE, {
        shiny::callModule(
          DTedit::dteditmod,
          "join_configuration",
          thedata = dMAppointments$join_config %>>% dplyr::collect(),
          view.cols = c("api", "apikey", "deviceId"),
          edit.cols = c("api", "apikey", "deviceId"),
          edit.label.cols = c("API", "API key", "Device ID"),
          input.types = list(api = "selectInput", apikey = "textInput", deviceId = "textInput"),
          input.choices = list(api = c("Join")), # only one choice!
          callback.insert = join_config.callback.insert,
          callback.delete = join_config.callback.delete,
          callback.update = join_config.callback.update
        )
      }
    )
  })
}

