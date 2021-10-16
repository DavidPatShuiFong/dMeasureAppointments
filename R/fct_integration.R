# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' dMeasureAppointments
#'
#' @md
#'
#' @name dMeasureAppointments
#' @title dMeasureAppointments
#'
#' @include utils_R6.R
#' Functions for Appointment handling
NULL

#' dMeasureIntegration
#'
#' @name dMeasureIntegration
#'
#' @description integration with dMeasure
#'   (especially DailyMeasure)
#'
#' @param information the information required
#'   `Provides` - modules provided (in this case, `dMeasureAppointments`)
#'   `Requires` - the modules required (including `dMeasure`)
#'   `moduleID` - IDs of modules to create
#'
#' @return vector of required information
#'
#' @export
dMeasureIntegration <- function(information) {
  if (information == "Provides") {return(c("dMeasureAppointments"))}
  if (information == "Requires") {return(c("dMeasure"))}
  if (information == "moduleID") {return(c("Appointments_dt"))}
  if (information == "configID") {return(c("dMeasureAppointments_config"))}
}

#' sidebarmenuPriority
#'
#' @name sidebarmenuPriority
#'
#' @description priority order to appear in sidebarmenu
#'   integration with DailyMeasure
#'
#' 50 is medium priority. larger numbers are higher priority
#'
#' @return priority.
#'
#' @export
sidebarmenuPriority <- function() {
  return(99) # very high priority
}

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

#' dMeasureAppointments class
#' @title dMeasureAppointments class
#' @description list appointments by clinician providers
#' @export
dMeasureAppointments <- R6::R6Class(
  "dMeasureAppointments",
  public = list(
    # dM is a dMeasure object
    dM = NULL, # pointer to dMeasure R6 object
    join_config = NULL, # configuration for 'join' integration
    sms_config = NULL, # configuration for 'sms' storage
    initialize = function(dMeasure_obj) {
      # dMeasure_obj is a R6 dMeasure object
      self$dM <- dMeasure_obj

      if (length(public_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(public_init_fields$name)) {
          if (public_init_fields$obj[[i]] == "dMeasureAppointments") {
            self[[public_init_fields$name[[i]]]] <-
              eval(public_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }
      if (length(private_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(private_init_fields$name)) {
          if (private_init_fields$obj[[i]] == "dMeasureAppointments") {
            private[[private_init_fields$name[[i]]]] <-
              eval(private_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }

      if (requireNamespace("shiny", quietly = TRUE)) {
        # set reactive version only if shiny is available
        # note that this is for reading (from programs calling this object) only!
        if (length(reactive_fields$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_fields$name)) {
            if (reactive_fields$obj[[i]] == "dMeasureAppointments") {
              self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                eval(reactive_fields$value[[i]]) # could 'quote' the value
              )
            }
          }
        }
        if (length(reactive_event$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_event$name)) {
            if (reactive_event$obj[[i]] == "dMeasureAppointments") {
              self[[reactive_event$name[[i]]]] <-
                eval(reactive_event$value[[i]]) # could 'quote' the value
            }
          }
        }
      }
    }
  )
  # this is a 'skeleton' class
  # it is filled in the with the '.public' function
)

#' dMeasureAppointment module - initialize Join configuration
#'
#' @name initialize_join_config
#'
#' @return list
#'
#' @export
initialize_join_config <- function() {
  return(
    list(
      tablename = "Appointments_joinConfig",
      variable_list = list(
        c("id", "integer"),
        c("api", "character"),
        # 'api' - name of SMS sending method
        # at time of writing, the 'api' can only be 'Join"
        c("apikey", "character"),
        c("deviceId", "character")
        # it is expected that there will be only one row in this table
        # containing the apikey and deviceId required to communicate
        # with 'Join' to send SMS via an Android phone
      )
    )
  )
}

#' dMeasureAppointment module - initialize SMS text configuration
#'
#' @name initialize_sms_config
#'
#' @return list
#'
#' @export
initialize_sms_config <- function() {
  return(
    list(
      tablename = "Appointments_smsConfig",
      variable_list = list(
        c("id", "integer"),
        c("smstext", "character")
      )
    )
  )
}

#' read the configuration database
#'
#' @name read_configuration_db
#'
#' @details
#'   reads join configuration.
#'   reads SMS configuration.
#'
#' @param dMeasureAppointment_obj R6 object
#'
#' @return Join configuration
#'
#' @export
read_configuration_db <- function(
  dMeasureAppointment_obj) {
  dMeasureAppointment_obj$read_configuration_db()
}
.public(dMeasureAppointments, "read_configuration_db", function() {
  # read configuration database for dMeasureAppointment
  # e.g. to send SMS via Join

  self$join_config <- self$dM$config_db$conn() %>>%
    dplyr::tbl("Appointments_joinConfig")
  # a link to the table. will contain apikey and deviceId

  self$sms_config <- self$dM$config_db$conn() %>>%
    dplyr::tbl("Appointments_smsConfig")
  # table of stored SMS messages

  private$set_reactive(self$join_configR, self$join_config %>>% dplyr::collect())
  private$set_reactive(self$sms_configR, self$sms_config %>>% dplyr::collect())

  return(self$join_config)
})
.reactive(dMeasureAppointments, "join_configR", NULL)
.reactive(dMeasureAppointments, "sms_configR", NULL)


#' initialize the configuration database
#'
#' @name initialize_configuration_db
#'
#' @description create, or expand, configuration database
#'
#' @details Creates, if necessary, the configuration files for Join
#'   and a store of 'saved' SMS messages.
#'
#'   If necessary, also adds additional columns to existing
#'   configuration tables
#'
#' @param config_db pointer to configuration database
#'
#' @export
initialize_configuration_db <- function(config_db) {
  # initialize configuration database for dMeasureAppointment
  # e.g. to send SMS via Join

  if (!is.null(config_db$conn())) {
    # configuration database is open
    x <- dMeasureAppointments::initialize_join_config()
    dMeasure::initialize_data_table(
      config_db,
      tablename = x$tablename,
      variable_list = x$variable_list
    )

    x <- dMeasureAppointments::initialize_sms_config()
    dMeasure::initialize_data_table(
      config_db,
      tablename = x$tablename,
      variable_list = x$variable_list
    )

  }
}
