# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Configuration interface elements of dMeasureAppointments
#'
#' requires R6 methods from fct_integration.R
#'
#' @include utils_help.R

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
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        2,
        shiny::actionButton(ns("sendSMS"), "Send SMS")
      ),
      shiny::column(
        10,
        shiny::helpText("Choose at least one appointment before trying to send an SMS"),
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      DT::DTOutput(ns("appointments_table"))
    ),
    # send SMS for selected appointments (will open modal dialog)
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

      # `dateformat` is a function to convert dates into desired date format
      dateformat <- dMAppointments$dM$formatdate()

      DailyMeasure::datatable_styled(
        dMAppointments$appointments_telephoneR() %>>%
          dplyr::mutate(AppointmentDate = dateformat(AppointmentDate)
          ),
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

    shiny::observeEvent(input$sendSMS, {
      if (length(input$appointments_table_rows_selected) > 0) {
        # only if someone to send an SMS to!
        shiny::showModal(
          shiny::modalDialog(
            title = "Send SMS",
            "Number of SMS to send : ", length(input$appointments_table_rows_selected),
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::selectInput(
                  inputId = ns("join_choice"), # currently there can only be one choice
                  label = "SMS configuration", # 'Join'
                  choices = dMAppointments$join_config %>>% dplyr::pull(api)
                )
              ),
              shiny::column(
                6,
                shiny::selectInput(
                  inputId = ns("sms_choices"), # pro-forma choices
                  label = "SMS pro-formas",
                  choices = dMAppointments$sms_config %>>% dplyr::pull(name)
                  # defaults to first choice
                )
              )
            ),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::textAreaInput(
                  inputId = ns("smsform"),
                  label = "SMS pro-forma",
                  rows = 6,
                  value = dMAppointments$sms_config %>>%
                    dplyr::collect() %>>% # force collect to do input$sms_choices comparison
                    head(1) %>>% # first row (if any)
                    dplyr::pull(smstext)
                )
              )
            ),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                12,
                sms_tags
              )
            ),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("sendSMSnow"), "Send SMSs now")
            ),
            size = "m"
          )
        )
      } else {
        shinytoastr::toastr_warning(
          message = "Must select at least one appointment to send!",
          position = "bottom-center"
        )
      }
    })
    shiny::observeEvent(input$sendSMSnow, {
      if (length(input$appointments_table_rows_selected) > 0) {
        # only if someone to send an SMS to!
        # `dateformat` is a function to convert dates into desired date format
        dateformat <- dMAppointments$dM$formatdate()

        smstable <- dMAppointments$appointments_telephoneR() %>>%
          dplyr::slice(input$appointments_table_rows_selected) %>>%
          # choose selected rows
          dplyr::mutate(text = input$smsform) %>>% # the form SMS
          # replace various patterns
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%PatientName%",
              replacement = Patient
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%Firstname%",
              replacement = Firstname
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%Surname%",
              replacement = Surname
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%Preferredname%",
              replacement = Preferredname
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%AppointmentDate%",
              replacement = dateformat(AppointmentDate)
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%AppointmentDateDMY%",
              replacement = format(AppointmentDate, "%d-%m-%Y")
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%AppointmentDateOrdinal%",
              replacement = toOrdinal::toOrdinalDate(AppointmentDate)
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%AppointmentDayOfWeek%",
              replacement = weekdays(AppointmentDate, abbreviate = FALSE)
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%AppointmentDayOfMonth%",
              replacement = lubridate::mday(AppointmentDate)
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%AppointmentDayOfMonthOrdinal%",
              replacement = toOrdinal::toOrdinal(lubridate::mday(AppointmentDate))
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%AppointmentMonth%",
              replacement = months(AppointmentDate, abbreviate = FALSE)
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%Appointment24hrTime%",
              replacement = AppointmentTime
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%AppointmentTime%",
              # time will be of the format '14:52'
              # convert to '02:52 PM'
              replacement = format(strptime(AppointmentTime, format = "%H:%M"), "%I:%M %p")
            )
          ) %>>%
          dplyr::mutate(
            text = stringi::stri_replace_all(
              text,
              fixed = "%Provider%",
              replacement = Provider
            )
          ) %>>%
          dplyr::filter(nchar(MobilePhone) > 0) # at this point, only accept mobile phone numbers
        by(
          # iterate over rows of 'smstable'
          smstable, seq_len(nrow(smstable)),
          function(row) {
            dMeasureAppointments::sms_join(
              row$text,
              row$MobilePhone,
              apikey = dMAppointments$join_config %>>%
                dplyr::collect() %>>%
                dplyr::filter(api == input$join_choice) %>>%
                dplyr::pull(apikey),
              deviceId = dMAppointments$join_config %>>%
                dplyr::collect() %>>%
                dplyr::filter(api == input$join_choice) %>>%
                dplyr::pull(deviceId)
            )
          }
        )
      }
      shiny::removeModal() # close the modal
    })
    shiny::observeEvent(input$sms_choices, {
      shiny::updateTextAreaInput(
        session = getDefaultReactiveDomain(),
        inputId = "smsform",
        value = dMAppointments$sms_config %>>%
          dplyr::collect() %>>% # force collect to do input$sms_choices comparison
          dplyr::filter(name == input$sms_choices) %>>%
          dplyr::pull(smstext)
      )
    }
    )
  })
}

