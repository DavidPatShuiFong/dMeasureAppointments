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
    shiny::actionButton(ns("sendSMS"), "Send SMS"),
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
                rows = 6
              )
            )
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("sendSMSnow"), "Send SMSs now")
          ),
          size = "m"
        )
      )
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
          dplyr::mutate(text = stringi::stri_replace_all(text, fixed = "%PatientName%", replacement = Patient)) %>>%
          dplyr::mutate(text = stringi::stri_replace_all(text, fixed = "%AppointmentDate%", replacement = dateformat(AppointmentDate))) %>>%
          dplyr::mutate(text = stringi::stri_replace_all(text, fixed = "%AppointmentTime%", replacement = AppointmentTime)) %>>%
          dplyr::mutate(text = stringi::stri_replace_all(text, fixed = "%Provider%", replacement = Provider)) %>>%
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

