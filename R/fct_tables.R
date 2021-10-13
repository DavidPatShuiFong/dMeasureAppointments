# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' table methods of dMeasureAppointments
#'
#' requires R6 methods from fct_integration.R
#'
#' @include fct_integration.R
NULL

#' patient appointment list
#'
#'  derived from dM$appointments_filtered_time
#'
#' @md
#'
#' @param dMeasureAppointments_obj R6 object
#'
#' @return dataframe of apppointments
#'  $Patient, $AppointmentDate, $AppointmentTime,
#'  $Provider, $Status
#'
#' @export
appointments <- function(
  dMeasureAppointments_obj) {
  dMeasureAppointments_obj$appointments()
}
.public(
  dMeasureAppointments, "appointments",
  function(
  ) {
    l <- self$dM$appointments_filtered_time

    # no modifications are made, currently
    l <- l %>>%
      dplyr::select(
        Patient, AppointmentDate, AppointmentTime,
        Provider, Status
      )

    return(l)
  }
)
.reactive_event(
  dMeasureAppointments, "appointmentsR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$appointments_filtered_timeR()
      ), {
        self$appointments(
        )
      }
    )
  )
)


#' patient appointment list with telephone numbers
#'
#'  derived from dM$appointments_filtered_time
#'
#' @md
#'
#' @param dMeasureAppointments_obj R6 object
#'
#' @return dataframe of apppointments
#'  $Patient, $AppointmentDate, $AppointmentTime,
#'  $Provider, $Status, $MobilePhone, $HomePhone, $WorkPhone
#'
#' @export
appointments_telephone <- function(
  dMeasureAppointments_obj) {
  dMeasureAppointments_obj$appointments_telephone()
}
.public(
  dMeasureAppointments, "appointments_telephone",
  function(
  ) {
    l <- self$dM$appointments_filtered_time

    intID <- l %>>%
      dplyr::pull(InternalID) %>>%
      c(-1)

    # no modifications are made, currently
    l <- l %>>%
      dplyr::left_join(
        self$dM$db$patients %>>%
          dplyr::select(
            InternalID, MobilePhone, HomePhone, WorkPhone
          ) %>>%
          dplyr::filter(InternalID %in% intID),
        copy = TRUE
      ) %>>%
      dplyr::select(
        Patient, AppointmentDate, AppointmentTime,
        Provider, Status,
        MobilePhone, HomePhone, WorkPhone
      )

    return(l)
  }
)
.reactive_event(
  dMeasureAppointments, "appointments_telephoneR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$appointments_filtered_timeR()
      ), {
        self$appointments_telephone(
        )
      }
    )
  )
)
