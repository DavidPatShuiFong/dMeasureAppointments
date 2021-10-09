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
#' @param screentag if neither `screentag` or `screentag_print` is
#'     defined then values are derived from `self$printcopy_view`
#' @param screentag_print
#'
#' @return dataframe of apppointments
#'  $Patient, $AppointmentDate, $AppointmentTime,
#'  $Provider, $Status
#'
#' @export
appointments <- function(
  dMeasureAppointments_obj,
  screentag = FALSE,
  screentag_print = TRUE) {
  dMeasureAppointments_obj$appointments(
    screentag,
    screentag_print
  )
}
.public(
  dMeasureAppointments, "appointments",
  function(
    screentag = FALSE,
    screentag_print = TRUE
    # currently, screentag and screentag_print are ignored
    # because there are no HTML tags available
  ) {
    if (!screentag && !screentag_print) {
      stop("One of 'screentag' or 'screentag_print' must be set to TRUE")
    } else if (screentag && screentag_print) {
      stop("Only one of 'screentag' or 'screentag_print' can be set to TRUE")
    }

    intID <- c(-1) # create 'empty' vector of intID

    l <- self$dM$appointments_filtered_time
    intID <- c(-1, l %>>% dplyr::pull(InternalID))

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
        self$dM$appointments_filtered_timeR(),
        self$printcopy_view()
      ), {
        self$appointments(
          screentag = !self$printcopy_view(),
          screentag_print = self$printcopy_view()
        )
      }
    )
  )
)
