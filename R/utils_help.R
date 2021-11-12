# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' help
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# help text for tags to use in SMS
sms_tags <- shiny::HTML(
  "<table>",
  "<tr><th>Tag</th><th>Description</th>",
  "<tr><td><code>%PatientName%</code></td> <td>name of patient</td></tr>",
  "<tr><td><code>%AppointmentDate%</code></td> <td>date of appointment</td></tr>",
  "<tr><td><code>%AppointmentDateDMY%</code></td> <td>date of appointment e.g. '23-12-2021</td></tr>",
  "<tr><td><code>%AppointmentDateOrdinal%</code></td> <td>date of appointment e.g. 'December 23rd, 2021'</td></tr>",
  "<tr><td><code>%AppointmentDayOfWeek%</code></td> <td>date of appointment day-of-week e.g. 'Monday'</td></tr>",
  "<tr><td><code>%AppointmentDayOfMonth%</code></td> <td>date of appointment day-of-month e.g. '23'</td></tr>",
  "<tr><td><code>%AppointmentDayOfMonthOrdinal%</code>&nbsp;&nbsp;&nbsp;&nbsp;</td> <td>date of appointment ordinal day-of-month e.g. '23rd'</td></tr>",
  "<tr><td><code>%AppointmentMonth%</code></td> <td>date of appointment month e.g. 'December'</td></tr>",
  "<tr><td><code>%Appointment24hrTime%</code></td> <td>appointment time e.g '16:30'</td></tr>",
  "<tr><td><code>%AppointmentTime%</code></td> <td>appointment time e.g. '4:30 PM'</td></tr>",
  "<tr><td><code>%Provider%</code></td> <td>name of provider</td></tr>",
  "</table>"
)
