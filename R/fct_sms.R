# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' sms_join
#'
#' @name sms_join
#'
#' @md
#'
#' @description Send SMS with Join
#'
#' @details
#'
#' Join (by joaoapps) allows sending SMSs with an Android phone connected to a Google account
#'   https://joaoapps.com/join/
#'
#'   Instructions to find `apkey` and `deviceId` can be found at http://www.juanmtech.com/join-by-joaoapps-beginners-guide/
#'
#'   In summary (after device has Join installed and connected to a Google account)
#'
#'   * Go to Join's web interface at https://joinjoaomgcd.appspot.com/, and login with your Google account
#'   * Choose the device which you will use to send SMSs
#'   * Choose 'Create a Join API for your device' from the right-hand-side list of options
#'   * The 'Device Id' and 'API Key' will be visible at the bottom of the web page
#'
#'   The returned response objected is interpreted by `httr::content`
#'
#'   `$success` will be `TRUE` or `FALSE`
#'
#'   If there is an error (`$success == FALSE`), then `$errorMessage` will contain an error message such as
#'
#'   * "User not Authenticated" (incorrect `apikey`)
#'   * "No device to send message to" (incorrect `deviceId`)
#'
#' @param smstext the text to send
#' @param smsnumber the sms number (in characters)
#' @param apikey apikey to Join
#' @param deviceId deviceId of the mobile device
#'
#' @return interpreted response object
#'
#' @export
sms_join <- function(smstext, smsnumber, apikey = NULL, deviceId = NULL) {
  httppush <- "https://joinjoaomgcd.appspot.com/_ah/api/messaging/v1/sendPush"
  response <- httr::GET(
    httppush,
    query = list(apikey = apikey, smsnumber = smsnumber, smstext = smstext, deviceId = deviceId)
  )

  return(httr::content(response))
}
