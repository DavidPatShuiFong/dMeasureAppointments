# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Configuration elements of dMeasureAppointments
#'
#' write to configuration database methods
#'
#' requires R6 methods from fct_integration.R
#'
#' @include fct_integration.R


################################################
# Join configuration

#' insert join_config item to configuration database
#'
#' @param dMeasureAppointments_obj R6 object
#' @param api name of API
#' @param apikey
#' @param deviceId
#'
#' @return ID of created item
#'
#' @export
write_join_config <- function(
  dMeasureAppointments_obj,
  api, apikey, deviceId
) {
  dMeasureAppointments_obj$write_join_config(
    api, apikey, deviceId
  )
}
.public(
  dMeasureAppointments, "write_join_config",
  function(
    api, apikey, deviceId) {
    # write join_config definition to configuration database

    if (api %in% (self$join_config$api)) {
      # this name already chosen
      warning("'", api, "' already exists as a defined API.")
      return(NULL)
    }

    if (api == "") {
      # empty name!
      warning("'API' cannot be empty!")
      return(NULL)
    }

    newID <- max(self$join_config$id, 0) + 1
    # initially might be an empty set, so need to append a '0'
    # note that 'id' is the identifier in the configuration database

    query <- paste0(
      "INSERT INTO Appointments_joinConfig",
      "(id, api, apikey, deviceId)",
      "VALUES ($id, $api, $apikey, $deviceId)"
    )
    data_for_sql <- list(
      id = newID, api = api, apikey = apikey, deviceId = deviceId
    )

    self$dM$config_db$dbSendQuery(query, data_for_sql)

    self$join_config <-
      DBI::dbReadTable(self$dM$config_db$conn(), "Appointments_joinConfig")
    # re-read configuration
    private$set_reactive(self$join_configR, self$join_config)
    # reactive version

    return(newID)
  }
)

#' update join_config item in configuration database
#'
#' @param dMeasureAppointments_obj R6 object
#' @param id row ID
#' @param api name of API
#' @param apikey
#' @param deviceId
#'
#' @return ID of patient list.
#'  stop errors generated for several different reasons of failure
#'
#' @export
update_join_config <- function(
  dMeasureAppointments_obj,
  id, api, apikey, deviceId
) {
  dMeasureAppointments_obj$update_join_config(
    id, api, apikey, deviceId
  )
}
.public(
  dMeasureAppointments, "update_join_config",
  function(
    id, api, apikey, deviceId) {
    # write updated join_config api information to configuration database

    if (!is.null(id)) {
      # ID has been defined
      current_config <- self$join_config %>>% dplyr::collect()

      if (api %in% (current_config[current_config$id != id, ]$api)) {
        # this name already chosen with a different ID
        stop("'", api, "' already exists in configuration.")
      }
      # otherwise, we are going to change the name
    } else {
      # id has *not* been defined, we need to find the ID
      # hopefully, the 'api' will help find the ID
      id <- which(api == current_config$api)
      if (length(id) == 0) {
        # length will == 1 if an ID was found
        # ID == numeric(0) if not found
        stop("'", api, "' does not define an API, and id not provided.")
      }
    }

    query <- paste(
      "UPDATE Appointments_joinConfig SET",
      "api = $api, apikey = $apikey, deviceId = $deviceId",
      "WHERE id = $id"
    )
    data_for_sql <- list(
      api = api, apikey = apikey, deviceId = deviceId, id = id
    )

    self$dM$config_db$dbSendQuery(query, data_for_sql)

    self$join_config <-
      DBI::dbReadTable(self$dM$config_db$conn(), "Appointments_joinConfig")
    # re-read configuration
    private$set_reactive(self$join_configR, self$join_config)
    # set reactive

    return(id)
  }
)

#' remove join definition from configuration database
#'
#' @param Appointments_obj R6 object
#' @param api name of API (at this stage, can only be 'Join')
#'
#' @return TRUE if removed successfully
#'
#' @export
remove_join_config <- function(dMeasureAppointments_obj, api) {
  dMeasureAppointments_obj$remove_join_config(
    api
  )
}
.public(
  dMeasureAppointments, "remove_join_config",
  function(api) {
    # remove join config item from configuration database

    if (!api %in% (self$join_config %>>% dplyr::pull(api))) {
      # this name already chosen
      warning("'", api, "' not in patient list.")
      return(FALSE)
    }

    query <- paste0(
      "DELETE FROM Appointments_joinConfig WHERE api = ?"
    )
    data_for_sql <- list(api)
    self$dM$config_db$dbSendQuery(query, data_for_sql)

    self$join_config <-
      DBI::dbReadTable(self$dM$config_db$conn(), "Appointments_joinConfig")
    # re-read join configuration list
    private$set_reactive(self$join_configR, self$join_config)

    return(TRUE)
  }
)


################################################
# SMS configuration

#' insert sms_config item to configuration database
#'
#' @param dMeasureAppointments_obj R6 object
#' @param name name of SMS text
#' @param smstext
#'
#' @return ID of created item
#'
#' @export
write_sms_config <- function(
  dMeasureAppointments_obj,
  name, smstext
) {
  dMeasureAppointments_obj$write_sms_config(
    name, smstext
  )
}
.public(
  dMeasureAppointments, "write_sms_config",
  function(
    name, smstext) {
    # write sms_config definition to configuration database

    if (name %in% (self$sms_config$name)) {
      # this name already chosen
      warning("'", name, "' already exists as a defined SMS text.")
      return(NULL)
    }

    if (name == "") {
      # empty name!
      warning("'Name' cannot be empty!")
      return(NULL)
    }

    newID <- max(self$sms_config$id, 0) + 1
    # initially might be an empty set, so need to append a '0'
    # note that 'id' is the identifier in the configuration database

    query <- paste0(
      "INSERT INTO Appointments_smsConfig",
      "(id, name, smstext)",
      "VALUES ($id, $name, $smstext)"
    )
    data_for_sql <- list(
      id = newID, name = name, smstext = smstext
    )

    self$dM$config_db$dbSendQuery(query, data_for_sql)

    self$sms_config <-
      DBI::dbReadTable(self$dM$config_db$conn(), "Appointments_smsConfig")
    # re-read configuratioin
    private$set_reactive(self$sms_configR, self$sms_config)
    # reactive version

    return(newID)
  }
)

#' update sms_config item in configuration database
#'
#' @param dMeasureAppointments_obj R6 object
#' @param id row ID
#' @param name name of SMS text
#' @param smstext
#'
#' @return ID of patient list.
#'  stop errors generated for several different reasons of failure
#'
#' @export
update_sms_config <- function(
  dMeasureAppointments_obj,
  id, name, smstext
) {
  dMeasureAppointments_obj$update_sms_config(
    id, name, smstext
  )
}
.public(
  dMeasureAppointments, "update_sms_config",
  function(
    id, name, smstext) {
    # write updated sms_config information to configuration database

    if (!is.null(id)) {
      # ID has been defined
      current_config <- self$sms_config %>>% dplyr::collect()

      if (name %in% (current_config[current_config$name != name, ]$name)) {
        # this name already chosen with a different ID
        stop("'", name, "' already exists in configuration.")
      }
      # otherwise, we are going to change the name
    } else {
      # id has *not* been defined, we need to find the ID
      # hopefully, the 'name' will help find the ID
      id <- which(name == current_config$name)
      if (length(id) == 0) {
        # length will == 1 if an ID was found
        # ID == numeric(0) if not found
        stop("'", name, "' does not define a SMS text, and id not provided.")
      }
    }

    query <- paste(
      "UPDATE Appointments_smsConfig SET",
      "name = $name, smstext = $smstext",
      "WHERE id = $id"
    )
    data_for_sql <- list(
      name = name, smstext = smstext, id = id
    )

    self$dM$config_db$dbSendQuery(query, data_for_sql)

    self$sms_config <-
      DBI::dbReadTable(self$dM$config_db$conn(), "Appointments_smsConfig")
    # re-read configuration
    private$set_reactive(self$sms_configR, self$sms_config)
    # set reactive

    return(id)
  }
)

#' remove SMS text definition from configuration database
#'
#' @param Appointments_obj R6 object
#' @param name name of SMS text
#'
#' @return TRUE if removed successfully
#'
#' @export
remove_sms_config <- function(dMeasureAppointments_obj, name) {
  dMeasureAppointments_obj$remove_sms_config(
    name
  )
}
.public(
  dMeasureAppointments, "remove_sms_config",
  function(name) {
    # remove SMS config item from configuration database

    if (!name %in% (self$sms_config %>>% dplyr::pull(name))) {
      # this name already chosen
      warning("'", name, "' not in patient list.")
      return(FALSE)
    }

    query <- paste0(
      "DELETE FROM Appointments_smsConfig WHERE name = ?"
    )
    data_for_sql <- list(name)
    self$dM$config_db$dbSendQuery(query, data_for_sql)

    self$sms_config <-
      DBI::dbReadTable(self$dM$config_db$conn(), "Appointments_smsConfig")
    # re-read SMS configuration list
    private$set_reactive(self$sms_configR, self$sms_config)

    return(TRUE)
  }
)
