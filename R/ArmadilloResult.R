#' @include ArmadilloDriver.R ArmadilloConnection.R
NULL

#' Class ArmadilloResult.
#'
#' An Armadillo result implementing the DataSHIELD Interface (DSI)
#' \code{\link{DSResult-class}}.
#'
#' @slot conn The connection used to create this result
#' @slot rval The result
#'
#' @importClassesFrom DSI DSResult
#' @export
#' @keywords internal
methods::setClass("ArmadilloResult",
  contains = "DSResult",
  slots = list(
    conn = "ArmadilloConnection",
    rval = "list"
  )
)

#' Get result info
#'
#' Await completion and get the information about a command
#' (if still available).
#'
#' @param dsObj \code{\link{ArmadilloResult-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#'
#' @return The result information. This should include the R expression
#' being executed (`expression`) and if the query is complete
#' (`{ "status" = "COMPLETED" }`).
#'
#' @importMethodsFrom DSI dsGetInfo
#' @export
methods::setMethod(
  "dsGetInfo", "ArmadilloResult",
  function(dsObj, ...) { # nolint
    if (dsObj@rval$async) {
      .retry_until_last_result(dsObj@conn)
      result <- httr::GET(
        handle = dsObj@conn@handle,
        path = "/lastcommand"
      )
      httr::content(result)
    } else {
      list(status = "COMPLETED")
    }
  }
)

#' Fetch the result
#'
#' Fetch the DataSHIELD operation result.
#'
#' @param res \code{\link{ArmadilloResult-class}} object.
#'
#' @return TRUE if table exists.
#'
#' @importMethodsFrom DSI dsFetch
#' @export
methods::setMethod(
  "dsFetch", "ArmadilloResult",
  function(res) {
    if (res@rval$async) {
      .retry_until_last_result(res@conn)
    } else {
      res@rval$result
    }
  }
)


#' Get whether the operation is completed
#'
#' Get whether the result from a previous assignment or aggregation operation
#' was completed, either with a successful status or a failed one. This call
#' must not wait for the completion, immediate response is expected. Once the
#' result is identified as being completed, the raw result the operation can be
#' got directly.
#'
#' @param res \code{\link{ArmadilloResult-class}} object.
#'
#' @return TRUE if operation is completed.
#'
#' @importMethodsFrom DSI dsIsCompleted
#' @export
methods::setMethod(
  "dsIsCompleted", "ArmadilloResult",
  function(res) { # nolint
    if (res@rval$async) {
      result <- httr::GET(
        handle = res@conn@handle,
        path = "/lastcommand"
      )
      
      .handle_last_command_error(result)
      
      httr::content(result)$status == "COMPLETED"
    } else {
      TRUE
    }
  }
)
