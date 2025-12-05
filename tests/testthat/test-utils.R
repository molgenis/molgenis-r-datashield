test_that(".handle_last_command_error throws error message", {
  ok  <- list(status_code = 200)
  get <- mock(ok)
  json_returned  <- mock(list(status = "FAILED", message = "Error", expression = "value={broken command"))
  json_formatted <- mock(list(message = "informative error message"))

  setClass("connection", slots = list(handle = "character",
                                      token  = "character",
                                      name   = "character"))
  connection <- new("connection", handle = "test", token = "token", name = "test_cohort")

  expect_error(
    with_mocked_bindings({
      .handle_last_command_error(connection)
    },
    GET      = get,
    content  = json_returned,
    fromJSON = json_formatted
    ),
    "Error"
  )

  expect_args(
    get, 1,
    handle = connection@handle,
    path   = "/lastcommand",
    config = add_headers(c(
      "Authorization" = paste0("Bearer ", connection@token)
    ))
  )
  expect_args(json_returned, 1, ok)
})

test_that(".handle_last_command_error only works if status is FAILED", {
  setClass("connection", slots = list(handle = "character",
                                      token  = "character"))
  connection <- new("connection", handle = "test", token = "token")
  ok   <- list(status_code = 200)
  get  <- mock(ok)
  content <- mock(list(status = "COMPLETED"))

  with_mocked_bindings({
    .handle_last_command_error(connection)
  },
  GET     = get,
  content = content
  )

  expect_args(
    get, 1,
    handle = connection@handle,
    path   = "/lastcommand",
    config = add_headers(c(
      "Authorization" = paste0("Bearer ", connection@token)
    ))
  )
})

test_that(".handle_request_error handles 401", {
  expect_error(
    .handle_request_error(list(status_code = 401)),
    "Unauthorized"
  )
})

test_that(".handle_request_error handles 400", {
  response     <- list(status_code = 400)
  httr_content <- mock(list(message = "Error"))

  with_mocked_bindings({
    expect_error(
      .handle_request_error(response),
      "Bad request: Error"
    )
  },
  content = httr_content
  )
})

test_that(".handle_request_error handles 500", {
  response     <- list(status_code = 500)
  httr_content <- mock(message = "Something went wrong while reading/writing in the storage")

  with_mocked_bindings({
    expect_error(
      .handle_request_error(response),
      "Internal server error: Something went wrong while reading/writing in the storage"
    )
  },
  content = httr_content
  )
})

test_that(".unlist_character_list handles empty list", {
  expect_equal(.unlist_character_list(list()), character())
})

test_that(".unlist_character_list unnests list", {
  input <- list(foo = list(a = "a", b = list(c = "d")))
  expect_equal(
    .unlist_character_list(input),
    c("foo.a" = "a", "foo.b.c" = "d")
  )
})

test_that(".deparse deparses simple calls to a single string", {
  expr <- quote(a + b * 2)

  result <- .deparse(expr)

  expect_type(result, "character")
  expect_length(result, 1L)
  # For simple expressions, .deparse should just match base::deparse collapsed
  expect_equal(result, paste(deparse(expr), collapse = "\n"))
})

test_that(".deparse collapses multi-line deparse output with newlines", {
  expr <- quote(some_really_long_function_name(arg1, arg2, arg3))

  # Force deparse() to behave as if it produced multiple lines,
  # independent of the actual R version's formatting
  with_mocked_bindings({
    result <- .deparse(expr)
  },
  deparse = function(e) c("line1 <- part1", "line2 <- part2")
  )

  expect_identical(result, "line1 <- part1\nline2 <- part2")
})

test_that(".deparse returns character input unchanged", {
  expr <- "already a character vector"

  result <- .deparse(expr)

  expect_identical(result, expr)
})

test_that(".deparse errors for non-language, non-character input", {
  expr <- 42

  expect_error(
    .deparse(expr),
    "Invalid expression"
  )
})

test_that(".retry_until_last_result handles 404 by retrieving lastcommand", {
  not_found      <- list(status_code = 404)
  ok             <- list(status_code = 200)
  httr_retry     <- mock(not_found)
  httr_get       <- mock(ok)
  json_returned  <- mock(list(status = "FAILED", message = "Error", expression = "value={broken command"))
  json_formatted <- mock(list(message = "informative error message"))

  setClass("connection", slots = list(handle = "character",
                                      token  = "character",
                                      name   = "character"))
  connection <- new("connection", handle = "test", token = "token", name = "test_cohort")

  expect_error(
    with_mocked_bindings({
      .retry_until_last_result(connection)
    },
    GET      = httr_get,
    content  = json_returned,
    RETRY    = httr_retry,
    fromJSON = json_formatted
    ),
    "Command 'broken command' failed on test_cohort: Error whilst evaluating"
  )

  expect_args(
    httr_get, 1,
    handle = connection@handle,
    path   = "/lastcommand",
    config = add_headers(c(
      "Authorization" = paste0("Bearer ", connection@token)
    ))
  )
  expect_args(json_returned, 1, ok)
})

test_that(".retry_until_last_result handles 404 by retrieving lastcommand", {
  not_found      <- list(status_code = 404)
  ok             <- list(status_code = 200)
  httr_retry     <- mock(not_found)
  httr_get       <- mock(ok)
  json_returned  <- mock(list(status = "FAILED", message = "Error", expression = "value={broken command"))
  json_formatted <- mock(list(message = "informative error message"))

  setClass("connection", slots = list(handle = "character",
                                      token  = "character",
                                      name   = "character"))
  connection <- new("connection", handle = "test", token = "token", name = "test_cohort")

  expect_error(
    with_mocked_bindings({
      .retry_until_last_result(connection)
    },
    GET      = httr_get,
    content  = json_returned,
    RETRY    = httr_retry,
    fromJSON = json_formatted
    ),
    "Command 'broken command' failed on test_cohort: Error whilst evaluating"
  )

  expect_args(
    httr_get, 1,
    handle = connection@handle,
    path   = "/lastcommand",
    config = add_headers(c(
      "Authorization" = paste0("Bearer ", connection@token)
    ))
  )
  expect_args(json_returned, 1, ok)
})

test_that(".handle_last_command_error handles 404 error", {
  get           <- mock(list(status = 404))
  json_returned <- mock(list(status = "FAILED", message = "Error", expression = "value={broken command"))
  json_formatted <- mock(list(message = "informative error message"))

  setClass("connection", slots = list(handle = "character",
                                      token  = "character",
                                      name   = "character"))
  connection <- new("connection", handle = "test", token = "token", name = "test_cohort")

  expect_silent(
    with_mocked_bindings({
      .handle_last_command_error(connection)
    },
    GET      = get,
    content  = json_returned,
    fromJSON = json_formatted
    )
  )
})
