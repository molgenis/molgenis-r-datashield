driver <- armadillo()
# Assume 'mock', 'expect_s4_class', 'expect_equal', 'expect_args', 'expect_called', 'expect_error', 'add_headers', 'packageVersion', 'DSMolgenisArmadillo' are available from loaded packages.
# For simplicity, assuming 'cookies', 'handle' are defined earlier in the test script scope,
# e.g.,
# cookies <- list(name = "JSESSIONID", value = "abcde")
# handle <- "mock_handle"


test_that("dsConnect returns an ArmadilloConnection", {
  response <- list(status_code = 200)

  fake_cookies <- data.frame(
    name  = "JSESSIONID",
    value = "abcde",
    stringsAsFactors = FALSE
  )

  httr_post    <- mock(response, cycle = TRUE)
  httr_cookies <- mock(fake_cookies)

  with_mocked_bindings({
    result <- dsConnect(
      driver,
      url      = "https://example.org",
      username = "admin",
      password = "admin",
      name     = "test"
    )
  },
  POST    = httr_post,
  cookies = httr_cookies
  )

  expect_s4_class(result, "ArmadilloConnection")
  expect_true(inherits(result@handle, "curl_handle") || inherits(result@handle, "handle"))
  expect_equal(result@cookies$name,  "JSESSIONID")
  expect_equal(result@cookies$value, "abcde")

  expect_args(httr_post, 1,
              handle = result@handle,
              path   = "/select-profile",
              body   = "default",
              config = add_headers("Authorization" = "Basic YWRtaW46YWRtaW4=")
  )
})

test_that("dsConnect selects profile if one is provided", {
  response <- list(status_code = 200)
  httr_post <- mock(response, cycle = TRUE)

  fake_cookies <- data.frame(
    name  = "JSESSIONID",
    value = "abcde",
    stringsAsFactors = FALSE
  )

  httr_post    <- mock(response, cycle = TRUE)
  httr_cookies <- mock(fake_cookies)

  with_mocked_bindings({
    result <- dsConnect(driver,
      url = "https://example.org",
      username = "admin",
      password = "admin",
      profile = "foo",
      name = "test"
    )
  },
  POST = httr_post,
  cookies = httr_cookies,
  )
  expect_s4_class(result, "ArmadilloConnection")
  expect_true(inherits(result@handle, "curl_handle") || inherits(result@handle, "handle"))
  expect_equal(result@cookies$name, "JSESSIONID")
  expect_equal(result@cookies$value, "abcde")

  expect_args(httr_post, 1,
    handle = result@handle,
    path = "/select-profile",
    body = "foo",
    config = add_headers("Authorization" = "Basic YWRtaW46YWRtaW4=")
  )
})

test_that("dsConnect returns an ArmadilloConnection", {
  response <- list(status_code = 200)

  fake_cookies <- data.frame(
    name  = "JSESSIONID",
    value = "abcde",
    stringsAsFactors = FALSE
  )

  httr_post    <- mock(response, cycle = TRUE)
  httr_cookies <- mock(fake_cookies)

  with_mocked_bindings({
    result <- dsConnect(driver,
      url = "https://example.org",
      username = "admin",
      password = "admin",
      name = "test"
    )
  },
  POST = httr_post,
  cookies = httr_cookies,
  )
  expect_s4_class(result, "ArmadilloConnection")
  inherits(result@handle, "curl_handle") || inherits(result@handle, "handle")
  expect_equal(result@cookies$name, "JSESSIONID")
  expect_equal(result@cookies$value, "abcde")

  expect_args(httr_post, 1,
    handle = result@handle,
    path = "/select-profile",
    body = "default",
    config = add_headers("Authorization" = "Basic YWRtaW46YWRtaW4=")
  )
})

test_that("dsConnect can log in with bearer token", {
  response <- list(status_code = 200)

  fake_cookies <- data.frame(
    name  = "JSESSIONID",
    value = "abcde",
    stringsAsFactors = FALSE
  )

  httr_post    <- mock(response, cycle = TRUE)
  httr_cookies <- mock(fake_cookies)

  with_mocked_bindings({
    result <- dsConnect(driver,
      url = "https://example.org",
      token = "abcde",
      name = "test"
    )
  },
  POST = httr_post,
  cookies = httr_cookies,
  )
  expect_s4_class(result, "ArmadilloConnection")
  inherits(result@handle, "curl_handle") || inherits(result@handle, "handle")
  expect_equal(result@cookies$name, "JSESSIONID")
  expect_equal(result@cookies$value, "abcde")

  expect_args(httr_post, 1,
    handle = result@handle,
    path = "/select-profile",
    body = "default",
    config = add_headers("Authorization" = "Bearer abcde")
  )
})

test_that("dsConnect restores user workspace", {
  response <- list(status_code = 200)
  ok <- list(status_code = 200)

  fake_cookies <- data.frame(
    name  = "JSESSIONID",
    value = "abcde",
    stringsAsFactors = FALSE
  )

  httr_post    <- mock(response, cycle = TRUE)
  httr_cookies <- mock(fake_cookies)

  with_mocked_bindings({
    result <- dsConnect(driver,
      url = "https://example.org",
      username = "admin",
      password = "admin",
      name = "test",
      restore = "keepit"
    )
  },
  POST = httr_post,
  cookies = httr_cookies,
  )
  expect_called(httr_post, 2)
  expect_args(httr_post, 1,
    handle = result@handle,
    path = "/select-profile",
    body = "default",
    config =
      add_headers("Authorization" = "Basic YWRtaW46YWRtaW4=")
  )
  expect_args(httr_post, 2,
    handle = result@handle,
    query = list(id = "keepit"),
    path = "/load-workspace",
    config = add_headers("Authorization" = "Basic YWRtaW46YWRtaW4=")
  )
})

test_that("dsConnect explains when you cannot load all workspaces", {
  expect_error(
    dsConnect(driver),
    "argument \"url\" is missing, with no default"
  )
})
