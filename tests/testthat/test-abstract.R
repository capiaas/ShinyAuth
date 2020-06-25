context("Abstract")

test_that('Initializing of abstract class fails', {
  expect_error(Abstract_ShinyAuth$new(
    client_id = 'client_id_test',
    client_secret = 'client_secret_test',
    app_url = 'http://127.0.0.1:3838/',
    auth_domain = 'auth.example.org'
  ), regexp = "This is an abstract class and can't be initialized directly. It needs to be extended.")
})


test_that('State generating function is containing 10 allphanumumeric chars', {
  ShinyAuth_Mock <- R6::R6Class('ShinyAuth_Mock', inherit = Abstract_ShinyAuth)
  auth <- ShinyAuth_Mock$new(
    client_id = 'client_id_test',
    client_secret = 'client_secret_test',
    app_url = 'http://127.0.0.1:3838/',
    auth_domain = 'auth.example.org'
  )
  state <- auth$generate_state()
  expect_match(state, "[a-zA-Z0-9]{10}")
})

test_that('Detection of authentication code in parameters', {
  ShinyAuth_Mock <- R6::R6Class('ShinyAuth_Mock', inherit = Abstract_ShinyAuth)
  auth <- ShinyAuth_Mock$new(
    client_id = 'client_id_test',
    client_secret = 'client_secret_test',
    app_url = 'http://127.0.0.1:3838/',
    auth_domain = 'auth.example.org'
  )
  params <- list(
    state = 'test-state'
  )
  expect_false(auth$has_auth_code(params))
  params$code = 'test-code'
  expect_true(auth$has_auth_code(params))
})
