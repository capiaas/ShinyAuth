context('Auth0')

test_that('Resulting object has correct classnames', {
  auth <- ShinyAuth_Auth0$new(
    client_id = 'client_id_test',
    client_secret = 'client_secret_test',
    app_url = 'http://127.0.0.1:3838/',
    auth_domain = 'auth.example.org',
    realm = 'realm_test'
  )
  expect_is(auth, c("ShinyAuth_Auth0", "Abstract_ShinyAuth", "R6"))
})


test_that('Testing entrypoint URLs is correct', {
  auth <- ShinyAuth_Auth0$new(
    client_id = 'client_id_test',
    client_secret = 'client_secret_test',
    app_url = 'http://127.0.0.1:3838/',
    auth_domain = 'auth.example.org'
  )
  expect_equal(auth$url_auth(state = 'state_test'), 'https://auth.example.org/authorize?client_id=client_id_test&redirect_uri=http%3A%2F%2F127.0.0.1%3A3838%2F&response_type=code&state=state_test&scope=openid%20profile%20email')
  expect_equal(auth$url_token(), 'https://auth.example.org/oauth/token')
  expect_equal(auth$url_userinfo(), 'https://auth.example.org/userinfo')
  expect_equal(auth$url_logout(), 'https://auth.example.org/v2/logout?client_id=client_id_test&returnTo=http%3A%2F%2F127.0.0.1%3A3838%2F')
})

test_that('Custom logout url works', {
  auth <- ShinyAuth_Auth0$new(
    client_id = 'client_id_test',
    client_secret = 'client_secret_test',
    app_url = 'http://127.0.0.1:3838/',
    auth_domain = 'auth.example.org',
  )
  expect_equal(auth$url_logout(return_url = 'http://logout.example.org/'), 'https://auth.example.org/v2/logout?client_id=client_id_test&returnTo=http%3A%2F%2Flogout.example.org%2F')
  expect_equal(auth$url_logout(federated = TRUE), 'https://auth.example.org/v2/logout?client_id=client_id_test&returnTo=http%3A%2F%2F127.0.0.1%3A3838%2F&federated')
  expect_equal(auth$url_logout(return_url = 'http://logout.example.org/', federated = TRUE), 'https://auth.example.org/v2/logout?client_id=client_id_test&returnTo=http%3A%2F%2Flogout.example.org%2F&federated')

})

test_that('Override scope works', {
  auth <- ShinyAuth_Auth0$new(
    client_id = 'client_id_test',
    client_secret = 'client_secret_test',
    app_url = 'http://127.0.0.1:3838/',
    auth_domain = 'auth.example.org',
    scope = 'openid profile email role_list'
  )
  expect_equal(auth$url_auth(state = 'state_test'), 'https://auth.example.org/authorize?client_id=client_id_test&redirect_uri=http%3A%2F%2F127.0.0.1%3A3838%2F&response_type=code&state=state_test&scope=openid%20profile%20email%20role_list')
})

test_that('We can override url templates', {
  auth <- ShinyAuth_Auth0$new(
    client_id = 'client_id_test',
    client_secret = 'client_secret_test',
    app_url = 'http://127.0.0.1:3838/',
    auth_domain = 'auth.example.org'
  )
  auth$url_auth_template = 'https://{auth_domain}:443/auth?client_id={client_id}&redirect_uri={encoded_redirect_uri}&response_type=code&state={state}&scope={encoded_scope}'
  expect_equal(auth$url_auth(state = 'state_test'), 'https://auth.example.org:443/auth?client_id=client_id_test&redirect_uri=http%3A%2F%2F127.0.0.1%3A3838%2F&response_type=code&state=state_test&scope=openid%20profile%20email')
})
