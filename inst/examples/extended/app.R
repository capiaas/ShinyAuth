library(shiny)
library(ShinyAuth)

options(shiny.port = 3838)

ShinyAuth_LocalKeycloak = R6::R6Class(
  'ShinyAuth_LocalKeycloak',
  inherit = ShinyAuth::ShinyAuth_Keycloak,
  public = list(
    url_auth_template = 'http://{auth_domain}/realms/{realm}/protocol/openid-connect/auth?client_id={client_id}&redirect_uri={encoded_redirect_uri}&response_type=code&state={state}&scope={encoded_scope}',
    url_token_template = 'http://{auth_domain}/realms/{realm}/protocol/openid-connect/token',
    url_userinfo_template = 'http://{auth_domain}/realms/{realm}/protocol/openid-connect/userinfo',
    url_logout_template = 'http://{auth_domain}/realms/{realm}/protocol/openid-connect/logout?redirect_uri={encoded_redirect_uri}'
  )
)

auth <- ShinyAuth_LocalKeycloak$new(
  client_id = Sys.getenv('LOCAL_CLIENT_ID'),
  client_secret = Sys.getenv('LOCAL_CLIENT_SECRET'),
  auth_domain = Sys.getenv('LOCAL_AUTH_DOMAIN'),
  realm = Sys.getenv('LOCAL_REALM'),
  app_url = 'http://127.0.0.1:3838/'
)

ui <- fluidPage(
  auth$scripts(),
  tableOutput('userinfo')
)

server <- function(input, output, session) {
  output$userinfo <- renderTable({session$user})
}

# Shortcut for shinyApp(ui = auth$ui(ui), auth$server(server))
auth$app(ui, server)
