library(shiny)
library(ShinyAuth)

options(shiny.port = 3838)

auth <- ShinyAuth_Keycloak$new(
  client_id = Sys.getenv('KEYCLOAK_CLIENT_ID'),
  client_secret = Sys.getenv('KEYCLOAK_CLIENT_SECRET'),
  auth_domain = Sys.getenv('KEYCLOAK_AUTH_DOMAIN'),
  realm = Sys.getenv('KEYCLOAK_REALM'),
  app_url = 'http://127.0.0.1:3838/',
  scope = 'openid profile email role_list'
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
