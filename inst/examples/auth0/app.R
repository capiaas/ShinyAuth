library(shiny)
library(ShinyAuth)

options(shiny.port = 3000)

auth <- ShinyAuth_Auth0$new(
  client_id = Sys.getenv('AUTH0_CLIENT_ID'),
  client_secret = Sys.getenv('AUTH0_CLIENT_SECRET'),
  auth_domain = Sys.getenv('AUTH0_AUTH_DOMAIN'),
  app_url = 'http://127.0.0.1:3000/'
)

ui <- fluidPage(
  auth$scripts(),
  tableOutput('userinfo')
)

server <- function(input, output, session) {
  output$userinfo <- renderTable({session$user})
}

auth$app(ui, server)
