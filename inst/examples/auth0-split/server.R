server <- function(input, output, session) {
  output$userinfo <- renderTable({session$user})
}

auth$server(server)
