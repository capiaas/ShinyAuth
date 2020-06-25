# ui <- fluidPage(
#     auth$scripts(),
#     tableOutput('userinfo')
# )

ui <- function(req) {
  fluidPage(
    auth$scripts(),
    tableOutput('userinfo')
  )
}

auth$ui(ui)
