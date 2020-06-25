#' Abstract Authentication Class
#'
#' Abstract Authentication Class from which all provider classes must inherit.
Abstract_ShinyAuth = R6::R6Class(
  "Abstract_ShinyAuth",
  public = list(

    #' @field url_auth_template URL template for auth entrypoint.
    url_auth_template = '',

    #' @field url_token_template URL template for token entrypoint.
    url_token_template = '',

    #' @field url_userinfo_template URL template for userinfo entrypoint.
    url_userinfo_template = '',

    #' @field url_logout_template URL template for logoue entrypoint.
    url_logout_template = '',

    #' @description
    #' Initialize Authentication class for shiny app
    #'
    #' @param client_id Client ID from Service Provider
    #' @param client_secret Client Secret from Service Provider
    #' @param app_url This application entrypoint url
    #' @param auth_domain The domain at which to reach authentication server
    #' @param scope Scope is a mechanism in OAuth 2.0 to limit an application's access to a user's account. An application can request one or more scopes, this information is then presented to the user in the consent screen, and the access token issued to the application will be limited to the scopes granted.
    #' @param ... Other parameters that will get stored in Service Provider settings. Can be used in url templates and in provider classes.
    #'
    #' @return A new authentication object
    initialize = function(client_id, client_secret, app_url, auth_domain, scope = 'openid profile email', ...) {
      if(class(self)[1] == "Abstract_ShinyAuth") {
        stop("This is an abstract class and can't be initialized directly. It needs to be extended.")
      }
      encoded_redirect_uri <- utils::URLencode(app_url, reserved = TRUE, repeated = TRUE)
      encoded_scope <- utils::URLencode(scope, reserved = TRUE, repeated = TRUE)
      private$service_provider_settings <- c(as.list(environment()), list(...))
    },

    #' @description
    #' Generate URL for auth entrypoint
    #'
    #' @param state State to use for XSRF detection
    #'
    #' @return URL for auth entrypoint
    url_auth = function(state) {
      params <- c(
        private$service_provider_settings,
        state = utils::URLencode(state, reserved = TRUE, repeated = TRUE)
      )
      glue::glue(self$url_auth_template, .envir = params)
    },

    #' @description
    #' Generate URL for token entrypoint
    #'
    #' @return URL for token entrypoint
    url_token = function() {
      glue::glue(self$url_token_template, .envir = private$service_provider_settings)
    },

    #' @description
    #' Generate URL for userinfo entrypoint
    #'
    #' @return URL for userinfo entrypoint
    url_userinfo = function() {
      glue::glue(self$url_userinfo_template, .envir = private$service_provider_settings)
    },

    #' @description
    #' Generate URL for logout entrypoint
    #'
    #' @param return_url Where user is redirected to after successful logout. Defaults to app url.
    #' @return URL for logout entrypoint
    url_logout = function(return_url = NULL) {
      params <- private$service_provider_settings
      if(!is.null(return_url)) {
        params$encoded_redirect_uri <- utils::URLencode(return_url, reserved = TRUE, repeated = TRUE)
      }
      glue::glue(self$url_logout_template, .envir = params)
    },

    #' @description
    #' Athenticating shiny server wrapper.
    #' It takes a shiny server functions, and starts it once we have successfully authenticated the user.
    #'
    #' @param server Shiny server function
    #' @return Shiny server function
    server = function(server) {
      function(input, output, session) {
        # Why isolate?
        params <- shiny::parseQueryString(isolate(session$clientData$url_search))
        if(!self$has_auth_code(params)) {
          # No auth code detected. Initiate redirect to authentication server
          # Generating random state
          # The state parameter is used to protect against XSRF.
          # >pplication generates a random string and send it to the authorization server using the state parameter.
          # The authorization server send back the state parameter.
          # If both state are the same => OK.
          # If state parameters are differents, someone else has initiated the request.
          state <- self$generate_state()
          # Saving state in browser session storage through websocket message -> JS call.
          session$sendCustomMessage("setOauth2State", state)
          # Redirect user to auth server
          session$sendCustomMessage("redirect", self$url_auth(state = state))
          # Do nothing more until user returns.
          return()
        }
        # We have a auth code. Now we need to validate it, and fetch user info.
        #str(params)
        code <- params$code
        state <- params$state
        observe({
          # Awaiting oauth2 state from browser session storage.
          req(input$oauth2State)
          # Compare incoming sate from auth server with browser storage.
          if(input$oauth2State != state) {
            # If different, we did not initiate login or response is tampered with.
            stop("Oauth2 state not matching")
          }

          access_token <- self$get_access_token(code)
          # We have valid access token, use it to fetch user info
          # Apply all user info to shiny session user property.
          # This is approximate same way Shiny Server Pro would put it.
          session$user  <- self$get_userinfo(access_token)
          # Everything should be ready to fire up the Shiny Application Server
          server(input, output, session)
        }, label = 'Authenticating server wrapper')
      }
    },

    #' @description
    #' Authenticating UI wrapper for shiny ui function
    #' It will serve a minimalist shiny ui if user needs authentication.
    #' Will run given ui if user seems to come back from authenticating server by redirect.
    #' It is then up to the server function to validate and populate ui.
    #'
    #' @param ui Shiny ui function or page object (ex. fluidPage)
    #' @return Shiny ui
    ui = function(ui) {
      function(req) {
        uiValue <- NULL
        if (!self$has_auth_code(shiny::parseQueryString(req$QUERY_STRING))) {
          # We are not authenticated. Serve minimalist ui for redirection.
          uiValue <- shiny::fillPage(self$scripts(), bootstrap = FALSE)
        } else {
          # We beleive we are authenticated, so we serve provided ui.
          # Borrowed from Shiny UI
          if (is.function(ui)) {
            if (length(formals(ui)) > 0) {
              # No corresponding ..stacktraceoff.., this is pure user code
              uiValue <- ..stacktraceon..(ui(req))
            } else {
              # No corresponding ..stacktraceoff.., this is pure user code
              uiValue <- ..stacktraceon..(ui())
            }
          } else {
            uiValue <- ui
          }
        }
        return(uiValue)
      }
    },

    #' @description
    #' Convenience function to wrap single file app in one go
    #'
    #' @param ui Shiny ui function
    #' @param server Shiny server function
    #'
    #' @return A full authenticated shiny app
    app = function(ui, server) {
      shinyApp(ui = self$ui(ui), server = self$server(server))
    },

    #' @description
    #' Injects needed JavaScript into user interface to be able to redirect and store/retrieve state
    #'
    #' @return Script tag
    scripts = function() {
      shiny::tags$head(
        shiny::tags$script(
          shiny::HTML("Shiny.addCustomMessageHandler('redirect', url => location.replace(url));"),
          shiny::HTML("Shiny.addCustomMessageHandler('setOauth2State', state => sessionStorage.setItem('state', state));"),
          shiny::HTML("$(document).on('shiny:sessioninitialized', event => {
            Shiny.setInputValue('oauth2State', sessionStorage.getItem('state'));
            history.replaceState(history.state, '', '/');
          });")
        )
      )
    },

    #' @description
    #' Check if URL query parameters contains an auth code.
    #'
    #' @param params List of parsed url parameters.
    #'
    #' @return Boolean if code exists.
    has_auth_code = function(params) {
      "code" %in% names(params)
    },

    #' @description
    #' Gett access token from athentication server.
    #' Used to get userinfo.
    #'
    #' @param code Authentication code from provider
    #'
    #' @return Access token
    get_access_token = function(code) {
      #' Got into some problems with httr and http/2 with Auth0.
      httr::set_config(httr::config(http_version = 0))
      resp <- httr::POST(self$url_token(),
                         body = list(
                           client_id     = private$service_provider_settings$client_id,
                           grant_type    = "authorization_code",
                           scope         = private$service_provider_settings$scope,
                           redirect_uri  = private$service_provider_settings$app_url,
                           client_secret = private$service_provider_settings$client_secret,
                           code          = code
                         ),
                         encode = "form")
      if(resp$status_code != 200) {
        stop('Error getting access token')
      }
      jsonlite::fromJSON(rawToChar(resp$content))$access_token
    },

    #' @description
    #' Get user info from authentication server
    #' Usually contains info on names, unique keys, email and sometimes roles.
    #' Usually somewhat configurable at service provider, so it could be used to pass along user roles/groups
    #'
    #' @seealso get_access_token
    #
    #' @param access_token Access token
    #'
    #' @return User info
    get_userinfo = function(access_token) {
      resp <- httr::GET(self$url_userinfo(), httr::add_headers(Authorization = paste0("Bearer ", access_token)))
      if(resp$status_code != 200) {
        stop('Error getting userinfo')
      }
      httr::content(resp, encoding = 'UTF-8')
    },

    #' @description
    #' Generating random state
    #' The state parameter is used to protect against XSRF.
    #' Application generates a random string and send it to the authorization server using the state parameter.
    #' The authorization server send back the state parameter.
    #' If both state are the same => OK.
    #' If state parameters are different, someone else has initiated the request.
    #' @todo Could support bookmarking state, to be able to come back to same UI state as we are leaving it.
    #'   Make it possible to pass along a custom state handling class?
    #'
    #' @return 10 random alpahuneric characters
    generate_state = function() {
      paste0(sample(c(LETTERS, letters, 0:9), size = 10, replace = TRUE), collapse = '')
    }
  ),
  private = list(
    service_provider_settings = list()
  )
)
