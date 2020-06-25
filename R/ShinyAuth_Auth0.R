#' Authentication Class for Auth0 authentication
#'
#' Provides methods for protecting a Shiny app against Auth0 as identity provider.
#' @include Abstract_ShinyAuth.R
#' @export
ShinyAuth_Auth0 = R6::R6Class(
  "ShinyAuth_Auth0",
  inherit = Abstract_ShinyAuth,
  public = list(

    #' @field url_auth_template URL template for auth entrypoint.
    url_auth_template = 'https://{auth_domain}/authorize?client_id={client_id}&redirect_uri={encoded_redirect_uri}&response_type=code&state={state}&scope={encoded_scope}',

    #' @field url_token_template URL template for token entrypoint.
    url_token_template = 'https://{auth_domain}/oauth/token',

    #' @field url_userinfo_template URL template for userinfo entrypoint.
    url_userinfo_template = 'https://{auth_domain}/userinfo',

    #' @field url_logout_template URL template for logoue entrypoint.
    url_logout_template = 'https://{auth_domain}/v2/logout?client_id={client_id}&returnTo={encoded_redirect_uri}{federated}',

    #' @description
    #' Initialize Auth0 authentication for shiny app
    #'
    #' @param client_id Client ID from Service Provider
    #' @param client_secret Client Secret from Service Provider
    #' @param app_url This application entrypoint url
    #' @param auth_domain The domain at which to reach authentication server
    #' @param ... Other parameters that will get stored in Service Provider settings. Can be used in url templates and in provider classes.
    #'
    #' @return A new authentication object
    initialize = function(client_id, client_secret, app_url, auth_domain, ...) {
      super$initialize(client_id, client_secret, app_url, auth_domain, ...)
    },

    #' @description
    #' Get log out url
    #'
    #' @param return_url The url to redirect to when user is loged out.
    #' @param federated To log out user from federated identity provider. (Not just this app)
    #'
    #' @return URL for logout entrypoint
    url_logout = function(return_url = NULL, federated = FALSE) {
      params <- private$service_provider_settings
      if(!is.null(return_url)) {
        params$encoded_redirect_uri <- utils::URLencode(return_url, reserved = TRUE, repeated = TRUE)
      }
      if(federated) {
        params$federated <- '&federated'
      } else {
        params$federated <- ''
      }
      glue::glue(self$url_logout_template, .envir = params)
    }
  )
)
Abstract_ShinyAuth$new
