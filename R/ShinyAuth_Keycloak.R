#' Authentication Class for Keycloak authentication
#'
#' Provides methods for protecting a Shiny app against Auth0 as identity provider.
#' @include Abstract_ShinyAuth.R
#' @export
ShinyAuth_Keycloak = R6::R6Class(
  "ShinyAuth_Keycloak",
  inherit = Abstract_ShinyAuth,
  public = list(

    #' @field url_auth_template URL template for auth entrypoint.
    url_auth_template = 'https://{auth_domain}/auth/realms/{realm}/protocol/openid-connect/auth?client_id={client_id}&redirect_uri={encoded_redirect_uri}&response_type=code&state={state}&scope={encoded_scope}',

    #' @field url_token_template URL template for token entrypoint.
    url_token_template = 'https://{auth_domain}/auth/realms/{realm}/protocol/openid-connect/token',

    #' @field url_userinfo_template URL template for userinfo entrypoint.
    url_userinfo_template = 'https://{auth_domain}/auth/realms/{realm}/protocol/openid-connect/userinfo',

    #' @field url_logout_template URL template for logout entrypoint.
    url_logout_template = 'https://{auth_domain}/auth/realms/{realm}/protocol/openid-connect/logout?redirect_uri={encoded_redirect_uri}',

    #' @description
    #' Initialize Keycloak authentication for shiny app
    #'
    #' @param client_id Client ID from IdP
    #' @param client_secret Client Secret from IdP
    #' @param app_url This application entrypoint url
    #' @param auth_domain The domain at which to reach authentication server
    #' @param realm Keycloak user realm
    #' @param ... Other parameters that will get stored in Service Provider settings. Can be used in url templates and in provider classes.
    #'
    #' @return A new authentication object
    initialize = function(client_id, client_secret, app_url, auth_domain, realm, ...) {
      super$initialize(client_id, client_secret, app_url, auth_domain, realm = realm, ...)
    }
  )
)
