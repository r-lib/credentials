#' Set your Github Personal Access Token
#'
#' Populates the `GITHUB_PAT` environment variable using the [git_credential][http_credentials]
#' manager, which `git` itself uses for storing passwords. The credential manager
#' will automatically and securily prompt the user for credentials when needed.
#'
#' Use this function in your [.Rprofile][Startup] script to automatically set
#' `GITHUB_PAT` for each R session without hardcoding your any secrets in plain text.
#'
#' @export
#' @param force_new forget existing pat, always ask for new one
#' @param verbose print some debugging messages
set_github_pat <- function(force_new = FALSE, verbose = TRUE){
  if(isTRUE(force_new))
    git_credential_forget('https://token@github.com')
  for(i in 1:3){
    cred <- git_credential_ask('https://token@github.com', verbose = verbose)
    if(length(cred$password)){
      if(nchar(cred$password) < 40){
        message("Please enter a token in the password field, not your master password! Let's try again :-)")
        message("To generate a new token, visit: https://github.com/settings/tokens")
        credential_reject(cred)
      } else {
        hx <- curl::handle_setheaders(curl::new_handle(), Authorization = paste("token", cred$password))
        req <- curl::curl_fetch_memory("https://api.github.com/user", handle = hx)
        if(req$status >= 400){
          message("Authentication failed. Token invalid.")
          credential_reject(cred)
        } else {
          data <- jsonlite::fromJSON(rawToChar(req$content))
          if(verbose == TRUE){
            Sys.setenv(GITHUB_PAT = cred$password)
            helper <- tryCatch(credential_helper_get()[1], error = "??")
            message(sprintf("Using GITHUB_PAT from %s (credential helper: %s)", data$name, helper))
            return(invisible())
          }
        }
      }
    }
  }
}
