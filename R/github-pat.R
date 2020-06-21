#' Set your Github Personal Access Token
#'
#' Populates the `GITHUB_PAT` environment variable using the [git_credential][http_credentials]
#' manager, which `git` itself uses for storing passwords. The credential manager
#' will automatically and securely prompt the user for credentials when needed.
#'
#' Use this function in your [.Rprofile][Startup] script to automatically set
#' `GITHUB_PAT` for each R session without hardcoding your any secrets in plain text.
#'
#' @export
#' @param force_new forget existing pat, always ask for new one.
#' @param validate checks with the github API that this token works. Defaults to
#' `TRUE` only in an interactive R session (not when running e.g. CMD check).
#' @param verbose prints a message showing the credential helper and PAT owner.
#' @return returns TRUE if a valid GITHUB_PAT was set, and FALSE in case of failure.
set_github_pat <- function(force_new = FALSE, validate = interactive(), verbose = validate){
  if(isTRUE(force_new))
    git_credential_forget('https://token@github.com')
  if(isTRUE(verbose))
    message("If prompted for GitHub credentials, enter your PAT in the password field")
  askpass <- Sys.getenv('GIT_ASKPASS')
  if(nchar(askpass)){
    # Hack to override prompt sentence to say "Token" instead of "Password"
    Sys.setenv(GIT_ASKTOKEN = askpass)
    Sys.setenv(GIT_ASKPASS = system.file('ask_token.sh', package = 'credentials', mustWork = TRUE))
    on.exit(Sys.setenv(GIT_ASKPASS = askpass), add = TRUE)
    on.exit(Sys.unsetenv('GIT_ASKTOKEN'), add = TRUE)
  }
  for(i in 1:3){
    # The username doesn't have to be real, Github seems to ignore username for PATs
    pat_user <- Sys.getenv("GITHUB_PAT_USER", 'token')
    cred <- git_credential_ask(sprintf('https://%s@github.com', pat_user), verbose = verbose)
    if(length(cred$password)){
      if(nchar(cred$password) < 40){
        message("Please enter a token in the password field, not your master password! Let's try again :-)")
        message("To generate a new token, visit: https://github.com/settings/tokens")
        credential_reject(cred)
        next
      }
      if(isTRUE(validate)) {
        hx <- curl::handle_setheaders(curl::new_handle(), Authorization = paste("token", cred$password))
        req <- curl::curl_fetch_memory("https://api.github.com/user", handle = hx)
        if(req$status_code >= 400){
          message("Authentication failed. Token invalid.")
          credential_reject(cred)
          next
        }
        if(verbose == TRUE){
          data <- jsonlite::fromJSON(rawToChar(req$content))
          helper <- tryCatch(credential_helper_get()[1], error = function(e){"??"})
          message(sprintf("Using GITHUB_PAT from %s (credential helper: %s)", data$name, helper))
        }
      }
      return(Sys.setenv(GITHUB_PAT = cred$password))
    }
  }
  if(verbose == TRUE){
    message("Failed to find a valid GITHUB_PAT after 3 attempts")
  }
  return(FALSE)
}
