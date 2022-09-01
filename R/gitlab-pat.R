#' Set your Gitlab Personal Access Token
#'
#' Populates the `GITLAB_PAT` environment variable using the [git_credential][http_credentials]
#' manager, which `git` itself uses for storing passwords. The credential manager
#' returns stored credentials if available, and securely prompt the user for
#' credentials when needed.
#'
#' Packages that require a `GITLAB_PAT` can call this function to automatically
#' set the `GITLAB_PAT` when needed. Users may call this function in their
#' [.Rprofile][Startup] script to automatically set `GITLAB_PAT` for each R
#' session without hardcoding any tokens on disk in plain-text.
#'
#' @export
#' @param instance gitlab instance URL (defaults to 'gitlab.com')
#' @param force_new forget existing pat, always ask for new one.
#' @param validate checks with the gitlab API that this token works. Defaults to
#' `TRUE` only in an interactive R session (not when running e.g. CMD check).
#' @param verbose prints a message showing the credential helper and PAT owner.
#' @return Returns `TRUE` if a valid GITLAB_PAT was set, and FALSE if not.
set_gitlab_pat <- function(instance = "gitlab.com", force_new = FALSE, validate = interactive(), verbose = validate){
  pat_user <- Sys.getenv("GITLAB_PAT_USER", 'PersonalAccessToken')
  pat_url <- sprintf('https://%s@%s', pat_user, instance)
  if(isTRUE(force_new))
    git_credential_forget(pat_url)
  if(isTRUE(verbose))
    message("If prompted for GitLab credentials, enter your PAT in the password field")
  askpass <- Sys.getenv('GIT_ASKPASS')
  if(nchar(askpass)){
    # Hack to override prompt sentence to say "Token" instead of "Password"
    Sys.setenv(GIT_ASKTOKEN = askpass)
    Sys.setenv(GIT_ASKPASS = system.file('ask_token.sh', package = 'credentials', mustWork = TRUE))
    Sys.setenv(GIT_ASKTOKEN_NAME = 'Personal Access Token (PAT)')
    on.exit(Sys.setenv(GIT_ASKPASS = askpass), add = TRUE)
    on.exit(Sys.unsetenv('GIT_ASKTOKEN_NAME'), add = TRUE)
    on.exit(Sys.unsetenv('GIT_ASKTOKEN'), add = TRUE)
  }
  for(i in 1:3){
    # The username doesn't have to be real, Github seems to ignore username for PATs
    cred <- git_credential_ask(pat_url, verbose = verbose)
    if(length(cred$password)){
      if(nchar(cred$password) < 20){
        message("Please enter a token in the password field, not your master password! Let's try again :-)")
        message(paste0("To generate a new token, visit: https://", instance, "/-/profile/personal_access_tokens"))
        credential_reject(cred)
        next
      }
      if(isTRUE(validate)) {
        hx <- curl::handle_setheaders(curl::new_handle(), `PRIVATE-TOKEN` = cred$password)
        req <- curl::curl_fetch_memory(paste0("https://", instance, "/api/v4/personal_access_tokens"), handle = hx)
        if(req$status_code >= 400){
          message("Authentication failed. Token invalid.")
          credential_reject(cred)
          next
        }
        if(verbose == TRUE){
          data <- jsonlite::fromJSON(rawToChar(req$content))
          helper <- tryCatch(credential_helper_get()[1], error = function(e){"??"})
          message(sprintf("Using GITLAB_PAT from %s (credential helper: %s)", data$name, helper))
        }
      }
      return(Sys.setenv(GITLAB_PAT = cred$password))
    }
  }
  if(verbose == TRUE){
    message("Failed to find a valid GITLAB_PAT after 3 attempts")
  }
  return(FALSE)
}

message <- function(...){
  base::message(...)
  utils::flush.console()
}
