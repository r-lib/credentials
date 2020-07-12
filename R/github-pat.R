#' Set your Github Personal Access Token
#'
#' Populates the `GITHUB_PAT` environment variable using a token stored with the
#' [git_credential][http_credentials] manager, which `git` itself uses for
#' storing passwords. The credential manager returns stored credentials if
#' available, and securely prompts the user for credentials when needed.
#'
#' Packages that require a `GITHUB_PAT` can call this function to automatically
#' set the `GITHUB_PAT` when needed. Users may call this function in their
#' [.Rprofile][Startup] script to automatically set `GITHUB_PAT` for each R
#' session without hardcoding any tokens on disk in plain-text.
#'
#' @export
#'
#' @param url A URL that conveys login information, such as protocol (e.g.
#'   "https"), remote hostname (e.g. "github.com" or
#'   "github.starkindustries.com"), and username (e.g. "tony"). See the
#'   input/output documentation for the [`git-credential`
#'   command](https://git-scm.com/docs/git-credential#IOFMT) for acceptable
#'   formats.
#' @param force_new Clear any existing, matching PAT from the store and
#'   re-prompt the user.
#' @param validate Whether to query the `/user` endpoint of the GitHub API to
#'   confirm that the token works. Defaults to `TRUE` only in an interactive R
#'   session (not when running e.g. CMD check). This API call respects a host
#'   that is not "github.com", i.e. it works for GitHub Enterprise.
#' @param verbose Whether to print a message revealing, e.g., the host, the PAT
#'   owner's username, and the associated credential helper.
#'
#' @return Returns `TRUE` if `GITHUB_PAT` was set, and FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' set_github_pat()
#'
#' set_github_pat("https://github.starkindustries.com")
#'
#' set_github_pat("https://tony@github.starkindustries.com")
#' }
set_github_pat <- function(url = "https://token@github.com",
                           force_new = FALSE,
                           validate = interactive(),
                           verbose = validate){
  # TODO: insert GITHUB_PAT_USER, if defined
  if(isTRUE(force_new))
    git_credential_forget(url)
  if(isTRUE(verbose))
    message("If prompted for GitHub credentials, enter your PAT in the password field")
  askpass <- Sys.getenv('GIT_ASKPASS')
  if(nchar(askpass)){
    # Hack to override prompt sentence to say "Token" instead of "Password"
    Sys.setenv(GIT_ASKTOKEN = askpass)
    Sys.setenv(GIT_ASKPASS = system.file('ask_token.sh', package = 'credentials', mustWork = TRUE))
    Sys.setenv(
      GIT_ASKTOKEN_NAME = sprintf('%s Personal Access Token (PAT)', parse_url(url)["host"])
    )
    on.exit(Sys.setenv(GIT_ASKPASS = askpass), add = TRUE)
    on.exit(Sys.unsetenv('GIT_ASKTOKEN_NAME'), add = TRUE)
    on.exit(Sys.unsetenv('GIT_ASKTOKEN'), add = TRUE)
  }
  for(i in 1:3){
    cred <- git_credential_ask(url, verbose = verbose)
    if(length(cred$password)){
      if(nchar(cred$password) < 40){
        message("Please enter a token in the password field, not your master password! Let's try again :-)")
        message("To generate a new token, visit: https://github.com/settings/tokens")
        credential_reject(cred)
        next
      }
      if(isTRUE(validate)) {
        hx <- curl::handle_setheaders(curl::new_handle(), Authorization = paste("token", cred$password))
        host <- parse_url(url)["host"]
        api_url <- switch(
          host,
          `github.com` = "https://api.github.com/user",
          sprintf("https://%s/api/v3/user", host)
        )
        req <- curl::curl_fetch_memory(api_url, handle = hx)
        if(req$status_code >= 400){
          # TODO: ideally we would reveal more about the error; we have the info
          message("Authentication failed. Token invalid.")
          credential_reject(cred)
          next
        }
        if(verbose == TRUE){
          data <- jsonlite::fromJSON(rawToChar(req$content))
          helper <- tryCatch(credential_helper_get()[1], error = function(e){"??"})
          message(sprintf(
            "Using GITHUB_PAT for %s@%s (credential helper: %s)",
            data$login, cred$host, helper
          ))
        }
      }
      return(Sys.setenv(GITHUB_PAT = cred$password, GITHUB_PAT_USER = cred$username))
    }
  }
  if(verbose == TRUE){
    message("Failed to find a valid GITHUB_PAT after 3 attempts")
  }
  return(FALSE)
}

message <- function(...){
  base::message(...)
  utils::flush.console()
}
