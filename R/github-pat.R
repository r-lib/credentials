#' Set your Github Personal Access Token
#'
#' Automatically sets the `GITHUB_PAT` environment variable. This function
#' calls out to the [git_credential][http_credentials] store, which
#' automatically and securely prompts the user for credentials if needed.
#'
#' @export
#' @param force_new forget existing pat, always ask for new one
#' @param verbose print some debugging messages
set_github_pat <- function(force_new = FALSE, verbose = TRUE){
  if(isTRUE(force_new))
    git_credential_forget('https://github.com')
  for(i in 1:3){
    cred <- git_credential_ask('https://github.com', verbose = verbose)
    if(length(cred$password)){
      if(nchar(cred$password) < 40){
        message("Please enter a PAT in the password field! Not your master password!")
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
            message(sprintf("Using GITHUB_PAT from %s", data$name))
            return(invisible())
          }
        }
      }
    }
  }
}
