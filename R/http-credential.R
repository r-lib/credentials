#' Load and store git HTTPS credentials
#'
#' This requires you have the `git` command line program installed.
#' [git_credential_read] looks up a suitable username/password from
#' the [`git-credential` store](https://git-scm.com/docs/gitcredentials).
#' If none are available it will attempt
#' to prompt the user for credentials and optionally also save those.
#'
#' The appearance and security policy of the credential store depends
#' on your version of git, your operating system and which
#' [credential_helper] is in use. On Windows and MacOS the credentials
#' are stored in the system password manager by default.
#'
#' It should be assumed that reading credentials always involves user
#' interaction. The user may be asked to unlock the system keychain or
#' enter new credentials. In reality, user interaction is usually only
#' required on the first authentication attempt, but the security policy
#' of most credential helpers prevent you from programatically testing
#' if the credentials are already unlocked.
#'
#' @export
#' @family credentials
#' @rdname http_credentials
#' @name http_credentials
#' @aliases credentials
#' @param url target url, possibly including username or path
#' @param save in case the user is prompted for credentials, attempt to
#' remember them.
#' @param verbose print errors from `git credential` to stdout
git_credential_read <- function(url = "https://github.com", save = TRUE, verbose = TRUE){
  cred <- parse_url(url)
  out <- credential_fill(cred = cred, verbose = verbose)
  if(isTRUE(save) && length(out) && length(out$password) && !is.na(out$password))
    credential_approve(out, verbose = verbose)
  out
}

#' @export
#' @rdname http_credentials
git_credential_update <- function(url = "https://github.com", verbose = TRUE){
  cred <- parse_url(url)
  credential_reject(cred)
  out <- credential_fill(cred = cred, verbose = verbose)
  credential_approve(out)
}

#' @export
#' @rdname http_credentials
git_credential_forget <- function(url = "https://github.com", verbose = TRUE){
  cred <- parse_url(url)
  credential_reject(cred)
}

