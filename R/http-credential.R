#' Lookup HTTP passwords from the git credential store
#'
#' This requires you have the `git` command line program installed.
#' [git_credential_read] looks up a suitable username/password from
#' the `git-credential` store. If none are available it will attempt
#' to prompt the user for credentials and return those.
#' Use [git_credential_update] to update the credential for a given url.
#'
#' The appearance and security policy of the credential store depends
#' on your the configuration of the version of git, your operating system
#' and which [credential_helper] is in use.
#'
#' @export
#' @family credentials
#' @rdname http_credentials
#' @name http_credentials
#' @aliases credentials
#' @param url target url, possibly including username or path
#' @param save in case the user is prompted for credentials, attempt to
#' remember them.
#' @param verbose print errors from `git credetial` to stdout
git_credential_read <- function(url = "https://github.com", save = TRUE, verbose = TRUE){
  cred <- parse_url(url)
  out <- credential_fill(cred = cred, verbose = verbose)
  if(isTRUE(save))
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

