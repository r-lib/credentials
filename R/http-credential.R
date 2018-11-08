#' Lookup HTTP passwords from the git credential store
#'
#' This requires you have the `git` command line program installed.
#' [git_credential_read] looks up a suitable username/password from
#' the `git-credential` store. If none are available it will attempt
#' to prompt the user for credentials and return those.
#' Use [git_credential_update] to update the credential for a given url.
#'
#' @export
#' @family credentials
#' @rdname http_credentials
#' @name http_credentials
#' @aliases credentials
#' @param url target url, possibly including username or path
#' @param verbose print errors from `git credetial` to stdout
git_credential_read <- function(url = "https://github.com", verbose = TRUE){
  cred <- parse_url(url)
  credential_fill(cred = cred, verbose = verbose)
}

#' @export
#' @rdname http_credentials
git_credential_update <- function(url = "https://github.com", verbose = TRUE){
  cred <- parse_url(url)
  credential_reject(cred)
  out <- credential_fill(cred = cred, verbose = verbose)
  credential_approve(out)
}

