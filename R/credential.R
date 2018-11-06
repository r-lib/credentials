#' Lookup passwords from the git credential store
#'
#' This requires you have the `git` command line program installed.
#' [read_http_credential] looks up a suitable username/password from
#' the `git-credential` store. If none are available it will attempt
#' to prompt the user for credentials and return those.
#' Use [save_http_credential] to update the credential for a given url.
#'
#' @export
#' @family credentials
#' @rdname http_credential
#' @name credential
#' @aliases credentials
#' @param url target url, possibly including username or path
#' @param git path of the `git` command line program
#' @param verbose print errors from `git credetial` to stdout
read_http_credential <- function(url = "https://github.com", git = "git", verbose = TRUE){
  cred <- parse_url(url)
  out <- credential_fill(cred = cred, git = git, verbose = verbose)
  c(out, list(git = git))
}

#' @export
#' @rdname http_credential
save_http_credential <- function(url = "https://github.com", git = "git", verbose = TRUE){
  cred <- parse_url(url)
  credential_reject(cred)
  out <- credential_fill(cred = cred, git = git, verbose = verbose)
  credential_approve(out)
}

