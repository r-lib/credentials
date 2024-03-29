#' Credential Helpers
#'
#' Git supports several back-end stores for HTTPS credentials called
#' helpers. Default helpers include `cache` and `store`, see the
#' [git-credentials](https://git-scm.com/docs/gitcredentials) manual
#' page for details.
#'
#' @export
#' @rdname credential_helper
#' @name credential_helper
credential_helper_list <- function(){
  text <- git_with_sys(c("help", "-a"))
  m <- gregexpr("credential-[^ \t]+", text)
  trimws(regmatches(text, m)[[1]])
}

#' @export
#' @rdname credential_helper
#' @name credential_helper
#' @param global if FALSE the setting is done per git repository, if
#' TRUE it is in your global user git configuration.
credential_helper_get <- function(global = FALSE){
  git <- find_git_cmd()
  args <- c("config", if(global) "--global", "credential.helper")
  git_with_sys(args)
}

#' @export
#' @rdname credential_helper
#' @name credential_helper
#' @param helper string with one of the supported helpers from [credential_helper_list]
credential_helper_set <- function(helper, global = FALSE){
  helper <- sub("^credential-", "", helper)
  args <- c("config", if(global) "--global", "credential.helper", helper)
  git_with_sys(args)
  credential_helper_get(global = global)
}
