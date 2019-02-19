#' Retrieve and store git HTTPS credentials
#'
#' Low-level wrappers for the [git-credential](https://git-scm.com/docs/git-credential)
#' command line tool. Try the user-friendly [git_credential_ask]
#' and [git_credential_update] functions first.
#'
#' The [credential_fill] function looks up credentials for a given host, and
#' if none exists it will attempt to prompt the user for new credentials. Upon
#' success it returns a list with the same `protocol` and `host` fields as the
#' `cred` input, and additional `username` and `password` fields.
#'
#' After you have tried to authenticate the provided credentials, you can report
#' back if the credentials were valid or not. Call [credential_approve] and
#' [credential_reject] with the `cred` that was returned by [credential_fill]
#' in order to validate or invalidate a credential from the store.
#'
#' Because git credential interacts with the system password manager, the appearance
#' of the prompts vary by OS and R frontend.  Note that [credential_fill] should
#' only be used interactively, because it may require the user to enter credentials
#' or unlock the system keychain. On the other hand [credential_approve] and
#' [credential_reject] are non-interactive and could be used to save or delete
#' credentials in a scripted program. However note that some credential helpers
#' (e.g. on Windows) have additional security restrictions that limit use of
#' [credential_approve] and [credential_reject] to credentials that were actually
#' entered by the user via [credential_fill]. Here it is not possible at all to
#' update the credential store without user interaction.
#'
#' @export
#' @rdname credential_api
#' @name credential_api
#' @param cred named list with at least fields `protocol` and `host` and
#' optionally also `path`, `username` ,`password`.
#' @param verbose emit some useful output about what is happening
#' @examples \donttest{
#' # Insert example cred
#' example <- list(protocol = "https", host = "example.com",
#'   username = "test", password = "secret")
#' credential_approve(example)
#'
#' # Retrieve it from the store
#' cred <- credential_fill(list(protocol = "https", host = "example.com", path = "/foo"))
#' print(cred)
#'
#' # Delete it
#' credential_reject(cred)
#' }
credential_fill <- function(cred, verbose = TRUE){
  out <- credential_exec("fill", cred = cred, verbose = verbose)
  data <- strsplit(out, "=", fixed = TRUE)
  key <- vapply(data, `[`, character(1), 1)
  val <- vapply(data, `[`, character(1), 2)
  structure(as.list(structure(val, names = key)), class = 'git_credential')
}

#' @export
#' @rdname credential_api
#' @name credential_api
credential_approve <- function(cred, verbose = TRUE){
  credential_exec("approve", cred = cred, verbose = verbose)
  invisible()
}

#' @export
#' @rdname credential_api
#' @name credential_api
credential_reject <- function(cred, verbose = TRUE){
  credential_exec("reject", cred = cred, verbose = verbose)
  invisible()
}

credential_exec <- function(command, cred, verbose){
  input <- cred_to_input(cred)
  on.exit(unlink(input))
  if(is_windows() || is_macos() || !isatty(stdin())){
    text <- git_with_sys(c("credential", command), input = input, verbose = verbose)
    strsplit(text, "\n", fixed = TRUE)[[1]]
  } else {
    # base::system can freeze RStudio Desktop or Windows
    git_with_base(c("credential", command), input = input, verbose = verbose)
  }
}

git_with_base <- function(command, input = "", verbose = TRUE){
  git <- find_git_cmd()
  res <- system2(git, command, stdin = input,
                 stdout = TRUE, stderr = ifelse(isTRUE(verbose), "", TRUE))
  status <- attr(res, "status")
  if(length(status) && !identical(status, 0L)){
    stop(paste(res, collapse = "\n"))
  }
  res
}

git_with_sys <- function(command, input = NULL, verbose = TRUE){
  git <- find_git_cmd()
  outcon <- rawConnection(raw(0), "r+")
  on.exit(close(outcon), add = TRUE)
  timeout <- ifelse(interactive(), 120, 10)
  status <- sys::exec_wait(git, command, std_out = outcon, std_err = verbose,
                           std_in = input, timeout = timeout)
  if(!identical(status, 0L)){
    stop(sprintf("Failed to call 'git %s'", paste(command, collapse = " ")), call. = FALSE)
  }
  trimws(rawToChar(rawConnectionValue(outcon)))
}

find_git_cmd <- function(git = getOption("git", "git"), error = TRUE){
  if(cmd_exists(git)){
    return(git)
  }
  if(is_windows()){
    locations <- c("C:\\PROGRA~1\\Git\\cmd\\git.exe",
                   "C:\\Program Files\\Git\\cmd\\git.exe")
    for(i in locations){
      if(cmd_exists(i)){
        return(i)
      }
    }
  }
  if(error){
    stop(sprintf("Could not find the '%s' command line util", git), call. = FALSE)
  }
}

has_git_cmd <- function(){
  !is.null(find_git_cmd(error = FALSE))
}

parse_url <- function(url, allow_ssh = TRUE){
  out <- strsplit(url, "://", fixed = TRUE)[[1]]
  if(length(out) < 2){
    if(!isTRUE(allow_ssh)){
      stop(sprintf("URL must start with e.g. https:// (found %s)", url))
    } else {
      protocol = 'ssh'
      rest = url
    }
  } else {
    protocol <- out[1]
    rest <- out[2]
  }
  password <- NULL
  username <- if(grepl("^[^/]+@", rest)){
    auth <- strsplit(rest, "@", fixed = TRUE)[[1]]
    rest <- paste(auth[-1], collapse = "@")
    password <- if(grepl(":", auth[1], fixed = TRUE)){
      auth <- strsplit(auth[1], ":", fixed = TRUE)[[1]]
      paste(auth[-1], collapse = ":")
    }
    auth[1]
  }
  rest <- strsplit(rest, "/", fixed = TRUE)[[1]]
  host <- rest[1]
  path <- if(length(rest) > 1){
    paste(rest[-1], collapse = "/")
  }
  c(
    username = username,
    password = password,
    protocol = protocol,
    host = host,
    path = path
  )
}

cred_to_input <- function(data, input = tempfile()){
  str <- paste(names(data), as.character(data), collapse = "\n", sep = "=")
  writeBin(charToRaw(sprintf("%s\n", str)), con = input)
  return(input)
}

cmd_exists <- function(cmd){
  nchar(Sys.which(cmd)) > 0
}

is_windows <- function(){
  identical(.Platform$OS.type, "windows")
}

is_macos <- function(){
  identical(tolower(Sys.info()[['sysname']]), "darwin")
}
