#' Retrieve and store git HTTPS credentials
#'
#' These are low-level wrappers for the [git-credential](https://git-scm.com/docs/git-credential)
#' command line tool. Try the more user-friendly [read_http_credential]
#' and [update_http_credential] functions first.
#'
#' The [credential_fill] function either looks up credentials, and if none exists
#' it will attempt to prompt the user for a username/password. The method for
#' storing and prompting depends on your OS and R frontend. Upon success it
#' returns a named list with the same `protocol` and `host` fields as the
#' `cred` input, and possibly added `username` and `password` fields.
#'
#' You are then supposed to try and authenticate with these credentials using
#' your client, and afterwards report back if the credentials were valid or not.
#' You should call [credential_approve] and [credential_reject] using the `cred`
#' that was returned by [credential_fill] in order to validate or invalidate a
#' credential from the store.
#'
#' @export
#' @rdname git_cmd
#' @name git_cmd
#' @param cred named list with at least fields `protocol` and `host` and
#' optionally also `path`, `username` ,`password`.
#' @param git name or path to your git executable
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
credential_fill <- function(cred, git = "git", verbose = TRUE){
  out <- credential_exec("fill", cred = cred, git = git, verbose = verbose)
  data <- strsplit(out, "=", fixed = TRUE)
  key <- vapply(data, `[`, character(1), 1)
  val <- vapply(data, `[`, character(1), 2)
  as.list(structure(val, names = key))
}

#' @export
#' @rdname git_cmd
#' @name git_cmd
credential_approve <- function(cred, git = "git", verbose = TRUE){
  credential_exec("approve", cred = cred, git = git, verbose = verbose)
  invisible()
}

#' @export
#' @rdname git_cmd
#' @name git_cmd
credential_reject <- function(cred, git = "git", verbose = TRUE){
  credential_exec("reject", cred = cred, git = git, verbose = verbose)
  invisible()
}

credential_exec <- function(command, cred, git, verbose){
  git <- find_git_cmd(git)
  input <- cred_to_input(cred)
  on.exit(unlink(input))
  if(is_windows() || !interactive() || !isatty(stdin())){
    # Note: isatty(stdin()) = TRUE in Windows RGui
    if(!nchar(Sys.getenv('GIT_TERMINAL_PROMPT'))){
      Sys.setenv(GIT_TERMINAL_PROMPT=0)
      on.exit(Sys.unsetenv('GIT_TERMINAL_PROMPT'))
    }
  }
  rs_path <- Sys.getenv('RS_RPOSTBACK_PATH')
  if(nchar(rs_path)){
    old_path <- Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH = old_path))
    rs_path <- unique(c(rs_path, sub("rpostback", 'postback', rs_path)))
    Sys.setenv(PATH = paste(c(old_path, rs_path), collapse = .Platform$path.sep))
  }
  if(is_windows() || is_macos() || !isatty(stdin())){
    git_with_sys(command, input = input, git = git, verbose = verbose)
  } else {
    # base::system can freeze RStudio Desktop or Windows
    git_with_base(command, input = input, git = git, verbose = verbose)
  }
}

git_with_base <- function(command, input, git, verbose){
  res <- system2(git, c("credential", command), stdin = input,
                 stdout = TRUE, stderr = ifelse(isTRUE(verbose), "", TRUE))
  status <- attr(res, "status")
  if(length(status) && !identical(status, 0L)){
    stop(paste(res, collapse = "\n"))
  }
  res
}

git_with_sys <- function(command, input, git, verbose){
  outcon <- rawConnection(raw(0), "r+")
  on.exit(close(outcon), add = TRUE)
  status <- sys::exec_wait(git, c("credential", command),
                           std_out = outcon, std_err = verbose, std_in = input)
  if(!identical(status, 0L)){
    stop(sprintf("Failed to call 'git credential'"))
  }
  strsplit(rawToChar(rawConnectionValue(outcon)), "\n", fixed = TRUE)[[1]]
}

find_git_cmd <- function(git = "git"){
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
  stop(sprintf("Could not find the '%s' command line util", git))
}

parse_url <- function(url){
  out <- strsplit(url, "://", fixed = TRUE)[[1]]
  if(length(out) < 2)
    stop("URL must start with e.g. https://")
  protocol <- out[1]
  rest <- out[2]
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
