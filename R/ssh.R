#' Manage your SSH keys



get_ssh_credentials <- function(){
  VAR <- ifelse(is_windows(), 'USERPROFILE', 'HOME')
  HOME <- Sys.getenv(VAR, normalizePath("~"))
  key <- normalizePath(file.path(HOME, ".ssh", "id_rsa"), mustWork = FALSE)
  if (!file.exists(path))
    stop("No suitable user key found.")
  openssl::read_key(path)
}

get_ssh_key <- function(name = "id_rsa"){
  VAR <- ifelse(is_windows(), 'USERPROFILE', 'HOME')
  HOME <- Sys.getenv(VAR, normalizePath("~"))
  key <- normalizePath(file.path(HOME, ".ssh", "id_rsa"), mustWork = FALSE)
  if (!file.exists(path))
    stop("No suitable user key found.")
}

ssh_config <- function(host = "github.com", ssh = "ssh"){
  if(!cmd_exists(ssh) && is_windows()){
    git <- find_git_cmd()
    dir1 <- dirname(git)
    dir2 <- dirname(dir1)
    dir3 <- file.path(dir2, "usr", "bin")
    path <- Sys.getenv('PATH')
    on.exit(Sys.setenv(PATH = path))
    Sys.setenv(PATH = paste(path, dir1, dir2, dir3, sep = .Platform$path.sep))
  }
  if(!cmd_exists(ssh)){
    stop("Failed to find the 'ssh' command line tool")
  }
  out <- sys::exec_internal(ssh, c("-G", host))
  txt <- strsplit(rawToChar(out$stdout), "\r?\n")[[1]]
  lines <- strsplit(txt, " ", fixed = TRUE)
  names <- vapply(lines, `[`, character(1), 1)
  values <- lapply(lines, `[`, -1)
  structure(values, names = names)
}
