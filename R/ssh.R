#' Manage your SSH keys
#'
#' Utility functions to help you setup and find your SSH key files for use
#' with git remotes or other ssh servers.
#'
#' @export
#' @family credentials
#' @rdname ssh_credentials
#' @name ssh_credentials
#' @param host target host (only matters if you have configured specific keys per host)
#' @param ssh path to your ssh executable (if you have one)
#' @param password string or callback function for passphrase, see [openssl::read_key]
lookup_ssh_key <- function(host = "github.com", ssh = "ssh", password = askpass){
  keyfile <- ssh_key(host = host, ssh = ssh)
  pubfile <- paste0(keyfile, ".pub")
  if(!file.exists(pubfile)){
    key <- openssl::read_key(keyfile, password = password)
    openssl::write_ssh(key$pubkey, pubfile)
  }
  list(
    path = keyfile,
    pubkey = openssl::write_ssh(openssl::read_pubkey(pubfile))
  )
}

#' @export
#' @rdname ssh_credentials
read_ssh_key <- function(host = "github.com", ssh = "ssh", password = askpass){
  keyfile <- ssh_key(host = host, ssh = ssh)
  openssl::read_key(keyfile, password = password)
}

ssh_key <- function(host = "github.com", ssh = "ssh"){
  key_paths <- tryCatch(ssh_identityfiles(host = host, ssh = ssh), error = function(e){
    file.path("~/.ssh", c("id_rsa", "id_dsa", "id_ecdsa", "id_ed25519", "id_xmss"))
  })
  key_paths <- normalize_home(key_paths)
  for(i in key_paths){
    if(file.exists(i))
      return(i)
  }
  stop("Failed to find your key file")
}

ssh_identityfiles <- function(host = "github.com", ssh = "ssh"){
  # Note there can be multiple 'identityfile' entries
  conf <- ssh_config(host = host, ssh = ssh)
  unique(unlist(conf[names(conf) == 'identityfile']))
}

ssh_config <- function(host = "github.com", ssh = "ssh"){
  ssh <- find_ssh_cmd(ssh)
  out <- sys::exec_internal(ssh, c("-G", host))
  txt <- strsplit(rawToChar(out$stdout), "\r?\n")[[1]]
  lines <- strsplit(txt, " ", fixed = TRUE)
  names <- vapply(lines, `[`, character(1), 1)
  values <- lapply(lines, `[`, -1)
  structure(values, names = names)
}

find_ssh_cmd <- function(ssh = "ssh"){
  if(cmd_exists(ssh))
    return(ssh)
  if(is_windows()){
    git <- Sys.which(find_git_cmd())
    bin <- dirname(git)
    usrbin <- file.path(dirname(bin), "usr", "bin")
    path <- Sys.getenv('PATH')
    on.exit(Sys.setenv(PATH = path))
    Sys.setenv(PATH = paste(path, bin, usrbin, sep = .Platform$path.sep))
    if(cmd_exists(ssh))
      return(Sys.which(ssh))
  }
  stop(sprintf("Could not find the '%s' command line util", ssh))
}

normalize_home <- function(path = NULL){
  path <- as.character(path)
  if(is_windows()){
    homedir <- Sys.getenv('USERPROFILE')
    is_home <- grepl("^~", path)
    path[is_home] <- paste0(homedir, substring(path[is_home], 2))
  }
  normalizePath(path, mustWork = FALSE)
}


#' @importFrom openssl askpass
#' @export
openssl::askpass

