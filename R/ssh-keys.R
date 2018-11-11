#' Your SSH key
#'
#' Utility functions to find or generate your SSH key for use with git remotes
#' or other ssh servers.
#'
#' Use [my_ssh_key()] to find the appropriate key file on your system to connect with a
#' given target host. In most cases this will simply be `ssh_home('id_rsa')` unless
#' you have configured ssh to use specific keys for specific hosts.
#'
#' To use your key to authenticate with GitHub, copy the pubkey from `my_ssh_key()` to
#' your profile: \url{https://github.com/settings/ssh/new}.
#'
#' If this is the first time you use ssh, [ssh_keygen] can help generate a key and
#' save it in the default location. This will also automatically opens the above Github
#' page in your browser where you can add the key to your profile.
#'
#' @export
#' @family credentials
#' @rdname ssh_credentials
#' @name ssh_credentials
#' @param host target host (only matters if you have configured specific keys per host)
#' @param auto_keygen if `TRUE` automatically generates a key if none exists yet.
#' Default `NA` is to prompt the user what to.
#' @param password string or callback function for passphrase, see [openssl::read_key]
my_ssh_key <- function(host = "github.com", auto_keygen = NA, password = askpass){
  keyfile <- find_ssh_key(host = host)
  if(is.null(keyfile)){
    if(isTRUE(auto_keygen) || (is.na(auto_keygen) && ask_user("No SSH key found. Generate one now?"))){
      keyfile <- ssh_home('id_rsa')
      ssh_keygen(keyfile, open_github = FALSE)
    } else {
      stop(sprintf("Failed to find ssh key file for %s", host))
    }
  }
  pubfile <- paste0(keyfile, ".pub")
  if(!file.exists(pubfile)){
    key <- openssl::read_key(keyfile, password = password)
    try(openssl::write_ssh(key$pubkey, pubfile), silent = TRUE)
  }
  list(
    key = keyfile,
    pubkey = openssl::write_ssh(openssl::read_pubkey(pubfile))
  )
}

#' @export
#' @rdname ssh_credentials
#' @param file destination path of the private key. For the public key, `.pub`
#' is appended to the filename.
#' @param open_github automatically open a browser window to let the user
#' add the key to Github.
#' @importFrom openssl write_ssh write_pem read_key write_pkcs1
ssh_keygen <- function(file = ssh_home('id_rsa'), open_github = interactive()){
  private_key <- normalizePath(file, mustWork = FALSE)
  if(file.exists(private_key)){
    cat(sprintf("Found existing RSA keyspair at: %s\n", private_key), file = stderr())
    key <- read_key(file)
  } else {
    cat(sprintf("Generating new RSA keyspair at: %s\n", private_key), file = stderr())
    key <- openssl::rsa_keygen()
    dir.create(dirname(private_key), showWarnings = FALSE)
    write_pkcs1(key, private_key)
    write_ssh(key$pubkey, paste0(private_key, '.pub'))
  }

  # See https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
  conf_file <- file.path(dirname(private_key), 'config')
  if(is_macos() && !file.exists(conf_file)){
    writeLines(c('Host *', '  AddKeysToAgent yes', '  UseKeychain yes',
                 paste('  IdentityFile ', private_key)), con = conf_file)
  }
  if(isTRUE(open_github)){
    cat("Opening browser to add your key: https://github.com/settings/ssh/new\n", file = stderr())
    utils::browseURL('https://github.com/settings/ssh/new')
  }
  list(
    key = private_key,
    pubkey = write_ssh(key$pubkey)
  )
}

#' @export
#' @rdname ssh_credentials
ssh_home <- function(file = NULL){
  if(length(file)){
    normalizePath(file.path(normalize_home("~/.ssh"), file), mustWork = FALSE)
  } else {
    normalize_home("~/.ssh")
  }
}


find_ssh_key <- function(host){
  key_paths <- tryCatch(ssh_identityfiles(host = host), error = function(e){
    warning(e$message, call. = FALSE, immediate. = TRUE)
    file.path("~/.ssh", c("id_rsa", "id_dsa", "id_ecdsa", "id_ed25519", "id_xmss"))
  })
  key_paths <- normalize_home(key_paths)
  for(i in key_paths){
    if(file.exists(i))
      return(i)
  }
  return(NULL)
}

ssh_identityfiles <- function(host){
  # Note there can be multiple 'identityfile' entries
  conf <- ssh_config(host = host)
  unique(unlist(conf[names(conf) == 'identityfile']))
}

ssh_config <- function(host){
  ssh <- find_ssh_cmd()
  out <- sys::exec_internal(ssh, c("-G", host))
  txt <- strsplit(rawToChar(out$stdout), "\r?\n")[[1]]
  lines <- strsplit(txt, " ", fixed = TRUE)
  names <- vapply(lines, `[`, character(1), 1)
  values <- lapply(lines, `[`, -1)
  structure(values, names = names)
}

ssh_version <- function(){
  ssh <- find_ssh_cmd()
  out <- sys::exec_internal(ssh, "-V")
  # ssh may print to stderr instead of stdout
  trimws(rawToChar(c(out$stdout, out$stderr)))
}

find_ssh_cmd <- function(ssh = getOption("ssh", "ssh")){
  if(cmd_exists(ssh))
    return(ssh)
  if(is_windows()){
    # ask 'git.exe' where it keeps 'ssh.exe'
    git <- Sys.which(find_git_cmd())
    res <- sys::exec_internal(git, c("-c", "alias.sh=!sh", "sh",
                       "-c", "cygpath -m $(which ssh)"), error = FALSE)
    if(res$status == 0L){
      ssh <- trimws(rawToChar(res$stdout))
      if(cmd_exists(ssh))
        return(Sys.which(ssh))
    }
    # Fallback: try to find ssh.exe ourselves
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

ask_user <- function(str){
  if(!interactive())
    return(FALSE)
  message(str)
  return(utils::menu(c("Yes", "No")) == 1)
}

#' @importFrom openssl askpass
#' @export
openssl::askpass
