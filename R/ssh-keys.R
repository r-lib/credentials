#' Managing Your SSH Key
#'
#' Utility functions to find or generate your SSH key for use with git remotes
#' or other ssh servers.
#'
#' Use [ssh_key_info()] to find the appropriate key file on your system to connect with a
#' given target host. In most cases this will simply be `ssh_home('id_rsa')` unless
#' you have configured ssh to use specific keys for specific hosts.
#'
#' To use your key to authenticate with GitHub, copy the pubkey from `ssh_key_info()` to
#' your profile: \url{https://github.com/settings/ssh/new}.
#'
#' If this is the first time you use ssh, [ssh_keygen] can help generate a key and
#' save it in the default location. This will also automatically opens the above Github
#' page in your browser where you can add the key to your profile.
#'
#' `ssh_read_key` reads a private key and caches the result (in memory) for the
#' duration of the R session. This prevents having to enter the key passphrase many
#' times. Only use this if `ssh-agent` is not available (i.e. Windows)
#'
#' @export
#' @family credentials
#' @rdname ssh_credentials
#' @name ssh_credentials
#' @param host target host (only matters if you have configured specific keys per host)
#' @param auto_keygen if `TRUE` automatically generates a key if none exists yet.
#' Default `NA` is to prompt the user what to.
ssh_key_info <- function(host = NULL, auto_keygen = NA){
  keyfile <- find_ssh_key(host = host)
  if(is.null(keyfile)){
    if(isTRUE(auto_keygen) || (is.na(auto_keygen) && ask_user("No SSH key found. Generate one now?"))){
      keyfile <- ssh_home('id_rsa')
      ssh_keygen(keyfile)
    } else {
      stop(sprintf("Failed to find ssh key file for %s", host))
    }
  }
  pubfile <- paste0(keyfile, ".pub")
  if(!file.exists(pubfile)){
    key <- ssh_read_key(keyfile)
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
#' @importFrom openssl write_ssh write_pem read_key write_pkcs1 read_pubkey
ssh_keygen <- function(file = ssh_home('id_rsa')){
  private_key <- normalizePath(file, mustWork = FALSE)
  pubkey_path <- paste0(private_key, ".pub")
  if(file.exists(private_key)){
    cat(sprintf("Found existing RSA keyspair at: %s\n", private_key), file = stderr())
    pubkey <- if(file.exists(pubkey_path)){
      read_pubkey(pubkey_path)
    } else {
      ssh_read_key(private_key)$pubkey
    }
  } else {
    cat(sprintf("Generating new RSA keyspair at: %s\n", private_key), file = stderr())
    key <- openssl::rsa_keygen()
    pubkey <- key$pubkey
    dir.create(dirname(private_key), showWarnings = FALSE)
    write_pkcs1(key, private_key)
    write_ssh(pubkey, pubkey_path)
    Sys.chmod(private_key, "0600")
  }

  # See https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
  conf_file <- file.path(dirname(private_key), 'config')
  if(is_macos() && !file.exists(conf_file)){
    writeLines(c('Host *', '  AddKeysToAgent yes', '  UseKeychain yes',
                 paste('  IdentityFile ', private_key)), con = conf_file)
  }
  list(
    key = private_key,
    pubkey = write_ssh(pubkey)
  )
}

#' @rdname ssh_credentials
#' @export
ssh_setup_github <- function(){
  info <- ssh_key_info()
  cat("Your public key:\n\n", info$pubkey, "\n\n", file = stderr())
  cat("Please copy the line above to GitHub: https://github.com/settings/ssh/new\n", file = stderr())
  if(interactive() && ask_user('Would you like to open a browser now?')){
    utils::browseURL('https://github.com/settings/ssh/new')
  }
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

#' @export
#' @rdname ssh_credentials
ssh_agent_add <- function(file = NULL){
  if(is.na(Sys.getenv('SSH_AUTH_SOCK', NA))){
    stop("ssh-agent is not running. If this is a server ssh in with: ssh -o 'ForwardAgent=yes'")
  }
  sys::exec_wait('ssh-add', as.character(file)) == 0
}

# Only used on Windows for now
ssh_agent_start <- function(){
  if(is_windows()){
    out <- sys::exec_internal('cmd', c('/C', 'start-ssh-agent 1>&2 && set'), error = FALSE)
    if(out$status != 0){
      warning("Failed to start ssh-agent", call. = FALSE)
    } else{
      con <- rawConnection(out$stdout)
      on.exit(close(con))
      vars <- readLines(con)
      vars <- vars[grepl('^SSH_(AGENT|AUTH)', vars)]
      if(length(vars)){
        tmp <- tempfile()
        writeLines(vars, tmp)
        readRenviron(tmp)
      }
    }
    return(rawToChar(out$stderr))
  }
}

find_ssh_key <- function(host = NULL){
  if(!length(host))
    host <- "*"
  key_paths <- tryCatch(ssh_identityfiles(host = host), error = function(e){
    message(e$message)
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

# Old SSH versions (Trusty, CentOS) do not support ssh -G
ssh_config <- function(host){
  ssh <- find_ssh_cmd()
  out <- sys::exec_internal(ssh, c("-G", host), error = FALSE)
  if(!identical(out$status, 0L))
    stop("Could not read ssh config. Using default settings.", call. = FALSE)
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
    # Try asking 'git.exe' where it looks for 'ssh.exe
    try({
      gitssh <- git_with_sys(c("-c", "alias.sh=!sh", "sh", "-c", "cygpath -m $(which ssh)"))
      if(nchar(gitssh) && cmd_exists(gitssh))
        return(Sys.which(gitssh))
    }, silent = TRUE)
    # Fallback: try to find ssh.exe ourselves in the usual places
    try({
      bin <- dirname(find_git_cmd())
      usrbin <- file.path(dirname(bin), "usr", "bin")
      path <- Sys.getenv('PATH')
      on.exit(Sys.setenv(PATH = path))
      Sys.setenv(PATH = paste(path, bin, usrbin, sep = .Platform$path.sep))
      if(cmd_exists(ssh))
        return(Sys.which(ssh))
    }, silent = TRUE)
  }
  stop(sprintf("No '%s' command found. Using default ssh settings.", ssh), call. = FALSE)
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
  return(utils::menu(c("Yes", "No"), title = str) == 1)
}

#' @export
#' @rdname ssh_credentials
ssh_update_passphrase <- function(file = ssh_home("id_rsa")){
  key <- openssl::read_key(file, password = function(x){
    askpass::askpass("Please enter your _current_ passphrase")
  })
  new1 <- askpass::askpass("Enter the _new_ passphrase")
  new2 <- askpass::askpass("To confirm, the your _new_ passphrase again")
  if(identical(new1, new2)){
    openssl::write_pkcs1(key, path = file, password = new1)
  } else {
    stop("Entered passhprases are not identical")
  }
  message("Passphrase has been updated!")

  # Wipe the key cache just in case
  environment(ssh_read_key)$store = new.env(parent = emptyenv())
}

#' @export
#' @rdname ssh_credentials
#' @param password a passphrase or callback function
#' @importFrom askpass askpass
ssh_read_key <- local({
  store = new.env(parent = emptyenv())
  function (file = ssh_home("id_rsa"), password = askpass){
    file <- normalizePath(file, mustWork = TRUE)
    hash <- openssl::md5(file)
    if(!length(store[[hash]])){
      store[[hash]] <- tryCatch(openssl::read_key(file, password = password), error = function(e){
        stop(sprintf("Unable to load key: %s", file), call. = FALSE)
      })
    }
    return(store[[hash]])
  }
})
