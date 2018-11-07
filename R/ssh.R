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
#' @param ssh path to your ssh executable (if you have one)
#' @param password string or callback function for passphrase, see [openssl::read_key]
my_ssh_key <- function(host = "github.com", ssh = "ssh", password = askpass){
  keyfile <- find_ssh_key(host = host, ssh = ssh)
  if(is.null(keyfile)){
    if(isTRUE(askYesNo("No ssh key found. Generate one? ", default = FALSE,
                       prompts = c("Yes (recommended)", "No", "Cancel")))){
      keyfile <- ssh_home('id_rsa')
      ssh_keygen(keyfile, open_github = FALSE)
    } else {
      stop("Failed to find ssh key file")
    }
  }
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
#' @param file destination path of the private key. For the public key, `.pub`
#' is appended to the filename.
#' @param open_github automatically open a browser window to let the user
#' add the key to Github.
#' @importFrom openssl write_ssh write_pem read_key write_pkcs1
ssh_keygen <- function(file = ssh_home('id_rsa'), open_github = TRUE){
  private_key <- normalizePath(file, mustWork = FALSE)
  if(file.exists(private_key)){
    cat(sprintf("Found existing RSA keyspair at: %s\n", private_key), file = stderr())
    key <- read_key(file)
  } else {
    cat(sprintf("Generating new RSA keyspair at: %s\n", private_key), file = stderr())
    key <- openssl::rsa_keygen()
    write_pkcs1(key, private_key)
    write_ssh(key$pubkey, paste0(private_key, '.pub'))
  }

  # See https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
  conf_file <- file.path(dirname(private_key), 'config')
  if(!file.exists(conf_file)){
    writeLines(c('Host *', '  AddKeysToAgent yes', '  UseKeychain yes',
                 paste('  IdentityFile ', private_key)), con = conf_file)
  }
  if(isTRUE(open_github)){
    cat("Opening browser to add your key: https://github.com/settings/ssh/new\n", file = stderr())
    utils::browseURL('https://github.com/settings/ssh/new')
  }
  list(
    path = private_key,
    pubkey = write_ssh(key$pubkey)
  )
}

#' @export
#' @rdname ssh_credentials
ssh_home <- function(file = NULL){
  if(length(file)){
    return(file.path(normalize_home("~/.ssh"), file))
  }
  normalize_home("~/.ssh")
}


find_ssh_key <- function(host, ssh){
  key_paths <- tryCatch(ssh_identityfiles(host = host, ssh = ssh), error = function(e){
    message(e)
    file.path("~/.ssh", c("id_rsa", "id_dsa", "id_ecdsa", "id_ed25519", "id_xmss"))
  })
  key_paths <- normalize_home(key_paths)
  for(i in key_paths){
    if(file.exists(i))
      return(i)
  }
  return(NULL)
}

ssh_identityfiles <- function(host, ssh){
  # Note there can be multiple 'identityfile' entries
  conf <- ssh_config(host = host, ssh = ssh)
  unique(unlist(conf[names(conf) == 'identityfile']))
}

ssh_config <- function(host, ssh){
  ssh <- find_ssh_cmd(ssh)
  out <- sys::exec_internal(ssh, c("-G", host))
  txt <- strsplit(rawToChar(out$stdout), "\r?\n")[[1]]
  lines <- strsplit(txt, " ", fixed = TRUE)
  names <- vapply(lines, `[`, character(1), 1)
  values <- lapply(lines, `[`, -1)
  structure(values, names = names)
}

find_ssh_cmd <- function(ssh){
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
