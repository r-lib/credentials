.onLoad <- function(libname, pkgname){
  # This is mostly for R.app (tty could mean macos server via ssh)
  if(is_macos() && !isatty(stdin())){
    mac_askpass = system.file('mac-askpass', package = 'credentials', mustWork = TRUE)
    if(is.na(Sys.getenv('GIT_ASKPASS', NA))){
      Sys.setenv("GIT_ASKPASS" = mac_askpass)
    }
    if(is.na(Sys.getenv('SSH_ASKPASS', NA))){
      Sys.setenv("SSH_ASKPASS" = mac_askpass)
    }
  }
}

.onAttach <- function(libname, pkgname){
  tryCatch({
    gitver <- git_with_sys("--version", NULL, FALSE)
    packageStartupMessage(sprintf("Found %s", gitver))
    helpers <- sub("^credential-", "", credential_helper_list())
    packageStartupMessage(sprintf("Supported HTTPS credential helpers: %s",
                                  paste(helpers, collapse = ", ")))
  }, error = function(e){
    if(is_windows()){
      packageStartupMessage("Git for Windows is not installed.\nDownload from: https://git-scm.com/download/win")
    } else {
      packageStartupMessage("Unable to find git :-(")
    }
  })

  tryCatch({
    sshver <- ssh_version()
    packageStartupMessage(sprintf("Found %s", sshver))
    tryCatch({
      key <- find_ssh_key()
      if(length(key)){
        packageStartupMessage(sprintf("Default SSH key: %s", key))
      } else {
        packageStartupMessage("No key found. Use ssh_keygen() to generate one!")
      }
    }, error = function(e){
      packageStartupMessage("Failed to lookup key file")
    })
  }, error = function(e){
    packageStartupMessage(e$message)
  })
}
