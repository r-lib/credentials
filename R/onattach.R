.onAttach <- function(libname, pkgname){
  tryCatch({
    gitver <- git_with_sys("--version", NULL, FALSE)
    packageStartupMessage(sprintf("Found %s", gitver))
    helpers <- sub("^credential-", "", credential_helper_ls())
    packageStartupMessage(sprintf("Supported git-credential helpers: %s",
                                  paste(helpers, collapse = ", ")))
  }, error = function(e){
    packageStartupMessage("Failed to find git :(")
  })

  tryCatch({
    sshver <- ssh_version()
    packageStartupMessage(sprintf("Found %s", sshver))
    tryCatch({
      key <- find_ssh_key("github.com")
      if(length(key)){
        packageStartupMessage(sprintf("SSH keyfile: %s", key))
      } else {
        packageStartupMessage("No key found. Use ssh_keygen() to generate one!")
      }
    }, error = function(e){
      packageStartupMessage("Failed to lookup key file")
    })
  }, error = function(e){
    packageStartupMessage("Failed to find ssh :(")
  })
}
