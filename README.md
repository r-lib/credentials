# credentials <img src="man/figures/logo.png" align="right" alt="logo" width="120" height = "139" style = "border: none; float: right;">

*This package is a joint effort from [rOpenSci](https://ropensci.org/) and the [Tidyverse](https://www.tidyverse.org/) team.*

> Tools for Managing SSH and Git Credentials

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-lib/credentials?branch=master&svg=true)](https://ci.appveyor.com/project/jeroen/credentials)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/credentials)](http://cran.r-project.org/package=credentials)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/credentials)](http://cran.r-project.org/web/packages/credentials/index.html)


Setup and retrieve HTTPS and SSH credentials for use with 'git' and 
other services. For HTTPS remotes the package interfaces the 'git-credential' 
utility which 'git' uses to store HTTP usernames and passwords. For SSH 
remotes we provide convenient functions to find or generate appropriate SSH 
keys. The package both helps the user to setup a local git installation, and
also provides a back-end for git/ssh client libraries to authenticate with 
existing user credentials.

### Setting your GITHUB_PAT

Automatically populate your `GITHUB_PAT` environment variable from the native git credential store. The credential manager will safely prompt the user for credentials when needed.

```r
credentials::set_github_pat()
```

Use this function in your `.Rprofile` if you want to automatically set `GITHUB_PAT` for each R session, without hardcoding your secret in plain text.

### Manage HTTPS credentials

Load or prompt the user for GitHub username and password:

```r
library(credentials)
git_credential_ask('https://github.com')
```

See which credential helper back-end your `git-credential` store is using:

```r
credentials::credential_helper_get()
```

### Manage SSH keys

Lookup the appropriate key, or prompt the user to generate one:

```r
library(credentials)
ssh_key_info()
```

You can copy-paste the public key directly to your [GitHub profile](https://github.com/settings/ssh/new)!

## For developers

Use the openssl package to read the user private key in R for encryption / signatures: 

```r
user <- ssh_key_info()
key <- ssh_read_key(user$key)
openssl::write_pem(key)
```

