# credentials

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/r-lib/credentials.svg?branch=master)](https://travis-ci.org/r-lib/credentials)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-lib/credentials?branch=master&svg=true)](https://ci.appveyor.com/project/jeroen/credentials)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/credentials)](http://cran.r-project.org/package=credentials)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/credentials)](http://cran.r-project.org/web/packages/credentials/index.html)

Tools for Managing SSH and Git Credentials

> Setup and retrieve HTTPS and SSH credentials for use with 'git' and 
  other services. For HTTPS remotes the package interfaces the 'git-credential' 
  utility which 'git' uses to store HTTP usernames and passwords. For SSH 
  remotes we provide convenient functions to find or generate appropriate SSH 
  keys. The package both helps the user to setup a local git installation, and
  also provides a back-end for git/ssh client libraries to authenticate with 
  existing user credentials.

## Installation

You can install the released version of credentials from Github:

``` r
remotes::install_github("r-lib/credentials")
```

## HTTPS credentials

Load or prompt the user for GitHub username and password:

```r
library(credentials)
git_credential_read('https://github.com')
```

See which credential helper back-end your `git-credential` store is using:

```r
credentials::credential_helper_get()
```

## SSH keys

Lookup the appropriate key, or prompt the user to generate one:

```r
library(credentials)
my_ssh_key()
```

You can copy-paste the public key directly to your [GitHub profile])(https://github.com/settings/ssh/new)!

## for developers

Use the openssl package to read the user private key in R for encryption or signatures: 

```r
user <- my_ssh_key()
key <- openssl::read_key(user$key)
openssl::write_pem(key)
```

