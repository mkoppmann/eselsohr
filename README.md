# Eselsohr

![Badge for CI workflow status](https://github.com/mkoppmann/eselsohr/actions/workflows/ci.yml/badge.svg)

Eselsohr is a self-hostable bookmark manager for storing web articles.
Read them later or share access to your collection.
It’s still in an early stage of development and not ready for production.

## Build Eselsohr

Pre-built binary releases are provided as CI artifacts.

To build the project manually, you’ll need [GHC](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler) and the [Cabal](https://www.haskell.org/cabal/) build tool.
Download this repository and change your working directory into it.
You can then install the executable with:

```shell
cabal install --install-method=copy --overwrite-policy=always
```

By default, the resulting binary gets stored in `~/.cabal/bin/eselsohr-exe`.

### Nix support

If you have [Nix installed](https://nixos.org/download.html) with [Flakes support](https://nixos.wiki/wiki/Flakes) you can enter the development environment by running `nix develop`.

## Deploy Eselsohr

Eselsohr is distributed as a single binary and does not have any other dependencies.
It can be configured by using [env vars](https://en.wikipedia.org/wiki/Environment_variable) or by using a configuration file (`eselsohr --config-file /path/to/file`).
By default it looks for an `.env` file in the current working directory.

The following values can be set:

* `DATA_FOLDER`: File path where data is getting persisted.
    Defaults to [XdgData](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#v:XdgData).
* `MAX_CONCURRENT_WRITES`: Number of max concurrently run write operations.
    Set a limit to avoid resource exhaustion.
    Must be larger or equal than 1.
    Not set by default.
* `LOG_LEVEL`: Level for the built-in logger.
    Defaults to [Error](https://hackage.haskell.org/package/co-log-core-0.2.1.1/docs/Colog-Core-Severity.html#t:Severity).
* `PORT`: Port number on which the web server will listen.
    Defaults to `6979`.
* `LISTEN_ADDR`: Address where the web server will listen.
    Defaults to `127.0.0.1`.
* `BASE_URL`: Base URL to generate HTML links.
    Defaults to `http://localhost`.
* `HTTPS`: Send `HSTS` HTTP header.
    Automatically enabled when `X-Forwarded-Proto` HTTP header is set to `https`.
    Defaults to `False`.
* `DISABLE_HSTS`: Do not send `HSTS` HTTP header, when `HTTPS` is set.
    Defaults to `False`.
* `CERT_FILE`: File path to the TLS certificate file.
    Not set by default.
* `KEY_FILE`: File path to the TLS key file.
    Not set by default.

Currently, all configuration parameters are optional, so starting Eselsohr can be as simple as executing the Eselsohr binary.
The `dist` directory in this repository provides deployment relevant files, like an example `rc` file for FreeBSD or a service file for systemd-based Linux distributions.

### Docker-based

Alternatively, a Docker image is provided.
You can build and run it like so:

```shell
sudo docker build -t eselsohr .
sudo docker run -p 6979:6979 -v eselsohr-data:/data eselsohr
```

## Architecture

The app is based on the [Three Layer Cake architecture](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html).
The initial code is based on the [three-layer](https://github.com/Holmusk/three-layer) project.

Eselsohr is used as a research playground for [capability-based security](https://en.wikipedia.org/wiki/Capability-based_security) in the context of web applications.

Common web applications use authentication with cookies or HTTP headers to enable identity-based authorization.
This has certain disadvantages, some of them are listed in the description of the [Waterken Server](http://waterken.sourceforge.net/web-key/).

In Eselsohr authorization works with capabilities.
A capability is a shareable, unforgeable token that references a piece of data, including the associated set of access rights.
In our case, authorized requests work with access tokens, [base32-encoded](https://en.wikipedia.org/wiki/Base32) binary data, which are transmitted either over HTTP query strings or in an HTML body.
An access token points to a capability, which points to a data structure called ObjectReference.
They can also have some additional, optional properties like a pet-name, or an expiration date.
An object reference gives access to a collection or a single resource and has the associated permissions encoded within.

Object references are required for accessing the global state, like fetching an article with a specific id.
This is enforced by authorized actions: a data type which corresponds to user actions like creating a new article, or changing an article’s title.
To obtain such an authorized action token, one has to pass a object reference and in some cases the id, on which the action will be performed, to functions, which evaluate if the required permissions are set in the object reference.
This forces us to do authorization checks and we can’t forget to do them.

The application tries to incorporate the [Principle of Least Privilege](https://en.wikipedia.org/wiki/Principle_of_least_privilege) wherever it can.
Instead of using one single data storage for everything, each article collection is stored as a separate resource in the system.
In theory, if Eselsohr would have a vulnerability like a SQL Injection, an attacker could only access their own data, because they do not have a reference to the other resources.
Because access tokens are encoded binary data, that point to an id of a resource and the id of a capability, they work across multiple server instances, as long as the server has access to that resource.

Eselsohr is written in the programming language [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)).
Although it’s never explicitly called one, Haskell is a great language to implement capability-based techniques, as functions are pure and data has to be explicitly passed as arguments to other functions and global state is rarely used.
Side-effects in Haskell are explicit and are normally done in the so-called IO monad.
But the IO monad alone means, that any side-effect can happen.
Eselsohr uses [type classes](https://www.haskell.org/tutorial/classes.html) in a way, that [represent access to certain IO actions](https://chrispenner.ca/posts/monadio-considered-harmful).
A function that wants to e.g. scrap a website, needs to have the `MonadScraper` constraint in its signature.
All side-effects are therefore explicit and only effects that are wrapped this way can be used.
