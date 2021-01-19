# Eselsohr

Eselsohr is a self-hostable bookmark manager for storing web articles.
Read them later or share access to your collection.
It’s still in an early stage of development and not ready for production.

## Build Eselsohr

Pre-built binary releases are provided as CI artifacts.

To build the project manually, you’ll need [GHC](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler) and the [Cabal](https://www.haskell.org/cabal/) build tool.
Download this repository and change your working directory into it.
You can then build the executable with:

```shell
cabal install -O2 --install-method=copy
```

By default, the resulting binary gets stored in `~/.cabal/bin/eselsohr`.

Some Linux distributions have problems with building static binaries, for example Arch Linux.
You can generate a dynamically-linked binary by setting:

```shell
cabal configure --disable-executable-static
```

## Deploy Eselsohr

Eselsohr is distributed as a single statically-linked binary and does not have any other system dependencies.
It can be configured by using [env vars](https://en.wikipedia.org/wiki/Environment_variable) or by using a configuration file (`eselsohr --config-file path/to/file`).
By default it looks for an `.env` file in the current working directory.

The following values can be set:

* `DATA_FOLDER`: File path where data is getting persisted. Defaults to [XdgData](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#v:XdgData).
* `LOG_LEVEL`: Level for the built-in logger. Defaults to [Error](https://hackage.haskell.org/package/co-log-core-0.2.1.1/docs/Colog-Core-Severity.html#t:Severity).
* `PORT`: Port number on which the web server will listen. Defaults to `6979`.
* `LISTEN_ADDR`: Address where the web server will listen. Defaults to `127.0.0.1`.
* `BASE_URL`: Base URL to generate HTML links. Defaults to `http://localhost`.

Currently, all configuration parameters are optional, so starting Eselsohr can be as simple as executing the Eselsohr binary.
The `dist` directory in this repository provides deployment relevant files, like an example `rc` file for FreeBSD or a service file for systemd-based Linux distributions.

## Architecture

The app is based on the [Three Layer Cake architecture](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html).
The initial code is based on the [three-layer](https://github.com/Holmusk/three-layer) project.

Eselsohr is used as a research playground for [capability-based security](https://en.wikipedia.org/wiki/Capability-based_security) in the context of web applications.

Common web applications use authentication with cookies or HTTP headers to enable identity-based authorization.
This has certain disadvantages, some of them are listed in the description of the [Waterken Server](http://waterken.sourceforge.net/web-key/).

In Eselsohr authorization works with capabilities.
A capability is a shareable, unforgeable token that references a piece of data, including the associated set of access rights.
In our case, authorized requests work with access tokens, which are transmitted either over HTTP Query Parameters or in an HTML body.
An access token points to a capability, which can have some optional properties like a pet-name, or an expiration date.
A capability points to an user action, the core of Eselsohr.
All actions that can be executed by an user are represented as values in the system, e.g. there are multiple actions per article, like showing, or deleting the article or marking it as read.
Also opening a new collection, or creating an access token for listing your articles is an user action.

This allows Eselsohr to provide a single API endpoint for everything.
Access tokens get converted to capabilities and user actions and the server handles them accordingly.
This means that security becomes explicit and represents the control flow of the application.
A single API endpoint also makes the API unguessable; the only way to perform actions on the system is by using access tokens that were given out by the system.
This inverts the classic API security model: Instead of having a predictable API where every endpoints needs to be protected to prevent malicious access to data, an unpredictable API means, that if you forget to share access to data, it stays secure.

The application tries to incorporate the [Principle of Least Privilege](https://en.wikipedia.org/wiki/Principle_of_least_privilege) wherever it can.
Instead of using one single data storage for everything, each article collection is stored as a separate resource in the system.
In theory, if Eselsohr would have a vulnerability like a SQL Injection, an attacker could only access their own data, because they do not have a reference to the other resources.
Access tokens are encoded binary data, that point to an id of a resource and the id of a capability; this means that they work across multiple server instances, as long as the server has access to that resource.

Eselsohr is written in the programming language [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)).
Although it’s never explicitly called one, Haskell is a great language to implement capability-based techniques, as functions are pure and data has to be explicitly passed as arguments to other functions and global state is rarely used.
Side-effects in Haskell are explicit and are normally done in the so-called IO monad.
But the IO monad alone means, that any side-effect can happen.
Eselsohr uses [type classes](https://www.haskell.org/tutorial/classes.html) in a way, that [represent access to certain IO actions](https://chrispenner.ca/posts/monadio-considered-harmful).
A function that wants to e.g. scrap a website, needs to have the `MonadScraper` constraint in its signature.
All side-effects are therefore explicit and only effects that are wrapped this way can be used.
