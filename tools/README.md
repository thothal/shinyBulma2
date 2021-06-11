# Build Tools

This documentation describes which build tools we are using. All build tools are 

## [`yarn`](https://yarnpkg.com/)

* `yarn` is used as the package manager to load external dependencies.
* Dependencies are added via `yarn add <dep>`, which will add an entry in the 
corresponding `package.json`.
* We can add *production* dependencies (i.e. used for the library itself) and 
*development* dependencies (i.e. used for *building* the library). This is achieved by
using the flag `--dev` in the `yarn add` call. 
* The standard behavior is to use [Caret Ranges (e.g. `^3.1.4`)](https://classic.yarnpkg.com/en/docs/dependency-versions/#toc-caret-ranges).
* That means that newer versions of a dependencies are considered (and consequently 
re-installed if asked to) that do not alter the first non zero digit.
* We do **not** tsore the dependencies on GitHub. The very idea of yarn is that with the
corresponding `package.json` one should get the same results on any system.

## [`grunt`](https://gruntjs.com/)

* `grunt` is a JavaScript task runner.
* We can use it to automatize certain build steps, like uglifying CSS or JavaScript files.
* In particular, we use it to copy the relevant bits and pieces from the `npm` packages to
the proper places in the R library folder structure (e.g. `inst/`).
* `grunt` uses plugins which are configured using a `Gruntfile.js` configuration script.
* There are a myriad of different plugins (all need to be installed via 
`yarn add <plugin> --dev`).
* In particular we can use `grunt-contrib-watch` to watch for changes in files (i.e. 
running tasks whenever a defined file changes) and `grunt-newer` to run tasks only if the 
target is newer.
* The task of this package `Gruntfile` is twofold:
   - "Compile" homebrewed CSS / JavaScript to the right place
   - Copy pre-compiled framework CSS / JS to the proper lication
* This should work seamlessly, thus according `watch` tasks need to be defined.
* In order to make this workflow even more automatic, we start `grunt watch` on startup of
the Rstudio session using `setHook("rstudio.session", ...)` in the project's `.Rprofile`.

## R installation scripts

* Potentially we may want to add R scripts to `tools/` which perform certain upgrade / 
build steps.
* These scripts could also be integrated into `grunt`via 
[grunt-run](https://github.com/spalger/grunt-run).
* The overarching strategy for the build process is that `grunt` is the orchestrator of 
all automated build steps.
* That is `grunt` makes sure that files which are either compiled or come from an external
source are moved automatically to the right destinations in the proper format.
* In this way, upgrading to newer versions of the underlying framework should be as easy 
as issuing `yarn install`.^[NB. Need to fully understand when `yarn` draws a new version.]
* Also homebrewed JavaScript and CSS files are created as we go and no manual effort 
should be needed.
* Hence, we always have to ensure that `grunt watch` is up and running (which should be 
the case thanks to the `.Rprofile` hook)

## Inspiring repos for using build tool chains

* https://github.com/rstudio/shinydashboard
* https://github.com/rstudio/bslib
