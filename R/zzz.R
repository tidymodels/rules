# nocov start

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  make_cubist()
  make_c5()
  make_rule_fit()

  s3_register("stats::update", "rule_fit")
  s3_register("stats::update", "cubist_rules")
  s3_register("stats::update", "C5_rules")

  s3_register("generics::tidy", "cubist")
  s3_register("generics::tidy", "xrf")

  maybe_register_tunable_methods()
}

# This package has specific methods for the `tunable()` generic. That generic
# is defined in the `tune` package. As of R 4.0, we need to register them.
# Since `tune` is not on CRAN, we only register them if tune is installed
maybe_register_tunable_methods <- function() {

    ns <- asNamespace("rules")

  names <- names(ns)
  names <- grep("tunable.rule_fit", names, fixed = TRUE, value = TRUE)

  for (name in names) {
    s3_register("tune::tunable", name, get(name, envir = ns))
  }

  invisible(NULL)
}

# vctrs:::s3_register()
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)

      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}

# nocov end
