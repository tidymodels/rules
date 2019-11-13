# Things that should have been exported from parsnip :-O

# nocov
update_dot_check <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    stop("Extra arguments will be ignored: ",
         paste0("`", names(dots), "`", collapse = ", "),
         call. = FALSE)
  }
  invisible(NULL)
}

show_fit <- function(model, eng) {
  mod <- parsnip::translate(x = model, engine = eng)
  fit_call <- parsnip::show_call(mod)
  call_text <- deparse(fit_call)
  call_text <- paste0(call_text, collapse = "\n")
  paste0("\\preformatted{\n", call_text, "\n}\n\n")
}

new_model_spec <- function(cls, args, eng_args, mode, method, engine) {
  spec_modes <- rlang::env_get(parsnip::get_model_env(), paste0(cls, "_modes"))
  if (!(mode %in% spec_modes))
    stop("`mode` should be one of: ",
         paste0("'", spec_modes, "'", collapse = ", "),
         call. = FALSE)

  out <- list(args = args, eng_args = eng_args,
              mode = mode, method = method, engine = engine)
  class(out) <- parsnip::make_classes(cls)
  out
}

is_varying <- function(x) {
  if (is.null(x)) {
    res <- FALSE
  }
  else {
    res <- if (rlang::is_quosure(x)) {
      isTRUE(all.equal(x[[-1]], quote(varying())))
    } else {
      isTRUE(all.equal(x, quote(varying())))
    }
  }
  res
}

null_value <- function(x) {
  if (rlang::is_quosure(x)) {
    res <- isTRUE(all.equal(rlang::get_expr(x), rlang::expr(NULL)))
  }
  else {
    res <- isTRUE(all.equal(x, NULL))
  }
  res
}

# nocov end


