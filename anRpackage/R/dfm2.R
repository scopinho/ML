dfm2 <-
function (x, tolower = TRUE, remove_padding = FALSE, verbose = quanteda_options("verbose"), 
                  ...) 
{
  x <- as.tokens(x)
  if (length(intersect(names(list(...)), names(formals("tokens"))))) {
    warning("'...' should not be used for tokens() arguments; use 'tokens()' first.", 
            call. = FALSE)
    x <- (function(x, tolower = TRUE, remove_padding = FALSE, 
                   stem = NULL, select = NULL, dictionary = NULL, thesaurus = NULL, 
                   groups = NULL, ...) {
      tokens.tokens(x, ...)
    })(x, ...)
  }
  if (tolower) {
    if (verbose) 
      catm2(" ...lowercasing\n", sep = "")
    x <- tokens_tolower(x)
    tolower <- FALSE
  }
  x <- (function(stem = NULL, select = NULL, remove = NULL, 
                 dictionary = NULL, thesaurus = NULL, valuetype = NULL, 
                 case_insensitive = NULL, groups = NULL, remove_padding = NULL, 
                 ...) {
    quanteda:::check_dots(..., method = c("dfm", "tokens"))
    if (verbose) {
      catm2(" ...found ", format(length(x), big.mark = ","), 
           " document", ifelse(length(x) > 1, "s", ""), 
           ", ", format(length(types(x)), big.mark = ","), 
           " feature", ifelse(length(types(x)) > 1, "s", 
                              ""), "\n", sep = "")
    }
    if (!is.logical(remove_padding) && is.null(remove)) 
      remove <- remove_padding
    if (!is.null(groups)) {
      warning("'groups' is deprecated; use dfm_group() instead", 
              call. = FALSE)
      if (verbose) 
        catm(" ...grouping texts\n")
      x <- tokens_group(x, groups = groups, fill = FALSE)
    }
    if (!is.null(valuetype)) {
      warning("valuetype is deprecated in dfm()", call. = FALSE)
      valuetype <- match.arg(valuetype, c("glob", "regex", 
                                          "fixed"))
    }
    if (!is.null(case_insensitive)) {
      warning("case_insensitive is deprecated in dfm()", 
              call. = FALSE)
      case_insensitive <- check_logical(case_insensitive)
    }
    else {
      case_insensitive <- TRUE
    }
    if (!is.null(dictionary) || !is.null(thesaurus)) {
      warning("'dictionary' and 'thesaurus' are deprecated; use dfm_lookup() instead", 
              call. = FALSE)
      if (!is.null(thesaurus)) 
        dictionary <- dictionary(thesaurus)
      if (verbose) 
        catm2(" ...")
      x <- tokens_lookup(x, dictionary = dictionary, exclusive = ifelse(!is.null(thesaurus), 
                                                                        FALSE, TRUE), valuetype = valuetype, case_insensitive = case_insensitive, 
                         verbose = verbose)
    }
    if (!is.null(select) || !is.null(remove)) {
      if (!is.null(select) && !is.null(remove)) 
        stop("only one of select and remove may be supplied at once", 
             call. = FALSE)
      if (!is.null(select)) {
        warning("'select' is deprecated; use dfm_select() instead", 
                call. = FALSE)
        pattern <- select
      }
      if (!is.null(remove)) {
        warning("'remove' is deprecated; use dfm_remove() instead", 
                call. = FALSE)
        pattern <- remove
      }
      if (verbose) 
        catm2(" ...")
      x <- tokens_select(x, pattern = pattern, selection = if (!is.null(select)) 
        "keep"
        else "remove", valuetype = valuetype, case_insensitive = case_insensitive, 
        verbose = verbose)
    }
    if (!is.null(stem)) {
      warning("'stem' is deprecated; use dfm_wordstem() instead", 
              call. = FALSE)
      stem <- check_logical(stem)
      language <- quanteda_options("language_stemmer")
      if (verbose) 
        if (verbose) 
          catm2(" ...stemming types (", stri_trans_totitle(language), 
               ")\n", sep = "")
      x <- tokens_wordstem(x, language = language)
    }
    return(x)
  })(remove_padding = remove_padding, ...)
  if (!is.logical(remove_padding)) 
    remove_padding <- FALSE
  remove_padding <- check_logical(remove_padding)
  if (remove_padding) 
    x <- tokens_remove(x, "", valuetype = "fixed")
  type <- types(x)
  attrs <- attributes(x)
  temp <- unclass(x)
  index <- quanteda:::unlist_integer(temp, use.names = FALSE)
  if (attrs$padding) {
    type <- c("", type)
    index <- index + 1L
  }
  temp <- Matrix:::sparseMatrix(j = index, p = cumsum(c(1L, lengths(x))) - 
                         1L, x = 1L, dims = c(length(x), length(type)))
  temp <- build_dfm2(temp, type, docvars = quanteda:::get_docvars(x, user = FALSE, 
                                                      system = FALSE), meta = attrs[["meta"]])
 # temp <- build_dfm2(temp, type, docvars = quanteda:::docvars.tokens(x, user = TRUE, 
                                                                  #system = TRUE), meta = attrs[["meta"]])
  if (attrs$meta$object$what != "dictionary") {
    id <- unique(index)
    if (attrs$padding) 
      id <- c(1L, setdiff(id, 1L))
    temp <- temp[, id]
  }
  dfm.dfm(temp, tolower = FALSE, verbose = verbose)
}
