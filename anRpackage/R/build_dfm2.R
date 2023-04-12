build_dfm2 <-
function (x, features, docvars = data.frame(), meta = list(), 
          class = "dfm", ...) 
{
  result <- new(class, as(as(as(x, "CsparseMatrix"), "generalMatrix"), 
                          "dMatrix"), docvars = docvars, meta = make_meta("dfm", 
                                                                          inherit = meta, ...))
  result@Dimnames <- list(docs = as.character(docvars[["docname_"]]), 
                          features = as.character(features))
  return(result)
}
