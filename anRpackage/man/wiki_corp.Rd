\name{wiki_corp}
\alias{wiki_corp}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("wiki_corp")}
\format{
  The format is:
List of 4
 $ documents:'data.frame':	1 obs. of  2 variables:
  ..$ texts : chr " anarchism originated as a term of abuse first used against early working class radicals including the diggers "| __truncated__
  ..$ doc_id: chr "text8"
 $ metadata :List of 2
  ..$ source : chr "/home/kohei/packages/quanteda/docs/articles/pkgdown/replication/* on x86_64 by kohei"
  ..$ created: chr "Fri Dec 22 13:55:11 2017"
 $ settings :List of 12
  ..$ stopwords          : NULL
  ..$ collocations       : NULL
  ..$ dictionary         : NULL
  ..$ valuetype          : chr "glob"
  ..$ stem               : logi FALSE
  ..$ delimiter_word     : chr " "
  ..$ delimiter_sentence : chr ".!?"
  ..$ delimiter_paragraph: chr "\n\n"
  ..$ clean_tolower      : logi TRUE
  ..$ clean_remove_digits: logi TRUE
  ..$ clean_remove_punct : logi TRUE
  ..$ units              : chr "documents"
  ..- attr(*, "class")= chr [1:2] "settings" "list"
 $ tokens   : NULL
 - attr(*, "class")= chr [1:2] "corpus" "list"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(wiki_corp)
## maybe str(wiki_corp) ; plot(wiki_corp) ...
}
\keyword{datasets}
