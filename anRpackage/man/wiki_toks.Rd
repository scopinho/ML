\name{wiki_toks}
\alias{wiki_toks}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("wiki_toks")}
\format{
  The format is:
List of 1
 $ text1: chr [1:17005207] "anarchism" "originated" "as" "a" ...
 - attr(*, "types")= chr [1:253854] "anarchism" "originated" "as" "a" ...
 - attr(*, "padding")= logi FALSE
 - attr(*, "class")= chr "tokens"
 - attr(*, "docvars")='data.frame':	1 obs. of  4 variables:
  ..$ docname_: chr "text1"
  ..$ docid_  : Factor w/ 1 level "text1": 1
  ..$ segid_  : int 1
  ..$ doc_id  : chr "text8"
 - attr(*, "meta")=List of 3
  ..$ system:List of 5
  .. ..$ package-version:Classes 'package_version', 'numeric_version'  hidden list of 1
  .. .. ..$ : int [1:3] 3 3 0
  .. ..$ r-version      :Classes 'R_system_version', 'package_version', 'numeric_version'  hidden list of 1
  .. .. ..$ : int [1:3] 4 2 3
  .. ..$ system         : Named chr [1:3] "Linux" "x86_64" "scopinho"
  .. .. ..- attr(*, "names")= chr [1:3] "sysname" "machine" "user"
  .. ..$ directory      : chr "/home/scopinho/github/ML/Embeddings"
  .. ..$ created        : Date[1:1], format: "2023-04-07"
  ..$ object:List of 6
  .. ..$ unit        : chr "documents"
  .. ..$ what        : chr "word"
  .. ..$ ngram       : int 1
  .. ..$ skip        : int 0
  .. ..$ concatenator: chr "_"
  .. ..$ summary     :List of 2
  .. .. ..$ hash: chr(0) 
  .. .. ..$ data: NULL
  ..$ user  :List of 2
  .. ..$ source : chr "/home/kohei/packages/quanteda/docs/articles/pkgdown/replication/* on x86_64 by kohei"
  .. ..$ created: chr "Fri Dec 22 13:55:11 2017"
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
data(wiki_toks)
## maybe str(wiki_toks) ; plot(wiki_toks) ...
}
\keyword{datasets}
