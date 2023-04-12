\name{build_dfm2}
\alias{build_dfm2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
build_dfm2(x, features, docvars = data.frame(), meta = list(), class = "dfm", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{features}{
%%     ~~Describe \code{features} here~~
}
  \item{docvars}{
%%     ~~Describe \code{docvars} here~~
}
  \item{meta}{
%%     ~~Describe \code{meta} here~~
}
  \item{class}{
%%     ~~Describe \code{class} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
