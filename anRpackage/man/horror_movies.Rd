\name{horror_movies}
\alias{horror_movies}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("horror_movies")}
\format{
  A data frame with 1000 observations on the following 20 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{original_title}}{a character vector}
    \item{\code{title}}{a character vector}
    \item{\code{original_language}}{a character vector}
    \item{\code{overview}}{a character vector}
    \item{\code{tagline}}{a character vector}
    \item{\code{release_date}}{a Date}
    \item{\code{poster_path}}{a character vector}
    \item{\code{popularity}}{a numeric vector}
    \item{\code{vote_count}}{a numeric vector}
    \item{\code{vote_average}}{a numeric vector}
    \item{\code{budget}}{a numeric vector}
    \item{\code{revenue}}{a numeric vector}
    \item{\code{runtime}}{a numeric vector}
    \item{\code{status}}{a character vector}
    \item{\code{adult}}{a logical vector}
    \item{\code{backdrop_path}}{a character vector}
    \item{\code{genre_names}}{a character vector}
    \item{\code{collection}}{a numeric vector}
    \item{\code{collection_name}}{a character vector}
  }
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
data(horror_movies)
## maybe str(horror_movies) ; plot(horror_movies) ...
}
\keyword{datasets}
