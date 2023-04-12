\name{resp}
\alias{resp}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("resp")}
\format{
  The format is:
List of 10
 $ url        : chr "https://api.openai.com/v1/embeddings"
 $ status_code: int 429
 $ headers    :List of 10
  ..$ date                     : chr "Thu, 06 Apr 2023 14:02:39 GMT"
  ..$ content-type             : chr "application/json; charset=utf-8"
  ..$ content-length           : chr "206"
  ..$ vary                     : chr "Origin"
  ..$ x-request-id             : chr "0324030ef2b0c37559fe1d651891a92c"
  ..$ strict-transport-security: chr "max-age=15724800; includeSubDomains"
  ..$ cf-cache-status          : chr "DYNAMIC"
  ..$ server                   : chr "cloudflare"
  ..$ cf-ray                   : chr "7b3a91d75ae5023d-GRU"
  ..$ alt-svc                  : chr "h3=\":443\"; ma=86400, h3-29=\":443\"; ma=86400"
  ..- attr(*, "class")= chr [1:2] "insensitive" "list"
 $ all_headers:List of 1
  ..$ :List of 3
  .. ..$ status : int 429
  .. ..$ version: chr "HTTP/2"
  .. ..$ headers:List of 10
  .. .. ..$ date                     : chr "Thu, 06 Apr 2023 14:02:39 GMT"
  .. .. ..$ content-type             : chr "application/json; charset=utf-8"
  .. .. ..$ content-length           : chr "206"
  .. .. ..$ vary                     : chr "Origin"
  .. .. ..$ x-request-id             : chr "0324030ef2b0c37559fe1d651891a92c"
  .. .. ..$ strict-transport-security: chr "max-age=15724800; includeSubDomains"
  .. .. ..$ cf-cache-status          : chr "DYNAMIC"
  .. .. ..$ server                   : chr "cloudflare"
  .. .. ..$ cf-ray                   : chr "7b3a91d75ae5023d-GRU"
  .. .. ..$ alt-svc                  : chr "h3=\":443\"; ma=86400, h3-29=\":443\"; ma=86400"
  .. .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
 $ cookies    :'data.frame':	0 obs. of  7 variables:
  ..$ domain    : logi(0) 
  ..$ flag      : logi(0) 
  ..$ path      : logi(0) 
  ..$ secure    : logi(0) 
  ..$ expiration: 'POSIXct' num(0) 
  ..$ name      : logi(0) 
  ..$ value     : logi(0) 
 $ content    : raw [1:206] 7b 0a 20 20 ...
 $ date       : POSIXct[1:1], format: "2023-04-06 14:02:39"
 $ times      : Named num [1:6] 0 0.0387 0.0623 0.1125 0.1125 ...
  ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
 $ request    :List of 7
  ..$ method    : chr "POST"
  ..$ url       : chr "https://api.openai.com/v1/embeddings"
  ..$ headers   : Named chr [1:3] "application/json, text/xml, application/xml, */*" "application/json" "Bearer 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"
  .. ..- attr(*, "names")= chr [1:3] "Accept" "Content-Type" "Authorization"
  ..$ fields    : NULL
  ..$ options   :List of 4
  .. ..$ useragent    : chr "libcurl/7.81.0 r-curl/5.0.0 httr/1.4.5"
  .. ..$ post         : logi TRUE
  .. ..$ postfieldsize: int 250913
  .. ..$ postfields   : raw [1:250913] 7b 22 6d 6f ...
  ..$ auth_token: NULL
  ..$ output    : list()
  .. ..- attr(*, "class")= chr [1:2] "write_memory" "write_function"
  ..- attr(*, "class")= chr "request"
 $ handle     :Class 'curl_handle' <externalptr> 
 - attr(*, "class")= chr "response"
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
data(resp)
## maybe str(resp) ; plot(resp) ...
}
\keyword{datasets}
