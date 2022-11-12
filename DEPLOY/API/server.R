# server.R
library(plumber)
# 'script_consumo.R' é o arquivo que contém nosso endpoint
r <- plumb("script_consumo.R")
# Pega a porta da variável de ambiente
port <- strtoi(Sys.getenv("PORT"))
r$run(port=port, host='0.0.0.0', swagger=TRUE)