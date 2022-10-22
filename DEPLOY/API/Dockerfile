FROM rstudio/plumber
LABEL lbl="Nossa imagem R para API de Modelo ML"
# Instalar pacote mgcv 
RUN R -e 'install.packages(c("tidyverse", "nlme", "mgcv"))'

# iCopia modelo e script
RUN mkdir /data
COPY modelo.RData /data
COPY script_consumo.R /data

# Plumb 
#EXPOSE 8080
#ENTRYPOINT ["R", "-e", \
#"pr <- plumber::plumb('/data/script_consumo.R'); pr$run(host = "0.0.0.0", port=8080)"]

CMD ["/data/script_consumo.R"]