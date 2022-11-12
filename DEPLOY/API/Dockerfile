FROM rstudio/plumber
LABEL lbl="Nossa imagem R para API de Modelo ML"
# Instalar pacote mgcv 
RUN R -e 'install.packages(c("tidyverse", "nlme", "mgcv"))'

# iCopia modelo e script
RUN mkdir /data
COPY modelo.RData /data
COPY script_consumo.R /data
COPY server.R /data
WORKDIR /data

# Plumb 
EXPOSE 8080

CMD ["Rscript", "server.R"]