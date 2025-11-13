# Use R + Shiny base image
FROM rocker/shiny:latest

# Copy the app
COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server/

# Install R packages your app needs
RUN R -e "install.packages('shiny')"

# Expose Shiny's default port
EXPOSE 3838

# Run Shiny server
CMD ["/usr/bin/shiny-server"]
