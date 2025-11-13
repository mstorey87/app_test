# Start from the Rocker Shiny base image
FROM rocker/shiny:latest

# Copy the app into the image
COPY . /srv/shiny-server/

# Install any additional R packages (only shiny needed here)
RUN R -e "install.packages('shiny')"

# Expose Shiny’s default port
EXPOSE 3838

# Run Shiny server
CMD ["/usr/bin/shiny-server"]
