
# Use the official R base image
FROM rocker/r-ver:latest

# Install system libraries (if necessary)
# For this example, we're not installing any system libraries, but often you'd need some for various R packages.
# RUN apt-get update && apt-get install -y \
#     lib<name>-dev

# Install R packages
RUN R -e "install.packages('languageserver', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('celestial', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('vscDebugger', repos='https://cloud.r-project.org/')"