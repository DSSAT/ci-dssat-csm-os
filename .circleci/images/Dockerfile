FROM debian:stable-slim as build
ARG DSSAT_GIT_REPO=https://github.com/DSSAT/CI-dssat-csm-os.git
ARG DSSAT_GIT_DATA=https://github.com/DSSAT/dssat-csm-data.git
ARG DSSAT_GIT_TAG=HEAD

# Install deps and update OS
RUN apt-get update
RUN apt-get install -y git ca-certificates gfortran cmake

# Checkout code
WORKDIR /dssat-csm-os
RUN git clone "${DSSAT_GIT_REPO}" .
RUN git checkout "${DSSAT_GIT_TAG}"

# Compile
RUN mkdir build
WORKDIR /dssat-csm-os/build
RUN cmake -DCMAKE_INSTALL_PREFIX=/Applications/DSSAT48 ..
RUN make
RUN make install

WORKDIR /dssat-csm-data
RUN git clone "${DSSAT_GIT_DATA}" .

FROM debian:stable-slim as runtime
ENV DSSAT_HOME=/Applications/DSSAT48/

#Create folder to run simulations
WORKDIR /
RUN mkdir /Applications

COPY --from=build /dssat-csm-data/ /Applications/DSSAT48
COPY --from=build /Applications/DSSAT48 /Applications/DSSAT48/

#COPY --from=build /Applications/DSSAT48/BatchFiles/All.L48 /Applications/DSSAT48/
#COPY --from=build /Applications/DSSAT48/BatchFiles/All.L48 /Applications/DSSAT48/

WORKDIR /Applications/DSSAT48
RUN sed -E 's|(DSSAT48/)([^/])([^/]*)|\1\U\2\L\3|' BatchFiles/All.L48 > All.L48
RUN sed -i 's/Bambaragroundnut/BambaraGroundnut/g' All.L48
RUN sed -i 's/Bellpepper/BellPepper/g' All.L48
RUN sed -i 's/Greenbean/GreenBean/g' All.L48
RUN sed -i 's/Pearlmillet/PearlMillet/g' All.L48
RUN sed -i 's/Pineapple/PineApple/g' All.L48
RUN sed -i 's/Sweetcorn/SweetCorn/g' All.L48

#Run A
WORKDIR /Applications/DSSAT48/A
RUN cp /Applications/DSSAT48/All.L48 .
RUN /Applications/DSSAT48/dscsm048 B All.L48

#Run B
WORKDIR /Applications/DSSAT48/B
RUN cp /Applications/DSSAT48/All.L48 .
RUN /Applications/DSSAT48/dscsm048 B All.L48