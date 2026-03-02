FROM ubuntu:20.04

WORKDIR /usr/src/recetox-aplcms
COPY conda/environment-dev.yaml /usr/src/recetox-aplcms/conda/environment-dev.yaml

# install R-base dependencies and wget
RUN apt-get update -y && \
    apt-get install -y libxml2-dev && \
    apt-get install -y libssl-dev && \
    apt-get install -y libcurl4-openssl-dev && \
    apt-get install -y libcgal-dev && \
    apt-get install -y libglu1-mesa-dev && \
    apt-get install -y libgcc-10-dev && \
    apt-get install -y wget && \
    apt-get install -y mono-runtime

# download and install miniconda
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh \
    && bash /tmp/miniconda.sh -b -p /bin/local/miniconda
ENV PATH="/bin/local/miniconda/bin:$PATH"

# configure conda and create recetox-aplcms-dev environment
RUN conda init bash && \
    conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main && \
    conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r && \
    conda update conda && \
    conda config --add channels conda-forge && \
    conda config --add channels bioconda && \
    conda config --set channel_priority strict && \
    conda env create -f conda/environment-dev.yaml

RUN /bin/local/miniconda/envs/recetox-aplcms-dev/bin/Rscript -e 'install.packages("vscDebugger", repos = "https://manuelhentschel.r-universe.dev")' -e 'rawrr::installRawFileReaderDLLs()' -e 'rawrr::installRawrrExe()'

ENTRYPOINT ["/bin/local/miniconda/envs/recetox-aplcms-dev/bin/R", "-e", "devtools::test()"]