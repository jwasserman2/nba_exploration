FROM --platform=linux/amd64 rocker/tidyverse:4

RUN R -e "install.packages(c('gam', 'splines'))"
RUN useradd -ms /bin/bash user
USER user
COPY --chown=user:user . ./home/user
WORKDIR /home/user
CMD ["bash"]
