FROM ubuntu:latest as lp-frontend
RUN mkdir /setup
COPY . /setup
WORKDIR /setup
RUN /setup/setup_frontend.sh

FROM ubuntu:latest as lp-full
EXPOSE 3000
EXPOSE 8000
WORKDIR /
RUN mkdir /setup
COPY . /setup
COPY run_backend.sh /usr/local/bin
COPY lemmingpants.conf /usr/local/etc
COPY --from=lp-frontend /setup/static /srv/static
CMD ["/bin/bash", "/usr/local/bin/run_backend.sh"]
RUN ["/bin/bash", "/setup/setup_backend.sh"]
