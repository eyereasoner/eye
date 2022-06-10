FROM swipl:latest
LABEL maintainer="https://github.com/bdevloed"

COPY eye.pl eye.sh.in install.sh /

RUN ./install.sh --prefix=/usr/local

ENTRYPOINT ["eye"]
