FROM asciidoctor/docker-asciidoctor:latest

RUN apk add --no-cache\
    git\
    rsync\
    erlang

RUN git config --global --add safe.directory /documents
WORKDIR /documents
CMD ["make"]
