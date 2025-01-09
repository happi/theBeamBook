FROM asciidoctor/docker-asciidoctor:latest

# Create a default user instead of root
RUN adduser --disabled-password --shell /bin/bash builder

RUN apk add --no-cache\
    git\
    rsync\
    erlang

USER builder
RUN git config --global --add safe.directory /documents
WORKDIR /documents
CMD ["make"]
