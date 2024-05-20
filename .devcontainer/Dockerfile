FROM erlang:27

ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update && apt-get install -y \
    git \
    gcc \
    rsync \
    postgresql \
    postgresql-contrib \
    wget \
    curl \
    make \
    build-essential \
    ruby \
    ruby-dev \
    asciidoctor \
    && apt-get clean

# Install rebar3
RUN git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap && \
    ./rebar3 local install

# Set environment variable for Erlang shell history
ENV ERL_AFLAGS "-kernel shell_history enabled"

# Initialize PostgreSQL database for examples
USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "CREATE USER myuser WITH SUPERUSER PASSWORD 'mypassword';" && \
    createdb -O myuser mydb

USER root

# Set the default command to start PostgreSQL and keep the container running
CMD service postgresql start && tail -f /var/lib/postgresql/data/logfile