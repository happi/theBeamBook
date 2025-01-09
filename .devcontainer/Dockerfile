FROM erlang:27

ENV DEBIAN_FRONTEND=noninteractive

# Create a default user for the devcontainer
RUN useradd -ms /bin/bash builder

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
    default-jre \
    graphviz \
    && apt-get clean

# Additional Ruby packages
RUN gem install asciidoctor-pdf asciidoctor-diagram rouge

# Install rebar3
RUN git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap && \
    ./rebar3 local install

# Set Erlang flags for unicode text and shell history
ENV ERL_AFLAGS "+pc unicode -kernel shell_history enabled"

# Initialize PostgreSQL database for examples
USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "CREATE USER myuser WITH SUPERUSER PASSWORD 'mypassword';" && \
    createdb -O myuser mydb

USER builder

# stop git from complaining
RUN git config --global --add safe.directory /workspaces/theBeamBook

# Set the default command to start PostgreSQL and keep the container running
CMD service postgresql start && tail -f /var/lib/postgresql/data/logfile
