FROM erlang:27

ENV DEBIAN_FRONTEND=noninteractive \
    ERL_AFLAGS="+pc unicode -kernel shell_history enabled"

RUN useradd -ms /bin/bash builder

# --- OS deps ----------------------------------------------------
RUN apt-get update && apt-get install -y \
    git gcc rsync wget curl make build-essential \
    ruby ruby-dev default-jre graphviz \
    postgresql postgresql-contrib \
    systemtap systemtap-sdt-dev \
    && apt-get clean

# --- Ruby gems via Bundler -------------------------------------
# Copy Gemfile first so Docker layer cache works
COPY ../Gemfile /tmp/Gemfile
WORKDIR /tmp
RUN gem install --no-document bundler && \
    bundle install
    
# Install rebar3
RUN git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap && \
    ./rebar3 local install

# Initialize PostgreSQL database for examples
USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "CREATE USER myuser WITH SUPERUSER PASSWORD 'mypassword';" && \
    createdb -O myuser mydb

USER builder

# stop git from complaining
RUN git config --global --add safe.directory /workspaces/theBeamBook

# Set the default command to start PostgreSQL and keep the container running
CMD ["/bin/bash", "-c", "service postgresql start && tail -f /var/lib/postgresql/data/logfile"]
