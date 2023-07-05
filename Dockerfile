FROM erlang:25.3 as builder

# Set working directory
WORKDIR /buildroot

# Install dependencies for building the server
RUN apt-get update && apt-get install -y \
    git \
    wget \
    openssl \
    ncurses-dev \
    libssl-dev

# Clone the star-server repo
RUN git clone https://github.com/olegzil/star_watch.git /buildroot/star_watch_server

# Download rebar3 from GitHub
RUN wget https://github.com/erlang/rebar3/releases/download/3.16.1/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Build the server
WORKDIR /buildroot/star_watch_server
RUN rebar3 as prod release

FROM ubuntu:latest

ENV DEBIAN_FRONTEND="noninteractive"

# Install necessary packages for the server
RUN apt-get update && apt-get install -y \
    erlang \
    postfix \
    libsasl2-modules

# Add the Ubuntu 20.04 package repository for libssl1.1
RUN apt-get install -y software-properties-common && \
    add-apt-repository -y "deb http://security.ubuntu.com/ubuntu focal-security main" && \
    apt-get update

# Install libssl1.1 from the specific repository
RUN apt-get install -y libssl1.1

# Copy the released application from the builder stage
COPY --from=builder /buildroot/star_watch_server/_build/prod/rel/star_watch_server /server

# Copy the server configuration files
COPY server_config.cfg /server/
COPY private-key.pem /server/

# Set up Postfix for Mailjet relay
RUN echo "relayhost = in-v3.mailjet.com:2525" > /etc/postfix/main.cf && \
    echo "smtp_tls_security_level = encrypt" >> /etc/postfix/main.cf && \
    echo "smtp_sasl_auth_enable = yes" >> /etc/postfix/main.cf && \
    echo "smtp_sasl_password_maps = hash:/etc/postfix/sasl_passwd" >> /etc/postfix/main.cf && \
    echo "smtp_sasl_security_options = noanonymous" >> /etc/postfix/main.cf

RUN echo "in-v3.mailjet.com:2525 66fe525ff7ed3f945f662b446f14f077:e2a5d55bea4ae9ff3e01b7ed48f0d727" > /etc/postfix/sasl_passwd &&\ 
	 postmap /etc/postfix/sasl_passwd && \
	 rm /etc/postfix/sasl_passwd && \
	 chmod 600 /etc/postfix/sasl_passwd.db && \
	 /etc/init.d/postfix restart

# Expose relevant ports
EXPOSE 8080
EXPOSE 2525
EXPOSE 587

# Start the server
CMD ["/server/bin/star_watch_server", "foreground"]

