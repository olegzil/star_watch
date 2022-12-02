# syntax=docker/dockerfile:1.0.0-experimental
# Build stage 0

FROM erlang:alpine
RUN apk add --no-cache gcc musl-dev
RUN apk add build-base
# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# install ssh client and git
RUN apk add --no-cache openssh-client git
# download public key for github.com
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan github.com >> ~/.ssh/known_hosts

#Clone the star-server repo
#RUN ssh -Tv git@github.com
RUN rm -rf /buildroot/star_watch_server
RUN --mount=type=ssh,id=starwatchsshkey git clone git@github.com:olegzil/star_watch.git /buildroot/star_watch_server

#Build the server
WORKDIR star_watch_server
RUN rebar3 as prod release

# Build stage 1
FROM alpine
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++


# Install the released application
COPY --from=0 /buildroot/star_watch_server/_build/prod/rel/star_watch_server /server
WORKDIR /mnt
RUN mkdir -p disks/star-watch-data/

# Expose relevant ports
EXPOSE 8080
EXPOSE 443
CMD ["/server/bin/star_watch_server", "foreground"]

