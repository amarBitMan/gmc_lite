# Build Stage
FROM erlang:25-alpine as builder

# Setting up working directory
WORKDIR /app
COPY src src/
COPY rebar.config .
COPY priv/sys.config priv/sys.config

# Create a Release
RUN rebar3 release

# Final Stage
FROM alpine

RUN apk --no-cache add --update openssl ncurses libstdc++ libgcc

COPY --from=builder /app/_build/default/rel/gmc_lite /opt/gmc_lite

EXPOSE 9011

# Copy src code to container
CMD ["/opt/gmc_lite/bin/gmc_lite", "foreground"]
