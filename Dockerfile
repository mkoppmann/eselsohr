############
# Metadata #
############
FROM scratch
LABEL maintainer="mkoppmann <dev@mkoppmann.at>"

###############
# Build image #
###############
FROM utdemir/ghc-musl:v24-ghc8107@sha256:fec0e81c1a3fbecc19772fb7b392dec1fd2b8b25a9f3852983987af15b920f40 AS build

# Install upx for shrinking the binary
RUN apk --no-cache add upx=~3.96

# Create the data folder for the deployment stage here, because there is no
# shell in distroless images available.
# Also create a new user so we don’t run the build process as root and create
# the folder for the files.
RUN mkdir -p /data \
 && adduser -D builder \
 && mkdir -p /build \
 && chown -R builder:builder /build
USER builder
WORKDIR /build

# Update cabal package database
RUN cabal update

# Copy files required for building all dependencies and build them
COPY *.cabal cabal.project ./
RUN cabal build \
        --dependencies-only \
        --disable-documentation \
        --disable-tests \
        -O2 \
        all

# Copy the rest of the source files and build the executable.
# After the build, run upx to shrink the binary.
COPY . .
RUN cabal install \
        --avoid-reinstalls \
        --disable-documentation \
        --disable-tests \
        --install-method=copy \
        --overwrite-policy=always \
        -O2 \
 && upx --best "${HOME}"/.cabal/bin/eselsohr-exe

####################
# Deployment image #
####################
FROM gcr.io/distroless/static:nonroot@sha256:d8afc7d6973f357162e2283551cf3347b2bb847a03d24510ee837f289505f8e3

# Copy executable from build stage
COPY --from=build /home/builder/.cabal/bin/eselsohr-exe /app/eselsohr

# Copy data folder from build stage with correct permissions
COPY --from=build --chown=nonroot:nonroot /data /data

WORKDIR /app
EXPOSE 6979

# Set Eselsohr’s data folder to /data
ENV DATA_FOLDER=/data

# Set Eselsohr’s listen address to all interfaces, so it can be used outside the
# container.
ENV LISTEN_ADDR=0.0.0.0

# Mark /data as volume for persistence
VOLUME ["/data"]

ENTRYPOINT ["./eselsohr"]
