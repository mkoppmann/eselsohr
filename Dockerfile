############
# Metadata #
############
FROM scratch
LABEL maintainer="mkoppmann <dev@mkoppmann.at>"

###############
# Build image #
###############
FROM haskell:9.6.2-slim@sha256:83ed7eedac231090907ed96b69b57f54a6db70ce043a714786f096b8ea11eb5a AS build

# Create the data folder for the deployment stage here, because there is no
# shell in distroless images available.
# Also create a new user so we don’t run the build process as root and create
# the folder for the files.
RUN mkdir -p /data \
 && adduser --system --group builder \
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
COPY . .
RUN cabal install \
        --disable-documentation \
        --disable-tests \
        --install-method=copy \
        -O2

####################
# Deployment image #
####################
FROM gcr.io/distroless/base:nonroot@sha256:c62385962234a3dae5c9e9777dedc863d99f676b7202cd073e90b06e46021994

# Copy missing shared libraries from build stage
COPY --from=build /lib/x86_64-linux-gnu/libz.so.1 /lib/x86_64-linux-gnu/libz.so.1
COPY --from=build /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.10

# Copy executable from build stage
COPY --from=build /home/builder/.cabal/bin/eselsohr-exe /app/eselsohr

# Copy static folder from build stage
COPY --from=build --chown=nonroot:nonroot /build/static /app/static

# Copy data folder from build stage with correct permissions
COPY --from=build --chown=nonroot:nonroot /data /data

WORKDIR /app
EXPOSE 6979

# Set Eselsohr’s data folder to /data
ENV DATA_FOLDER=/data

# Set Eselsohr’s listen address to all interfaces, so it can be used outside the
# container.
ENV LISTEN_ADDR=0.0.0.0

# Set Eselsohr’s static folder path
ENV STATIC_FOLDER_PATH=/app/static

# Mark /data as volume for persistence
VOLUME ["/data"]

ENTRYPOINT ["./eselsohr"]
