FROM debian:12-slim

ARG USER_ID=1000
ARG GROUP_ID=1000
ARG OCAML_VERSION=4.14.2

ENV DEBIAN_FRONTEND=noninteractive

# Native build tools and the system libraries required by OCaml packages and LLVM.
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        build-essential \
        ca-certificates \
        cmake \
        curl \
        git \
        gnupg \
        libgmp-dev \
        libxml2-dev \
        libz-dev \
        m4 \
        ninja-build \
        opam \
        pkg-config \
        python3-dev \
    && curl --fail --silent --show-error --location \
        https://apt.llvm.org/llvm-snapshot.gpg.key \
        | gpg --dearmor --output /usr/share/keyrings/llvm-archive-keyring.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/llvm-archive-keyring.gpg] https://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-16 main" \
        > /etc/apt/sources.list.d/llvm.list \
    && apt-get update \
    && apt-get install --yes --no-install-recommends \
        clang-16 \
        lld-16 \
        llvm-16 \
        llvm-16-dev \
    && rm -rf /var/lib/apt/lists/*

# LLVM's versioned bin directory provides the unversioned clang, clang++, and llc
# commands used by the existing project scripts.
ENV PATH="/usr/lib/llvm-16/bin:/home/developer/.opam/blink/bin:${PATH}" \
    LLVM_DIR="/usr/lib/llvm-16/lib/cmake/llvm" \
    CC="clang" \
    CXX="clang++" \
    OPAMROOT="/home/developer/.opam" \
    OPAMSWITCH="blink"

RUN groupadd --gid "${GROUP_ID}" developer \
    && useradd --create-home --uid "${USER_ID}" --gid "${GROUP_ID}" --shell /bin/bash developer

USER developer

RUN opam init --bare --disable-sandboxing --yes \
    && opam switch create blink "ocaml-base-compiler.${OCAML_VERSION}" --yes

# Install dependencies in an image layer so source edits do not reinstall them.
WORKDIR /tmp/blink-dependencies
COPY --chown=developer:developer frontend/blink.opam ./blink.opam
RUN opam install . --deps-only --with-test --yes

WORKDIR /workspace

CMD ["bash"]
