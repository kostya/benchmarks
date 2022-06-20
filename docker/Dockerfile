FROM debian:testing

ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1 \
    DEBIAN_FRONTEND=noninteractive
COPY ./apt.pkgs /tmp/apt.pkgs
RUN apt-get update && xargs apt-get install -y < /tmp/apt.pkgs \
    && rm -rf /var/lib/apt/lists/*

RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-12 10 \
    && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-12 10 \
    && update-alternatives --install /usr/bin/gdc gdc /usr/bin/gdc-12 10 \
    && update-alternatives --install /usr/bin/gccgo gccgo /usr/bin/gccgo-12 10 \
    && update-alternatives --install /usr/bin/clang clang /usr/bin/clang-13 10 \
    && update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-13 10 \
    && nuget update -self \
    && useradd -m dev

USER dev
RUN mkdir /home/dev/bin
WORKDIR /home/dev/bin
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV PATH="/home/dev/bin/:/home/dev/.local/bin/:${PATH}" \
    JQ_LIB_DIR=/usr/lib/x86_64-linux-gnu/ \
    LC_ALL=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8

RUN curl -Lo /home/dev/bin/coursier https://git.io/coursier-cli \
	&& chmod +x /home/dev/bin/coursier

# https://www.ruby-lang.org/en/downloads/
ARG RUBY=ruby-3.1.2
RUN curl https://cache.ruby-lang.org/pub/ruby/3.1/$RUBY.tar.gz | tar -xz
WORKDIR /home/dev/bin/$RUBY
RUN CC=clang ./configure --prefix=/home/dev/bin/ruby && make -j && make install
ENV PATH="/home/dev/bin/ruby/bin:${PATH}"
WORKDIR /home/dev/bin
RUN rm -rf $RUBY

# https://github.com/MLton/mlton/releases
ARG MLTON=20210117
RUN curl -L \
    https://github.com/MLton/mlton/releases/download/on-$MLTON-release/mlton-$MLTON-1.amd64-linux-glibc2.31.tgz \
    | tar -xz
ENV PATH="/home/dev/bin/mlton-$MLTON-1.amd64-linux-glibc2.31/bin/:${PATH}"

# https://pypy.org/download.html
ARG PYPY=pypy3.9-v7.3.9-linux64
RUN curl \
    https://downloads.python.org/pypy/$PYPY.tar.bz2 \
    | tar -xj \
    && rm /home/dev/bin/$PYPY/bin/python*
ENV PATH="/home/dev/bin/$PYPY/bin:${PATH}"

# https://www.scala-lang.org/download/
ARG SCALA=3.1.2
RUN curl -L \
    https://github.com/lampepfl/dotty/releases/download/$SCALA/scala3-$SCALA.tar.gz \
    | tar -xz
ENV PATH="/home/dev/bin/scala3-$SCALA/bin/:${PATH}"

# https://jdk.java.net/
ARG JDK=18.0.1.1
RUN curl \
    https://download.java.net/java/GA/jdk${JDK}/65ae32619e2f40f3a9af3af1851d6e19/2/GPL/openjdk-${JDK}_linux-x64_bin.tar.gz \
    | tar -xz
ENV PATH="/home/dev/bin/jdk-$JDK/bin:${PATH}"

# https://github.com/crystal-lang/crystal/releases
ARG CRYSTAL=crystal-1.4.1-1
RUN curl -L \
	https://github.com/crystal-lang/crystal/releases/download/1.4.1/$CRYSTAL-linux-x86_64.tar.gz \
	| tar -xz
ENV PATH="/home/dev/bin/$CRYSTAL/bin:${PATH}"

# https://github.com/ldc-developers/ldc/releases
ARG LDC=ldc2-1.29.0-linux-x86_64
RUN curl -L \
	https://github.com/ldc-developers/ldc/releases/download/v1.29.0/$LDC.tar.xz \
	| tar -xJ
ENV PATH="/home/dev/bin/$LDC/bin/:${PATH}"

# https://nim-lang.org/install_unix.html
ARG NIM=nim-1.6.6
RUN curl \
	https://nim-lang.org/download/$NIM-linux_x64.tar.xz \
	| tar -xJ
ENV PATH="/home/dev/bin/$NIM/bin/:${PATH}"

# https://dlang.org/download.html
RUN curl \
    https://s3.us-west-2.amazonaws.com/downloads.dlang.org/releases/2022/dmd.2.100.0.linux.tar.xz \
    | tar -xJ
ENV PATH="/home/dev/bin/dmd2/linux/bin64/:${PATH}"

# https://www.jruby.org/download
ARG JRUBY=9.3.4.0
RUN curl \
	https://repo1.maven.org/maven2/org/jruby/jruby-dist/$JRUBY/jruby-dist-$JRUBY-bin.tar.gz \
	| tar -xz \
	&& ln -s /home/dev/bin/jruby-$JRUBY/bin/jruby /home/dev/bin/jruby

# https://julialang.org/downloads/
ARG JULIA=julia-1.7.3
RUN curl \
	https://julialang-s3.julialang.org/bin/linux/x64/1.7/$JULIA-linux-x86_64.tar.gz \
	| tar -xz
ENV PATH="/home/dev/bin/$JULIA/bin/:${PATH}"

# https://swift.org/download/
ARG SWIFT=swift-5.6.1-RELEASE-ubuntu20.04
RUN curl -L \
	https://swift.org/builds/swift-5.6.1-release/ubuntu2004/swift-5.6.1-RELEASE/$SWIFT.tar.gz \
	| tar -xz \
    && ln -s /home/dev/bin/$SWIFT/usr/bin/swift /home/dev/bin/swift \
    && ln -s /home/dev/bin/$SWIFT/usr/bin/swiftc /home/dev/bin/swiftc
ENV LD_LIBRARY_PATH=/home/dev/bin/$SWIFT/usr/lib/swift/linux:$LD_LIBRARY_PATH

# https://clojure.org/guides/install_clojure
ARG CLOJURE=1.11.1.1113
RUN curl \
	https://download.clojure.org/install/linux-install-$CLOJURE.sh \
	| bash -s -- --prefix /home/dev/

ENV GHCUP_INSTALL_BASE_PREFIX="/home/dev"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
	| BOOTSTRAP_HASKELL_NONINTERACTIVE="y" sh
ENV PATH="/home/dev/.ghcup/bin/:${PATH}"

# https://www.haskell.org/ghc/
ARG GHC_VER=9.2.3
RUN ghcup install ghc $GHC_VER && ghcup set ghc $GHC_VER

# Shared packages for all Haskell code
RUN cabal update \
        && cabal install network raw-strings-qq --lib \
        && cabal install hlint
ENV GHC_PACKAGE_PATH="~/.cabal/store/ghc-${GHC_VER}/package.db:"

# https://github.com/graalvm/graalvm-ce-builds/releases
ARG GRAALVM=22.1.0
RUN curl -L \
        https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-$GRAALVM/graalvm-ce-java17-linux-amd64-$GRAALVM.tar.gz \
	| tar -xz \
    && ln -s /home/dev/bin/graalvm-ce-java17-$GRAALVM/bin/gu /home/dev/bin/gu
RUN gu install ruby \
    && ln -s /home/dev/bin/graalvm-ce-java17-$GRAALVM/bin/truffleruby /home/dev/bin/truffleruby
# TODO: uncomment after truffleruby properly supports OpenSSL 3
# RUN /home/dev/bin/graalvm-ce-java17-$GRAALVM/languages/ruby/lib/truffle/post_install_hook.sh

# https://github.com/dotnet/core/tree/main/release-notes
RUN mkdir dotnet && curl \
    https://download.visualstudio.microsoft.com/download/pr/e075dadf-22a9-482b-9387-bf8341a4f837/ab20e3e34c2c8be290d9938590f208ed/dotnet-sdk-6.0.203-linux-x64.tar.gz \
    | tar -xz -C dotnet
ENV PATH="/home/dev/bin/dotnet:${PATH}"
ENV DOTNET_ROOT="/home/dev/bin/dotnet"
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1

# https://nodejs.org/en/download/current/
ARG NODE=v18.2.0
RUN curl \
	https://nodejs.org/dist/$NODE/node-$NODE-linux-x64.tar.xz \
	| tar -xJ
ENV PATH="/home/dev/bin/node-$NODE-linux-x64/bin/:${PATH}"

# https://golang.org/dl/
RUN curl -L \
    https://go.dev/dl/go1.18.2.linux-amd64.tar.gz \
    | tar -xz
ENV PATH="/home/dev/bin/go/bin/:${PATH}"

# https://www.rust-lang.org/tools/install
ENV CARGO_HOME="/home/dev/bin/.cargo" PATH="/home/dev/bin/.cargo/bin:${PATH}"
RUN curl https://sh.rustup.rs | sh -s -- -y

# https://ocaml.org/releases/
ARG OCAML=4.14.0
RUN opam init --disable-sandboxing -n --root=/home/dev/bin/opam --compiler=ocaml-base-compiler.$OCAML
ENV PATH="/home/dev/bin/opam/ocaml-base-compiler.$OCAML/bin/:${PATH}"

# https://kotlinlang.org/docs/command-line.html
ARG KOTLIN=1.6.21
RUN curl -L -o kotlin-compiler-$KOTLIN.zip \
	https://github.com/JetBrains/kotlin/releases/download/v$KOTLIN/kotlin-compiler-$KOTLIN.zip \
	&& unzip kotlin-compiler-$KOTLIN.zip \
	&& rm kotlin-compiler-$KOTLIN.zip
ENV PATH="/home/dev/bin/kotlinc/bin/:${PATH}"

# https://download.racket-lang.org/
ARG RACKET=8.5
RUN curl -o racket-$RACKET-x86_64-linux-cs.sh \
    https://www.cs.utah.edu/plt/installers/$RACKET/racket-$RACKET-x86_64-linux-cs.sh \
    && sh racket-$RACKET-x86_64-linux-cs.sh --unix-style --dest /home/dev/ \
    && rm racket-$RACKET-x86_64-linux-cs.sh

# https://github.com/vlang/v/releases
ARG VLANG=weekly.2022.21
RUN curl -Lo v_linux.zip \
        https://github.com/vlang/v/releases/download/$VLANG/v_linux.zip \
        && unzip -q v_linux.zip \
        && rm v_linux.zip
ENV PATH="/home/dev/bin/v/:${PATH}"

# https://ziglang.org/download
ARG ZIG=zig-linux-x86_64-0.9.1
RUN curl \
    https://ziglang.org/download/0.9.1/$ZIG.tar.xz \
    | tar -xJ
ENV PATH="/home/dev/bin/$ZIG/:${PATH}"

# Using more stable LuaJIT, used in Alpine and other distros
# https://github.com/openresty/luajit2/tags
RUN export VERSION=2.1-20220411 \
    && curl -L \
    https://github.com/openresty/luajit2/archive/refs/tags/v$VERSION.tar.gz \
    | tar -xz \
    && make -C luajit2-$VERSION -j \
    && make -C luajit2-$VERSION install PREFIX=/home/dev/bin/luajit \
    && rm -rf luajit2-$VERSION
ENV PATH="/home/dev/bin/luajit/bin:${PATH}"

ARG SCRIPTS_VER=unknown
COPY *.rb ./

RUN ./versions.rb

ENTRYPOINT ["./run.rb"]

RUN cat /etc/os-release
