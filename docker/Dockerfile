FROM debian:testing

RUN apt-get update \
  && apt-get install -y --no-install-recommends gnupg dirmngr \
  && rm -rf /var/lib/apt/lists/* \
  && export GNUPGHOME="$(mktemp -d)" \
  && gpg --batch --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
  && gpg --batch --export --armor 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF > /etc/apt/trusted.gpg.d/mono.gpg.asc \
  && gpgconf --kill all \
  && rm -rf "$GNUPGHOME" \
  && apt-key list | grep Xamarin \
  && apt-get purge -y --auto-remove gnupg dirmngr

# https://www.mono-project.com/download/stable/
ARG MONO_VERSION=6.8.0.105
RUN echo "deb http://download.mono-project.com/repo/debian stable-stretch/snapshots/$MONO_VERSION main" > /etc/apt/sources.list.d/mono-official-vs.list

RUN  apt-get update \
  && apt-get install -y wget xz-utils build-essential libtool-bin cmake git curl vim powercap-utils \
     libgmp-dev libssl-dev libssl1.1 libxml2 lbzip2 libtinfo5 libtool zlib1g libicu63 \
     libpcre3-dev libevent-dev libboost-dev libjson-c-dev libblas-dev liblapack-dev libopenblas-dev \
     libjq-dev libonig-dev \
     python3-numpy python3 python3-ujson \
     mono-devel nuget \
     chezscheme gccgo gdc valac \
     luajit lua5.3 lua-socket lua-posix \
     tcl tcllib php elixir clang-9 opam \
  && rm -rf /var/lib/apt/lists/* /tmp/* \
  && ln -s /usr/bin/clang-9 /usr/bin/clang

# Requires for https://crates.io/crates/jq-sys in Debian
ENV JQ_LIB_DIR=/usr/lib/x86_64-linux-gnu/

WORKDIR /opt

RUN curl -Lo /usr/local/bin/coursier https://git.io/coursier-cli \
    && chmod +x /usr/local/bin/coursier

# https://github.com/MLton/mlton/releases
ARG MLTON=20180207
RUN wget --progress=dot:giga -O - \
    https://github.com/MLton/mlton/releases/download/on-$MLTON-release/mlton-$MLTON-1.amd64-linux.tgz \
    | tar -xz
ENV PATH="/opt/mlton-$MLTON-1.amd64-linux/bin/:${PATH}"

# https://www.ruby-lang.org/en/downloads/
RUN export VERSION=ruby-2.7.1 \
    && wget --progress=dot:giga -O - \
    https://cache.ruby-lang.org/pub/ruby/2.7/$VERSION.tar.gz \
    | tar -xz \
    && cd $VERSION \
    && ./configure --prefix=/opt/ruby && make -j && make install \
    && cd .. && rm -rf $VERSION
ENV PATH="/opt/ruby/bin:${PATH}"

# https://github.com/dotnet/core/tree/master/release-notes
ARG DOTNET_CORE=3.1.102
RUN mkdir dotnet && wget --progress=dot:giga -O - \
    https://download.visualstudio.microsoft.com/download/pr/57e63f03-ebdf-4c22-96ff-2b85dc70cf7f/988576869e82a80f4a97ca5a733a5295/dotnet-sdk-$DOTNET_CORE-linux-x64.tar.gz \
    | tar -xz -C dotnet
ENV PATH="/opt/dotnet:${PATH}"

# https://pypy.org/download.html
ARG PYPY=pypy3.6-v7.3.0-linux64
RUN wget --progress=dot:giga -O - \
    https://bitbucket.org/pypy/pypy/downloads/$PYPY.tar.bz2 \
    | tar -xj
ENV PATH="/opt/$PYPY/bin:${PATH}"

# https://www.scala-lang.org/download/
ARG SCALA=2.13.1
RUN wget --progress=dot:giga -O - \
    https://downloads.lightbend.com/scala/$SCALA/scala-$SCALA.tgz \
    | tar -xz
ENV PATH="/opt/scala-$SCALA/bin/:${PATH}"

# https://jdk.java.net/
ARG JDK=14
RUN wget --progress=dot:giga -O - \
    https://download.java.net/java/GA/jdk14/076bab302c7b4508975440c56f6cc26a/36/GPL/openjdk-${JDK}_linux-x64_bin.tar.gz \
    | tar -xz
ENV PATH="/opt/jdk-$JDK/bin:${PATH}"

# https://github.com/crystal-lang/crystal/releases
ARG CRYSTAL=crystal-0.33.0-1
RUN wget --progress=dot:giga -O - \
    https://github.com/crystal-lang/crystal/releases/download/0.33.0/$CRYSTAL-linux-x86_64.tar.gz \
    | tar -xz
ENV PATH="/opt/$CRYSTAL/bin:${PATH}"

# https://github.com/ldc-developers/ldc/releases
ARG LDC=ldc2-1.20.1-linux-x86_64
RUN wget --progress=dot:giga -O - \
    https://github.com/ldc-developers/ldc/releases/download/v1.20.1/$LDC.tar.xz \
    | tar -xJ
ENV PATH="/opt/$LDC/bin/:${PATH}"

# https://nim-lang.org/install_unix.html
ARG NIM=nim-1.0.6
RUN wget --progress=dot:giga -O - \
    https://nim-lang.org/download/$NIM-linux_x64.tar.xz \
    | tar -xJ
ENV PATH="/opt/$NIM/bin/:${PATH}"

# https://dlang.org/download.html
RUN wget --progress=dot:giga -O - \
    http://downloads.dlang.org/releases/2.x/2.091.0/dmd.2.091.0.linux.tar.xz \
    | tar -xJ
ENV PATH="/opt/dmd2/linux/bin64/:${PATH}"

# https://nodejs.org/en/download/current/
ARG NODE=v13.12.0
RUN wget --progress=dot:giga -O - \
    https://nodejs.org/dist/$NODE/node-$NODE-linux-x64.tar.xz \
    | tar -xJ
ENV PATH="/opt/node-$NODE-linux-x64/bin/:${PATH}"

# https://www.jruby.org/download
ARG JRUBY=9.2.11.1
RUN wget --progress=dot:giga -O - \
    https://repo1.maven.org/maven2/org/jruby/jruby-dist/$JRUBY/jruby-dist-$JRUBY-bin.tar.gz \
    | tar -xz \
    && ln -s /opt/jruby-$JRUBY/bin/jruby /usr/bin/jruby

# https://golang.org/dl/
RUN wget --progress=dot:giga -O - \
    https://dl.google.com/go/go1.14.1.linux-amd64.tar.gz \
    | tar -xz
ENV PATH="/opt/go/bin/:${PATH}"

# https://www.rust-lang.org/tools/install
ENV CARGO_HOME="/opt/.cargo" PATH="/opt/.cargo/bin:${PATH}"
RUN wget -O - https://sh.rustup.rs | sh -s -- -y

# https://julialang.org/downloads/
ARG JULIA=julia-1.4.0
RUN wget --progress=dot:giga -O - \
    https://julialang-s3.julialang.org/bin/linux/x64/1.4/$JULIA-linux-x86_64.tar.gz \
    | tar -xz
ENV PATH="/opt/$JULIA/bin/:${PATH}"

# https://swift.org/download/
ARG SWIFT=swift-5.2.1-RELEASE-ubuntu18.04
RUN wget --progress=dot:giga -O - \
    https://swift.org/builds/swift-5.2.1-release/ubuntu1804/swift-5.2.1-RELEASE/$SWIFT.tar.gz \
    | tar -xz \
    && ln -s /opt/$SWIFT/usr/bin/swift /usr/bin/swift

# https://github.com/vlang/v
RUN git clone https://github.com/vlang/v \
    && cd v \
    && make
ENV PATH="/opt/v/:${PATH}"

# https://ocaml.org/releases/
ARG OCAML=4.10.0
RUN opam init --disable-sandboxing -n --root=/opt/opam --compiler=$OCAML
ENV PATH="/opt/opam/$OCAML/bin/:${PATH}"

# https://clojure.org/community/downloads
ARG CLOJURE=1.10.1.536
RUN wget --progress=dot:giga -O - \
    https://download.clojure.org/install/linux-install-$CLOJURE.sh \
    | bash -s

# https://download.racket-lang.org/
ARG RACKET=7.6
RUN wget --progress=dot:giga \
    https://mirror.racket-lang.org/installers/$RACKET/racket-$RACKET-x86_64-linux.sh \
    && sh racket-$RACKET-x86_64-linux.sh --unix-style --dest /usr/ \
    && rm racket-$RACKET-x86_64-linux.sh

ENV GHCUP_INSTALL_BASE_PREFIX="/opt"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE="y" sh
ENV PATH="/opt/.ghcup/bin/:${PATH}"

# https://www.haskell.org/ghc/
ARG GHC_VER=8.10.1
RUN ghcup install $GHC_VER && ghcup set $GHC_VER

# Shared packages for all Haskell code
RUN cabal update && cabal install network-simple --lib
ENV GHC_PACKAGE_PATH="~/.cabal/store/ghc-${GHC_VER}/package.db:"

# https://github.com/graalvm/graalvm-ce-builds/releases
ARG GRAALVM=20.0.0
RUN wget --progress=dot:giga -O - \
    https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-$GRAALVM/graalvm-ce-java11-linux-amd64-$GRAALVM.tar.gz \
    | tar -xz \
    && ln -s /opt/graalvm-ce-java11-$GRAALVM/bin/gu /usr/bin/gu
RUN gu install ruby \
    && /opt/graalvm-ce-java11-$GRAALVM/languages/ruby/lib/truffle/post_install_hook.sh \
    && ln -s /opt/graalvm-ce-java11-$GRAALVM/bin/truffleruby /usr/bin/truffleruby

# https://kotlinlang.org/docs/tutorials/command-line.html
ARG KOTLIN=1.3.71
RUN wget --progress=dot:giga \
    https://github.com/JetBrains/kotlin/releases/download/v$KOTLIN/kotlin-compiler-$KOTLIN.zip \
    && unzip kotlin-compiler-$KOTLIN.zip \
    && rm kotlin-compiler-$KOTLIN.zip
ENV PATH="/opt/kotlinc/bin/:${PATH}"

ARG SCRIPTS_VER=unknown
COPY *.rb ./

RUN ./versions.rb

ENTRYPOINT ["./run.rb"]

RUN cat /etc/os-release
