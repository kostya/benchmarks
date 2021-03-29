FROM debian:testing

ARG APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
RUN apt-get update \
        && apt-get install -y --no-install-recommends apt-transport-https dirmngr gnupg ca-certificates \
        && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
        && echo "deb https://download.mono-project.com/repo/debian stable-buster main" > /etc/apt/sources.list.d/mono-official-stable.list \
        && apt-get update \
	&& apt-get install -y wget xz-utils build-essential libtool-bin cmake git curl vim powercap-utils \
	libgmp-dev libssl-dev libssl1.1 libxml2 lbzip2 libtinfo5 libtool zlib1g \
	libpcre3-dev libevent-dev libjson-c-dev libblas-dev liblapack-dev libopenblas-dev \
	libjq-dev libonig-dev libjson-glib-dev \
	python3-numpy python3 python3-ujson python3-pip \
	mono-devel nuget \
	chezscheme gcc-10 gccgo-10 gdc-10 valac cpanminus \
	luajit lua5.4 liblua5.4-dev libluajit-5.1-dev \
        tcl tcllib php elixir clang-11 opam

RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-10 10 \
	&& update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-10 10 \
	&& update-alternatives --install /usr/bin/gdc gdc /usr/bin/gdc-10 10 \
	&& update-alternatives --install /usr/bin/gccgo gccgo /usr/bin/gccgo-10 10 \
        && update-alternatives --install /usr/bin/clang clang /usr/bin/clang-11 10 \
        && rm -rf /var/lib/apt/lists/* /tmp/*

# Requires for https://crates.io/crates/jq-sys in Debian
ENV JQ_LIB_DIR=/usr/lib/x86_64-linux-gnu/

WORKDIR /opt

RUN curl -Lo /usr/local/bin/coursier https://git.io/coursier-cli \
	&& chmod +x /usr/local/bin/coursier

# https://github.com/MLton/mlton/releases
ARG MLTON=20210117
RUN wget --progress=dot:giga -O - \
	https://github.com/MLton/mlton/releases/download/on-$MLTON-release/mlton-$MLTON-1.amd64-linux-glibc2.31.tgz \
	| tar -xz
ENV PATH="/opt/mlton-$MLTON-1.amd64-linux-glibc2.31/bin/:${PATH}"

# https://www.ruby-lang.org/en/downloads/
RUN export VERSION=ruby-3.0.0 \
	&& wget --progress=dot:giga -O - \
	https://cache.ruby-lang.org/pub/ruby/3.0/$VERSION.tar.gz \
	| tar -xz \
	&& cd $VERSION \
	&& ./configure --prefix=/opt/ruby && make -j && make install \
	&& cd .. && rm -rf $VERSION
ENV PATH="/opt/ruby/bin:${PATH}"

# https://pypy.org/download.html
ARG PYPY=pypy3.7-v7.3.3-linux64
RUN wget --progress=dot:giga -O - \
        https://downloads.python.org/pypy/$PYPY.tar.bz2 \
	| tar -xj
ENV PATH="/opt/$PYPY/bin:${PATH}"

# https://www.scala-lang.org/download/
ARG SCALA=2.13.5
RUN wget --progress=dot:giga -O - \
	https://downloads.lightbend.com/scala/$SCALA/scala-$SCALA.tgz \
	| tar -xz
ENV PATH="/opt/scala-$SCALA/bin/:${PATH}"

# https://jdk.java.net/
ARG JDK=16
RUN wget --progress=dot:giga -O - \
    https://download.java.net/java/GA/jdk16/7863447f0ab643c585b9bdebf67c69db/36/GPL/openjdk-${JDK}_linux-x64_bin.tar.gz \
    | tar -xz
ENV PATH="/opt/jdk-$JDK/bin:${PATH}"

# https://github.com/crystal-lang/crystal/releases
ARG CRYSTAL=crystal-1.0.0-1
RUN wget --progress=dot:giga -O - \
	https://github.com/crystal-lang/crystal/releases/download/1.0.0/$CRYSTAL-linux-x86_64.tar.gz \
	| tar -xz
ENV PATH="/opt/$CRYSTAL/bin:${PATH}"

# https://github.com/ldc-developers/ldc/releases
ARG LDC=ldc2-1.25.1-linux-x86_64
RUN wget --progress=dot:giga -O - \
	https://github.com/ldc-developers/ldc/releases/download/v1.25.1/$LDC.tar.xz \
	| tar -xJ
ENV PATH="/opt/$LDC/bin/:${PATH}"

# https://nim-lang.org/install_unix.html
ARG NIM=nim-1.4.4
RUN wget --progress=dot:giga -O - \
	https://nim-lang.org/download/$NIM-linux_x64.tar.xz \
	| tar -xJ
ENV PATH="/opt/$NIM/bin/:${PATH}"

# https://dlang.org/download.html
RUN wget --progress=dot:giga -O - \
    https://s3.us-west-2.amazonaws.com/downloads.dlang.org/releases/2021/dmd.2.096.0.linux.tar.xz \
    | tar -xJ
ENV PATH="/opt/dmd2/linux/bin64/:${PATH}"

# https://www.jruby.org/download
ARG JRUBY=9.2.16.0
RUN wget --progress=dot:giga -O - \
	https://repo1.maven.org/maven2/org/jruby/jruby-dist/$JRUBY/jruby-dist-$JRUBY-bin.tar.gz \
	| tar -xz \
	&& ln -s /opt/jruby-$JRUBY/bin/jruby /usr/bin/jruby

# https://julialang.org/downloads/
ARG JULIA=julia-1.6.0
RUN wget --progress=dot:giga -O - \
	https://julialang-s3.julialang.org/bin/linux/x64/1.6/$JULIA-linux-x86_64.tar.gz \
	| tar -xz
ENV PATH="/opt/$JULIA/bin/:${PATH}"

# https://swift.org/download/
ARG SWIFT=swift-5.3.3-RELEASE-ubuntu20.04
RUN wget --progress=dot:giga -O - \
	https://swift.org/builds/swift-5.3.3-release/ubuntu2004/swift-5.3.3-RELEASE/$SWIFT.tar.gz \
	| tar -xz \
	&& ln -s /opt/$SWIFT/usr/bin/swift /usr/bin/swift

# https://clojure.org/community/downloads
ARG CLOJURE=1.10.3.814
RUN wget --progress=dot:giga -O - \
	https://download.clojure.org/install/linux-install-$CLOJURE.sh \
	| bash -s

ENV GHCUP_INSTALL_BASE_PREFIX="/opt"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
	| BOOTSTRAP_HASKELL_NONINTERACTIVE="y" sh
ENV PATH="/opt/.ghcup/bin/:${PATH}"

# https://www.haskell.org/ghc/
ARG GHC_VER=8.10.4
RUN ghcup install ghc $GHC_VER && ghcup set ghc $GHC_VER

# Shared packages for all Haskell code
RUN cabal update \
        && cabal install network raw-strings-qq --lib \
        && cabal install hlint
ENV GHC_PACKAGE_PATH="~/.cabal/store/ghc-${GHC_VER}/package.db:"

# https://github.com/graalvm/graalvm-ce-builds/releases
ARG GRAALVM=21.0.0.2
RUN wget --progress=dot:giga -O - \
	https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-$GRAALVM/graalvm-ce-java11-linux-amd64-$GRAALVM.tar.gz \
	| tar -xz \
	&& ln -s /opt/graalvm-ce-java11-$GRAALVM/bin/gu /usr/bin/gu
RUN gu install ruby \
	&& /opt/graalvm-ce-java11-$GRAALVM/languages/ruby/lib/truffle/post_install_hook.sh \
	&& ln -s /opt/graalvm-ce-java11-$GRAALVM/bin/truffleruby /usr/bin/truffleruby

# https://github.com/luarocks/luarocks/wiki/Download
ARG LUAROCKS=3.5.0
RUN wget --progress=dot:giga -O - \
        http://luarocks.org/releases/luarocks-$LUAROCKS.tar.gz \
        | tar -xz \
        && cd luarocks-$LUAROCKS \
        && ./configure --prefix=/opt/luarocks/lua \
        && make -j && make install \
        && ./configure --with-lua-interpreter=luajit --prefix=/opt/luarocks/luajit \
        && make -j && make install \
	&& cd .. && rm -rf luarocks-$LUAROCKS

# https://github.com/dotnet/core/tree/master/release-notes
RUN mkdir dotnet && wget --progress=dot:giga -O - \
    https://download.visualstudio.microsoft.com/download/pr/e0707d53-0310-4767-9db1-67ddc41f6fd8/38761ffd39860954aa7c8a54cbb025ca/dotnet-sdk-5.0.104-linux-x64.tar.gz \
    | tar -xz -C dotnet
ENV PATH="/opt/dotnet:${PATH}"
ENV DOTNET_ROOT="/opt/dotnet"
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1

# https://nodejs.org/en/download/current/
ARG NODE=v15.12.0
RUN wget --progress=dot:giga -O - \
	https://nodejs.org/dist/$NODE/node-$NODE-linux-x64.tar.xz \
	| tar -xJ
ENV PATH="/opt/node-$NODE-linux-x64/bin/:${PATH}"

# https://golang.org/dl/
RUN wget --progress=dot:giga -O - \
    https://golang.org/dl/go1.16.2.linux-amd64.tar.gz \
    | tar -xz
ENV PATH="/opt/go/bin/:${PATH}"

# https://www.rust-lang.org/tools/install
ENV CARGO_HOME="/opt/.cargo" PATH="/opt/.cargo/bin:${PATH}"
RUN wget -O - https://sh.rustup.rs | sh -s -- -y

# https://ocaml.org/releases/
ARG OCAML=4.12.0
RUN opam init --disable-sandboxing -n --root=/opt/opam --compiler=ocaml-base-compiler.$OCAML
ENV PATH="/opt/opam/$OCAML/bin/:${PATH}"

# https://kotlinlang.org/docs/tutorials/command-line.html
ARG KOTLIN=1.4.31
RUN wget --progress=dot:giga \
	https://github.com/JetBrains/kotlin/releases/download/v$KOTLIN/kotlin-compiler-$KOTLIN.zip \
	&& unzip kotlin-compiler-$KOTLIN.zip \
	&& rm kotlin-compiler-$KOTLIN.zip
ENV PATH="/opt/kotlinc/bin/:${PATH}"

# https://download.racket-lang.org/
ARG RACKET=8.0
RUN wget --progress=dot:giga \
	https://mirror.racket-lang.org/installers/$RACKET/racket-$RACKET-x86_64-linux-cs.sh \
	&& sh racket-$RACKET-x86_64-linux-cs.sh --unix-style --dest /usr/ \
	&& rm racket-$RACKET-x86_64-linux-cs.sh

# https://github.com/vlang/v/releases
ARG VLANG=weekly.2021.12.2
RUN wget --progress=dot:giga \
        https://github.com/vlang/v/releases/download/$VLANG/v_linux.zip \
        && unzip v_linux.zip \
        && rm v_linux.zip
ENV PATH="/opt/v/:${PATH}"

ARG SCRIPTS_VER=unknown
COPY *.rb ./

RUN ./versions.rb

ENTRYPOINT ["./run.rb"]

RUN cat /etc/os-release
