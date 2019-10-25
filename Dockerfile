FROM archlinux/base

RUN pacman -Syu --noconfirm base-devel tar lzop \
    gcc-go nim crystal go dmd gdc ldc rust nodejs ocaml racket \
    pypy ruby python2 python3 julia clang mono ghc tcl jq php clojure rlwrap \
    jdk-openjdk jruby scala kotlin elixir lua luajit mlton dotnet-sdk

RUN useradd builder -m && \
    echo "builder ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/builder && \
    chmod 0440 /etc/sudoers.d/builder

RUN echo $'#!/bin/sh \n\
    cd /tmp \n\
    curl -L https://aur.archlinux.org/cgit/aur.git/snapshot/$1.tar.gz | sudo -u builder tar -xz \n\
    cd $1 \n\
    PKGEXT=".pkg.tar.lzo" sudo -u builder makepkg --noconfirm -sic' > install && \
    chmod +x install

# AUR packages are exprimental, so we want the image layer per package

RUN ./install vlang

RUN ./install graal-bin
RUN ./install truffleruby-bin

RUN ./install rock
RUN ./install chez-scheme

RUN ./install icu63
RUN ./install swift-bin

RUN ln -s /opt/dotnet/sdk/2.2.108/FSharp/RunFsc.sh /usr/bin/fsharp

RUN gcc --version && echo "---" \
    && nim --version && echo "---" \
    && crystal --version && echo "---" \
    && go version && echo "---" \
    && gccgo --version && echo "---" \
    && dmd --version && echo "---" \
    && gdc --version && echo "---" \
    && ldc2 --version && echo "---" \
    && rustc --version && echo "---" \
    && scala --version && echo "---" \
    && node -e "console.log('Nodejs ' + process.version)" && echo "---" \
    && pypy --version && echo "---" \
    && ruby --version && echo "---" \
    && python2 --version && echo "---" \
    && python3 --version && echo "---" \
    && jruby --version && echo "---" \
    && java --version && echo "---" \
    && julia --version && echo "---" \
    && clang --version && echo "---" \
    && mcs --version && echo "---" \
    && rock --version && echo "---" \
    && perl --version && echo "---" \
    && ghc --version && echo "---" \
    && echo 'puts "Tcl $tcl_version"' | tclsh && echo "---" \
    && jq --version && echo "---" \
    && swift --version && echo "---" \
    && kotlin -version && echo "---" \
    && php --version && echo "---" \
    && dotnet --info && echo "---" \
    && elixir --version && echo "---" \
    && lua -v && echo "---" \
    && luajit -v && echo "---" \
    && truffleruby --version && echo "---" \
    && mlton && echo "---" \
    && fsharp --help | sed -n 1p && echo "---" \
    && ocaml --version && echo "---" \
    && racket --version && echo "---" \
    && echo '(scheme-version)' | scheme -q && echo "---" \
    && v --version && echo "---" \
    && clj -e '(str "Clojure " (clojure-version))' && echo "---" \
    echo " END"
