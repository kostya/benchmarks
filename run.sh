#!/bin/sh -xe

docker_build="docker build docker/ -t benchmarks"
docker_alpine_build="docker build docker/ -t benchmarks-alpine -f docker/Dockerfile.alpine"

docker_run="docker run --privileged -it --rm -v $PWD:/src benchmarks"
docker_alpine_run="docker run -it --rm -v $PWD:/src benchmarks-alpine"

build() {
    eval "$docker_build"
}

build_alpine() {
    eval "$docker_alpine_build"
}

rebuild() {
    eval "$docker_build --no-cache"
}

shell() {
    eval "$docker_run shell"
}

shell_alpine() {
    eval "$docker_alpine_run shell"
}

versions() {
    eval "$docker_run versions"
}

toc() {
    git-md-toc -u README.md
}

"$@"
