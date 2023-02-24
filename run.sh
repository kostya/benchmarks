#!/bin/sh -xe

docker_build="docker build docker/ -t benchmarks"
docker_alpine_build="docker build docker/ -t benchmarks-alpine -f docker/Dockerfile.alpine"

docker_run="docker run -it --rm -v $PWD:/src benchmarks"
docker_alpine_run="docker run -it --rm -v $PWD:/src benchmarks-alpine"

exit_trap_command=""

tear_down() {
    eval "$exit_trap_command"
}
trap tear_down EXIT

add_exit_trap() {
    to_add=$1
    if [ "$exit_trap_command" ]; then
        exit_trap_command="$exit_trap_command; $to_add"
    else
        exit_trap_command="$to_add"
    fi
}

build() {
    eval "$docker_build"
}

build_alpine() {
    eval "$docker_alpine_build"
}

rebuild() {
    eval "$docker_build --no-cache"
}

setup_shell() {
    # TODO: apply the proper solution (probably by using the privileged monitor)
    # References (CVE-2020-8694): https://lists.debian.org/debian-lts-announce/2020/12/msg00015.html
    old_mode=$(stat -c "%a" /sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj)
    energy_uj=/sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj
    chmod 0444 "$energy_uj"
    add_exit_trap "chmod $old_mode $energy_uj"
}

shell() {
    setup_shell
    eval "$docker_run shell"
}

shell_alpine() {
    setup_shell
    eval "$docker_alpine_run shell"
}

versions() {
    eval "$docker_run versions"
}

toc() {
    git-md-toc -u README.md
}

update_apt() {
    ./docker/update_apt.rb docker/apt.pkgs
}

lint() {
    docker run --rm -i hadolint/hadolint < docker/Dockerfile
    docker run --rm -i hadolint/hadolint < docker/Dockerfile.alpine
}

"$@"
