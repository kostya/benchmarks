docker := env docker
docker_build := $(docker) build docker/ -t benchmarks

.PHONY: build
build:
	$(docker_build)

.PHONY: rebuild
rebuild:
	$(docker_build) --no-cache

docker_run = $(docker) run -it --rm -v $(shell pwd):/src benchmarks

.PHONY: scaling_governor
performance_governor:
	#sudo cpupower frequency-set -g performance
	echo "TEST"

.PHONY: shell
shell: performance_governor
	$(docker_run) shell

.PHONY: versions
versions:
	$(docker_run) versions

.PHONY: toc
toc:
	git-md-toc -u README.md

.PHONY: update_apt
update_apt:
	./docker/update_apt.rb docker/apt.pkgs

.PHONY: lint
lint:
	docker run --rm -i hadolint/hadolint < docker/Dockerfile
