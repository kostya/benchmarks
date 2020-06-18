.PHONY: build
build:
	docker build docker/ -t benchmarks

docker_run = docker run -it --rm -v $(shell pwd):/src benchmarks

.PHONY: scaling_governor
performance_governor:
	sudo cpupower frequency-set -g performance

.PHONY: shell
shell: performance_governor
	$(docker_run) shell

.PHONY: versions
versions:
	$(docker_run) versions

.PHONY: toc
toc:
	git-md-toc -u -t
