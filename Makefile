app_name := haskell-cron-matcher
image := ${app_name}-stack

container_work_dir := /app
cache_dir := ${container_work_dir}/.stack-work

uid := $(shell id -u)
gid := $(shell id -g)

stack := docker run \
	--rm \
	--tty \
	--init \
	--user "${uid}:${gid}" \
	--volume "${PWD}:${container_work_dir}" \
	--env "STACK_ROOT=${cache_dir}" \
	--workdir "${container_work_dir}" \
	${image}

# Targets

.PHONY: build-docker
build-docker:
	docker build --tag=${image} .
	${stack} stack --system-ghc --local-bin-path=./.local install hindent hfmt

.PHONY: build
build:
	${stack} stack build

.PHONY: test
test:
	${stack} stack test

.PHONY: format
format:
	${stack} .local/hfmt -w src/
