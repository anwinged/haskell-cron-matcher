app_name := haskell-cron-matcher

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
	fpco/stack-build:lts-15.16

hfmt := docker run \
	--rm \
	--tty \
	--init \
	--user "${uid}:${gid}" \
	--volume "${PWD}:${container_work_dir}" \
	--workdir "${container_work_dir}" \
	anwinged/hfmt:master

# Targets

.PHONY: build
build:
	mkdir -p .stack-work
	${stack} stack build

.PHONY: test
test:
	mkdir -p .stack-work
	${stack} stack test

.PHONY: format
format:
	${hfmt} -w app/ src/ test/
