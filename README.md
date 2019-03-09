# Haskell cron matcher

Made just for fun and test haskell.

## Usage

Match current time with given cron pattern.

	$ haskell-cron-matcher "10-20 * * * *"

See return code for result:

* 0 - matched
* 1 - dismatched
* 2 - pattern parse error

## Build

You can use docker for build Stack and application:

	$ make build-docker
	$ make build

## Tests

	$ make test
