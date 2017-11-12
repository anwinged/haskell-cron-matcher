# haskell-cron-matcher

Match current time with given cron pattern.

	$ haskell-cron-matcher "10-20 * * * * *"

See return code for result:

* 0 - matched
* 1 - dismatched
* 2 - pattern parse error
