[@digital_henge](https://twitter.com/digital_henge)
===

tweets equinoxes, solstices, moon phases, zodiac signs, and (TODO) eclipses in minimal emoji representation. I'm rather fond of this one, in part because it was the first thing I'd made aside from dumb joke bots. a quiet, pretty thing

first version was in javascript, written three months after I'd started programming, with about the quality that would imply. moon phases were computed from a copy-pasted version of conway's "in your head" algorithm and often multiple days off. everything else was pulled from hand-compiled json arrays. initially I just had cron run it four times a day and what it checked/possibly tweeted was determined by a switchcase on whatever hour it happened to be

new version in haskell, literally because I wanted to keep the bot running but got sick of fighting with nodejs to build. most formulas taken from jean meeus's wonderful book "astronomical algorithms". delta t formulas taken from NASA. daily cron job schedules events for the next day using at, allowing them to be tweeted when they occur, rather than at a fixed time. moon phases are accurate to within a half hour or so (probably because I got lazy calculating obliquity and nutation). equinoxes and solstices are accurate to within a minute or two. UTC is used for both input and output (calculations themselves are generally done in terrestrial time) for sake of simplicity (since the server I run this on uses UTC... for sake of simplicity)
