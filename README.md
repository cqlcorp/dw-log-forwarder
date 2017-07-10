dw-log-forwarder
==============

This application pulls down Demandware logs over WebDAV, parses them into a friendly JSON structure, and forwards them to LogStash. WebDAV requests are lightweight and only pull down new logs since the last pull. You can also specify that the app should pull down and process previous days of archived logs.

It is intended to be run on a schedule of some small interval continuously, say, every five minutes.

Building
-----------

This application is written in Haskell using [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). If this is your first time on a Haskell project, welcome! You're going to love it! Here's what you need to get started:

1. Clone the repository. `cd` into the repository root
2. Install stack: https://docs.haskellstack.org/en/stable/README/
    1. Stack is the de facto build, test, and package management solution for Haskell projects
3. You'll then need to do a global Stack setup by running `stack setup` from the command line.
    1. This will pull down the latest stable packages and GHC (Glasgow Haskell Compiler) from [stackage](https://www.stackage.org/).
4.  Now you should be able to build the project by running `stack build`. If this is your first time, it may take a while, but don't worry! It's just pulling down and compiling lots of dependencies. Go play a game of ping pong. Subsequent builds will be fast.

Testing
----------

Tests can be run using `stack test`. All tests exist under the `test` directory, and all the important log parsing tests happen in `test/DwLog/LogParsingSpec.hs`.

All changes to log parsers should be accompanied by new tests to ensure backwards and forwards compatibility.

Local Testing without Logstash
--------------------------------------------

Logstash is not necessary for local debugging. You can just enter a folder name in `app.config` at `output.outputFolder` and all JSON logs will get dumped to files corresponding to the name of the log file.

Database
-------------

A SQL Server database is required for this application to run. The migrations for this are housed in the [migrations/mssql](migrations/mssql) folder. (Future versions of this application will remove the SQL Server requirement)

Processing Archived Log Files
------------------------------------------

In addition to tailing log files over WebDav, this application can be requested to pull down archives for certain days (a day is UTC midnight to midnight). You just need to specify the day for the instance in the `dw.DayFetchQueue` table in the database. On the next run, the app will pull down those days and dump them in the `queue` folder.

The application will then look in the `queue` folder for any existing `.log` or `.log.gz` files and parse them before trying to tail current WebDAV logs. There is a timeout value in `app.config` at `maxDayQueueProcessSeconds` which cuts off archive processing after so many seconds, allowing us to keep tailing current log files in a timely fashion.

#### Local Testing without Querying DW via WebDAV

If you want to do some local debugging but don't want to poll WebDAV, you can just dump an appropriate `.log` or `.log.gz` file in the `queue` directory. On each run, it will move the file to the `queue/processed` folder.

Omitting Logs via Regular Expression
-----------------------------------------------------

You can also specify blanket regular expressions in the database table `dw.LogOmissionRegex` to omit certain logs in the case of an emergency, but be careful here: This is really meant as a critical band-aid to avoid flooding Logstash with a tsunami of unwanted logs. For instance: say there's a crawler that generates millions of error logs. You can avoid Elasticsearch bloat by entering a regular expression that omits anything with `\bstupid-bot\b` in the log.

If there are certain logs you wish to permanently omit, it may be worth specifying those in code. All this is just to avoid too much processing CPU since Demandware generates lots and lots of logs.

Authors
-------

* [Chad Gilbert](https://github.com/freakingawesome) - [CQL Inc.](https://github.com/cqlcorp)

Roadmap
=======

- [ ] Add support for a SQLite backing database
- [ ] Add parsing for more log types as they become available
- [ ] Create a Docker image to ease installation (which could include Elasticsearch and Logstash)
