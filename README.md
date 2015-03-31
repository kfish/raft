[![Build Status](https://secure.travis-ci.org/kfish/raft.png)](http://travis-ci.org/kfish/raft)

raft
====

A Haskell implementation of the [Raft consensus protocol](http://raftconsensus.github.io/),
deconstructed as a menagerie of cooperating abstractions for communication, storage
and control. A pure protocol description allows local testing and validation of
exactly the code that is run on a production cluster.
