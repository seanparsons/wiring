Gluing together your ReaderT/WriterT/StateT/RWSTs.
==================================================

[![Build Status](https://secure.travis-ci.org/seanparsons/wiring.svg)](http://travis-ci.org/seanparsons/wiring)

Using the above monad transformers can be difficult when the need to compose them arises, as a ReaderT with an environment of Network.HTTP.Client.Manager doesn't line up correctly with a ReaderT with an environment of Database.PostgreSQL.Simple.Connection.

The solution is to transform those instances of ReaderT where the environment is a superset of both of those environments and this package provides methods to do exactly that.