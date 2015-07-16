# InfoWiki #

MediaWiki XML dumps → info files converter.

Converts dumps directly into info, because `makeinfo` consumes too
much memory on big files.

I wanted a nice offline wiki viewer; info viewers are offline and
nice; this converter is a bridge.


## Screenshot ##

![Wikipedia in an info viewer and in a web browser](http://paste.uberspace.net/simplewiki.png)


## Performance ##

Currently it takes approximately 14 minutes to convert the "simple
English" wiki (over 100k articles), but there's a few things that can
be optimized. Memory consumption is very modest.


## To be done ##

Tables are not implemented yet, node name escaping should be refined,
and there is a lot of little things here and there.
