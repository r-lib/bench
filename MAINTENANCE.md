## Current state

The CRAN version of bench is very stable. The dev version has the half finished continuous benchmarking code, which should either be extended or removed, see future directions for discussion.

## Known outstanding issues

I don't think there are any major known issues, possibly https://github.com/r-lib/bench/issues/112 if we are able to reproduce the problem, though I suspect it is due to either byte code compilation or CPU pipelining or related issues that we cannot fix.

## Future directions

Continuous benchmarking - feature development of this was largely derailed by COVID-19, I originally proposed it as a topic for a talk at UseR!2020, but when that conference went to virtual only it became less of a priority.
In addition the ursa labs conbench package has provided a good alternative to what is done in the bench package (https://ursalabs.org/blog/announcing-conbench/) and https://github.com/ursacomputing/arrowbench.

Possible future directions would be talking more with the arrow team on bench features which would be useful for their use cases and extending bench functionality with them.
