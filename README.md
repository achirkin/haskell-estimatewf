#### haskell-estimatewf

Estimate execution time of scientific workflows given their DAGs and provenance logs for their tasks.
For further information on methods, see [our paper in WORKS'14 proceedings](http://dl.acm.org/citation.cfm?id=2691177).
This is my early haskell work, so it is obviously dirty and not efficient.

The easiest way to compile program from the source code is to use Haskell
[`stack`](http://docs.haskellstack.org/en/stable/README.html).
In a command line within the project's folder, type following:
```
stack install
```
Example usage:
```
haskell-estimatewf 6 3.0 testwf.json cdfs.csv
```
This command reads workflow DAG from testwf.json
and estimates workflow CDF on a grid containing 2^6 points,
with boundaries roughly equal to 3 sigmas of total execution time deviation.
