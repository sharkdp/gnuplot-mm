gnuplot-mm
==========

This package provides GnuPlot bindings for Mathematica. It provides the functions `gnuPlot` and `gnuListPlot` which work (to some extend) analogous to Mathematicas own `Plot` and `ListPlot` functions.

Examples
--------
gnuplot-mm generates a gnuplot command file for you, runs it in the background and imports the output back into Mathematica.
Simply calling

```Mathematica
gnuPlot[Exp[-y^2] Cos[2y], {y, -3, 3}, PlotStyle -> "set grid y @DGRAY"]
```

produces the following output

![](https://rawgithub.com/sharkdp/gnuplot-mm/master/doc/HTMLFiles/gnuplot_4.gif "gnuplot-mm example 1")

You can also use gnuplots *cairolatex* terminal from within Mathematica to produce LaTeX-rendered output:

![](https://raw.github.com/sharkdp/gnuplot-mm/master/doc/HTMLFiles/gnuplot_10.gif "gnuplot-mm example 2")


See more features in action: https://rawgithub.com/sharkdp/gnuplot-mm/master/doc/gnuplot.htm

Installation
------------

Clone the repository into your `~/.Mathematica` folder and load `gnuplot-mm/gnuplot.m` via `Get[..]`.
