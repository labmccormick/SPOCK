## Test environments
* OS X , R 3.6.3
* ubuntu 20.04, R 3.6.3
* ubuntu 18.04, R 3.6.3
* local windows 10, R 3.6.3

## R CMD check results
There were no ERRORs or WARNINGs.

There is one NOTE:
> checking R code for possible problems ... NOTE
  create.plots: no visible binding for global variable 'x'
  create.plots: no visible binding for global variable 'y'
  create.plots: no visible binding for global variable 'ypre'
  create.plots: no visible binding for global variable 'yfiltered'
  create.plots: no visible binding for global variable 'ycorrected'
  Undefined global functions or variables:
    x y ycorrected yfiltered ypre

This note comes from a ggplot sequence of commands where the data field is
defined to a data.frame and then the aes use elements of that data.frame when
I tried to change it to data.frame$x (y,ypre,etc) then ggplot gave a collection
of warnings like this one:
Use of `toplot$yfiltered` is discouraged. Use `yfiltered` instead.


