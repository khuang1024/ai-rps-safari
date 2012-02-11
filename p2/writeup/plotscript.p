set   autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels
set xtic auto                          # set xtics automatically
set ytic auto                          # set ytics automatically
set title "f(x) = 10 * arctan (.05 * (x - 280)) + 16"
set xlabel "Score"
set ylabel "Weight"
set xr [200:400]
set out 'a.eps'
set terminal postscript eps color lw 1 "Helvetica" 28
plot 10*atan(.05*(x-280))+16