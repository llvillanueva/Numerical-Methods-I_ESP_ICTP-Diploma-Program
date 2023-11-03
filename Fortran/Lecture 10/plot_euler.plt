# plot.plt
set term png
set output "midpointfux.png"
set title "Simple Pendulum (large initial angle)"
set xlabel "Time"
plot "midpoint.txt" using 1:2 with lines title "x(t)", "midpoint.txt" using 1:3 with lines title "v(t)"

# plot.plt
set output "midpointgfux.png"
set title "Simple Pendulum (large initial angle)"
set xlabel "Time"
plot "midpointgfux.txt" using 1:2 with lines title "x(t)", "midpointgfux.txt" using 1:3 with lines title "v(t)"

set term png
set output "eulergfux.png"
set title "Simple Pendulum (large initial angle)"
set xlabel "Time"
plot "eulergfux.txt" using 1:2 with lines title "x(t)", "eulergfux.txt" using 1:3 with lines title "v(t)"

# plot.plt
set output "euler.png"
set title "Simple Pendulum (large initial angle)"
set xlabel "Time"
plot "euler.txt" using 1:2 with lines title "x(t)", "euler.txt" using 1:3 with lines title "v(t)"
