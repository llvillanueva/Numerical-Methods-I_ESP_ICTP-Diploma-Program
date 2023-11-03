# plot.plt
set term png
set output "verlet.png"
set title "Qubit (Verlet Method)"
set xlabel "Time"
plot "verlet.txt" using 1:2 with lines title "<g|p(t)|g>", "verlet.txt" using 1:3 with lines title "<e|p(t)|e>"


set term png
set output "eulerA11.png"
set title "Qubit (Euler Method)"
set xlabel "Time"
plot "eulerA11.txt" using 1:2 with lines title "<g|p(t)|g>", "eulerA11.txt" using 1:3 with lines title "<e|p(t)|e>"
