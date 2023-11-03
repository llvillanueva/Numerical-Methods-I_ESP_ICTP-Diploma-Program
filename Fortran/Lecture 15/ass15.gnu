set term png
set output "ass15hist.png"
set title 'Histogram representation of f(x) using the Freedman-Diaconis rule'
set xlabel 'x'
set ylabel 'Frequency'

# Define the number of bins and the bin width
num_bins = 67
bin_width = 1.0 / num_bins

# Define the range of the x-axis
xmin = -2.93509007
xmax = 6.63116455

# Create the histogram
bin(x) = floor((x - xmin) / bin_width) 
plot "ass15.txt" using (bin($1)*bin_width+xmin):(1.0) smooth frequency with boxes notitle

set output "rejection.png"
set title 'Histogram representation of f(x) from Rejection Method'
set xlabel 'x'
set ylabel 'Frequency'

# Define the number of bins and the bin width
num_bins = 10000
bin_width = 1.0 / num_bins

# Define the range of the x-axis
xmin = -10
xmax = 10

# Create the histogram
bin(x) = floor((x - xmin) / bin_width) 
plot "ass15.txt" using (bin($1)*bin_width+xmin):(1.0) smooth frequency with boxes notitle


# plot.plt
set term png
set output "ECDF.png"
set title "Emperical Cumulative Distribution Function"
set xlabel 'x'
set ylabel 'r/M'
plot "C(x).txt" using 1:2 with points notitle

# plot.plt
set term png
set output "fx.png"
set title "100000 points generated from f(x)"
set xlabel "Trial number"
set ylabel "x"
plot "fx.txt" using 1:2 with points notitle

# plot.plt
set term png
set output "fdr.png"
set title "Plot of f(x) from Freedman-Diaconis Rule"
set xlabel "x"
#set ylabel "x"
plot "A15hist.txt" using 1:2 with line notitle

# plot.plt
set term png
set output "GKDE.png"
set title "Gaussian Kernel Density Estimation"
set xlabel "x"
plot "GKDE.txt" using 1:2 with lines notitle

