set term png
set output "f3x2.png"
# Define the number of bins and the bin width
num_bins = 10000.0
bin_width = 1.0 / num_bins

# Define the range of the x-axis
xmin = 0
xmax = 1

# Create the histogram
bin(x) = floor((x - xmin) / bin_width)
plot "f3x2.txt" using (bin($1)*bin_width+xmin):(1.0) smooth frequency with boxes notitle


set term png
set output "z1.png"
set title 'Box-Muller Method (Z1)'
set xlabel 'Value'
set ylabel 'Frequency'

# Define the number of bins and the bin width
num_bins = 100
bin_width = 1.0 / num_bins

# Define the range of the x-axis
xmin = 0
xmax = 1

# Create the histogram
bin(x) = floor((x - xmin) / bin_width)
plot "box-muller.txt" using (bin($1)*bin_width+xmin):(1.0) smooth frequency with boxes notitle

set output "z2.png"
set title 'Box-Muller Method (Z2)'
set xlabel 'Value'
set ylabel 'Frequency'

# Define the number of bins and the bin width
num_bins = 100
bin_width = 1.0 / num_bins

# Define the range of the x-axis
xmin = 0
xmax = 1

# Create the histogram
bin(x) = floor((x - xmin) / bin_width)
plot "box-muller.txt" using (bin($2)*bin_width+xmin):(1.0) smooth frequency with boxes notitle



# plot.plt
set term png
set output "LCM2.png"
set title "1000 couples (Linear Congruential Method)"
set xlabel "x"
set ylabel "y"
plot 'LCMxy.txt' with points pt 7 notitle


# plot.plt
set term png
set output "rnxy.png"
set title "1000 couples (Fortran inbuilt Function)"
set xlabel "x"
set ylabel "y"
plot 'rnxy.txt' with points pt 7 notitle
