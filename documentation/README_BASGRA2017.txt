FILES CHANGED IN SEP 2017 (compared to Dec 2016):

initialise_BASGRA_general.R
- In function plot_output, we ensure that 'g_range', i.e. the range for the variable, can handle missing values: 'na.rm=TRUE'

initialise_BASGRA_general.R & BASGRA.f90
- More output variables specified.

plants.f90
- In subroutine Growth, remobilisation is now set to zero on harvest days: NSHMOB *:= NOHARV
- KNMAX and KN no longer allowed to become infinite

