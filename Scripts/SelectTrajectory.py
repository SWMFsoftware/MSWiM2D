import os
import numpy as np
import datetime as dt
import math
from dateutil import rrule
import spiceypy as spice

import matplotlib.pyplot as plt

# Load kernels from metakernel files.
mswim2d_dir = os.path.dirname(os.path.realpath(__file__)) + '/..'
kernel_dir = '{}/Scripts/SpiceKernels'.format(mswim2d_dir)
spice.furnsh('{}/planetsKern.txt'.format(kernel_dir))
spice.furnsh('{}/cassiniKern.txt'.format(kernel_dir))
spice.furnsh('{}/voyagerKern.txt'.format(kernel_dir))
spice.furnsh('{}/stereoulyssesKern.txt'.format(kernel_dir))
spice.furnsh('{}/newhorizonsKern.txt'.format(kernel_dir))
spice.furnsh('{}/junoKern.txt'.format(kernel_dir))

# Set defaults.
satellite = 'EARTH'
startdate_in  = '2000/01/01'
enddate_in  = '2000/02/01'
starttime_in = 1
endtime_in = 1
interval = 'day'

# Read input parameters from file.
infile = open('{}/TrajectoryParameters.txt'.format(mswim2d_dir),'r')
lines = infile.readlines()
for line in lines:
    if('Satellite Name:' in line): satellite = line.split(':')[-1].upper().strip()
    if('Start Date' in line): startdate_in = line.split(':')[-1].strip()
    if('End Date' in line):   enddate_in   = line.split(':')[-1].strip()
    if('Start Time' in line): starttime_in = line.split('):')[-1].strip()
    if('End Time' in line):   endtime_in   = line.split('):')[-1].strip()
    if('Resolution' in line): interval = line.split(':')[-1].strip()
infile.close()

# Construct start and end times.
start_time = dt.datetime(int(startdate_in[:4]), int(startdate_in[5:7]), int(startdate_in[-2:]),
                         int(starttime_in[:2]), int(starttime_in[3:5]), int(starttime_in[6:]))
end_time = dt.datetime(int(enddate_in[:4]), int(enddate_in[5:7]), int(enddate_in[-2:]),
                       int(endtime_in[:2]), int(endtime_in[3:5]), int(endtime_in[6:]))

# Create target name for SPICE.
planets_list=['EARTH','MARS','JUPITER','SATURN','URANUS','NEPTUNE','PLUTO']
satellites_dict={'CASSINI':[dt.datetime(1902,3,30),dt.datetime(2038,8,8)],
                 'VOYAGER 1':[dt.datetime(1977,9,6),dt.datetime(2030,12,31)],
                 'VOYAGER 2':[dt.datetime(1977,8,21),dt.datetime(2030,12,31)],
                 'JUNO':[dt.datetime(2011,8,6),dt.datetime(2020,8,22)],
                 'ULYSSES':[dt.datetime(1990,10,7),dt.datetime(2050,1,1)],
                 'STEREO':[dt.datetime(2006,10,26),dt.datetime(2050,1,1)],
                 'NEW HORIZONS': [dt.datetime(2015, 1, 1), dt.datetime(2015, 2, 1)],}
target_name = 'EARTH'
if(satellite in planets_list): target_name = satellite + ' BARYCENTER'
elif(satellite in satellites_dict.keys()): target_name = satellite
else: exit('Warning: invalid target name {}.'.format(satellite))

# Warn for targets leaving ecliptic plane.
if(satellite=='ULYSSES' and end_time > dt.datetime(1992,2,1)):
    print('Warning: Ulysses leaves +/- 6 lat HGI in 1992/02.')
if(satellite=='VOYAGER 1' and end_time > dt.datetime(1981,8,1)):
    print('Warning: Voyager I leaves +/- 6 lat HGI in 1981/08.')

# Check to see if times match available data for satellite.
if(satellite in satellites_dict.keys()):
    if(start_time < satellites_dict[satellite][0] or
            end_time > satellites_dict[satellite][1]):
        exit('Error: Data only available from {} to {}.'.format(satellites_dict[satellite][0].strftime('%Y/%m/%d'),
                                                                satellites_dict[satellite][1].strftime('%Y/%m/%d')))

# Construct array of timestamps based on provided endpoints and desired interval.
target_times = list()
if interval.upper()=='DAY': target_times = rrule.rrule(rrule.DAILY, dtstart=start_time, until=end_time)
elif interval.upper()=='HOUR': target_times = rrule.rrule(rrule.HOURLY, dtstart=start_time, until=end_time)
elif interval.upper()=='MINUTE': target_times = rrule.rrule(rrule.MINUTELY, dtstart=start_time, until=end_time)
else:
    exit('Error: Unsupported interval. Supported intervals are \'DAY\', \'HOUR\', and \'MINUTE\'.')
timestamps = list()
for stamp in target_times:
    timestamps.append(stamp)
    timestamps[-1] = spice.str2et(timestamps[-1].strftime('%m/%d/%Y %H:%M:%S'))

# Get target object positions.
positions, lightTimes = spice.spkpos(target_name,                # target body name
                                     timestamps,                 # user-selected output times
                                     'HCI',                      # output reference frame
                                     'NONE',                     # aberration correction flag
                                     'SOLAR SYSTEM BARYCENTER')  # observing body name/coordinate origin


# Convert units and coordinate system.
for i in np.arange(0, np.shape(positions)[0]): # for each timestamp
    for j in np.arange(0, np.shape(positions)[1]): # for each dimension
        positions[i][j] = spice.convrt(positions[i][j], 'KM', 'AU') # convert from km to AU
positions = positions.T # transpose for easier indexing

# Release kernels.
spice.kclear()

# Write trajectory in satellite file format.
sat_file = open('{}/{}.dat'.format(mswim2d_dir, satellite.upper()), 'w')
sat_file.write('#COORD\nHGI\n\n')
sat_file.write('year mo dy hr mn sc msc x y z\n#START\n')

for i,time in enumerate(target_times):
    sat_file.write(' {} {} {} {:2d} {:2d} 00 000 '.format(int(time.year), int(time.month), int(time.day),
                                                          int(time.hour), int(time.minute)))
    sat_file.write('{:.3f} {:.3f} 0.0\n'.format(positions[0][i],positions[1][i]))
sat_file.close()

