import sys, os

basename = sys.argv[1]

os.system('convert -delay 1.67 anim/' + basename + '_*.ppm ' + basename + '.gif')
os.system('rm anim/' + basename + '_*.ppm')