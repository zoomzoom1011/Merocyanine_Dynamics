import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np
import sys
import os

#read task title and go to the directory
task_title = sys.argv[1]
os.chdir( task_title )
specf = task_title + '_expop.dat'

dict1 = {}
lines = []
time = {}

#read input file
infile = open(specf, 'r')
for line in infile:
	k = line.strip()
	if k:
		lines.append(k)
infile.close()

#get all parameters
for i in range(0,len(lines)-1):
	if(lines[i] =='num_nmax'):
		j = i + 1
		num_nmax = int(lines[j])
	if(lines[i]=='num_steps'):
		j = i + 1
		num_steps = int(lines[j])
	if(lines[i]=='trajectories'):
		for j in range(2,num_steps+2):
			element = lines[i+j]
			temp = element.split(' ')
			temp1 = [x for x in temp if x]
			temp1 = np.asarray(temp1,float)
			dict1[j-2]= temp1[1:]
			time[j-2] = temp1[0]

fig, ax = plt.subplots()
x2 = np.arange(1,num_nmax+1)

# animation change with frame
def animate(frame):
	ax.clear()
	plt.xlabel('Molecule Site')
	plt.ylabel('Population')
	plt.title('Time Evolution of Excitation')
	plt.xlim(0,num_nmax+1)
	plt.ylim(0,1.1)
	plt.xticks(np.arange(1,num_nmax+1,1))
	plt.yticks(np.arange(0.0,1.1,0.1))

	x1 = dict1[frame]
	barWidth=0.8
	rects1 = ax.bar(x2, x1, width=barWidth, align='center')
	s = "Time: %8.0f ps" % (time[frame]*1.0E12)
	plt.legend([rects1], [s])

simulation = animation.FuncAnimation(fig,animate,300,interval=1, repeat=False)
plt.show()

# save as mp4 for display
FFwriter = animation.FFMpegWriter()
simulation.save(task_title+'.mp4', writer=FFwriter)


