import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np
import sys
import os

# plt.rcParams['animation.ffmpeg_path'] = '/usr/bin/ffmpeg'
# plt.rcParams['savefig.bbox'] = 'tight'
task_title = sys.argv[1]
os.chdir( task_title )
specf = task_title + '_expop.dat'

dict1 = {}
# namedict = {}
lines = []
time = {}
# xy = []

infile = open(specf, 'r')
for line in infile:
	k = line.strip()
	if k:
		lines.append(k)
infile.close()

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
# print(time)
fig, ax = plt.subplots()
# plt.xlabel('x values')
# plt.ylabel('y values')
# plt.title('example')
# plt.ylim(0,1)
# plt.xticks(np.arange(1,num_nmax+1,1))
# x2 = np.arange(1,num_nmax+1)
# print(num_nmax)
x2 = np.arange(1,num_nmax+1)
# print(x2)
def animate(frame):
	# print(frame)
	ax.clear()
	plt.xlabel('Molecule Site')
	plt.ylabel('Population')
	plt.title('Time Evolution of Excitation')
	plt.xlim(0,num_nmax+1)
	plt.ylim(0,1.1)
	plt.xticks(np.arange(1,num_nmax+1,1))
	plt.yticks(np.arange(0.0,1.1,0.1))
	# plt.legend(["Time:ps"])
	# plt.legend('Time:ps')
	# plt.legend(['Time:'+ time[frame]*1.0E12 + 'ps'])
	
	# fig = plt.figure()
	# plt.xlabel(r'Energy (h$\omega$)')
	# plt.ylabel('Absorption (a.u.)')
	# plt.title('Absorption Spectrum')
	# plt.ylim(0,1)
	# ax1.hist(x1[:curr], normed=True, bins=range(1, 9, 1, alpha=0.5, align='left')
	# ax = fig.add_subplot(111, xlim=(1,self.num_nmax+1), ylim=(0,1))
	# ax = 
	x1 = dict1[frame]
	barWidth=0.8
	# print(x1)
	rects1 = ax.bar(x2, x1, width=barWidth, align='center')
	s = "Time: %8.0f ps" % (time[frame]*1.0E12)
	plt.legend([rects1], [s])

	# self.dots = self.ax.scatter([], [], self.radii)
	# ani = animation.FuncAnimation(fig, self.update, frames=364, interval=10, repeat=True)
	# ani.save('animation.mp4')

simulation = animation.FuncAnimation(fig,animate,300,interval=1, repeat=False)
FFwriter = animation.FFMpegWriter()
# simulation.save('animation.mp4', writer="ffmpeg")
simulation.save(task_title+'.mp4', writer=FFwriter)
# plt.tight_layout()
plt.show()

# def updateData(curr):
#	 # if curr <=2: return
#	 # for ax in (ax1, ax2, ax3, ax4):
#	 ax1.clear()
#	 plt.xlabel('x values')
#	 plt.ylabel('y values')
#	 plt.title('example')
#	 plt.ylim(0,1)
#	 # plt.xlim(0,9)
#	 ax1.hist(x1[:curr], normed=True, bins=range(1, 9, 1, alpha=0.5, align='left')
#	 # ax1.hist(x1[:curr], normed=True, bins=np.linspace(1,8, num=21), alpha=0.5)
#	 # print(x1[:curr])
#	 # ax2.hist(x2[:curr], normed=True, bins=np.linspace(0,15,num=21), alpha=0.5)
#	 # ax3.hist(x3[:curr], normed=True, bins=np.linspace(7,20,num=21), alpha=0.5)
#	 # ax4.hist(x4[:curr], normed=True, bins=np.linspace(14,20,num=21), alpha=0.5)

# simulation = animation.FuncAnimation(fig, updateData, interval=10, repeat=False)

# plt.show()