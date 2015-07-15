#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
import time

plt.ion()

with open("data.txt") as f:
	data = f.read()

data = data.split('\n')
data.pop()


#fig = plt.figure(figsize=(8,4))
wm = plt.get_current_fig_manager()
wm.window.wm_geometry("800x800+100+0")
plt.title("bla")
plt.ylabel('Scores')
plt.xlabel('obj')
while(True):
	try:
		with open("data.txt") as f:
			data = f.read()

		if len(data)>1:
			data = data.split('\n')
			plt.clf()
			plt.axis([-0.5,1.0,-0.1,1.5])
			plt.title("positions")
			plt.ylabel('Y')
			plt.xlabel('X')

			data.pop()
			data.pop()
			idobj = [float(row.split(' ')[0]) for row in data]
			#print idobj
			x = [float(row.split(' ')[1]) for row in data]
			#print x
			y = [float(row.split(' ')[2]) for row in data]
			z = [float(row.split(' ')[3]) for row in data]
			#fx = [float(row.split(' ')[3]) for row in data]
			#fy = [float(row.split(' ')[4]) for row in data]
		
			color = [(float(row.split(' ')[4]),float(row.split(' ')[5]),float(row.split(' ')[6])) for row in data]
		
			plt.scatter(x,y,c=color,marker='x')
			#plt.plot( [0.2,0.2],[0.07,0.13],color='black', linewidth=3,alpha=0.6)
			#plt.plot( [0.15,0.15],[-0.03,0.03],color='black', linewidth=3,alpha=0.6)
			#plt.plot( [0.2,0.2],[0.0,0.2],color='black', linewidth=2,alpha=0.6)
			plt.draw()
	except IOError:
		time.sleep(0.1)
		print 'bad'
	#time.sleep(0.0)
	#print 'ok'
