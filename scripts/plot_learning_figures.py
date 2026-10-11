"""Scientific learning figures. Run from the SurvCast source root after learning_examples.R."""
from pathlib import Path
import csv
import os
from math import comb
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ASSETS = Path(os.environ.get('LEARNING_ASSETS', 'docs/learning-assets'))
COLORS = ['#126b72', '#ba7141', '#466d95']
plt.rcParams.update({'font.family': 'DejaVu Sans', 'font.size': 11, 'axes.spines.top': False,
                     'axes.spines.right': False, 'axes.labelcolor': '#173d50',
                     'xtick.color': '#173d50', 'ytick.color': '#173d50', 'svg.hashsalt': 'survcast-learning-v1'})
def finish(fig, name):
    fig.tight_layout()
    fig.savefig(ASSETS / (name + '.svg'), metadata={'Date': None}, bbox_inches='tight')
    plt.close(fig)
def rows(name):
    with (ASSETS/name).open(newline='') as f: return list(csv.DictReader(f))

t = np.linspace(.001, 36, 500)
fig, axs = plt.subplots(1,2,figsize=(11,4.2))
for k, color in zip([.7,1,2], COLORS):
    axs[0].plot(t,np.exp(-np.log(2)*(t/12)**k),color=color,label=f'k = {k}')
    axs[1].plot(t,np.log(2)*k/12*(t/12)**(k-1),color=color,label=f'k = {k}')
axs[0].axvline(12,color='#a9bdc1',ls=':');axs[0].axhline(.5,color='#a9bdc1',ls=':')
axs[0].set(ylabel='S(t)',ylim=(0,1));axs[1].set(ylabel='h(t) per month',ylim=(0,.4))
for ax in axs: ax.set_xlabel('Follow-up age (months)');ax.legend(frameon=False)
finish(fig,'survival-hazard')

u=np.linspace(0,24,300)
fig,ax=plt.subplots(figsize=(9,4))
ax.plot(u,np.exp(-np.log(2)*(u/12)**2),color=COLORS[0],label='New patient: a = 0')
ax.plot(u,np.exp(-np.log(2)*(((8+u)/12)**2-(8/12)**2)),color=COLORS[1],label='Event-free at a = 8')
ax.set(xlabel='Remaining time u (months)',ylabel='Conditional event-free probability',ylim=(0,1));ax.legend(frameon=False)
finish(fig,'conditional-survival')

fig,ax=plt.subplots(figsize=(9,3.7))
ax.step([0,3,6,12,24],[.03,.06,.09,.06,.06],where='post',lw=2,color=COLORS[0])
for b in [3,6,12]:ax.axvline(b,color='#a9bdc1',ls=':')
ax.set(xlabel='Follow-up age (months)',ylabel='Event hazard per month',ylim=(0,.11))
finish(fig,'pwe-hazard')

r=rows('prediction-example-curves.csv')
x=np.array([float(v['day_months'])-12 for v in r])
fig,ax=plt.subplots(figsize=(9,4.4))
ax.fill_between(x,[float(v['lower']) for v in r],[float(v['upper']) for v in r],color='#dcebed',label='Pointwise 95% prediction interval')
ax.plot(x,[float(v['median']) for v in r],color=COLORS[0],label='Median')
ax.plot(x,[float(v['mean']) for v in r],color=COLORS[1],ls='--',label='Mean')
ax.axhline(40,color='#a9bdc1',ls=':');ax.set(xlabel='Time after current DCO (months)',ylabel='Cumulative event count');ax.legend(frameon=False)
finish(fig,'count-forecast')

r=rows('prediction-example-mass.csv');fig,ax=plt.subplots(figsize=(9,4))
ax.bar([float(v['count']) for v in r],[float(v['probability']) for v in r],width=.7,color=COLORS[0],label='1000 simulated trials')
z=np.arange(101);ax.plot(z+40,[comb(100,int(j))*.5**100 for j in z],color=COLORS[1],ls='--',label='40 + Binomial(100, 0.5)')
ax.set(xlim=(68,112),xlabel='Cumulative count at horizon',ylabel='Probability');ax.legend(frameon=False)
finish(fig,'count-mass')

r=rows('simulation-example-km.csv');fig,ax=plt.subplots(figsize=(9,4.3))
for g,col in zip(['A','B'],COLORS):
    d=[v for v in r if v['group']==g];times=np.array([float(v['time_day'])/30.4375 for v in d]);surv=np.array([float(v['survival']) for v in d])
    ax.step(times,surv,where='post',color=col,lw=2,label=g)
    cens=np.array([float(v['n_censor'])>0 for v in d]);ax.scatter(times[cens],surv[cens],marker='+',color=col,s=40)
ax.set(xlabel='Individual follow-up age (months)',ylabel='Observed KM survival',ylim=(0,1.02));ax.legend(frameon=False,title='Group')
finish(fig,'km-observed')

fig,ax=plt.subplots(figsize=(10,3.3))
ax.set(xlim=(0,24),ylim=(0,3),yticks=[],xlabel='Study time (months)',xticks=[0,5,13,18,24])
ax.spines['left'].set_visible(False)
ax.hlines(2.2,0,24,color='#b7c7cb',lw=2);ax.hlines(1.1,5,18,color=COLORS[0],lw=3)
ax.axvline(13,color=COLORS[1],ls='--');ax.scatter([5,13,18],[1.1]*3,c=COLORS,s=65,zorder=3)
for xx,txt in [(5,'Entry E=5'),(13,'DCO C=13'),(18,'Event day=18')]:ax.text(xx,.65,txt,ha='center',fontsize=10)
ax.text(9,1.5,'Observed age a=8',ha='center');ax.text(16,1.5,'Remaining R=5',ha='center');ax.text(13,2.55,'Forecast starts at DCO',ha='center',color=COLORS[1])
finish(fig,'patient-clocks')
print('Created seven standalone SVG learning figures.')
