from os import listdir
import pandas as pd
import matplotlib.pyplot as plt

results = {}
for t in ['ii', 'iiLDB', 'pt', 'ptB']:
    results[t] = {'setup': [], 'query': []}

for t in ['ii', 'iiLDB', 'pt']:
    results[t]['queryTimings'] = {}
    for i in range(51, 90 + 1):
        results[t]['queryTimings'][i] = []

for f in listdir('.'):
    type = f.split('_')[0]
    if type in ['ii', 'iiLDB', 'pt', 'ptB']:
        file = open(f, 'r')
        lines = file.readlines()
        for i, line in enumerate(lines):
            tokens = line.split('\t')
            if i <= 2 and tokens[0] in ["setup", "query"]:
                results[type][tokens[0]].append(int(tokens[1]))
            if i > 4:
                results[type]['queryTimings'][int(tokens[0])].append(int(tokens[1]))

for t in ['ii', 'iiLDB', 'pt', 'ptB']:
    results[t]['setup'] = 1.0 * sum(results[t]['setup']) / len(results[t]['setup']) / 1e9
    results[t]['query'] = 1.0 * sum(results[t]['query']) / len(results[t]['query']) / 1e9
for t in ['ii', 'iiLDB', 'pt']:
    for i in range(51, 90 + 1):
        results[t]['queryTimings'][i] = 1.0 * sum(results[t]['queryTimings'][i]) / len(
            results[t]['queryTimings'][i]) / 1e9

queryTime = pd.Series([results[t]['query'] for t in ['ii', 'iiLDB', 'pt', 'ptB']],
                      index=['Inverted Index', 'Inverted Index, Level-DB', 'Pass-Through', 'Pass-Through Batch-Mode'])
setupTime = pd.Series([results[t]['setup'] for t in ['ii', 'iiLDB', 'pt', 'ptB']],
                      index=['Inverted Index', 'Inverted Index, Level-DB', 'Pass-Through', 'Pass-Through Batch-Mode'])
df = pd.DataFrame({'setup': setupTime, 'query': queryTime})
df = df[['setup', 'query']]
print df
ax = df.plot(kind='bar')
for i, p in enumerate(ax.patches):
    print i, p.get_height()
    ax.annotate("%.2f" % p.get_height(), (p.get_x() + (i == 4 or i == 5) * 0.05 - (i <= 3) * 0.1, p.get_height() + 150))

ax.set(xlabel='Inverted Index Type', ylabel='Time [s]')
ax.set_xticks([0, 1, 2, 3])
ax.set_xticklabels(['Inverted Index', 'Inverted Index, Level-DB', 'Pass-Through', 'Pass-Through Batch-Mode'],
                   rotation=0)

iiQueryTimes = pd.Series(results['ii']['queryTimings'])
ptQueryTimes = pd.Series(results['pt']['queryTimings'])
ptBQueryTimes = pd.Series([results['ptB']['query'] / len(results['iiLDB']['queryTimings'].keys())] * len(
    results['iiLDB']['queryTimings'].keys()), index=results['iiLDB']['queryTimings'].keys())
iiLDBQueryTimes = pd.Series(results['iiLDB']['queryTimings'])

df = pd.DataFrame({'ii': iiQueryTimes, 'pt': ptQueryTimes, 'iiLDB': iiLDBQueryTimes, 'ptB': ptBQueryTimes})
df1 = df[['ii', 'iiLDB']]
df2 = df[['pt', 'ptB']]
ax = df1.plot()
ax.set(xlabel='QueryId', ylabel='Query Time [s]')
ax.set_xticks(results['iiLDB']['queryTimings'].keys()[::2])

ax = df2.plot()
ax.set(xlabel='QueryId', ylabel='Query Time [s]')
ax.set_xticks(results['iiLDB']['queryTimings'].keys()[::2])

# print results
plt.show()
