import os
import re
from collections import Counter
import matplotlib.pyplot as plt

directory = 'article/_posts/'
file_pattern = re.compile(r'^(\d{4})-\d{2}-\d{2}.*\.md$')

year_count = Counter()

for filename in os.listdir(directory):
    match = file_pattern.match(filename)
    if match:
        year = int(match.group(1))
        year_count[year] += 1

years = sorted(year_count.keys())
counts = [year_count[year] for year in years]

plt.bar(years, counts)
plt.xlabel('Year')
plt.ylabel('Number of Posts')
plt.title('Number of Posts per Year')
plt.xticks(years, years)
plt.savefig('posts-per-year.png')
