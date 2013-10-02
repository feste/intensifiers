unigrams = ["crazy", "enormously", "exceedingly", "excessively", "extremely",
            "horribly", "hugely", "insanely", "quite", "really", "terribly",
            "uncommonly", "vastly", "very", "wildly"]
files = ["googlebooks-eng-all-1gram-20120701-" + w[0] for w in unigrams]

import re


#indices=[]

#for i in indices:
i=14

f = open(files[i], "r");
word = unigrams[i]
pattern = word + "\t(19[0-9][0-9]|20[0-9][0-9])\t([0-9]+)\t"

for line in f:
  m = re.match(pattern, line)
  if (m):
    year, count = m.group(1,2)
    print word + "\t" + year + "\t" + count
    if (year == "2008"):
      break
f.close()
