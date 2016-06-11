# https://github.com/vim/vim
# brammool

# https://github.com/torvalds/linux
# torvalds

import requests
import json
import pprint

##GET /repos/:owner/:repo/commits
##author is a parameter
##since is a parameter YYYY-MM-DDTHH:MM:SSZ ISO 8601 format

linux_commits = requests.get("https://api.github.com/repos/torvalds/linux/commits?author=torvalds&?since=2010-01-01T00:00:00Z")


vim_commits = requests.get("https://api.github.com/repos/vim/vim/commits?author=brammool&?since=2010-01-01T00:00:00Z")

# print(linux_commits.text[0:1000])
# print(vim_commits.text[0:1000])

linux_json = json.loads(linux_commits.text)
vim_json = json.loads(vim_commits.text)

# pprint.pprint(linux_json[0]["commit"]["message"])

linux_messages = [i["commit"]["message"] for i in linux_json]
vim_messages = [i["commit"]["message"] for i in vim_json]

with open("linus.txt", "w") as f:
    f.writelines(linux_messages)
    
with open("vim.txt", "w") as f:
    f.writelines(vim_messages)
