import os, getpass, shutil

cwd = os.getcwd();
HOME = "/home/" + getpass.getuser() + "/"

for files in os.listdir(cwd):
    if os.path.isdir(files) and != files == ".git" and != files == "init.py": 
        shutil.copytree(cwd + "/" + files, HOME + "/" files)
    else:
        shutil.copy(cwd + "/" + files, HOME)

text = """Remember to:
1) Insert password into muttrc and .weechat/irc.conf
2) Install Cask and install all the emacs plugins
3) Run a BundleInstall inside Vim"""

print (text)
