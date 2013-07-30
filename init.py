import os, getpass, shutil

cwd = os.getcwd();
HOME = "/home/" + getpass.getuser() + "/"

for files in os.listdir(cwd):
    if os.path.isdir(files) and != files == ".git" and != files == "init.py": 
        shutil.copytree(cwd + "/" + files, HOME + "/" files)
    else:
        shutil.copy(cwd + "/" + files, HOME)

text = """Remember to:
1: Install Zsh & OhMyZsh, URxvt (To match .Xdefaults), Tmux, MPD & ncmpcpp, Irssi, Compile Vim from source with Python support (YouCompleteMe), Emacs if needed
2: to run :BundleInstall inside vim to install plugins 
3: You also need to compile YouCompleteMe inside the dicrtory. 
4: Remove the PASSWORD from your Irssi settings """
