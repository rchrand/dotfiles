import os, getpass, shutil

cwd = os.getcwd();
HOME = "/home/" + getpass.getuser() + "/"

for files in os.listdir(cwd):
    if os.path.isdir(files) and files == ".git" and files == "init.py":
        shutil.copytree(cwd + "/" + files, HOME)
    else:
        shutil.copy(cwd + "/" + files, HOME)

print ("Remember:
1: to run :BundleInstall inside vim to install plugins 
2: You also need to compile YouCompleteMe inside the dicrtory. 
3: Remove the PASSWORD from your Irssi settings
4: Install Zsh & OhMyZsh, URxvt (To match .Xdefaults), Tmux, Mpd & ncmpcpp, Compile Vim from source with Python support (YouCompleteMe), Emacs if needed ")

