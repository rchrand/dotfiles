import os, getpass, shutil

cwd = os.getcwd();
HOME = "/home/" + getpass.getuser() + "/"

for files in os.listdir(cwd):
    if os.path.isdir(files) and != files == ".git" and != files == "init.py": 
        shutil.copytree(cwd + "/" + files, HOME + "/" files)
    else:
        shutil.copy(cwd + "/" + files, HOME)

text = """Remember to:
1: Install a terminal (URxvt Iterm2), mpd & ncmpcpp, weechat, emacs (run package-list install, if needed), vim (MacVim), zsh and oh my zsh, rbenv, ag, (packer or homebrew).
2: Change Ctrl to Caps lock - xmodmap or control center
3: Get Google Chrome Unstable and/or Firefox Nightly as default (If firefox, remember .vimperatorrc)
4: Remove the PASSWORD from your mutt settings """

print (text)
