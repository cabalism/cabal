FROM gitpod/workspace-base

# > make users-guide gp sync-done guide
# > 
# > }
# python3 -m venv .python-sphinx-virtualenv
# The virtual environment was not created successfully because ensurepip is not
# available.  On Debian/Ubuntu systems, you need to install the python3-venv
# package using the following command.

#     apt install python3.8-venv

# You may need to use sudo with that command.  After installing the python3-venv
# package, recreate your virtual environment.

# Failing command: ['/workspace/cabal/.python-sphinx-virtualenv/bin/python3', '-Im', 'ensurepip', '--upgrade', '--default-pip']

# make: *** [Makefile:242: .python-sphinx-virtualenv] Error 1
RUN sudo apt-get update && sudo apt install -y python3.8-venv