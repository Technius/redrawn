# Dependency setup

Requires Python 3.7+.

Make sure you've activated a virtualenv first. 
```bash
pip3 install virtualenv
virtualenv venv

For Mac/Linux: 
source venv/bin/activate

For Windows: 
source venv/Scripts/activate
```

Clone the modified version of Tyrell (outside of this directory):
```bash
git clone https://github.ucsb.edu/bryantan/Tyrell.git
pip install -e Tyrell
```

Go back into this directory and install other dependencies:
```bash
pip install -r requirements.txt
```
