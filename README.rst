Dot Emacs
=========

"Emacs is a great operating system, lacking only a decent editor." :P


Python
------

```python
$ python3 -m venv ~/env
$ source ~/env/bin/activate
(env) $ pip install pipx
(env) $ pipx install pipx
(env) $ pipx ensurepath
(env) $ deactivate
$ source ~/.bash_profile
$ pipx install black
$ pipx install flake8
$ pipx install ipython
$ pipx inject --include-apps --include-deps ipython jupyter
$ pipx inject --include-apps --include-deps ipython jupyterlab
$ pipx inject --include-apps --include-deps ipython jupytext
$ pipx inject --include-apps ipython voila
$ pipx inject --include-apps ipython bokeh
$ pipx runpip ipython install diskcache gspread pandas matplotlib SQLAlchemy
$ pipx inject --include-apps ipython sshtunnel
$ pipx inject --include-apps ipython PyMySQL
$ pipx install isort
$ pipx install mypy
$ pipx install py-spy
$ pipx install pygments
$ pipx install pylint
$ pipx install python-language-server[all]
```
