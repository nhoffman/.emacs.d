import os
import platform

from SCons.Script import ARGUMENTS, Variables, Environment, Alias

if platform.system() == 'Darwin':
    emacs = '/Applications/Emacs.app/Contents/MacOS/Emacs'
else:
    emacs = 'emacs'

vars = Variables()
vars.Add('emacs', default=ARGUMENTS.get('emacs', emacs))
env = Environment(ENV=os.environ, variables=vars)

el, = env.Command(
    target='init.el',
    source='init.org',
    action='$emacs -script org-export/org-tangle.el -infile $SOURCE'
)
Alias('el', el)

