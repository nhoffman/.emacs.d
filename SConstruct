import os
import platform

from SCons.Script import ARGUMENTS, Variables, Environment

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
    action='$emacs -script org-export/org-tangle.el -infile init.org'
    )


# publish:
# cd ../.emacs.d.ghpages && \
#     git checkout gh-pages && \
#     git commit -a -m "update ghpages" && \
#     git push origin gh-pages
# cd ${CWD}
