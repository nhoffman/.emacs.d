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

pull, = env.Command(
    target='.pull-ghpages',
    source='init.org',
    action=('(cd gh-pages && '
            'git checkout gh-pages && '
            'git fetch && '
            'git reset --hard origin/gh-pages) | '
            'tee $TARGET')
    )
Alias('pull-ghpages', pull)

html, = env.Command(
    target='gh-pages/index.html',
    source='init.org',
    action=('$emacs -script org-export/org2html.el '
            '-infile $SOURCE -outfile $TARGET')
)
Alias('html', html)
Depends(html, pull)

push, = env.Command(
    target='.push-ghpages',
    source='init.org',
    action=('(cd gh-pages && '
            'git checkout gh-pages && '
            'git commit -a -m "update ghpages" && '
            'git push origin gh-pages) | '
            'tee $TARGET')
    )
Alias('push-ghpages', push)
Depends(push, [html])
