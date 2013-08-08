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

html, = env.Command(
    target='gh-pages/index.html',
    source='init.org',
    action=('$emacs -script org-export/org2html.el '
            '-infile $SOURCE -outfile $TARGET')
)

publish_log, = env.Command(
    target='publish.log',
    source='init.org',
    action=('(cd gh-pages && '
            'git commit -a -m "update ghpages" && '
            'git push origin gh-pages) | '
            'tee $TARGET')
    )
Alias('publish', publish_log)
Depends(publish_log, [html])
