import os
import platform

import SCons
from SCons.Script import ARGUMENTS, Variables, Environment, Alias, Default

if platform.system() == 'Darwin':
    emacs = '/Applications/Emacs.app/Contents/MacOS/Emacs'
else:
    emacs = 'emacs'

vars = Variables()
vars.Add('emacs', default=ARGUMENTS.get('emacs', emacs))
vars.Add('init_dir', default=ARGUMENTS.get('init-dir', os.path.abspath('org-export')))
env = Environment(ENV=dict(os.environ,
                           PATH=':'.join(['emacs-env/bin', os.environ['PATH']])),
                  variables=vars)

Help(vars.GenerateHelpText(env))

org_file = 'init.org'

el, = env.Command(
    target='init.el',
    source=org_file,
    action=('$emacs --batch --eval '
            '\'(progn (require (quote package))(package-initialize)'
            '(find-file "$SOURCE")(org-mode)(print org-version)(org-babel-tangle))\'')
)
Alias('tangle', el)
Default(el)

html, = env.Command(
    target='html/index.html',
    source=org_file,
    action=('org-export/org-export html '
            '--package-dir $init_dir '
            '--infile $SOURCE --outfile $TARGET '
            '--bootstrap --embed-css')
)
Alias('html', html)

publish, = env.Command(
    target='ghp-import.log',
    source=html,
    action='ghp-import -p html > $TARGET'
)
Alias('publish', publish)

reset_log, = env.Command(
    target='reset_log.txt',
    source=html,
    action='git push origin :gh-pages'
)
Alias('reset', reset_log)

if GetOption('help'):
    print 'Available Build Aliases:'
    print '-----'
    for alias in sorted(SCons.Node.Alias.default_ans.keys()):
        print alias
