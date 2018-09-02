(require 'package)

(package-initialize)

(unless (package-installed-p 'material-theme)
    (package-install 'material-theme))

(setq prelude-theme 'material)
