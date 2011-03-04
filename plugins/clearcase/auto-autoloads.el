;;; DO NOT MODIFY THIS FILE
(if (featurep 'clearcase-autoloads) (error "Feature clearcase-autoloads already loaded"))

(package-provide 'clearcase :version 1.1 :author-version "/main/laptop/165" :type 'regular)

;;;### (autoloads (clearcase-unintegrate clearcase-integrate) "clearcase" "clearcase/clearcase.el")

(defalias 'clearcase-install 'clearcase-integrate)

(autoload 'clearcase-integrate "clearcase" "\
Enable ClearCase integration" t nil)

(autoload 'clearcase-unintegrate "clearcase" "\
Disable ClearCase integration" t nil)

;;;***

(provide 'clearcase-autoloads)
