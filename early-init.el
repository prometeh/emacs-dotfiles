;;; early-init.el -- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
;; Set the eln-cache path inside .cache directory
;; dont forget to define the environment for EMACSNATIVELOADPATH
;; somethinkg like:
;; export EMACSNATIVELOADPATH="$XDG_COFIG_HOME/emacs/.cache/temp"
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  ".cache/temp/eln-cache/" user-emacs-directory))))
(setq native-comp-eln-load-path '("~/.config/emacs/.cache/temp/eln-cache/"))
;; keeping emacs away from package in favor of straight
(setq package-enable-at-startup nil)

;;; early-init.el ends here
