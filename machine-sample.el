; machine.el
; Author: Jake Voytko
; Time-stamp: <2016-07-21 20:18:52 jvoytko>
;
; Include any initialization that should not be committed to Github. For instance,
; corporation-specific elisp files, any local machine paths, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Point this to any php file in the directory to index. ac-php tags generation
;; uses the filename in the active buffer, and loads when PHP is active. May be
;; a string, or a list of strings.
(setq jv-php-path nil)
