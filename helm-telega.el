;;; helm-telega.el --- Helm <3 Telega  -*- lexical-binding: t -*-

;; * Header
;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/telega-user/helm-telega
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (memoize "1.1.0") (dash "2.16.0") (helm "3.2") (telega "20190818"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; * README                                                            :README:

;; ** Screenshots

;; *** Insert
;; [[file:screenshot/insert.gif]]

;; *** Describe sticker
;; [[file:screenshot/describe.gif]]

;; *** Toggling favourites
;; [[file:screenshot/favourites.gif]]

;; *** Share stickers
;; [[file:screenshot/share.gif]]

;; ** Installation

;; =helm-telega= depends on =helm=, =telega=, =dash=, =memoize=. All of them can
;; be downloaded from melpa.

;; Clone this repo and put it under your emacs's =load-path=, and add this
;; snippt to your configuration file

;; #+BEGIN_SRC emacs-lisp
;; (with-eval-after-load 'telega
;;   (require 'helm-telega))
;; #+END_SRC

;; ** Interactive functions

;; | Functions                                 | Usage                                              |
;; |-------------------------------------------+----------------------------------------------------|
;; | =helm-telega-stickerset-choose=           | Choose sticker from stickerset with helm           |
;; | =helm-telega-sticker-mini=                | Choose sticker from your favourite or recent usage |
;; | =helm-telega-sticker-favourite-or-recent= | Alias of =helm-telega-sticker-mini=                |

;; ** Feature list

;; - [X] Pick stickers from stickerset
;; - [X] Pick stickers from recent usage and favourites
;; - [ ] Pick stickers from emoji

;; ** Contributions

;; PRs and issues are welcomed.

;; * Code
;;; Code:

(require 'helm)
(require 'helm-mode)                    ;For `helm-comp-read'

(require 'telega)
(require 'telega-sticker)
(require 'telega-util)

(require 'dash)
(require 'cl-lib)
(require 'pcase)
(require 'eieio)
(require 'memoize)                      ;`defmemoize'
(require 'seq)                          ;Sticker collection operation
(require 'format-spec)

(autoload 'url-expand-file-name "url-expand")

;;; Customize

(defvar helm-telega-preview-limit 3)
(defvar helm-telega-width 60)
(defvar helm-telega-preivew-select-method 'head)

(defvar helm-telega--current-chat nil
  "A variable to hold current chat for helm.")

;;; Utils

(defalias 'helm-telega-get-prop #'plist-get
  "Get PROP from telega object OBJ.
\n(fn OBJ PROP)")

(defsubst helm-telega--get-real-match (cand)
  "Return the real string of CAND for matching.

Sign: (-> Str Str)"
  (get-text-property 0 'helm-telega--real-match cand))

(defun helm-telega--pick-seq (seq count method)
  "Pick COUNT elements from SEQ with METHOD

Sign: (All (T) (-> (Seqof T) Long (U 'head 'random) (Seqof T)))

Method can be one of following:

- `head' :: pick from head of seq
- `random' :: randomly pick"
  (cl-case method
    (head (seq-take seq count))
    (random
     (let ((num-seq (number-sequence 0 (- (seq-length seq) 1)))
           result)
       (dotimes (_ count)
         (let ((idx (seq-random-elt num-seq)))
           (push (seq-elt seq idx) result)
           (setq num-seq (delq idx num-seq))))
       (nreverse result)))))

;; NOTE: I don't whether it's safe to memoize this,
;; would display image change without modifying sticker?
(defmemoize helm-telega--make-sticker-image (sticker)
  "Create a display image from STICKER.

Sign: (-> Sticker Str)"
  (let* ((simage (telega-media--image
                  (cons sticker 'telega-sticker--create-image)
                  (cons sticker :sticker)))
         (img-size (image-size simage))
         (pad-text (or (helm-telega-get-prop (cdr simage) :telega-text)
                       (make-string (ceiling (car img-size)) ?X))))
    (propertize pad-text
                'rear-nonsticky '(display)
                'display simage)))

(defun helm-telega--make-actions (action-spec &optional keymap-sym)
  "Create helm actions from ACTION-SPEC.

Sign: (->* (Listof (U (List Str Sym Sym) (List Str Sym)))
           (Sym)
           (Listof (Cons Str Sym)))

ACTION-SPEC should be a list of (DOCSTRING ACTION &optional EXECUTOR)

Where:
DOCSTRING can contain two format specs, `%u' and `%k', respectively
stand for the key bindings of `universal-argument' and EXECUTOR in KEYMAP-SYM.

ACTION should be a helm action, EXECUTOR if supplied, should be an interactive
command that executes the ACTION."
  (cl-labels ((subst-cmd (command)
                (let ((template (--if-let keymap-sym
                                    (format "\\<%s>\\[%%s]" it)
                                  "\\[%s]")))
                  (format template command))))
    (mapcar (cl-function
             (lambda ((desc action &optional command))
               (cons
                (-> desc
                    (format-spec `((?u . ,(subst-cmd 'universal-argument))
                                   ,@(when command
                                       (list (cons ?k (subst-cmd command))))))
                    (substitute-command-keys))
                action)))
            action-spec)))

(defsubst helm-telega--make-display-string! (str)
  "Make an appropriate string from STR for display.

Sign: (-> Str Str)

If STR is shorter than `helm-telega-width', pad it with space,
otherwise truncate it."
  (truncate-string-to-width str helm-telega-width nil ?\s ?\.))

(defsubst helm-telega--set-of-sticker (sticker)
  "Return the set of STICKER.

Sign: (-> Sticer Stickerset)"
  (telega-stickerset-get
   (helm-telega-get-prop sticker :set_id)))

;;; Actions

(defun helm-telega-maybe-insert-sticker-to-chat! (_cand)
  "Insert marked candidates into chat if available."
  (--if-let helm-telega--current-chat
      (with-telega-chatbuf it
        (mapc #'telega-chatbuf-sticker-insert
              (helm-marked-candidates :all-sources t)))))

(defun helm-telega-sticker-toggle-favourite! (_cand)
  "Toggling favourite of marked candidates."
  (--doto (helm-marked-candidates :all-sources t)
    (mapc #'telega-sticker-toggle-favorite it)
    (message "Toggled %s sticker(s)' favourite status" (length it))))

(defun helm-telega-describe-set-of-sticker! (cand)
  "Describe stickerset of CAND."
  (telega-describe-stickerset (helm-telega--set-of-sticker cand)
                              helm-telega--current-chat))

(defun helm-telega-share-sticker-link! (cand)
  "Share stickerset link of CAND.

With prefix arg, save link to clipboard instead of kill ring."
  (-let* (((&plist :name) (helm-telega--set-of-sticker cand))
          (url-base (or (helm-telega-get-prop telega--options :t_me_url)
                        "https://t.me/"))
          (url-rest (concat "addstickers/" name))
          (full-url (url-expand-file-name url-rest url-base))
          (dest (if helm-current-prefix-arg "system clipboard" "kill ring")))
    (if helm-current-prefix-arg
        (with-temp-buffer
          (insert full-url)
          (clipboard-kill-region (point-min) (point-max)))
      (kill-new full-url))
    (message "The url of stickerset \"%s\" have been saved to %s"
             name dest)))

(defun helm-telega--make-action-executor (action extor-name)
  "Make executor named EXTOR-NAME for ACTION."
  (prog1 (defalias extor-name `(lambda ()
                                 (interactive)
                                 (with-helm-alive-p
                                   (helm-exit-and-execute-action ',action)))
           (format "Run action `%s'." action))
    (put extor-name 'helm-only t)))

;;;###autoload
(defvar helm-telega-sticker-source-keymap
  (-doto (make-sparse-keymap)
    (define-key (kbd "C-s") (helm-telega--make-action-executor
                             'helm-telega-share-sticker-link!
                             'helm-telega-do-share-sticker-link))
    (define-key (kbd "M-F") (helm-telega--make-action-executor
                             'helm-telega-sticker-toggle-favourite!
                             'helm-telega-do-sticker-toggle-favourite)))
  "The keymap used in `helm-telega-source-sticker'")

(defvar helm-telega--sticker-source-action-spec
  '(("Maybe insert" helm-telega-maybe-insert-sticker-to-chat!)
    ("Toggle favourite(s), `%k'" helm-telega-sticker-toggle-favourite!
     helm-telega-do-sticker-toggle-favourite)
    ("Describe set" helm-telega-describe-set-of-sticker!)
    ("Save stickerset link to kill ring, `%k, %u to system clipboard'"
     helm-telega-share-sticker-link! helm-telega-do-share-sticker-link))
  "The action spec of `helm-telega-source-sticker'.

See `helm-telega--make-actions' for more information.")


;;; Source

(defun helm-telega--make-stickers-candidate! (sticker)
  "Make a sticker candidate from STICKER.

Sign: (-> Sticker (Cons Str Sticker))"
  (let* ((emoji (telega-sticker-emoji sticker))
         (name (or (telega-emoji-name emoji) "(No abbrev)"))
         (str (format "%s %s" emoji name)))
    (-> str
        (helm-telega--make-display-string!)
        (concat " " (helm-telega--make-sticker-image sticker))
        (propertize 'helm-telega--real-match str)
        (cons sticker))))

(defun helm-telega--update-chat! ()
  "Update `helm-telega--current-chat'"
  (setq helm-telega--current-chat telega-chatbuf--chat))

(defclass helm-telega-source-sticker (helm-source-sync)
  ((init :initform #'helm-telega--update-chat!)
   ;; NOTE: Without this, resume a sticker source will use the chat
   ;; at where first time the source was created, which is counterintitutive.
   (resume :initform #'helm-telega--update-chat!)
   (action :initform (helm-telega--make-actions
                      helm-telega--sticker-source-action-spec
                      'helm-telega-sticker-source-keymap))
   (keymap :initform helm-telega-sticker-source-keymap))
  "Helm source for stickers completion.")

(defsubst helm-telega--make-stickers-source! (title stickers)
  "Make a sticker source with TITLE and STICKERS.

Sign: (-> Str (Seqof Sticker) helm-telega-source-sticker)"
  (helm-make-source title 'helm-telega-source-sticker
    :candidates (seq-map #'helm-telega--make-stickers-candidate! stickers)))

;;; Stickersets Utils

(defsubst helm-telega--stickers-source-from-stickerset! (sset)
  "Make a sticker source from SSET.

Sign: (-> Stickerset helm-telega-source-sticker)"
  (-let [(&plist :title :stickers) sset]
    (helm-telega--make-stickers-source! title stickers)))

(defsubst helm-telega--installed-stickersets! ()
  "Return installed stickersets.

Sign: (-> (Seqof Stickerset))"
  (mapcar #'telega-stickerset-get telega--stickersets-installed-ids))

(defun helm-telega--stickersets-candidates! (ssets)
  "Map each element in SSETS to a stickerset candidate.

Sign: (-> (Seqof Stickerset) (Listof (Cons Str Stickerset)))"
  (let* ((count helm-telega-preview-limit)
         (pr (make-progress-reporter "Collecting stickersets"
                                     0 (seq-length ssets))))
    (seq-map-indexed
     (lambda (sset index)
       (-let [(&plist :title :stickers) sset]
         (progress-reporter-do-update pr index)
         (--> (helm-telega--pick-seq stickers count
                                     helm-telega-preivew-select-method)
              (seq-map #'helm-telega--make-sticker-image it)
              (apply #'concat (helm-telega--make-display-string! title) " " it)
              (propertize it 'helm-telega--real-match title)
              (cons it sset))))
     ssets)))

(cl-defun helm-telega-stickerset-comp-read!
    (&optional
       (prompt "Choose one stickerset: ")
       (stickersets (helm-telega--installed-stickersets!))
     &aux
       (ssets-cand (helm-telega--stickersets-candidates! stickersets)))
  "Completing read STICKERSETS with PROMPT."
  (helm-comp-read prompt ssets-cand
                  :match-part #'helm-telega--get-real-match
                  :name "Stickersets"))

;;; Interactive functions

;;;###autoload
(defun helm-telega-stickerset-choose ()
  "Preconfigured `helm' for choose sticker from stickerset."
  (interactive)
  (let* ((sset (helm-telega-stickerset-comp-read!))
         (source (helm-telega--stickers-source-from-stickerset! sset)))
    (helm :sources source
          :buffer "*helm telega stickers*")))

;;;###autoload
(defun helm-telega-sticker-mini ()
  "Preconfigured `helm' for choose sticker from favourite and recent used."
  (interactive)
  (helm :sources (list (helm-telega--make-stickers-source!
                        "Favourite" (telega--getFavoriteStickers))
                       (helm-telega--make-stickers-source!
                        "Recent" (telega--getRecentStickers)))
        :buffer "*helm telega stickers mini*"))

;;;###autoload
(defalias 'helm-telega-sticker-favourite-or-recent #'helm-telega-sticker-mini)

;; * Footer

(provide 'helm-telega)

;; Local Variables:
;; coding: utf-8
;; End:

;;; helm-telega.el ends here
