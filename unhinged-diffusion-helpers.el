;;; unhinged-diffusion-helpers.el --- Internal helper functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2026 Bernd Wachter
;;
;;; Commentary:
;;
;; Various internal helper functions, grouped here for convenience.
;;
;; Unlike the other modules this is not scoped to the name.
;;
;;; Code:

(require 'org)

;;;; Org buffer navigation
;;
;; Sections and per-step entries in a diffusion buffer are tagged with
;; unique org properties, so insertion points can be found by property
;; lookups.

(defconst unhinged-diffusion--prop-section "UNHINGED-SECTION"
  "Org property tagging the static sections of a diffusion buffer.

Known values: \"canvas\", \"step-history\", \"edit-log\".")

(defconst unhinged-diffusion--prop-step-snapshot "UNHINGED-STEP-SNAPSHOT"
  "Org property tagging per-step thumbnail entries in * Step History.

Value is \"STEP/TOTAL\".")

(defconst unhinged-diffusion--prop-step-log "UNHINGED-STEP-LOG"
  "Org property tagging per-step subsections in * Edit Log.

Value is \"STEP/TOTAL\".")

(defun unhinged-diffusion--goto-entry (prop value)
  "Move point to the org heading of the first entry with property PROP = VALUE.

Return t on success, nil if no such entry exists."
  (let ((pos (org-find-property prop value)))
    (when pos
      (goto-char pos)
      (org-back-to-heading t)
      t)))

(defun unhinged-diffusion--goto-section (id)
  "Move point to the end of the heading in ID section.

Return t if the section exists, nil otherwise."
  (when (unhinged-diffusion--goto-entry unhinged-diffusion--prop-section id)
    (org-end-of-subtree)
    t))

(defun unhinged-diffusion--section-end (id)
  "Return the end position of the UNHINGED-SECTION ID section.

Return nil if there is no such section."
  (save-excursion
    (when (unhinged-diffusion--goto-section id)
      (point))))

(defun unhinged-diffusion--insert-entry (level title prop value)
  "Insert at point an org heading of LEVEL named TITLE with property
PROP set to VALUE.  Leaves point after the property drawer, ready for
content insertion."
  (insert (format "%s %s\n:PROPERTIES:\n:%s: %s\n:END:\n"
                  (make-string level ?*)
                  title
                  prop
                  value)))

(defun unhinged-diffusion--random-float ()
  "Return a random float between 0.0 (inclusive) and 1.0 (exclusive)."
  (/ (float (random 1000000000)) 1000000000.0))

(defun unhinged-diffusion--gaussian-random (mean stddev)
  "Generate Gaussian random number with MEAN and STDDEV.

Uses the Box-Muller transform."
  (let* ((u1 (max 1e-10 (unhinged-diffusion--random-float)))
         (u2 (unhinged-diffusion--random-float))
         (z0 (* (sqrt (* -2.0 (log u1)))
                (cos (* 2.0 float-pi u2)))))
    (+ mean (* stddev z0))))

(defun unhinged-diffusion--rgb-to-argb (r g b &optional a)
  "Convert R, G, B channel values (0-255) to ARGB32 integer.

Optional A (alpha) defaults to 255."
  (logior (ash (or a 255) 24)
          (ash (round r) 16)
          (ash (round g) 8)
          (round b)))

(defun unhinged-diffusion--argb-to-rgba (argb)
  "Convert ARGB32 integer to a list (R G B A), each 0-255."
  (list (logand (ash argb -16) #xFF)
        (logand (ash argb -8) #xFF)
        (logand argb #xFF)
        (logand (ash argb -24) #xFF)))

(defun unhinged-diffusion--parse-color (color)
  "Parse COLOR string to a list (R G B).

Supports #RRGGBB hex strings."
  (cond
   ((string-match "^#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)$" color)
    (list (string-to-number (match-string 1 color) 16)
          (string-to-number (match-string 2 color) 16)
          (string-to-number (match-string 3 color) 16)))
   (t (error "Unsupported color format: %s" color))))

(defun unhinged-diffusion--clamp (value min max)
  "Clamp VALUE between MIN and MAX."
  (if (< value min) min (if (> value max) max value)))

(defun unhinged-diffusion--blend-pixel (data idx r g b a)
  "Blend RGB values into pixel at DATA[IDX] with alpha A.

If A is 255, replaces the pixel outright."
  (if (= a 255)
      (aset data idx (unhinged-diffusion--rgb-to-argb r g b))
    (let* ((old (unhinged-diffusion--argb-to-rgba (aref data idx)))
           (blend-a (/ a 255.0))
           (new-r (+ (* (nth 0 old) (- 1.0 blend-a)) (* r blend-a)))
           (new-g (+ (* (nth 1 old) (- 1.0 blend-a)) (* g blend-a)))
           (new-b (+ (* (nth 2 old) (- 1.0 blend-a)) (* b blend-a))))
      (aset data idx (unhinged-diffusion--rgb-to-argb new-r new-g new-b)))))

(provide 'unhinged-diffusion-helpers)

;;; unhinged-diffusion-helpers.el ends here
