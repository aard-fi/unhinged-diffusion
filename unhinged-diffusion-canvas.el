;;; unhinged-diffusion-canvas.el --- Canvas init and info -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2026 Bernd Wachter
;;
;;; Commentary:
;;
;; This module contains the canvas interactions, and some of the buffer
;; handling. The latter is still a bit of a mess, and needs to be cleaned
;; up properly at some point.
;;
;;; Code:

(unless (fboundp 'canvas-refresh)
  (error "Unhinged Diffusion requires the Emacs canvas API.  Please use Emacs 31+ with canvas support, or Emacs 32."))

(require 'unhinged-diffusion-vars)
(require 'unhinged-diffusion-helpers)

(defun unhinged-diffusion-canvas-create (buffer width height)
  "Create a new canvas buffer named BUFFER with WIDTH x HEIGHT dimensions.

Initialize with Gaussian noise around mid-gray."
  (let ((buf (get-buffer-create buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      ;; Show inserted step thumbnails inline without a manual toggle
      (setq-local org-startup-with-inline-images t)
      (let ((data (make-vector (* width height) #xFF808080)))
        (setq unhinged-diffusion--canvas
              `(image :type canvas
                      :id ,(intern (format "unhinged-diffusion-%s" buffer))
                      :data-width ,width
                      :data-height ,height
                      :data ,data))
        (setq unhinged-diffusion--step 0)
        (unhinged-diffusion--insert-entry
         1 "Canvas" unhinged-diffusion--prop-section "canvas")
        (let ((start (point)))
          (insert " ")
          (let ((ov (make-overlay start (point) nil nil nil)))
            (overlay-put ov 'display unhinged-diffusion--canvas)
            (overlay-put ov 'read-only t)
            (overlay-put ov 'unhinged-diffusion-canvas-overlay t)))
        (insert "\n")
        (unhinged-diffusion--insert-entry
         1 "Step History" unhinged-diffusion--prop-section "step-history")
        (insert "\n")
        (unhinged-diffusion--insert-entry
         1 "Edit Log" unhinged-diffusion--prop-section "edit-log")
        (unhinged-diffusion-canvas--init-noise)))
    buf))

(defun unhinged-diffusion-canvas-dimensions ()
  "Return (WIDTH . HEIGHT) of current buffer's canvas."
  (let ((spec (cdr unhinged-diffusion--canvas)))
    (cons (plist-get spec :data-width)
          (plist-get spec :data-height))))

(defun unhinged-diffusion-canvas--init-noise ()
  "Fill the current buffer's canvas with Gaussian noise around mid-grey.

Noise is applied independently to each RGB channel."
  (let* ((spec (cdr unhinged-diffusion--canvas))
         (data (plist-get spec :data))
         (size (length data)))
    (dotimes (i size)
      (let ((r (unhinged-diffusion--gaussian-random 128 40))
            (g (unhinged-diffusion--gaussian-random 128 40))
            (b (unhinged-diffusion--gaussian-random 128 40)))
        (aset data i (unhinged-diffusion--rgb-to-argb
                      (unhinged-diffusion--clamp r 0 255)
                      (unhinged-diffusion--clamp g 0 255)
                      (unhinged-diffusion--clamp b 0 255)))))
    (unhinged-diffusion-canvas--refresh)))

(defun unhinged-diffusion-canvas--log-edit (fmt &rest args)
  "Append an edit log entry under the correct step subsection.

Finds or creates a ** Step N/M heading under * Edit Log and inserts there."
  (when (derived-mode-p 'org-mode)
    (let* ((inhibit-read-only t)
           (step-num (or unhinged-diffusion--step 0))
           (total-steps (or unhinged-diffusion--total-steps 0))
           (entry (format "- =%s=\n" (apply #'format fmt args)))
           (step-id (format "%d/%d" step-num total-steps)))
      (save-excursion
        (goto-char (point-min))
        (cond
         ;; Existing step subsection: append at the end of it.
         ((unhinged-diffusion--goto-entry unhinged-diffusion--prop-step-log step-id)
          (goto-char (org-entry-end-position))
          (insert entry))
         ;; No subsection yet: create one at the end of * Edit Log.
         ((unhinged-diffusion--goto-section "edit-log")
          (insert "\n")
          (unhinged-diffusion--insert-entry
           2 (format "Step %d/%d" step-num total-steps)
           unhinged-diffusion--prop-step-log step-id)
          (insert entry)))))))

;; helper function to make sure draw operations show up in buffers
(defun unhinged-diffusion-canvas--refresh ()
  "Refresh the canvas display after pixel data has been modified.

Calls `canvas-refresh' with RELOAD-DATA t so the display engine
re-copies the :data vector into its internal pixel buffer."
  (when unhinged-diffusion--canvas
    (condition-case nil
        (canvas-refresh unhinged-diffusion--canvas t)
      (error nil))))

;; pixel access
(defun unhinged-diffusion-canvas--pixel-get-data ()
  "Get the pixel data vector from current buffer's canvas."
  (plist-get (cdr unhinged-diffusion--canvas) :data))

(defun unhinged-diffusion-canvas-pixel-get (x y)
  "Get ARGB32 value at pixel X, Y in current buffer.

Returns nil if coordinates are out of bounds."
  (let* ((dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims)))
    (when (and (>= x 0) (< x width) (>= y 0) (< y height))
      (let ((data (unhinged-diffusion-canvas--pixel-get-data)))
        (aref data (+ x (* y width)))))))

(defun unhinged-diffusion-canvas-pixel-set (x y argb)
  "Set pixel at X, Y to ARGB32 value.

Does nothing if coordinates are out of bounds."
  (let* ((dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims)))
    (when (and (>= x 0) (< x width) (>= y 0) (< y height))
      (let ((data (unhinged-diffusion-canvas--pixel-get-data)))
        (aset data (+ x (* y width)) argb)
        (unhinged-diffusion-canvas--refresh)))))

;; canvas manipulation
;;
;; For a first proof of concept having those functions operate on rectangles
;; and by taking coordinates of top left and bottom right corners is fine,
;; though to get better images out of this thing we probably need to allow
;; better control over the areas.
;;
;; Most sensible approach probably would be to have one or more pixel iterators
;; that can be passed as argument, clean up llm input (they have a tendency
;; to get the order of points wrong), calculate the area, and then have the
;; following draw functions applied over the area.

(defun unhinged-diffusion-canvas-blur-region (x1 y1 x2 y2 radius)
  "Apply box blur to the rectangular region X1,Y1 to X2,Y2.

RADIUS controls the blur kernel size (pixels in each direction)."
  (let* ((dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims))
         (data (unhinged-diffusion-canvas--pixel-get-data))
         (rx1 (max 0 (min x1 x2)))
         (ry1 (max 0 (min y1 y2)))
         (rx2 (min width (max x1 x2)))
         (ry2 (min height (max y1 y2)))
         (tmp (make-vector (* width height) 0)))
    (when (and data (> radius 0))
      ;; Copy original to temp
      (dotimes (i (length data))
        (aset tmp i (aref data i)))
      ;; Apply box blur using temp as source
      (cl-loop for py from ry1 below ry2 do
               (cl-loop for px from rx1 below rx2 do
                        (let ((sum-r 0) (sum-g 0) (sum-b 0) (count 0))
                          (cl-loop for dy from (- radius) to radius do
                                   (cl-loop for dx from (- radius) to radius do
                                            (let ((sx (+ px dx))
                                                  (sy (+ py dy)))
                                              (when (and (>= sx 0) (< sx width)
                                                         (>= sy 0) (< sy height))
                                                (let ((c (unhinged-diffusion--argb-to-rgba (aref tmp (+ sx (* sy width))))))
                                                  (setq sum-r (+ sum-r (nth 0 c)))
                                                  (setq sum-g (+ sum-g (nth 1 c)))
                                                  (setq sum-b (+ sum-b (nth 2 c)))
                                                  (setq count (1+ count)))))))
                          (when (> count 0)
                            (aset data (+ px (* py width))
                                  (unhinged-diffusion--rgb-to-argb
                                   (/ (float sum-r) count)
                                   (/ (float sum-g) count)
                                   (/ (float sum-b) count)))))))
      (unhinged-diffusion-canvas--refresh)
      (unhinged-diffusion-canvas--log-edit
       "blur_region(%d,%d→%d,%d, r:%d)" x1 y1 x2 y2 radius))))

(defun unhinged-diffusion-canvas-sharpen-region (x1 y1 x2 y2 intensity)
  "Apply sharpening to region X1,Y1 to X2,Y2.<

INTENSITY is 0.0 to 1.0, controlling the strength of the unsharp mask."
  (let* ((dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims))
         (data (unhinged-diffusion-canvas--pixel-get-data))
         (rx1 (max 0 (min x1 x2)))
         (ry1 (max 0 (min y1 y2)))
         (rx2 (min width (max x1 x2)))
         (ry2 (min height (max y1 y2)))
         (tmp (make-vector (* width height) 0))
         (amount (unhinged-diffusion--clamp intensity 0.0 1.0)))
    (when data
      ;; Copy original to temp
      (dotimes (i (length data))
        (aset tmp i (aref data i)))
      ;; Simple unsharp mask: pixel += (pixel - blur) * amount
      (cl-loop for py from ry1 below ry2 do
               (cl-loop for px from rx1 below rx2 do
                        (let ((orig (unhinged-diffusion--argb-to-rgba (aref tmp (+ px (* py width))))))
                          (let ((sum-r 0) (sum-g 0) (sum-b 0) (count 0))
                            ;; 3x3 blur
                            (cl-loop for dy from -1 to 1 do
                                     (cl-loop for dx from -1 to 1 do
                                              (let ((sx (+ px dx))
                                                    (sy (+ py dy)))
                                                (when (and (>= sx 0) (< sx width)
                                                           (>= sy 0) (< sy height)
                                                           (not (and (= dx 0) (= dy 0))))
                                                  (let ((c (unhinged-diffusion--argb-to-rgba (aref tmp (+ sx (* sy width))))))
                                                    (setq sum-r (+ sum-r (nth 0 c)))
                                                    (setq sum-g (+ sum-g (nth 1 c)))
                                                    (setq sum-b (+ sum-b (nth 2 c)))
                                                    (setq count (1+ count)))))))
                            (when (> count 0)
                              (let* ((blur-r (/ (float sum-r) count))
                                     (blur-g (/ (float sum-g) count))
                                     (blur-b (/ (float sum-b) count))
                                     (new-r (+ (nth 0 orig) (* (- (nth 0 orig) blur-r) amount)))
                                     (new-g (+ (nth 1 orig) (* (- (nth 1 orig) blur-g) amount)))
                                     (new-b (+ (nth 2 orig) (* (- (nth 2 orig) blur-b) amount))))
                                (aset data (+ px (* py width))
                                      (unhinged-diffusion--rgb-to-argb
                                       (unhinged-diffusion--clamp new-r 0 255)
                                       (unhinged-diffusion--clamp new-g 0 255)
                                       (unhinged-diffusion--clamp new-b 0 255)))))))))
      (unhinged-diffusion-canvas--refresh)
      (unhinged-diffusion-canvas--log-edit
       "sharpen_region(%d,%d→%d,%d, i:%.2f)" x1 y1 x2 y2 intensity))))

(defun unhinged-diffusion-canvas-add-noise (x1 y1 x2 y2 amount)
  "Add random noise to region X1,Y1 to X2,Y2.

AMOUNT controls intensity (0-255 range, typically 10-50)."
  (let* ((dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims))
         (data (unhinged-diffusion-canvas--pixel-get-data))
         (rx1 (max 0 (min x1 x2)))
         (ry1 (max 0 (min y1 y2)))
         (rx2 (min width (max x1 x2)))
         (ry2 (min height (max y1 y2)))
         (amt (float amount)))
    (when data
      (cl-loop for py from ry1 below ry2 do
               (cl-loop for px from rx1 below rx2 do
                        (let* ((idx (+ px (* py width)))
                               (c (unhinged-diffusion--argb-to-rgba (aref data idx)))
                               (new-r (unhinged-diffusion--clamp (+ (nth 0 c) (* (- (unhinged-diffusion--random-float) 0.5) amt 2)) 0 255))
                               (new-g (unhinged-diffusion--clamp (+ (nth 1 c) (* (- (unhinged-diffusion--random-float) 0.5) amt 2)) 0 255))
                               (new-b (unhinged-diffusion--clamp (+ (nth 2 c) (* (- (unhinged-diffusion--random-float) 0.5) amt 2)) 0 255)))
                          (aset data idx (unhinged-diffusion--rgb-to-argb new-r new-g new-b)))))
      (unhinged-diffusion-canvas--refresh)
      (unhinged-diffusion-canvas--log-edit
       "add_noise(%d,%d→%d,%d, amt:%d)" x1 y1 x2 y2 amount))))

(defun unhinged-diffusion-canvas-blend-colour (x y color alpha)
  "Blend COLOR at pixel X,Y with ALPHA (0-255).

COLOR is a hex string like \"#RRGGBB\"."
  (let* ((rgba (unhinged-diffusion--parse-color color))
         (r (nth 0 rgba))
         (g (nth 1 rgba))
         (b (nth 2 rgba))
         (a (unhinged-diffusion--clamp alpha 0 255))
         (dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims))
         (data (unhinged-diffusion-canvas--pixel-get-data)))
    (when (and data (>= x 0) (< x width) (>= y 0) (< y height))
      (let* ((idx (+ x (* y width)))
             (old (unhinged-diffusion--argb-to-rgba (aref data idx)))
             (blend-a (/ a 255.0))
             (new-r (+ (* (nth 0 old) (- 1.0 blend-a)) (* r blend-a)))
             (new-g (+ (* (nth 1 old) (- 1.0 blend-a)) (* g blend-a)))
             (new-b (+ (* (nth 2 old) (- 1.0 blend-a)) (* b blend-a))))
        (aset data idx (unhinged-diffusion--rgb-to-argb new-r new-g new-b))
        (unhinged-diffusion-canvas--refresh)
        (unhinged-diffusion-canvas--log-edit
         "blend_color(%d,%d, %s, α:%d)" x y color alpha)))))

(defun unhinged-diffusion-canvas-adjust-brightness-contrast (x1 y1 x2 y2 brightness contrast)
  "Adjust BRIGHTNESS (-255 to 255) and CONTRAST (-1.0 to 1.0) in region.

Positive CONTRAST increases contrast; negative decreases it."
  (let* ((dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims))
         (data (unhinged-diffusion-canvas--pixel-get-data))
         (rx1 (max 0 (min x1 x2)))
         (ry1 (max 0 (min y1 y2)))
         (rx2 (min width (max x1 x2)))
         (ry2 (min height (max y1 y2)))
         (bri (float brightness))
         (contrast-factor (if (>= contrast 0)
                              (+ 1.0 contrast)
                            (/ 1.0 (+ 1.0 (abs contrast))))))
    (when data
      (cl-loop for py from ry1 below ry2 do
               (cl-loop for px from rx1 below rx2 do
                        (let* ((idx (+ px (* py width)))
                               (c (unhinged-diffusion--argb-to-rgba (aref data idx)))
                               (new-r (unhinged-diffusion--clamp
                                       (+ (* (- (nth 0 c) 128) contrast-factor) 128 bri)
                                       0 255))
                               (new-g (unhinged-diffusion--clamp
                                       (+ (* (- (nth 1 c) 128) contrast-factor) 128 bri)
                                       0 255))
                               (new-b (unhinged-diffusion--clamp
                                       (+ (* (- (nth 2 c) 128) contrast-factor) 128 bri)
                                       0 255)))
                          (aset data idx (unhinged-diffusion--rgb-to-argb new-r new-g new-b)))))
      (unhinged-diffusion-canvas--refresh)
      (unhinged-diffusion-canvas--log-edit
       "adjust_bc(%d,%d→%d,%d, b:%d, c:%.2f)" x1 y1 x2 y2 brightness contrast))))

(provide 'unhinged-diffusion-canvas)

;;; unhinged-diffusion-canvas.el ends here
