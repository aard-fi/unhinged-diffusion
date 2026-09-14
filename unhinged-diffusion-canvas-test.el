;;; unhinged-diffusion-canvas-test.el --- Interactive canvas refresh testbed -*- lexical-binding: t; -*-
;;
;; This is a test file for making sure the canvas drawing routines work properly.
;; Probably not interesting for most, but added just in case somebody cares.
;;
;; To use, eval this buffer, M-x udc-test-canvas-create

(require 'cl-lib)

(unless (fboundp 'canvas-refresh)
  (error "This requires the Emacs canvas API."))

(defvar udc-test--canvas nil)
(defvar udc-test--canvas-buffer "*udc-canvas-test*")

(defun udc-test--rgb-to-argb (r g b)
  "Pack R G B (0-255 floats) into an ARGB32 integer."
  (logior #xFF000000
          (logior (ash (round r) 16)
                  (logior (ash (round g) 8) (round b)))))

(defun udc-test--refresh-A ()
  "Strategy A: just canvas-refresh."
  (canvas-refresh udc-test--canvas))

(defun udc-test--refresh-B ()
  "Strategy B: canvas-refresh + force-window-update."
  (canvas-refresh udc-test--canvas)
  (force-window-update (current-buffer)))

(defun udc-test--refresh-C ()
  "Strategy C: canvas-refresh + force-window-update + redisplay."
  (canvas-refresh udc-test--canvas)
  (force-window-update (current-buffer))
  (redisplay t))

(defun udc-test--refresh-D ()
  "Strategy D: clear-image-cache on the spec."
  (canvas-refresh udc-test--canvas)
  (condition-case nil
      (clear-image-cache udc-test--canvas)
    (error nil))
  (force-window-update (current-buffer)))

(defun udc-test--refresh-E ()
  "Strategy E: delete + recreate the overlay."
  (canvas-refresh udc-test--canvas)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\* Canvas\n" nil t)
      (let ((pos (point)))
        (dolist (ov (overlays-at pos))
          (when (overlay-get ov 'udc-test-canvas)
            (delete-overlay ov)))
        (let ((ov (make-overlay pos (1+ pos) nil nil nil)))
          (overlay-put ov 'display udc-test--canvas)
          (overlay-put ov 'udc-test-canvas t)))))
  (force-window-update (current-buffer)))

(defun udc-test--refresh-F ()
  "Strategy F: delete overlay, clear image cache, recreate overlay."
  (canvas-refresh udc-test--canvas)
  (condition-case nil
      (clear-image-cache udc-test--canvas)
    (error nil))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\* Canvas\n" nil t)
      (let ((pos (point)))
        (dolist (ov (overlays-at pos))
          (when (overlay-get ov 'udc-test-canvas)
            (delete-overlay ov)))
        (let ((ov (make-overlay pos (1+ pos) nil nil nil)))
          (overlay-put ov 'display udc-test--canvas)
          (overlay-put ov 'udc-test-canvas t)))))
  (force-window-update (current-buffer)))

(defun udc-test--refresh-G ()
  "Strategy G: rebuild the image spec with a fresh id, keep same data."
  (let ((spec (cdr udc-test--canvas)))
    (setq udc-test--canvas
          `(image :type canvas
                  :id ,(gensym "udc-")
                  :data-width ,(plist-get spec :data-width)
                  :data-height ,(plist-get spec :data-height)
                  :data ,(plist-get spec :data)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\* Canvas\n" nil t)
        (let ((pos (point)))
          (dolist (ov (overlays-at pos))
            (when (overlay-get ov 'udc-test-canvas)
              (delete-overlay ov)))
          (let ((ov (make-overlay pos (1+ pos) nil nil nil)))
            (overlay-put ov 'display udc-test--canvas)
            (overlay-put ov 'udc-test-canvas t))))))
  (force-window-update (current-buffer)))

(defun udc-test-canvas-create ()
  "Create a 256×256 test canvas buffer with Gaussian noise."
  (interactive)
  (let ((buf (get-buffer-create udc-test--canvas-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (let* ((w 256) (h 256)
             (data (make-vector (* w h) #xFF808080)))
        ;; fill with noise
        (dotimes (i (* w h))
          (aset data i (udc-test--rgb-to-argb
                        (+ 128 (* (- (random) 0.5) 80))
                        (+ 128 (* (- (random) 0.5) 80))
                        (+ 128 (* (- (random) 0.5) 80)))))
        (setq udc-test--canvas
              `(image :type canvas
                      :id ,(intern (format "udc-test-%s" buf))
                      :data-width ,w
                      :data-height ,h
                      :data ,data))
        (insert "* Canvas\n")
        (let ((pos (point)))
          (insert " ")
          (let ((ov (make-overlay pos (point) nil nil nil)))
            (overlay-put ov 'display udc-test--canvas)
            (overlay-put ov 'udc-test-canvas t)
            (overlay-put ov 'rear-nonsticky '(display))))
        (insert "\n\n* Edit Log\n")
        (insert "Evaluate a refresh strategy then run M-x udc-test-draw-red-square\n")))
    (pop-to-buffer buf)))

(defun udc-test-draw-red-square ()
  "Draw a solid red 50×50 square at (50,50).  Then try each refresh."
  (interactive)
  (let* ((spec (cdr udc-test--canvas))
         (data (plist-get spec :data))
         (w (plist-get spec :data-width))
         (h (plist-get spec :data-height)))
    (dotimes (dy 50)
      (dotimes (dx 50)
        (let ((x (+ 50 dx))
              (y (+ 50 dy)))
          (when (and (< x w) (< y h))
            (aset data (+ x (* y w)) #xFFFF0000)))))
    (message "Red square drawn.  Now evaluate a refresh strategy.")))

(defun udc-test-draw-green-rectangle ()
  "Draw a semi-transparent green rectangle at bottom half."
  (interactive)
  (let* ((spec (cdr udc-test--canvas))
         (data (plist-get spec :data))
         (w (plist-get spec :data-width))
         (h (plist-get spec :data-height)))
    (dotimes (py (/ h 2))
      (dotimes (px w)
        (let* ((y (+ (/ h 2) py))
               (idx (+ px (* y w)))
               (old (udc-test--argb-to-rgba (aref data idx))))
          (aset data idx
                (udc-test--rgb-to-argb
                 (+ (* (nth 0 old) 0.7) (* 0 0.3))
                 (+ (* (nth 1 old) 0.7) (* 128 0.3))
                 (+ (* (nth 2 old) 0.7) (* 0 0.3)))))))
    (message "Green rectangle drawn.  Now evaluate a refresh strategy.")))

(defun udc-test-blur-whole ()
  "Blur the entire canvas."
  (interactive)
  (let* ((spec (cdr udc-test--canvas))
         (data (plist-get spec :data))
         (w (plist-get spec :data-width))
         (h (plist-get spec :data-height))
         (tmp (make-vector (* w h) 0)))
    (dotimes (i (* w h))
      (aset tmp i (aref data i)))
    (dotimes (py h)
      (dotimes (px w)
        (let ((sum-r 0) (sum-g 0) (sum-b 0) (count 0))
          (cl-loop for dy from -1 to 1 do
                   (cl-loop for dx from -1 to 1 do
                            (let ((sx (+ px dx))
                                  (sy (+ py dy)))
                              (when (and (>= sx 0) (< sx w)
                                         (>= sy 0) (< sy h))
                                (let ((c (udc-test--argb-to-rgba (aref tmp (+ sx (* sy w))))))
                                  (setq sum-r (+ sum-r (nth 0 c))
                                        sum-g (+ sum-g (nth 1 c))
                                        sum-b (+ sum-b (nth 2 c))
                                        count (1+ count)))))))
          (when (> count 0)
            (aset data (+ px (* py w))
                  (udc-test--rgb-to-argb
                   (/ (float sum-r) count)
                   (/ (float sum-g) count)
                   (/ (float sum-b) count)))))))
    (message "Whole canvas blurred.  Now evaluate a refresh strategy.")))

(defun udc-test--argb-to-rgba (argb)
  "Convert ARGB32 integer to (R G B) list."
  (list (logand (ash argb -16) #xFF)
        (logand (ash argb -8) #xFF)
        (logand argb #xFF)))

(defun udc-test-export-png ()
  "Export current canvas to a temp PNG and open it externally."
  (interactive)
  (let* ((spec (cdr udc-test--canvas))
         (data (plist-get spec :data))
         (w (plist-get spec :data-width))
         (h (plist-get spec :data-height))
         (ppm (make-temp-file "udc-test-" nil ".ppm"))
         (png (make-temp-file "udc-test-" nil ".png")))
    (with-temp-file ppm
      (set-buffer-multibyte nil)
      (insert (format "P6\n%d %d\n255\n" w h))
      (dotimes (i (length data))
        (let ((argb (aref data i)))
          (insert (unibyte-string (logand (ash argb -16) #xFF))
                  (unibyte-string (logand (ash argb -8) #xFF))
                  (unibyte-string (logand argb #xFF))))))
    (if (executable-find "convert")
        (progn
          (call-process "convert" nil nil nil ppm png)
          (message "Exported to %s" png)
          (delete-file ppm))
      (message "No ImageMagick convert; PPM left at %s" ppm))))

(provide 'unhinged-diffusion-canvas-test)
;;; unhinged-diffusion-canvas-test.el ends here
