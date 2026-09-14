;;; unhinged-diffusion-draw.el --- Canvas draw functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2026 Bernd Wachter
;;
;;; Commentary:
;;
;; This file provides the drawing primitives we let the model have.
;;
;;; Code:

(require 'unhinged-diffusion-vars)
(require 'unhinged-diffusion-canvas)

(defun unhinged-diffusion-draw-rectangle (x y w h color &optional alpha)
  "Draw a filled rectangle with top-left at X,Y, size WxH, and COLOR.

COLOR is a hex string like \"#RRGGBB\".  Optional ALPHA is 0-255,
defaulting to 255 (fully opaque)."
  (let* ((rgba (unhinged-diffusion--parse-color color))
         (r (nth 0 rgba))
         (g (nth 1 rgba))
         (b (nth 2 rgba))
         (a (or alpha 255))
         (dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims))
         (data (unhinged-diffusion-canvas--pixel-get-data))
         (x1 (max 0 x))
         (y1 (max 0 y))
         (x2 (min width (+ x w)))
         (y2 (min height (+ y h))))
    (when (and data (> w 0) (> h 0))
      (cl-loop for py from y1 below y2 do
               (cl-loop for px from x1 below x2 do
                        (unhinged-diffusion--blend-pixel
                         data (+ px (* py width)) r g b a)))
      (unhinged-diffusion-canvas--refresh)
      (unhinged-diffusion-canvas--log-edit
       "draw_rectangle(%d,%d %dx%d, %s, α:%d)" x y w h color a))))

(defun unhinged-diffusion-draw-circle (cx cy rad color &optional alpha)
  "Draw a filled circle centered at CX,CY with radius RAD and COLOR.

COLOR is a hex string like \"#RRGGBB\".  Optional ALPHA is 0-255."
  (let* ((rgba (unhinged-diffusion--parse-color color))
         (r (nth 0 rgba))
         (g (nth 1 rgba))
         (b (nth 2 rgba))
         (a (or alpha 255))
         (dims (unhinged-diffusion-canvas-dimensions))
         (width (car dims))
         (height (cdr dims))
         (data (unhinged-diffusion-canvas--pixel-get-data))
         (x1 (max 0 (round (- cx rad))))
         (y1 (max 0 (round (- cy rad))))
         (x2 (min width (round (+ cx rad))))
         (y2 (min height (round (+ cy rad))))
         (rad-sq (* rad rad)))
    (when (and data (> rad 0))
      (cl-loop for py from y1 below y2 do
               (cl-loop for px from x1 below x2 do
                        (when (<= (+ (* (- px cx) (- px cx))
                                     (* (- py cy) (- py cy)))
                                  rad-sq)
                          (unhinged-diffusion--blend-pixel
                           data (+ px (* py width)) r g b a))))
      (unhinged-diffusion-canvas--refresh)
      (unhinged-diffusion-canvas--log-edit
       "draw_circle(%d,%d r:%d, %s, α:%d)" cx cy rad color a))))

(defun unhinged-diffusion-draw-line (x1 y1 x2 y2 color &optional alpha width)
  "Draw a line from X1,Y1 to X2,Y2 with COLOR.

Optional ALPHA is 0-255.  Optional WIDTH is line thickness in pixels."
  (let* ((rgba (unhinged-diffusion--parse-color color))
         (r (nth 0 rgba))
         (g (nth 1 rgba))
         (b (nth 2 rgba))
         (a (or alpha 255))
         (line-width (or width 1))
         (dims (unhinged-diffusion-canvas-dimensions))
         (cwidth (car dims))
         (cheight (cdr dims))
         (data (unhinged-diffusion-canvas--pixel-get-data))
         (dx (abs (- x2 x1)))
         (dy (abs (- y2 y1)))
         (sx (if (< x1 x2) 1 -1))
         (sy (if (< y1 y2) 1 -1))
         (err (- dx dy)))
    (when data
      (cl-labels ((plot (px py)
                    (let ((hw (/ (1- line-width) 2.0)))
                      (cl-loop for oy from (floor (- py hw)) to (ceiling (+ py hw)) do
                               (cl-loop for ox from (floor (- px hw)) to (ceiling (+ px hw)) do
                                        (when (and (>= ox 0) (< ox cwidth)
                                                   (>= oy 0) (< oy cheight))
                                          (unhinged-diffusion--blend-pixel
                                           data (+ ox (* oy cwidth)) r g b a)))))))
        (plot x1 y1)
        (while (and (not (= x1 x2)) (not (= y1 y2)))
          (let ((e2 (* 2 err)))
            (when (> e2 (- dy))
              (setq err (- err dy))
              (setq x1 (+ x1 sx)))
            (when (< e2 dx)
              (setq err (+ err dx))
              (setq y1 (+ y1 sy)))
            (plot x1 y1))))
      (unhinged-diffusion-canvas--refresh)
      (unhinged-diffusion-canvas--log-edit
       "draw_line(%d,%d→%d,%d, %s, α:%d, w:%d)" x1 y1 x2 y2 color a line-width))))

(defun unhinged-diffusion-draw-primitive (shape coords color &optional alpha)
  "Draw a primitive SHAPE with COORDS and COLOR.
SHAPE is one of `rectangle', `circle', `line'.
COORDS is a list or vector of numbers whose meaning depends on SHAPE:
  - rectangle: (x y width height)
  - circle:    (cx cy radius)
  - line:      (x1 y1 x2 y2)
Optional ALPHA is 0-255."
  (let ((coord-list (if (vectorp coords) (append coords nil) coords)))
    (pcase shape
      ((or "rectangle" 'rectangle)
       (apply #'unhinged-diffusion-draw-rectangle (append coord-list (list color alpha))))
      ((or "circle" 'circle)
       (apply #'unhinged-diffusion-draw-circle (append coord-list (list color alpha))))
      ((or "line" 'line)
       (apply #'unhinged-diffusion-draw-line (append coord-list (list color alpha))))
      (_ (error "Unknown shape: %s" shape)))))

(provide 'unhinged-diffusion-draw)
;;; unhinged-diffusion-draw.el ends here
