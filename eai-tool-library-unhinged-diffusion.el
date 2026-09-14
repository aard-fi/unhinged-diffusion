;;; eai-tool-library-unhinged-diffusion.el --- tool bindings for unhinged diffusion -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This package provides tool definitions for unhinged-diffusion that hook
;; into eai-tool-library, allowing vision-capable LLMs to manipulate a canvas
;; through denoising operations.
;;
;; Currently this is a bit too tightly coupled to unhinged diffusion, so
;; probably loading that without trying to use unhinged diffusion doesn't
;; make much sense.
;;
;;; Code:

(require 'unhinged-diffusion-vars)
(require 'unhinged-diffusion-draw)
(require 'unhinged-diffusion-canvas)
(require 'eai-tool-library)

(defvar eai-tool-library-unhinged-diffusion-tools '()
  "The list of unhinged diffusion related tools.")

(defvar eai-tool-library-unhinged-diffusion-tools-maybe-safe '()
  "The list of unhinged diffusion related tools which may be destructive,
but typically the LLM behaves.")

(defvar eai-tool-library-unhinged-diffusion-tools-unsafe '()
  "The list of unhinged diffusion related tools which are not safe.")

(defvar eai-tool-library-unhinged-diffusion-category-name "unhinged-diffusion"
  "The category name used for tool registration.")

;; Helper to resolve buffer argument
(defun eai-tool-library-unhinged-diffusion--resolve-buffer (buffer-name)
  "Resolve BUFFER-NAME to a live buffer object.
Returns the buffer or signals an error."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer %s not found" buffer-name))
    buf))

;; Drawing Primitives

(defun eai-tool-library-unhinged-diffusion--draw-rectangle (buffer x y width height color &optional alpha)
  "Draw a filled rectangle on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-draw-rectangle x y width height color alpha)
    "Rectangle drawn."))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--draw-rectangle
 :name "diffusion-draw-rectangle"
 :description "Draw a filled rectangle on the diffusion canvas.  Use this to establish coarse shapes, block in regions of color, or create structural elements.  Be conservative: small to medium rectangles work best for gradual refinement."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "x"
                     :type integer
                     :description "X coordinate of the top-left corner.")
             '(:name "y"
                     :type integer
                     :description "Y coordinate of the top-left corner.")
             '(:name "width"
                     :type integer
                     :description "Width of the rectangle in pixels.")
             '(:name "height"
                     :type integer
                     :description "Height of the rectangle in pixels.")
             '(:name "color"
                     :type string
                     :description "Fill color as a hex string, e.g. '#FF0000' for red.")
             '(:name "alpha"
                     :type integer
                     :description "Optional alpha transparency (0-255).  255 is fully opaque.  Use lower values for gentle blending."
                     :optional t))
 :category "unhinged-diffusion")

(defun eai-tool-library-unhinged-diffusion--draw-circle (buffer cx cy radius color &optional alpha)
  "Draw a filled circle on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-draw-circle cx cy radius color alpha)
    "Circle drawn."))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--draw-circle
 :name "diffusion-draw-circle"
 :description "Draw a filled circle on the diffusion canvas.  Good for rounded shapes, blobs, or organic forms."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "cx"
                     :type integer
                     :description "X coordinate of the circle center.")
             '(:name "cy"
                     :type integer
                     :description "Y coordinate of the circle center.")
             '(:name "radius"
                     :type integer
                     :description "Radius of the circle in pixels.")
             '(:name "color"
                     :type string
                     :description "Fill color as a hex string, e.g. '#00FF00'.")
             '(:name "alpha"
                     :type integer
                     :description "Optional alpha transparency (0-255)."
                     :optional t))
 :category "unhinged-diffusion")

(defun eai-tool-library-unhinged-diffusion--draw-line (buffer x1 y1 x2 y2 color &optional alpha width)
  "Draw a line on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-draw-line x1 y1 x2 y2 color alpha width)
    "Line drawn."))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--draw-line
 :name "diffusion-draw-line"
 :description "Draw a line on the diffusion canvas.  Useful for edges, contours, and structural lines."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "x1"
                     :type integer
                     :description "X coordinate of the start point.")
             '(:name "y1"
                     :type integer
                     :description "Y coordinate of the start point.")
             '(:name "x2"
                     :type integer
                     :description "X coordinate of the end point.")
             '(:name "y2"
                     :type integer
                     :description "Y coordinate of the end point.")
             '(:name "color"
                     :type string
                     :description "Line color as a hex string, e.g. '#0000FF'.")
             '(:name "alpha"
                     :type integer
                     :description "Optional alpha transparency (0-255)."
                     :optional t)
             '(:name "width"
                     :type integer
                     :description "Optional line width in pixels (default 1)."
                     :optional t))
 :category "unhinged-diffusion")

(defun eai-tool-library-unhinged-diffusion--draw-primitive (buffer shape coords color &optional alpha)
  "Draw a primitive shape on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-draw-primitive shape coords color alpha)
    (format "Drew %s with color %s." shape color)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--draw-primitive
 :name "diffusion-draw-primitive"
 :description "Draw a primitive shape on the diffusion canvas.  This is a unified drawing tool that dispatches to rectangle, circle, or line based on the SHAPE argument.  Use this when you want to quickly place a shape without worrying about which specific tool to call."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "shape"
                     :type string
                     :description "The shape to draw: 'rectangle', 'circle', or 'line'.")
             '(:name "coords"
                     :type array
                     :items (:type integer)
                     :description "Coordinates for the shape.  For rectangle: [x, y, width, height].  For circle: [cx, cy, radius].  For line: [x1, y1, x2, y2].")
             '(:name "color"
                     :type string
                     :description "Color as a hex string, e.g. '#RRGGBB'.")
             '(:name "alpha"
                     :type integer
                     :description "Optional alpha transparency (0-255)."
                     :optional t))
 :category "unhinged-diffusion")

;; Filters

(defun eai-tool-library-unhinged-diffusion--blur-region (buffer x1 y1 x2 y2 radius)
  "Apply a box blur to a region on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-canvas-blur-region x1 y1 x2 y2 radius)
    (format "Blurred region [%d,%d] to [%d,%d] with radius %d."
            x1 y1 x2 y2 radius)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--blur-region
 :name "diffusion-blur-region"
 :description "Apply a box blur to a rectangular region of the canvas.  Blurring smooths noise and helps coherent shapes emerge.  Use small radii (1-3) for subtle smoothing, larger radii for heavy softening.  This is the primary denoising tool."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "x1"
                     :type integer
                     :description "X coordinate of the top-left corner of the region.")
             '(:name "y1"
                     :type integer
                     :description "Y coordinate of the top-left corner of the region.")
             '(:name "x2"
                     :type integer
                     :description "X coordinate of the bottom-right corner of the region.")
             '(:name "y2"
                     :type integer
                     :description "Y coordinate of the bottom-right corner of the region.")
             '(:name "radius"
                     :type integer
                     :description "Blur radius in pixels.  Typical values: 1, 2, or 3."))
 :category "unhinged-diffusion")

(defun eai-tool-library-unhinged-diffusion--sharpen-region (buffer x1 y1 x2 y2 intensity)
  "Apply sharpening to a region on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-canvas-sharpen-region x1 y1 x2 y2 intensity)
    (format "Sharpened region [%d,%d] to [%d,%d] with intensity %.2f."
            x1 y1 x2 y2 intensity)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--sharpen-region
 :name "diffusion-sharpen-region"
 :description "Apply sharpening to a rectangular region.  Sharpening enhances edges and fine details.  Use conservatively (intensity 0.1-0.5) to avoid amplifying noise.  Best applied after some blurring has already created smooth shapes."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "x1"
                     :type integer
                     :description "X coordinate of the top-left corner of the region.")
             '(:name "y1"
                     :type integer
                     :description "Y coordinate of the top-left corner of the region.")
             '(:name "x2"
                     :type integer
                     :description "X coordinate of the bottom-right corner of the region.")
             '(:name "y2"
                     :type integer
                     :description "Y coordinate of the bottom-right corner of the region.")
             '(:name "intensity"
                     :type number
                     :description "Sharpening strength, 0.0 to 1.0.  Start with 0.2."))
 :category "unhinged-diffusion")

;; Color Operations

(defun eai-tool-library-unhinged-diffusion--blend-color (buffer x y color alpha)
  "Blend COLOR at pixel X,Y on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-canvas-blend-colour x y color alpha)
    (format "Blended color %s at [%d,%d] with alpha %d."
            color x y alpha)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--blend-color
 :name "diffusion-blend-color"
 :description "Blend a color into a single pixel on the canvas.  This is the finest-grained control available — use it for subtle color corrections, smoothing transitions, or nudging pixels toward the target palette."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "x"
                     :type integer
                     :description "X coordinate of the pixel.")
             '(:name "y"
                     :type integer
                     :description "Y coordinate of the pixel.")
             '(:name "color"
                     :type string
                     :description "Color as a hex string, e.g. '#AABBCC'.")
             '(:name "alpha"
                     :type integer
                     :description "Blend amount (0-255).  128 is 50% mix.  Use low values for subtle adjustments."))
 :category "unhinged-diffusion")

(defun eai-tool-library-unhinged-diffusion--adjust-brightness-contrast (buffer x1 y1 x2 y2 brightness contrast)
  "Adjust brightness and contrast in a region on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-canvas-adjust-brightness-contrast x1 y1 x2 y2 brightness contrast)
    (format "Adjusted brightness by %d and contrast by %.2f in region [%d,%d] to [%d,%d]."
            brightness contrast x1 y1 x2 y2)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--adjust-brightness-contrast
 :name "diffusion-adjust-brightness-contrast"
 :description "Adjust brightness and contrast in a rectangular region.  Brightness shifts all values up or down; contrast expands or compresses the range around mid-gray.  Use for tonal correction and making shapes more distinct."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "x1"
                     :type integer
                     :description "X coordinate of the top-left corner of the region.")
             '(:name "y1"
                     :type integer
                     :description "Y coordinate of the top-left corner of the region.")
             '(:name "x2"
                     :type integer
                     :description "X coordinate of the bottom-right corner of the region.")
             '(:name "y2"
                     :type integer
                     :description "Y coordinate of the bottom-right corner of the region.")
             '(:name "brightness"
                     :type integer
                     :description "Brightness adjustment, -255 to 255.  Positive brightens, negative darkens.")
             '(:name "contrast"
                     :type number
                     :description "Contrast adjustment, -1.0 to 1.0.  Positive increases contrast."))
 :category "unhinged-diffusion")

;; Noise

(defun eai-tool-library-unhinged-diffusion--add-noise (buffer x1 y1 x2 y2 amount)
  "Add random noise to a region on the diffusion canvas in BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (unhinged-diffusion-canvas-add-noise x1 y1 x2 y2 amount)
    (format "Added noise with amount %d to region [%d,%d] to [%d,%d]."
            amount x1 y1 x2 y2)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--add-noise
 :name "diffusion-add-noise"
 :description "Add random noise to a region.  This might seem counter-intuitive, but controlled noise injection can help break up banding, restore texture, or encourage the LLM to see new patterns in subsequent steps.  Use small amounts (10-30)."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "x1"
                     :type integer
                     :description "X coordinate of the top-left corner of the region.")
             '(:name "y1"
                     :type integer
                     :description "Y coordinate of the top-left corner of the region.")
             '(:name "x2"
                     :type integer
                     :description "X coordinate of the bottom-right corner of the region.")
             '(:name "y2"
                     :type integer
                     :description "Y coordinate of the bottom-right corner of the region.")
             '(:name "amount"
                     :type integer
                     :description "Noise intensity, typically 10-50.  Higher values create more visible grain."))
 :category "unhinged-diffusion")

;; Canvas Management

;; the LLM doesn't quite know how to use that, and does stupid stuff - so when
;; using the default unhinged diffusion entry point this tool will be disabled.
;; It's still here for if/when the tools will be useful without the unhinged
;; diffusion harness
(defun eai-tool-library-unhinged-diffusion--canvas-create (name width height)
  "Create a new diffusion canvas with NAME, WIDTH, and HEIGHT."
  (unhinged-diffusion-canvas-create name width height)
  (format "Created new diffusion canvas '%s' (%dx%d) filled with Gaussian noise."
          name width height))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--canvas-create
 :name "diffusion-canvas-create"
 :description "Create a new unhinged diffusion canvas buffer with the given dimensions, initialized with Gaussian noise.  Use this to start a fresh diffusion process.  The canvas will be displayed automatically."
 :args (list '(:name "name"
                     :type string
                     :description "Name for the new buffer, e.g. '*diffusion-cat*'.")
             '(:name "width"
                     :type integer
                     :description "Canvas width in pixels.  Recommended: 128, 256, or 512.")
             '(:name "height"
                     :type integer
                     :description "Canvas height in pixels.  Recommended: 128, 256, or 512."))
 :category "unhinged-diffusion")

(defun eai-tool-library-unhinged-diffusion--get-status (buffer)
  "Get the current status of a diffusion canvas BUFFER."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (let* ((dims (unhinged-diffusion-canvas-dimensions))
           (step (or unhinged-diffusion--step 0))
           (total (or unhinged-diffusion--total-steps 0))
           (prompt (or unhinged-diffusion--prompt "none")))
      (format "Canvas '%s': %dx%d pixels.  Step %d/%d.  Prompt: '%s'."
              buffer (car dims) (cdr dims) step total prompt))))

(defun eai-tool-library-unhinged-diffusion--request-extra-steps (buffer steps)
  "Request EXTRA-STEPS additional diffusion steps for BUFFER.
Call this when the image needs more refinement than originally planned."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (let ((run (gethash (buffer-name (current-buffer))
                        unhinged-diffusion--active-runs)))
      (if run
          (let ((old-total (plist-get run :total)))
            (plist-put run :total (+ old-total steps))
            (format "Granted %d extra steps.  Total steps now: %d → %d."
                    steps old-total (+ old-total steps)))
        (format "No active run in %s." buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--request-extra-steps
 :name "diffusion-request-extra-steps"
 :description "Request additional diffusion steps if the image is not converging well.  Call this when you need more time to refine details."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "steps"
                     :type integer
                     :description "Number of extra steps to add (typically 2–5)."))
 :category "unhinged-diffusion")

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--get-status
 :name "diffusion-get-status"
 :description "Get the current status of a diffusion canvas: dimensions, current step, total steps, and prompt.  Use this to orient yourself before deciding which tools to apply."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer."))
 :category "unhinged-diffusion")

;; Hand-off notes: let the model pin down emerging structures between steps

(defun eai-tool-library-unhinged-diffusion--set-notes (buffer notes)
  "Record emerging-structure NOTES for BUFFER's diffusion run.
The notes replace any recorded earlier and are quoted in later
steps' prompts, so the model keeps refining started structures
instead of starting them a second time."
  (with-current-buffer (eai-tool-library-unhinged-diffusion--resolve-buffer buffer)
    (let ((text (and (stringp notes) (> (length notes) 0) notes)))
      (setq unhinged-diffusion--notes text)
      (if text
          (unhinged-diffusion-canvas--log-edit "notes: %s" text)
        (unhinged-diffusion-canvas--log-edit "notes cleared"))
      (if text
          (format "Notes recorded.  They will be quoted in later steps: %s" text)
        "Notes cleared."))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-unhinged-diffusion-tools
 :function #'eai-tool-library-unhinged-diffusion--set-notes
 :name "diffusion-set-notes"
 :description "Record composition notes for later diffusion steps: major structures you have started and their locations, as absolute canvas pixel coordinates.  Call this whenever you begin a significant structure (subject, head, horizon, large region) so later steps keep refining it in place instead of starting a duplicate elsewhere.  Replaces all previously recorded notes, so include everything still relevant."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the diffusion canvas buffer.")
             '(:name "notes"
                     :type string
                     :description "The notes, as a short factual list.  Example: 'head outline emerging around (150,20)-(220,90); sky above y=60; grass below y=200'."))
 :category "unhinged-diffusion")

(provide 'eai-tool-library-unhinged-diffusion)
;;; eai-tool-library-unhinged-diffusion.el ends here
