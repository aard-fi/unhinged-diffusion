;;; unhinged-diffusion-vars.el --- Shared variables for unstable diffusion -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2026 Bernd Wachter
;;
;;; Commentary:
;;
;; This file contains customisation as well as shared variable definitions of
;; mostly buffer local variables.
;;
;;; Code:

(defgroup unhinged-diffusion nil
  "Unhinged diffusion settings."
  :group 'applications)

(defcustom unhinged-diffusion-default-width 256
  "Default canvas width in pixels."
  :type 'integer
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-default-height 256
  "Default canvas height in pixels."
  :type 'integer
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-default-steps 10
  "Default number of diffusion steps."
  :type 'integer
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-tool-profiles
  '((filters
     "diffusion-get-status"
     "diffusion-blur-region"
     "diffusion-sharpen-region"
     "diffusion-add-noise"
     "diffusion-adjust-brightness-contrast"
     "diffusion-blend-color"
     "diffusion-request-extra-steps")
    (full
     "diffusion-get-status"
     "diffusion-blur-region"
     "diffusion-sharpen-region"
     "diffusion-add-noise"
     "diffusion-adjust-brightness-contrast"
     "diffusion-blend-color"
     "diffusion-draw-rectangle"
     "diffusion-draw-circle"
     "diffusion-draw-line"
     "diffusion-draw-primitive"
     "diffusion-request-extra-steps"))
  "Alist of tool profiles for diffusion runs.

Each element is (PROFILE-NAME . TOOL-NAMES) where TOOL-NAMES is a list of
strings naming gptel tools to make available to the model.

Special symbol `all' means use every registered tool (except excluded ones)."
  :type '(alist :key-type symbol
                :value-type (repeat string))
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-default-profile 'full
  "Default tool profile for `unhinged-diffusion-generate'."
  :type '(choice (const :tag "Filters only (no drawing)" filters)
                 (const :tag "Full toolkit" full)
                 (symbol :tag "Custom profile"))
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-models
  '((default . nil))
  "Alist of model configurations for diffusion runs.

Each element is (NAME . BACKEND-SPEC) where BACKEND-SPEC is passed to gptel.
A nil spec means use gptel's default backend.  This allows running the same
prompt through multiple models for comparison."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-auto-show-debug-buffer nil
  "If non-nil, automatically pop to the debug buffer when a run starts."
  :type 'boolean
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-show-activity-indicator t
  "Show a pulsating activity indicator in the mode line while a run is active."
  :type 'boolean
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-activity-frames
  '("✨" "💫" "✨" "🌟" "✨" "💫")
  "Emoji frames cycled trough for activity indicators."
  :type '(repeat string)
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-activity-interval 0.4
  "Seconds between frames of activity indicators."
  :type 'number
  :group 'unhinged-diffusion)

(defface unhinged-diffusion-activity-face
  '((t :inherit mode-line-emphasis))
  "Face for the pulsating modeline activity indicator."
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-retryable-status-patterns
  '("429" "Too Many Requests"
    "502" "503" "504"
    "Service Unavailable" "Bad Gateway" "Gateway Timeout"
    "timeout" "Connection refused" "Connection reset")
  "A list of substrings to search the status of failed requests for.

If any of those is found retry the request with exponential backoff instead
of failing the current run."
  :type '(repeat string)
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-max-tool-rounds 20
  "Maximum number of tool-call rounds allowed per diffusion step.

When exceeded, further tool calls are refused and the model is told
to summarize and finish the step instead."
  :type 'integer
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-step-timeout 300
  "Seconds of callback inactivity before a step is considered stalled.

A stalled step's request is aborted and the step retried."
  :type 'integer
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-commentary-max-length 1000
  "Maximum characters of a model response inserted as step commentary.

Responses longer than this are truncated; nil disables truncation."
  :type '(choice (integer :tag "Maximum characters")
                 (const :tag "Unlimited" nil))
  :group 'unhinged-diffusion)

(defcustom unhinged-diffusion-system-prompt
  "You are a diffusion model sampler kernel operating via tool calls.
You receive a canvas image and a target description.  Your job is to slowly
steer the random noise toward the target image by applying subtle refinements.

Use the provided canvas tools to:
- Blur regions to smooth noise into coherent shapes
- Blend colors to nudge pixels toward the target palette
- Sharpen edges where shapes are emerging
- Draw primitives (rectangles, circles, lines) to establish structure
- Adjust brightness/contrast globally or regionally

Be conservative: each step should make only small changes.  The canvas starts
as pure noise and gradually converges.  Think like a real diffusion U-Net:
predict what structures should emerge, then gently remove noise while
preserving emerging forms.

Coordinate discipline: all tool coordinates are absolute pixels of the actual
canvas.  The exact canvas dimensions are stated in every step prompt — use
them as given, and never infer a different scale from the displayed image.
When establishing composition, regions should span the full canvas range
quoted in the step prompt.

Phase guidance by step:
- Early steps (1–3): focus on large-scale composition.  Place sky, ground, and
  subject blobs.  Use large regions and low alpha (10–40).
- Mid steps (4–7): refine shapes.  Smooth transitions, sharpen edges,
  correct proportions.  Medium regions, medium alpha (40–80).
- Late steps (8–10): add fine detail.  Use small regions (3–15 pixels),
  high alpha (80–255), and single-pixel blend_color calls for whiskers,
  eyes, texture, and crisp edges.

If the image is not converging well, you may call diffusion-request-extra-steps
and specify how many additional steps you need."
  "System prompt used for diffusion steps."
  :type 'string
  :group 'unhinged-diffusion)

;; buffer local variables for state keeping
(defvar-local unhinged-diffusion--canvas nil
  "The canvas image spec in the current buffer.")

(defvar-local unhinged-diffusion--step 0
  "Current diffusion step number.")

(defvar-local unhinged-diffusion--prompt nil
  "Current diffusion prompt.")

(defvar-local unhinged-diffusion--total-steps nil
  "Total number of diffusion steps.")

;; orchestration state tracking
(defvar unhinged-diffusion--active-runs (make-hash-table :test 'equal)
  "A hash table mapping buffer names to active diffusion run states.

Each value is a plist with keys :prompt :step :total :status :timer
:watchdog :fsm :epoch :retries :nudge-step :prompt-buffers :backend
:model :profile.  Status is one of running, done, error, cancelled.")

(provide 'unhinged-diffusion-vars)

;;; unhinged-diffusion-vars.el ends here
