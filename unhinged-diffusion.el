;;; unhinged-diffusion.el --- Emulate stable diffusion with canvas and LLMs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2026 Bernd Wachter
;;
;;; Commentary:
;;
;; This package emulates parts of how Stable Diffusion works by initialising
;; an Emacs canvas with Gaussian noise, and then emulating its denoising steps
;; by utilising vision-capable LLMs through tool calls.
;;
;; The core idea:
;; - Phase 1 (setup): Create a canvas, fill with noise
;; - Phase 2 (denoising loop): Send canvas snapshot to LLM with a prompt;
;;   LLM uses tools (blur, blend, sharpen, draw primitives) to refine the image
;; - Phase 3 (output): The canvas contains the final image
;;
;; This provides low-level canvas manipulation functions suitable for reuse
;; in arbitrary Lisp code, and tool definitions hooking into eai-tool-library
;; on top.
;;
;;; Code:

(require 'cl-lib)
(require 'image-converter)
(require 'unhinged-diffusion-vars)
(require 'unhinged-diffusion-helpers)
(require 'eai-tool-library-unhinged-diffusion)

(unless (fboundp 'canvas-refresh)
  (error "Unhinged Diffusion requires the Emacs canvas API.  Please use Emacs 31+ with canvas support, or Emacs 32."))

;; bunch of gptel declarations to make the byte compiler happy.
;; we're not requiring gptel here - we assume the user has gptel
;; configured and loaded. If not interesting things will happen.
(declare-function gptel-request "gptel" (prompt &rest args))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel--known-backends)
(declare-function gptel-get-backend "gptel" (name))
(declare-function gptel-tool-function "gptel" (tool))
(declare-function gptel--map-tool-args "gptel" (tool args))
(declare-function gptel-fsm-info "gptel" (fsm))
(declare-function gptel-backend-name "gptel" (backend))
(declare-function gptel-tool-name "gptel" (tool))
(defvar gptel--request-alist)
(declare-function hash-table-keys "subr-x" (table))

;; debug stuff
(defvar unhinged-diffusion--debug-buffer-name "*unhinged-diffusion-debug*"
  "Name of the debug/status buffer.")

(defun unhinged-diffusion--log (fmt &rest args)
  "Write a timestamped line to the debug buffer.

Safe to call from async callbacks — errors are demoted."
  (condition-case err
      (let ((buf (get-buffer-create unhinged-diffusion--debug-buffer-name)))
        (with-current-buffer buf
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert (format-time-string "%Y-%m-%d %H:%M:%S.%3N ")
                    (apply #'format fmt args)
                    "\n"))))
    (error (message "unhinged-diffusion--log error: %S" err))))

(defun unhinged-diffusion--debug-clear ()
  "Clear the debug buffer."
  (interactive)
  (when-let* ((buf (get-buffer unhinged-diffusion--debug-buffer-name)))
    (with-current-buffer buf
      (erase-buffer))))

;;;; Internal utilities

;;;; Canvas creation and access

(defun unhinged-diffusion--tools-for-profile (profile)
  "Return filtered gptel tool list for PROFILE symbol.

Looks up PROFILE in `unhinged-diffusion-tool-profiles'.
If PROFILE is nil or not found, returns all available tools."
  (let ((all-tools (append (bound-and-true-p eai-tool-library-unhinged-diffusion-tools)
                           (bound-and-true-p eai-tool-library-unhinged-diffusion-tools-maybe-safe)))
        (wanted (cdr (assq profile unhinged-diffusion-tool-profiles))))
    (seq-filter (lambda (tool)
                  (and (eq (type-of tool) 'gptel-tool)
                       (not (string= (gptel-tool-name tool)
                                     "diffusion-canvas-create"))
                       (or (null wanted)
                           (member (gptel-tool-name tool) wanted))))
                all-tools)))

;;;; Export

(defun unhinged-diffusion-canvas-to-ppm (buffer file)
  "Export canvas in BUFFER to FILE as a P6 (binary) PPM image.

Returns the output file path."
  (with-current-buffer buffer
    (let* ((spec (cdr unhinged-diffusion--canvas))
           (data (plist-get spec :data))
           (width (plist-get spec :data-width))
           (height (plist-get spec :data-height)))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert (format "P6\n%d %d\n255\n" width height))
        (dotimes (i (length data))
          (let* ((argb (aref data i))
                 (r (logand (ash argb -16) #xFF))
                 (g (logand (ash argb -8) #xFF))
                 (b (logand argb #xFF)))
            (insert (unibyte-string r))
            (insert (unibyte-string g))
            (insert (unibyte-string b))))))
    file))

(defun unhinged-diffusion-canvas-to-image-file (buffer &optional format)
  "Export canvas in BUFFER to a temporary image file.

Optional FORMAT is a file extension like \"png\" or \"ppm\".
Returns a cons (FILE-PATH . ACTUAL-FORMAT)."
  (let* ((fmt (or format "png"))
         (tmp-ppm (make-temp-file "unhinged-diffusion-" nil ".ppm"))
         (tmp-img (make-temp-file "unhinged-diffusion-" nil (concat "." fmt))))
    (unhinged-diffusion-canvas-to-ppm buffer tmp-ppm)
    (if (or (string= fmt "ppm") (string= fmt "pgm"))
        (progn
          (rename-file tmp-ppm tmp-img t)
          (cons tmp-img fmt))
      (let* ((ppm-data (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally tmp-ppm)
                         (buffer-string)))
             (png-data (image-convert ppm-data 'image/ppm)))
        (with-temp-file tmp-img
          (set-buffer-multibyte nil)
          (insert png-data))
        (delete-file tmp-ppm)
        (cons tmp-img "png")))))

;;;; Modeline activity indicator, highly experimental code
;;
;; Problem is that steps take so long, so we'd like to have some indicator
;; things are not dead yet - and would like to prevent it from scrolling out
;; of the visible screen.

(defvar unhinged-diffusion--activity-timer nil
  "Timer driving the pulsating modeline activity indicator.")

(defvar unhinged-diffusion--activity-frame 0
  "Current frame index of the pulsating activity indicator.")

(defvar unhinged-diffusion--activity-modeline
  '(:eval (unhinged-diffusion--activity-string))
  "Mode line construct showing the diffusion activity indicator.

Installed in `mode-line-front-space' while runs are active, so it
renders before the buffer name.")

(defun unhinged-diffusion--activity-string ()
  "Return the mode line activity indicator for the current buffer.

Only buffers with a running diffusion run display anything."
  (when unhinged-diffusion-show-activity-indicator
    (let ((run (gethash (buffer-name (current-buffer))
                        unhinged-diffusion--active-runs)))
      (when (and run (eq (plist-get run :status) 'running))
        (let* ((frames unhinged-diffusion-activity-frames)
               (frame (nth (mod unhinged-diffusion--activity-frame
                                (max 1 (length frames)))
                           frames)))
          (propertize
           (format " %s %d/%d" frame
                   unhinged-diffusion--step
                   (or unhinged-diffusion--total-steps 0))
           'face 'unhinged-diffusion-activity-face
           'help-echo "Unhinged diffusion: running"))))))

(defun unhinged-diffusion--activity-tick ()
  "Advance the activity indicator and refresh mode lines."
  (setq unhinged-diffusion--activity-frame
        (mod (1+ unhinged-diffusion--activity-frame)
             (max 1 (length unhinged-diffusion-activity-frames))))
  (force-mode-line-update t))

(defun unhinged-diffusion--activity-start ()
  "Install the indicator in `mode-line-front-space' and start the pulse."
  (when unhinged-diffusion-show-activity-indicator
    (cl-pushnew 'unhinged-diffusion--activity-modeline mode-line-front-space)
    (unless unhinged-diffusion--activity-timer
      (setq unhinged-diffusion--activity-timer
            (run-with-timer 0 unhinged-diffusion-activity-interval
                            #'unhinged-diffusion--activity-tick)))))

(defun unhinged-diffusion--activity-stop ()
  "Stop the pulse if no diffusion run is active anymore."
  (let ((running nil))
    (maphash (lambda (_ run)
               (when (eq (plist-get run :status) 'running)
                 (setq running t)))
             unhinged-diffusion--active-runs)
    (when (and unhinged-diffusion--activity-timer (not running))
      (cancel-timer unhinged-diffusion--activity-timer)
      (setq unhinged-diffusion--activity-timer nil)
      (setq mode-line-front-space
            (delq 'unhinged-diffusion--activity-modeline
                  mode-line-front-space))
      ;; One last refresh so the indicator disappears promptly.
      (force-mode-line-update t))))

;;;; Diffusion orchestration
;;
;; This is the magic that kicks the LLM into a specific number of rounds,
;; and tries to keep it on track. It also takes care of letting the user
;; see the progress we're making.

(defun unhinged-diffusion--insert-step-commentary (buffer step total text)
  "Insert the model's final TEXT as commentary under the snapshot entry
for STEP/TOTAL in BUFFER's Step History.

Lines are indented so org cannot misinterpret them as headings."
  (when (and (buffer-live-p buffer)
             (stringp text)
             (> (length (replace-regexp-in-string "[ \t\n\r]" "" text)) 0))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (cleaned (replace-regexp-in-string
                      "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" ""
                      (if (and unhinged-diffusion-commentary-max-length
                               (> (length text)
                                  unhinged-diffusion-commentary-max-length))
                          (concat (substring
                                   text 0 unhinged-diffusion-commentary-max-length)
                                  " …")
                        text))))
        (save-excursion
          (goto-char (point-min))
          (when (unhinged-diffusion--goto-entry
                 unhinged-diffusion--prop-step-snapshot
                 (format "%d/%d" step total))
            (goto-char (org-entry-end-position))
            (insert (mapconcat
                     (lambda (line)
                       (if (string-match-p "\\`[ \t]*\\'" line)
                           ""
                         (concat "  " line)))
                     (split-string cleaned "\n")
                     "\n"))
            (insert "\n")))))))

(defun unhinged-diffusion--mark-run-finished (buffer status)
  "Mark the run in BUFFER as finished with STATUS."
  (let ((run (gethash (buffer-name buffer) unhinged-diffusion--active-runs)))
    (when run
      (plist-put run :status status)
      (when-let* ((timer (plist-get run :timer)))
        (cancel-timer timer)
        (plist-put run :timer nil))
      (when-let* ((watchdog (plist-get run :watchdog)))
        (cancel-timer watchdog)
        (plist-put run :watchdog nil))
      (when-let* ((prompt-bufs (plist-get run :prompt-buffers)))
        (dolist (pb prompt-bufs)
          (when (buffer-live-p pb)
            (kill-buffer pb)))
        (plist-put run :prompt-buffers nil)))
    ;; Stop the pulse once no run is active anymore.
    (unhinged-diffusion--activity-stop)))

(defun unhinged-diffusion--set-run-timer (run timer)
  "Install TIMER as RUN's pending timer, cancelling any previous one.

Prevents overlapping chains from two live timers on the same run."
  (when-let* ((old (plist-get run :timer)))
    (cancel-timer old))
  (plist-put run :timer timer))

(defun unhinged-diffusion--arm-watchdog (run buffer prompt step total)
  "Rearm RUN's inactivity watchdog for BUFFER's step STEP of TOTAL.

Called on every callback event, so any sign of life postpones it."
  (when-let* ((old (plist-get run :watchdog)))
    (cancel-timer old))
  (plist-put run :watchdog
             (run-with-timer unhinged-diffusion-step-timeout nil
                             #'unhinged-diffusion--watchdog-fire
                             buffer prompt step total)))

(defun unhinged-diffusion--watchdog-fire (buffer prompt step total)
  "Abort a stalled step request and schedule a retry of the step."
  (let ((run (and (buffer-live-p buffer)
                  (gethash (buffer-name buffer) unhinged-diffusion--active-runs))))
    (when (and run (eq (plist-get run :status) 'running))
      (plist-put run :watchdog nil)
      (unhinged-diffusion--log
       "Watchdog: step %d/%d stalled for >%ds, aborting and retrying"
       step total unhinged-diffusion-step-timeout)
      (message "Unhinged diffusion step %d/%d stalled; aborting and retrying..."
               step total)
      ;; Kill the stalled request without signalling our own callback
      ;; (it belongs to the superseded epoch and must not fire).
      (unhinged-diffusion--abort-gptel-request (plist-get run :fsm) 'quiet)
      (plist-put run :epoch (1+ (or (plist-get run :epoch) 0)))
      (unhinged-diffusion--set-run-timer
       run
       (run-at-time "2 sec" nil
                    #'unhinged-diffusion--execute-step
                    buffer prompt step total)))))

;; TODO, this is starting to be a bit long, probably we should split it up soon
(defun unhinged-diffusion--make-step-callback (buffer prompt step total epoch)
  "Return a callback function for diffusion step STEP of TOTAL on BUFFER.

EPOCH is the request generation this callback belongs to; callbacks
from superseded requests (after a retry re-fired the step) are
ignored.  Handles tool calls, tool results, and final responses to
chain steps automatically.  A final response with no tool calls in
the whole request triggers one nudge retry of the same step."
  (let ((tool-rounds 0))
    (lambda (response info)
      ;; Ignore stale callbacks from cancelled/finished runs, or from
      ;; requests superseded by a retry (epoch mismatch).
      (let ((run (and (buffer-live-p buffer)
                      (gethash (buffer-name buffer) unhinged-diffusion--active-runs))))
        (when (and run (eq (plist-get run :status) 'running)
                   (eq (plist-get run :epoch) epoch))
          ;; Any callback event proves the request is alive; postpone the
          ;; inactivity watchdog.
          (unhinged-diffusion--arm-watchdog run buffer prompt step total)
          (let ((resp-type (cond ((eq response 'abort) 'abort)
                                 ((null response) 'nil)
                                 ((stringp response) 'string)
                                 ((and (consp response) (eq (car response) 'tool-call)) 'tool-call)
                                 ((and (consp response) (eq (car response) 'tool-result)) 'tool-result)
                                 ((and (consp response) (eq (car response) 'reasoning)) 'reasoning)
                                 (t (type-of response)))))
            (unhinged-diffusion--log
             "=== CALLBACK %s step %d/%d ===" (buffer-name buffer) step total)
            (unhinged-diffusion--log "response type: %S" resp-type)
            (unhinged-diffusion--log "info status: %S" (plist-get info :status))
            (when (stringp response)
              (unhinged-diffusion--log "response text (%d chars):\n%s"
                                       (length response) response))
            (when (and (consp response) (eq (car response) 'tool-call))
              (unhinged-diffusion--log "tool-calls count: %d" (length (cdr response)))
              (dolist (tc (cdr response))
                (unhinged-diffusion--log "  tool: %S, args: %S"
                                         (gptel-tool-name (nth 0 tc)) (nth 1 tc))))
            (when (and (consp response) (eq (car response) 'tool-result))
              (unhinged-diffusion--log "tool-results count: %d" (length (cdr response)))))
          (cond
           ;; Tool calls pending confirmation — execute them ourselves.
           ((and (consp response) (eq (car response) 'tool-call))
            (setq tool-rounds (1+ tool-rounds))
            (if (> tool-rounds unhinged-diffusion-max-tool-rounds)
                ;; Budget exhausted: refuse the calls so the model wraps
                ;; up with a text response instead of looping forever.
                (progn
                  (message "Unhinged diffusion step %d/%d: tool budget exhausted (%d rounds), refusing further tools"
                           step total tool-rounds)
                  (dolist (tool-call (cdr response))
                    (funcall (nth 2 tool-call)
                             (format "Tool budget for this step is exhausted (%d rounds).  Do not call any more tools; reply with a short text summary of the current canvas state instead."
                                     unhinged-diffusion-max-tool-rounds))))
              (message "Unhinged diffusion step %d/%d: executing %d tool call(s) (round %d)..."
                       step total (length (cdr response)) tool-rounds)
              (dolist (tool-call (cdr response))
                (let* ((tool-spec (nth 0 tool-call))
                       (args (nth 1 tool-call))
                       (cb (nth 2 tool-call))
                       (arg-values (if (fboundp 'gptel--map-tool-args)
                                       (gptel--map-tool-args tool-spec args)
                                     (error "gptel--map-tool-args not available")))
                       (result (condition-case err
                                   (apply (gptel-tool-function tool-spec) arg-values)
                                 (error (format "Tool error: %S" err)))))
                  (funcall cb result)))))
           ;; After executing tools, gptel will continue and call this callback again.

           ;; Tool results — tools executed, waiting for LLM's follow-up response.
           ((and (consp response) (eq (car response) 'tool-result))
            (message "Unhinged diffusion step %d/%d: tools executed, awaiting LLM response..."
                     step total))

           ;; Reasoning text — model is thinking.  Log it and continue waiting.
           ((and (consp response) (eq (car response) 'reasoning))
            (let ((reasoning-text (cdr response)))
              (unhinged-diffusion--log "Reasoning (%d chars):\n%s"
                                       (length reasoning-text) reasoning-text)
              (message "Unhinged diffusion step %d/%d: model reasoning..." step total)))

           ;; Final string response — step complete.
           ((stringp response)
            (message "Unhinged diffusion step %d/%d complete." step total)
            (with-current-buffer buffer
              (setq unhinged-diffusion--step step))
            ;; File the model's narration under this step's picture.
            (unhinged-diffusion--insert-step-commentary buffer step total response)
            ;; Step succeeded: reset the 429 backoff counter.
            (plist-put run :retries 0)
            (if (and (zerop tool-rounds)
                     (not (eq (plist-get run :nudge-step) step)))
                ;; The model narrated its intentions but never called a
                ;; tool, so the canvas didn't change.  Give it exactly one
                ;; chance to act on its own description.
                (progn
                  (plist-put run :nudge-step step)
                  (plist-put run :epoch (1+ epoch))
                  (message "Unhinged diffusion step %d/%d: model narrated without tools, retrying with nudge..."
                           step total)
                  (unhinged-diffusion--set-run-timer
                   run
                   (run-at-time "0.5 sec" nil
                                #'unhinged-diffusion--execute-step
                                buffer prompt step total)))
              (if (< step total)
                  ;; Schedule next step after a short delay.
                  (unhinged-diffusion--set-run-timer
                   run
                   (run-at-time "0.5 sec" nil
                                #'unhinged-diffusion--execute-step
                                buffer prompt (1+ step) total))
                ;; Done
                (message "Unhinged diffusion complete! Buffer: %s" (buffer-name buffer))
                (unhinged-diffusion--mark-run-finished buffer 'done))))

           ;; Request aborted.
           ((eq response 'abort)
            (message "Unhinged diffusion step %d/%d aborted." step total)
            (unhinged-diffusion--mark-run-finished buffer 'cancelled))

           ;; Error (nil response).
           ((null response)
            (let ((status (or (plist-get info :status) "unknown error")))
              (if (cl-some (lambda (pattern) (string-match-p pattern status))
                           unhinged-diffusion-retryable-status-patterns)
                  (progn
                    ;; Rate-limited: exponential backoff and retry same step.
                    (let* ((retries (or (plist-get run :retries) 0))
                           (delay (min (* (expt 2 retries) 2) 60))) ; max 60s
                      ;; Bump the epoch before scheduling the retry, so any
                      ;; late callbacks from the failed request (e.g. the
                      ;; backend completing it after all) can't schedule a
                      ;; duplicate chain alongside the retry.
                      (plist-put run :epoch (1+ epoch))
                      (plist-put run :retries (1+ retries))
                      ;; Kill the superseded request if gptel still tracks it.
                      (unhinged-diffusion--abort-gptel-request (plist-get run :fsm))
                      (message "Unhinged diffusion step %d/%d hit retryable error (%s). Retrying in %d seconds (attempt %d)..."
                               step total status delay (1+ retries))
                      (unhinged-diffusion--log "Retryable error: %s. Retry in %ds (attempt %d)" status delay (1+ retries))
                      (unhinged-diffusion--set-run-timer
                       run
                       (run-at-time (format "%d sec" delay) nil
                                    #'unhinged-diffusion--execute-step
                                    buffer prompt step total))))
                ;; Real error — mark run as failed.
                (message "Unhinged diffusion step %d/%d failed: %s"
                         step total status)
                (unhinged-diffusion--mark-run-finished buffer 'error))))

           ;; Catch-all for anything unexpected.
           (t
            (unhinged-diffusion--log
             "UNEXPECTED response type %S in callback for %s: %S"
             (type-of response) (buffer-name buffer) response)
            (message "Unhinged diffusion step %d/%d: unexpected response %S"
                     step total response))))))))

(defun unhinged-diffusion--execute-step (buffer prompt step total)
  "Execute a single diffusion STEP on BUFFER.

This is the synchronous-ish entry point that fires the gptel request."
  (let* ((run (gethash (buffer-name buffer) unhinged-diffusion--active-runs)))
    ;; Skip if run was cancelled
    (when (and run (eq (plist-get run :status) 'running))
      (message "Unhinged diffusion step %d/%d..." step total)
      (let* ((img-result (unhinged-diffusion-canvas-to-image-file buffer))
             (img-file (car img-result))
             (phase (cond ((<= step (max 1 (floor (* total 0.3)))) "composition")
                          ((<= step (max 1 (floor (* total 0.7)))) "refinement")
                          (t "detail")))
             (canvas-size (with-current-buffer buffer
                            (unhinged-diffusion-canvas-dimensions)))
             (context-prompt
              (concat
               (format "Target: '%s'. Current step: %d/%d. Phase: %s. The canvas buffer is named '%s'. Use your denoiser tools to slowly steer the random noise into the target image. Make small, conservative changes." prompt step total phase (buffer-name buffer))
               ;; The model only sees a rendered image and tends to invent
               ;; a coordinate scale for it, compressing the composition
               ;; into a corner.  State the real geometry explicitly.
               (format " The canvas is exactly %dx%d pixels.  Tool coordinates are absolute canvas pixels: (0,0) is the top-left corner and (%d,%d) is the bottom-right.  The image you are shown IS this full canvas at 1:1 scale — a region covering the whole image is (0,0) to (%d,%d).  Never invent a smaller or larger coordinate range."
                       (car canvas-size) (cdr canvas-size)
                       (1- (car canvas-size)) (1- (cdr canvas-size))
                       (1- (car canvas-size)) (1- (cdr canvas-size)))
               ;; Nudged step: the model previously narrated without
               ;; calling any tool.  Force it to act this time.
               (when (eq (plist-get run :nudge-step) step)
                 "\n\nIMPORTANT: Your previous reply only described what you intended to do, without calling any tools. Apply those changes NOW by calling the canvas tools. You must issue at least one tool call; do not just describe.")))
             (profile (plist-get run :profile))
             (tools (unhinged-diffusion--tools-for-profile profile))
             ;; New request generation: invalidate callbacks from any
             ;; superseded request still floating around.
             (epoch (1+ (or (plist-get run :epoch) 0))))
        (plist-put run :epoch epoch)
        (with-current-buffer buffer
          (setq unhinged-diffusion--step step)
          (setq unhinged-diffusion--prompt prompt)
          (setq unhinged-diffusion--total-steps total)
          ;; Append step thumbnail to Step History as a subsection.
          ;; Tagged with the step id, so retries don't insert duplicates.
          (let ((inhibit-read-only t)
                (snapshot-id (format "%d/%d" step total)))
            (save-excursion
              (goto-char (point-min))
              (if (unhinged-diffusion--goto-entry
                   unhinged-diffusion--prop-step-snapshot snapshot-id)
                  (message "Step %d/%d: snapshot already in Step History, skipping"
                           step total)
                ;; Append at end of Step History section (before * Edit Log)
                (when (unhinged-diffusion--goto-section "step-history")
                  (insert "\n")
                  (unhinged-diffusion--insert-entry
                   2 (format "Step %d/%d" step total)
                   unhinged-diffusion--prop-step-snapshot snapshot-id)
                  (let ((beg (point)))
                    (insert (format "[[file:%s]]\n" img-file))
                    (message "Step %d/%d: inserted [[file:%s]] into Step History"
                             step total img-file)
                    ;; Render the just-inserted link inline; org only
                    ;; displays file links as images when this has run.
                    (when (fboundp 'org-display-inline-images)
                      (org-display-inline-images nil t beg (point)))))))))
        (when (fboundp 'gptel-request)
          (let ((prompt-buf (generate-new-buffer " *unhinged-diffusion-prompt*"))
                (run-backend (plist-get run :backend))
                (run-model (plist-get run :model)))
            (with-current-buffer prompt-buf
              (org-mode)
              (setq-local gptel-use-tools t)
              (when tools
                (setq-local gptel-tools tools))
              ;; Persist backend/model across timer callbacks
              (when run-backend
                (setq-local gptel-backend run-backend))
              (when run-model
                (setq-local gptel-model run-model))
              (insert (format "[[file:%s]]\n\n" img-file))
              (insert context-prompt)
              (unhinged-diffusion--log
               "=== REQUEST %s step %d/%d ===" (buffer-name buffer) step total)
              (unhinged-diffusion--log "img-file: %s" img-file)
              (unhinged-diffusion--log "backend: %S, model: %S"
                                       (when gptel-backend (gptel-backend-name gptel-backend))
                                       gptel-model)
              (unhinged-diffusion--log "tools: %d" (length tools))
              (unhinged-diffusion--log "prompt-buf contents:\n%s"
                                       (buffer-substring-no-properties (point-min) (point-max)))
              (unhinged-diffusion--log "img-file size: %d bytes"
                                       (or (file-attribute-size (file-attributes img-file)) -1))
              (when run
                (plist-put run :prompt-buffers
                           (cons prompt-buf (plist-get run :prompt-buffers))))
              (let ((fsm (gptel-request
                             nil
                           :buffer prompt-buf
                           :system unhinged-diffusion-system-prompt
                           :callback (unhinged-diffusion--make-step-callback
                                      buffer prompt step total epoch))))
                (when run (plist-put run :fsm fsm))
                ;; Cover the silent window before the first callback.
                (unhinged-diffusion--arm-watchdog run buffer prompt step total)
                (unhinged-diffusion--log
                 "gptel-request returned FSM: %S" fsm)
                (message "Unhinged diffusion step %d/%d: waiting for LLM response..." step total)))))))))

(defun unhinged-diffusion-step (buffer prompt step total-steps &optional profile)
  "Take a single diffusion STEP on BUFFER.

Prompt the LLM with PROMPT and step info STEP/TOTAL-STEPS.

Optional PROFILE selects a tool profile from `unhinged-diffusion-tool-profiles'.

The LLM will use available tools to refine the canvas. Returns the gptel request
object (async).

This is a low-level building block.  For fully automated runs, use
`unhinged-diffusion-generate' instead."
  (let* ((img-file (car (unhinged-diffusion-canvas-to-image-file buffer)))
         (context-prompt
          (format "Target: '%s'. Current step: %d/%d. The canvas buffer is named '%s'. Use your denoiser tools to slowly steer the random noise into the target image. Make small, conservative changes." prompt step total-steps (buffer-name buffer)))
         (tools (unhinged-diffusion--tools-for-profile profile)))
    (with-current-buffer buffer
      (setq unhinged-diffusion--step step)
      (setq unhinged-diffusion--prompt prompt)
      (setq unhinged-diffusion--total-steps total-steps))
    ;; Return gptel request - caller can customize backend via let-binding
    (when (fboundp 'gptel-request)
      (let ((prompt-buf (generate-new-buffer " *unhinged-diffusion-prompt*")))
        (with-current-buffer prompt-buf
          (org-mode)
          (setq-local gptel-use-tools t)
          (when tools
            (setq-local gptel-tools tools))
          (insert (format "[[file:%s]]\n\n" img-file))
          (insert context-prompt)
          (gptel-request
              nil
            :system unhinged-diffusion-system-prompt))))))

(defun unhinged-diffusion-run (buffer prompt &optional steps profile)
  "Run full diffusion process on BUFFER with PROMPT for STEPS steps.

If STEPS is nil, use `unhinged-diffusion-default-steps'.

Optional PROFILE selects a tool profile from `unhinged-diffusion-tool-profiles'.

Runs via async gptel callbacks. This is the low-level orchestration entry point."
  (when unhinged-diffusion-auto-show-debug-buffer
    (unhinged-diffusion-debug))
  (let ((total (or steps unhinged-diffusion-default-steps)))
    (with-current-buffer buffer
      (setq unhinged-diffusion--prompt prompt)
      (setq unhinged-diffusion--total-steps total)
      (setq unhinged-diffusion--step 0))
    (puthash (buffer-name buffer)
             `(:prompt ,prompt :step 0 :total ,total :status running
                       :backend ,gptel-backend :model ,gptel-model
                       :prompt-buffers nil :profile ,profile :epoch 0 :retries 0)
             unhinged-diffusion--active-runs)
    (unhinged-diffusion--activity-start)
    (unhinged-diffusion--execute-step buffer prompt 1 total)))

(defun unhinged-diffusion--abort-gptel-request (fsm &optional quiet)
  "Abort the gptel request associated with FSM.

Unless QUIET is non-nil, signal abort to our callback so run state
cleans up.  Returns t if a request was found and aborted."
  (when (and fsm (boundp 'gptel--request-alist) gptel--request-alist)
    (let ((entry (cl-find-if (lambda (e) (eq (cadr e) fsm))
                             gptel--request-alist)))
      (when entry
        (let* ((info (condition-case nil (gptel-fsm-info fsm) (error nil)))
               (abort-fn (cddr entry))
               (cb (when info (plist-get info :callback))))
          ;; Signal abort to callback so our state cleans up
          (unless quiet
            (when (functionp cb)
              (condition-case err
                  (funcall cb 'abort info)
                (error (message "Diffusion abort callback error: %S" err)))))
          ;; Kill the underlying process / connection
          (when (functionp abort-fn)
            (funcall abort-fn)))
        t))))

(defun unhinged-diffusion-cancel (buffer)
  "Cancel any diffusion run in BUFFER and abort its gptel request."
  (interactive (list (current-buffer)))
  (let ((run (gethash (buffer-name buffer) unhinged-diffusion--active-runs)))
    (if run
        (progn
          (unhinged-diffusion--mark-run-finished buffer 'cancelled)
          ;; Abort underlying gptel request if any
          (when-let* ((fsm (plist-get run :fsm)))
            (unhinged-diffusion--abort-gptel-request fsm))
          ;; Remove from tracking if buffer is dead
          (unless (buffer-live-p buffer)
            (remhash (buffer-name buffer) unhinged-diffusion--active-runs))
          (message "Unhinged diffusion cancelled in %s" (or (buffer-name buffer) "dead buffer")))
      (message "No diffusion run in %s" (or (buffer-name buffer) "dead buffer")))))

(defun unhinged-diffusion-cancel-all ()
  "Cancel every tracked diffusion run and abort all pending gptel requests.

Also cleans up dead entries and kills stray prompt buffers."
  (interactive)
  (let ((cancelled 0)
        (bufs-to-cancel '()))
    ;; Collect live buffers from tracked runs and purge dead entries
    (maphash (lambda (buf-name _run)
               (let ((buf (get-buffer buf-name)))
                 (when (and buf (buffer-live-p buf))
                   (push buf bufs-to-cancel))
                 ;; Remove dead entries
                 (unless (and buf (buffer-live-p buf))
                   (remhash buf-name unhinged-diffusion--active-runs))))
             unhinged-diffusion--active-runs)
    ;; Cancel each tracked run
    (dolist (buf bufs-to-cancel)
      (unhinged-diffusion-cancel buf)
      (setq cancelled (1+ cancelled)))
    ;; Also abort any gptel requests that use our system prompt
    (when (and (boundp 'gptel--request-alist) gptel--request-alist)
      (dolist (entry gptel--request-alist)
        (let* ((fsm (cadr entry))
               (info (when fsm (condition-case nil (gptel-fsm-info fsm) (error nil))))
               (sys (when info (plist-get info :system))))
          (when (and sys (string= sys unhinged-diffusion-system-prompt))
            (unhinged-diffusion--abort-gptel-request fsm)
            (setq cancelled (1+ cancelled))))))
    (message "Cancelled %d diffusion run(s)" cancelled)))

(defun unhinged-diffusion-generate (prompt &optional steps width height model profile)
  "Generate an image from PROMPT using the full diffusion pipeline.

Creates a fresh canvas, runs STEPS denoising steps, and returns the buffer.

Optional WIDTH and HEIGHT default to `unhinged-diffusion-default-width/height'.

Optional MODEL specifies which backend/model to use.  It can be:
- nil: use the current `gptel-backend' and `gptel-model'
- A string naming a backend (e.g. ollama-mac): use that backend with the
  current `gptel-model'
- A cons cell (BACKEND-NAME . MODEL-SYMBOL): use both

Optional PROFILE is a symbol from `unhinged-diffusion-tool-profiles'.

This is the main high-level entry point for one-shot image generation.

Example usage:
  (unhinged-diffusion-generate \"a cat\" 10 256 256)
  (unhinged-diffusion-generate \"a cat\" 10 256 256 \"ollama-mac\")
  (unhinged-diffusion-generate \"a cat\" 10 256 256
    (cons \"ollama-mac\" (quote glm-5.3-flash:cloud)))
  (unhinged-diffusion-generate \"a cat\" 10 256 256 nil \='filters)
  "
  (interactive
   (list (read-string "Diffusion prompt: ")
         (read-number "Steps: " unhinged-diffusion-default-steps)
         (read-number "Width: " unhinged-diffusion-default-width)
         (read-number "Height: " unhinged-diffusion-default-height)
         (let ((models (mapcar #'car unhinged-diffusion-models)))
           (when models
             (completing-read
              "Model (empty for default): "
              (append models (hash-table-keys gptel--known-backends))
              nil t)))
         (intern (completing-read
                  "Tool profile: "
                  (mapcar #'symbol-name
                          (mapcar #'car unhinged-diffusion-tool-profiles))
                  nil t
                  (symbol-name unhinged-diffusion-default-profile)))))
  (let* ((w (or width unhinged-diffusion-default-width))
         (h (or height unhinged-diffusion-default-height))
         (buf-name (format "*unhinged-diffusion-%s*"
                           (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" prompt)))
         (prof (or profile unhinged-diffusion-default-profile))
         ;; Resolve model argument into backend and model symbol
         (backend-override (cond
                            ((null model) nil)
                            ((consp model)
                             (cons (gptel-get-backend (car model)) (cdr model)))
                            ((stringp model)
                             (cons (gptel-get-backend model) gptel-model))
                            (t nil))))
    ;; Clean up any existing run in a buffer with the same name
    (when-let* ((existing (get-buffer buf-name)))
      (unhinged-diffusion-cancel existing)
      (kill-buffer existing))
    (let ((buf (unhinged-diffusion-canvas-create buf-name w h)))
      (pop-to-buffer buf)
      (if backend-override
          (let ((gptel-backend (car backend-override))
                (gptel-model (cdr backend-override)))
            (ignore gptel-backend gptel-model)
            (unhinged-diffusion-run buf prompt steps prof))
        (unhinged-diffusion-run buf prompt steps prof))
      buf)))

(defun unhinged-diffusion-generate-with-models (prompt models &rest kwargs)
  "Generate images from PROMPT through multiple MODELS in parallel.

MODELS is a list of symbols or cons cells (NAME . BACKEND) from
`unhinged-diffusion-models' alist.

KWARGS can include :steps, :width, :height keys.
Returns an alist of (MODEL . BUFFER)."
  (let* ((steps (or (plist-get kwargs :steps) unhinged-diffusion-default-steps))
         (width (or (plist-get kwargs :width) unhinged-diffusion-default-width))
         (height (or (plist-get kwargs :height) unhinged-diffusion-default-height))
         (result-buffers '()))
    (dolist (model-spec models)
      (let* ((model-name (if (consp model-spec) (car model-spec) model-spec))
             (backend (if (consp model-spec) (cdr model-spec)
                        (cdr (assq model-spec unhinged-diffusion-models))))
             (buf-name (format "*unhinged-diffusion-%s-%s*"
                               model-name
                               (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" prompt)))
             (buf (unhinged-diffusion-canvas-create buf-name width height)))
        (with-current-buffer buf
          (setq unhinged-diffusion--prompt prompt)
          (setq unhinged-diffusion--total-steps steps)
          (setq unhinged-diffusion--step 0))
        (pop-to-buffer buf)
        (let ((gptel-backend (or backend gptel-backend)))
          (unhinged-diffusion-run buf prompt steps))
        (push (cons model-name buf) result-buffers)))
    (nreverse result-buffers)))

(defun unhinged-diffusion-run-with-models (buffer prompt models &optional steps)
  "Run diffusion on BUFFER with PROMPT through multiple MODELS.

MODELS is a list of symbols from `unhinged-diffusion-models'. Each model gets
its own canvas copy and runs independently. Returns an alist of
(MODEL . RESULT-BUFFER).

Note: this copies the current canvas state of BUFFER.  For fresh generation
from prompt, use `unhinged-diffusion-generate-with-models' instead."
  (let ((total (or steps unhinged-diffusion-default-steps))
        (result-buffers '()))
    (dolist (model-spec models)
      (let* ((model-name (if (consp model-spec) (car model-spec) model-spec))
             (backend (if (consp model-spec)
                          (cdr model-spec)
                        (cdr (assq model-spec unhinged-diffusion-models))))
             (result-buf (generate-new-buffer
                          (format "*unhinged-diffusion-%s-%s*"
                                  model-name (file-name-nondirectory (buffer-name buffer))))))
        ;; Copy current canvas state to result buffer
        (with-current-buffer result-buf
          (erase-buffer)
          (let* ((orig-spec (with-current-buffer buffer
                              (cdr unhinged-diffusion--canvas)))
                 (width (plist-get orig-spec :data-width))
                 (height (plist-get orig-spec :data-height))
                 (orig-data (plist-get orig-spec :data))
                 (new-data (make-vector (* width height) #xFF808080)))
            (dotimes (i (length orig-data))
              (aset new-data i (aref orig-data i)))
            (setq unhinged-diffusion--canvas
                  `(image :type canvas
                          :id ,(intern (format "unhinged-diffusion-%s" model-name))
                          :data-width ,width
                          :data-height ,height
                          :data ,new-data))
            (setq unhinged-diffusion--step 0)
            (setq unhinged-diffusion--prompt prompt)
            (setq unhinged-diffusion--total-steps total)
            (insert (propertize " " 'display unhinged-diffusion--canvas))))
        ;; Run diffusion with model-specific backend
        (let ((gptel-backend (or backend gptel-backend)))
          (unhinged-diffusion-run result-buf prompt total))
        (push (cons model-name result-buf) result-buffers)))
    (nreverse result-buffers)))

;;;; Convenience commands

(defun unhinged-diffusion-new (name width height)
  "Create a new unhinged diffusion canvas.

NAME is the buffer name, WIDTH and HEIGHT are canvas dimensions."
  (interactive
   (list (read-string "Buffer name: " "*unhinged-diffusion*")
         (read-number "Width: " unhinged-diffusion-default-width)
         (read-number "Height: " unhinged-diffusion-default-height)))
  (let ((buf (unhinged-diffusion-canvas-create name width height)))
    (pop-to-buffer buf)
    (message "Created unhinged diffusion canvas %dx%d in %s" width height name)
    buf))

(defun unhinged-diffusion-status ()
  "Show the status of the current buffer's diffusion run."
  (interactive)
  (let* ((run (gethash (buffer-name (current-buffer)) unhinged-diffusion--active-runs))
         (step (or unhinged-diffusion--step 0))
         (total (or unhinged-diffusion--total-steps 0))
         (prompt (or unhinged-diffusion--prompt "none"))
         (status (if run (plist-get run :status) 'idle)))
    (message "Unhinged diffusion in %s: step %d/%d, status %s, prompt: '%s'"
             (buffer-name (current-buffer)) step total status prompt)))

(defun unhinged-diffusion-active-runs ()
  "List all active diffusion runs."
  (interactive)
  (let ((runs '()))
    (maphash (lambda (buf-name run)
               (when (eq (plist-get run :status) 'running)
                 (push (format "%s: step %d/%d — '%s'"
                               buf-name
                               (plist-get run :step)
                               (plist-get run :total)
                               (plist-get run :prompt))
                       runs)))
             unhinged-diffusion--active-runs)
    (if runs
        (message "Active runs:\n%s" (string-join runs "\n"))
      (message "No active diffusion runs."))))

(defun unhinged-diffusion-debug ()
  "Pop to the unhinged diffusion debug / status buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create unhinged-diffusion--debug-buffer-name)))

(defun unhinged-diffusion-pending-requests ()
  "Show pending gptel requests related to diffusion runs."
  (interactive)
  (if (not (boundp 'gptel--request-alist))
      (message "No gptel--request-alist bound.")
    (let ((pending '())
          (our-fsms (let ((fsms '()))
                      (maphash (lambda (_ run)
                                 (when-let* ((fsm (plist-get run :fsm)))
                                   (push fsm fsms)))
                               unhinged-diffusion--active-runs)
                      fsms)))
      (dolist (entry gptel--request-alist)
        (let* ((proc (car entry))
               (fsm (cadr entry))
               (info (when fsm (condition-case nil (gptel-fsm-info fsm) (error nil))))
               (buf (when info (plist-get info :buffer)))
               (backend (when info (plist-get info :backend)))
               (model (when info (plist-get info :model)))
               (sys (when info (plist-get info :system)))
               (is-ours (or (member fsm our-fsms)
                            (and sys (string= sys unhinged-diffusion-system-prompt)))))
          (when is-ours
            (push (format "  %s: backend=%S model=%S status=%S proc-alive=%s"
                          (or (when buf (buffer-name buf)) "unknown")
                          (when backend (gptel-backend-name backend))
                          model
                          (or (when info (plist-get info :status)) "unknown")
                          (and (processp proc) (process-live-p proc) "yes"))
                  pending))))
      (if pending
          (message "Pending diffusion requests:\n%s" (string-join pending "\n"))
        (message "No pending diffusion requests.")))))

(defun unhinged-diffusion-export (file)
  "Export current buffer's canvas to FILE as PPM."
  (interactive
   (list (read-file-name "Export to PPM: " nil "unhinged-diffusion.ppm")))
  (unhinged-diffusion-canvas-to-ppm (current-buffer) file)
  (message "Exported to %s" file))

(provide 'unhinged-diffusion)
;;; unhinged-diffusion.el ends here
