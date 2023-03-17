; Gimp plugin "Heal selection"
;
; Copyright 2023 Matteo Italia (matteo at mitalia.net)
; Copyright 2009 lloyd konneker (bootch at nc.rr.com)
; Based on smart_remove.scm Copyright 2000 by Paul Harrison.
;
; Version:
;   1.0 lloyd konneker lkk 9/21/2009 Initial version in python.
;   (See release notes for differences over P. Harrison's prior version in scheme language.)
;   1.1 Matteo Italia mitalia 3/10/2023 Conversion back to scheme language
;
; License:
;
;   This program is free software; you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation; either version 2 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   The GNU Public License is available at
;   http://www.gnu.org/copyleft/gpl.html
;

(define (script-fu-heal-selection target-image target-drawable sampling-radius sample-from filling-order)
  ; GIMP 2 functions return (0) for failure and (1) for success, while in
  ; GIMP 3 they return #f or #t; we use this wrapper to check for success
  ; in a version-independent manner
  (define (gbool gvalue) (if (boolean? gvalue) gvalue (<> (car gvalue) 0)))
  (define (assert v msg) (or v (throw msg)))

  ; helpers for boundaries
  (define (b-left   b) (car b))
  (define (b-top    b) (list-ref b 1))
  (define (b-right  b) (list-ref b 2))
  (define (b-bottom b) (list-ref b 3))
  (define (b-width  b) (- (b-right b) (b-left b)))
  (define (b-height b) (- (b-bottom b) (b-top b)))

  (define debug #f)

  ; Create stencil selection in a temp image to pass as source (corpus) to plugin resynthesizer,
  ; which does the substantive work
  (if (gbool (gimp-selection-is-empty target-image))
    (gimp-message _"You must first select a region to heal.")
    (begin
      (gimp-image-undo-group-start target-image)
      (let* ((target-bounds (cdr (gimp-drawable-mask-bounds target-drawable)))
             ; In duplicate image, create the sample (corpus).
             ; (I tried to use a temporary layer, but found it easier to use duplicate image.)
             (temp-image (car (assert (gimp-image-duplicate target-image) "Failed duplicate image")))
             ; !!! The drawable can be a mask (grayscale channel), don't restrict to layer
             (work-drawable (car (assert (gimp-image-get-active-drawable temp-image) "Failed get active drawable")))
             ; save for later use
             (org-selection (car (gimp-selection-save temp-image))))
        ; grow and punch hole, making a frisket iow stencil iow donut
        (gimp-selection-grow temp-image sampling-radius)
        ; !!! Note that if selection is a bordering ring already, growing expanded it inwards.
        ; Which is what we want, to make a corpus inwards.
        (let* ((grown-selection (car (gimp-selection-save temp-image))))
          ; Cut hole where the original selection was, so we don't sample from it.
          (gimp-image-select-item temp-image CHANNEL-OP-SUBTRACT org-selection)
          ;Selection (to be the corpus) is donut or frisket around the original target T
          ;  xxx
          ;  xTx
          ;  xxx
          (let* ((frisket-bounds (cdr (gimp-drawable-mask-bounds grown-selection)))
                 (new-bounds)
                 (imageWidth (car (gimp-image-width temp-image)))
                 (imageHeight (car (gimp-image-height temp-image)))
                 (x-clamp (lambda (x) (min (max x 0) imageWidth)))
                 (y-clamp (lambda (y) (min (max y 0) imageHeight))))
            (cond
              ((= sample-from 0) ; all around
               ; Crop to the entire frisket
               (set! new-bounds frisket-bounds))
              ((= sample-from 1) ; sides
               ; Crop to target height and frisket width: XTX
               (set! new-bounds (list (b-left frisket-bounds) (b-top target-bounds)
                                      (b-right frisket-bounds) (b-bottom target-bounds))))
              ((= sample-from 2) ; above and below
               ; X Crop to target width and frisket height
               ; T
               ; X
               (set! new-bounds (list (b-left target-bounds) (b-top frisket-bounds)
                                      (b-right target-bounds) (b-bottom frisket-bounds)))))
            ; clamp to image size
            (set! new-bounds (list (x-clamp (b-left new-bounds))
                                   (y-clamp (b-top new-bounds))
                                   (x-clamp (b-right new-bounds))
                                   (y-clamp (b-bottom new-bounds))))
            (gimp-image-crop temp-image (b-width new-bounds) (b-height new-bounds)
                                        (b-left new-bounds) (b-top new-bounds))
            ; Encode two script params into one resynthesizer param.
            ; use border 1 means fill target in random order
            ; use border 0 is for texture mapping operations, not used by this script
            (let* ((user-border
                     (cond
                       ; User wants NO order, ie random filling
                       ((= filling-order 0)
                        1)
                       ; Inward to corpus. 2, 3, 4
                       ((= filling-order 1)
                        ; !!! Offset by 2 to get past the original two boolean values
                        (+ sample-from 2))
                       ; Outward from image center.
                       (else
                         ; 5+0=5 outward concentric
                         ; 5+1=6 outward from sides
                         ; 5+2=7 outward above and below
                         (+ sample-from 5)))))
              (if debug (gimp-display-new temp-image))
              (plug-in-resynthesizer RUN-NONINTERACTIVE
                                     target-image target-drawable
                                     0 0 user-border
                                     work-drawable
                                     -1 -1 0.0 0.117 16 500)
              )))
        ; clean up
        (if (not debug) (gimp-image-delete temp-image)))
      (gimp-image-undo-group-end target-image)
      )
    )
  )


(script-fu-register "script-fu-heal-selection"
  _"_Heal selection..."
  _"Heal the selection from the surroundings as if using the heal tool."
  "Lloyd Konneker"
  "2009 Lloyd Konneker"
  "2009"
  "RGB*, GRAY*"
  SF-IMAGE       "Input image"         0
  SF-DRAWABLE    "Input drawable"         0
  SF-ADJUSTMENT  _"Context sampling width (pixels)" '(50 1 65535 1 10 0 SF-SPINNER)
  SF-OPTION      _"Sample from"     '(_"All around" _"Sides" _"Above and below")
  SF-OPTION      _"Filling order"   '(_"Random" _"Inwards towards center" _"Outwards from center")
)

(script-fu-menu-register "script-fu-heal-selection"
                         "<Image>/Filters/Enhance")
