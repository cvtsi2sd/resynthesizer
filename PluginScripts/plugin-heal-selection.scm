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

(define (script-fu-heal-selection timg tdrawable samplingRadiusParam directionParam orderParam)
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
  (if (gbool (gimp-selection-is-empty timg))
    (gimp-message _"You must first select a region to heal.")
    (begin
      (gimp-image-undo-group-start timg)
      (let* ((targetBounds (cdr (gimp-drawable-mask-bounds tdrawable)))
             ; In duplicate image, create the sample (corpus).
             ; (I tried to use a temporary layer, but found it easier to use duplicate image.)
             (tempImage (car (assert (gimp-image-duplicate timg) "Failed duplicate image")))
             ; !!! The drawable can be a mask (grayscale channel), don't restrict to layer
             (work_drawable (car (assert (gimp-image-get-active-drawable tempImage) "Failed get active drawable")))
             ; save for later use
             (orgSelection (car (gimp-selection-save tempImage))))
        ; grow and punch hole, making a frisket iow stencil iow donut
        (gimp-selection-grow tempImage samplingRadiusParam)
        ; !!! Note that if selection is a bordering ring already, growing expanded it inwards.
        ; Which is what we want, to make a corpus inwards.
        (let* ((grownSelection (car (gimp-selection-save tempImage))))
          ; Cut hole where the original selection was, so we don't sample from it.
          (gimp-image-select-item tempImage CHANNEL-OP-SUBTRACT orgSelection)
          ;Selection (to be the corpus) is donut or frisket around the original target T
          ;  xxx
          ;  xTx
          ;  xxx
          (let* ((frisketBounds (cdr (gimp-drawable-mask-bounds grownSelection)))
                 (newBounds)
                 (imageWidth (car (gimp-image-width tempImage)))
                 (imageHeight (car (gimp-image-height tempImage)))
                 (x-clamp (lambda (x) (min (max x 0) imageWidth)))
                 (y-clamp (lambda (y) (min (max y 0) imageHeight))))
            (cond
              ((= directionParam 0) ; all around
               ; Crop to the entire frisket
               (set! newBounds frisketBounds))
              ((= directionParam 1) ; sides
               ; Crop to target height and frisket width: XTX
               (set! newBounds (list (b-left frisketBounds) (b-top targetBounds)
                                     (b-right frisketBounds) (b-bottom targetBounds))))
              ((= directionParam 2) ; above and below
               ; X Crop to target width and frisket height
               ; T
               ; X
               (set! newBounds (list (b-left targetBounds) (b-top frisketBounds)
                                     (b-right targetBounds) (b-bottom frisketBounds)))))
            ; clamp to image size
            (set! newBounds (list (x-clamp (b-left newBounds))
                                  (y-clamp (b-top newBounds))
                                  (x-clamp (b-right newBounds))
                                  (y-clamp (b-bottom newBounds))))
            (gimp-image-crop tempImage (b-width newBounds) (b-height newBounds)
                                       (b-left newBounds) (b-top newBounds))
            ; Encode two script params into one resynthesizer param.
            ; use border 1 means fill target in random order
            ; use border 0 is for texture mapping operations, not used by this script
            (let* ((useBorder
                     (cond
                       ; User wants NO order, ie random filling
                       ((= orderParam 0)
                        1)
                       ; Inward to corpus. 2, 3, 4
                       ((= orderParam 1)
                        ; !!! Offset by 2 to get past the original two boolean values
                        (+ directionParam 2))
                       ; Outward from image center.
                       (else
                         ; 5+0=5 outward concentric
                         ; 5+1=6 outward from sides
                         ; 5+2=7 outward above and below
                         (+ directionParam 5)))))
              (if debug (gimp-display-new tempImage))
              (plug-in-resynthesizer RUN-NONINTERACTIVE
                       timg tdrawable
                       0 0 useBorder
                       work_drawable
                       -1 -1 0.0 0.117 16 500)
              )))
        ; clean up
        (if (not debug) (gimp-image-delete tempImage)))
      (gimp-image-undo-group-end timg)
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
