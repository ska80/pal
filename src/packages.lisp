;;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;;
;;;; Pixel Art Library is published under the MIT license
;;;;
;;;; Copyright (c) 2006 Tomi Neste
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is furnished to do
;;;; so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

(in-package #:cl-user)

(defpackage #:net.common-lisp.pal-ffi
  (:nicknames #:pal-ffi)
  (:use #:cl)
  (:export #:+no-event+
           #:+gl-line-smooth+
           #:+gl-polygon-smooth+
           #:fade-out-music
           #:fade-in-music
           #:make-font
           #:+gl-pack-alignment+
           #:gl-flush
           #:gl-read-pixels
           #:gl-pixel-store
           #:+gl-scissor-test+
           #:free-surface
           #:gl-get-integer
           #:+gl-max-texture-size+
           #:+gl-smooth+
           #:+gl-compile+
           #:+gl-points+
           #:free
           #:calloc
           #:music-music
           #:register-resource
           #:sample-chunk
           #:load-foreign-libraries
           #:sample
           #:music
           #:resource
           #:resource-p
           #:sample-p
           #:music-p
           #:gl-get-error
           #:gl-scissor
           #:gl-point-size
           #:+gl-flat+
           #:gl-rectf
           #:gl-shade-model
           #:gl-line-width
           #:+gl-point+
           #:+gl-line-loop+
           #:+gl-point-smooth+
           #:get-application-folder
           #:+gl-rgb+
           #:get-rgba
           #:+gl-nearest+
           #:u8
           #:u11
           #:u16
           #:+gl-color-buffer-bit+
           #:+gl-current-bit+
           #:+gl-depth-buffer-bit+
           #:+gl-enable-bit+
           #:+gl-line-bit+
           #:gl-push-attrib
           #:gl-pop-attrib
           #:+gl-alpha-test+
           #:+gl-alpha-test-func+
           #:+gl-one+
           #:+gl-zero+
           #:+gl-greater+
           #:gl-alpha-func
           #:+gl-depth-test+
           #:+gl-vendor+
           #:+gl-renderer+
           #:+gl-depth-buffer-bit+
           #:+gl-version+
           #:+gl-extensions+
           #:gl-get-string
           #:gl-rotatef
           #:gl-scalef
           #:gl-pop-matrix
           #:gl-push-matrix
           #:gl-translatef
           #:gl-tex-coord2f
           #:image-width
           #:image-tx2
           #:image-ty2
           #:image-texture-width
           #:image-height
           #:image-texture-height
           #:+gl-texture-mag-filter+
           #:+gl-unsigned-byte+
           #:+gl-texture-min-filter+
           #:+gl-linear+
           #:+gl-rgba+
           #:gl-tex-parameteri
           #:gl-gen-textures
           #:gl-bind-texture
           #:gl-teximage2D
           #:+gl-quads+
           #:+gl-src-alpha+
           #:+gl-dst-alpha+
           #:+gl-one-minus-dst-alpha+
           #:+gl-one-minus-dst-color+
           #:+gl-one-minus-src-alpha+
           #:+gl-one-minus-src-color+
           #:gl-blendfunc
           #:gl-begin
           #:gl-color4ub
           #:+gl-blend+
           #:gl-end
           #:gl-vertex2f
           #:gl-vertex2i
           #:gl-vertex3i
           #:+gl-lines+
           #:+gl-polygon+
           #:gl-viewport
           #:gl-ortho
           #:+gl-color-buffer-bit+
           #:gl-clear
           #:gl-disable
           #:gl-enable
           #:+gl-cull-face-test+
           #:+gl-depth-test+
           #:+gl-projection+
           #:+gl-modelview+
           #:+gl-texture-2d+
           #:gl-matrix-mode
           #:gl-load-identity
           #:gl-clear-color
           #:gl-swap-buffers
           #:gl-set-attribute
           #:+gl-red-size+
           #:+gl-green-size+
           #:+gl-blue-size+
           #:+gl-alpha-size+
           #:+gl-buffer-size+
           #:+gl-doublebuffer+
           #:+gl-depth-size+
           #:+gl-stencil-size+
           #:image
           #:sample
           #:music
           #:font
           #:play-channel-timed
           #:filled-pie-rgba
           #:pause-music
           #:rewind-musi
           #:set-position
           #:paused-music
           #:quickload-raw
           #:resume-music
           #:volume-music
           #:volume-chunk
           #:play-music
           #:load-music
           #:button
           #:halt-music
           #:load-wav
           #:play-channel
           #:convert-surface-format
           #:surface-pixelformat
           #:pixel-rgba
           #:init-framerate
           #:set-framerate
           #:framerate-delay
           #:framerate-manager
           #:src
           #:+no-fading+
           #:resize-event
           #:quit-ttf
           #:unicode
           #:+button-middle+
           #:mod
           #:+key-down-event+
           #:warp-mouse
           #:keyboard-event
           #:+mus-wav+
           #:screen
           #:+init-noparachute+
           #:bitsperpixel
           #:rw-from-file
           #:gloss
           #:rectangle
           #:+yv12-overlay+
           #:enable-keyrepeat
           #:+anyformat+
           #:+channels+
           #:rect
           #:quit-event
           #:+asyncblit+
           #:surface
           #:+mus-mod+
           #:unused1
           #:video-mode-ok
           #:get-key-name
           #:h
           #:+init-joystick+
           #:key
           #:bpp
           #:mode
           #:set-caption
           #:set-alpha
           #:init-subsystem
           #:fill-rect
           #:yrel
           #:icon
           #:get-mouse-state
           #:srcrect
           #:+srccolorkey+
           #:gmask
           #:xrel
           #:+openglblit+
           #:title
           #:gshift
           #:flip
           #:mouse-motion-event
           #:fmt
           #:which
           #:channels
           #:bytesperpixel
           #:+mus-ogg+
           #:format-version
           #:+init-timer+
           #:event
           #:aloss
           #:+fullscreen+
           #:+audio-s16+
           #:poll-event
           #:+audio-s16msb+
           #:flag
           #:+init-cdrom+
           #:+init-everything+
           #:state
           #:+max-value+
           #:+mouse-motion-event+
           #:locked
           #:halt-channel
           #:+mus-mid+
           #:+fading-out+
           #:rshift
           #:+audio-s16lsb+
           #:+noframe+
           #:map-rbg
           #:rloss
           #:+prealloc+
           #:height
           #:quit-subsystem
           #:alpha
           #:+ttf-style-underline+
           #:convert-surface
           #:dstrect
           #:init-ttf
           #:pitch
           #:toggle
           #:display-format
           #:w
           #:sdl-key
           #:gain
           #:+init-audio+
           #:get-event
           #:width
           #:palette
           #:+opengl+
           #:+resize-event+
           #:+ttf-style-normal+
           #:clip-rect
           #:load-image
           #:+srcalpha+
           #:+button-left+
           #:+init-video+
           #:display-format-alpha
           #:+mouse-button-down-event+
           #:flags
           #:+channel-post+
           #:ms
           #:+expose-event+
           #:init
           #:get-clip-rect
           #:dst
           #:+mus-mp3+
           #:unused
           #:+init-eventthread+
           #:active-event
           #:x
           #:+resizable+
           #:sym
           #:+mus-none+
           #:blit
           #:free-all-resources
           #:free-resource
           #:+fading-in+
           #:+swsurface+
           #:file
           #:bmask
           #:+active-event+
           #:+key-up-event+
           #:frequency
           #:+button-wheeldown+
           #:set-video-mode
           #:+quit-event+
           #:offset
           #:show-cursor
           #:ashift
           #:+default-channels+
           #:+yuy2-overlay+
           #:a
           #:+mus-cmd+
           #:+audio-s8+
           #:colorkey
           #:+hwpalette+
           #:+hwaccel+
           #:+ttf-style-bold+
           #:get-tick
           #:font
           #:y
           #:set-color-key
           #:pixelformat
           #:update-rect
           #:channel
           #:+doublebuf+
           #:refcount
           #:+button-wheelup+
           #:chunksize
           #:interval
           #:+button-right+
           #:quit
           #:+iyuv-overlay+
           #:r
           #:+rleaccel+
           #:surface-map-rgba
           #:surface-map-rgb
           #:create-rgb-surface
           #:create-rgb-surface-from
           #:g
           #:sdl-mod
           #:hwdata
           #:+uyvy-overlay+
           #:get-relative-mouse-state
           #:pixels
           #:+mouse-button-up-event+
           #:bshift
           #:delay
           #:bloss
           #:chunk
           #:color
           #:scancode
           #:+yvyu-overlay+
           #:font-image
           #:font-glyphs
           #:font-height
           #:b
           #:+default-frequency+
           #:map-rgba
           #:rmask
           #:open-audio
           #:close-audio
           #:map-rgb
           #:image-p
           #:font-p
           #:amask
           #:mouse-button-event
           #:keysym))

(defpackage #:net.common-lisp.pal
  (:nicknames #:pal)
  (:use #:cl)
  (:import-from #:net.common-lisp.pal-ffi
                #:register-resource #:load-foreign-libraries
                #:image-p #:image #:font #:font-p #:sample #:music #:sample-p #:music-p #:resource #:resource-p
                #:image-width #:image-height
                #:u8 #:u11 #:u16)
  (:export #:open-pal
           #:with-pal
           #:close-pal
           #:get-gl-info
           #:load-foreign-libraries
           #:free-resource
           #:free-all-resources
           #:define-tags
           #:add-tag
           #:tag
           #:sample
           #:music
           #:image
           #:sample-p
           #:music-p
           #:image-p
           #:font-p
           #:get-application-folder
           #:get-application-file
           #:data-path
           #:with-resource

           #:randomly
           #:random-elt
           #:clamp
           #:do-n

           #:handle-events
           #:key-pressed-p
           #:keysym-char
           #:test-keys
           #:event-loop
           #:wait-keypress
           #:get-mouse-pos
           #:get-mouse-x
           #:get-mouse-y

           #:clear-screen
           #:get-screen-width
           #:get-screen-height
           #:set-cursor
           #:set-mouse-pos
           #:get-fps

           #:with-transformation
           #:rotate
           #:translate
           #:scale
           #:set-blend-mode
           #:reset-blend
           #:set-blend-color
           #:with-blend
           #:with-clipping
           #:update-screen

           #:image-from-array
           #:image-from-fn
           #:load-image-to-array
           #:screen-to-array

           #:load-image
           #:image-width
           #:image-height
           #:draw-polygon
           #:draw-polygon*
           #:draw-rectangle
           #:draw-point
           #:draw-line
           #:draw-arrow
           #:draw-image
           #:draw-image*
           #:draw-circle

           #:load-font
           #:get-font-height
           #:draw-text
           #:get-text-size
           #:draw-fps
           #:message

           #:load-sample
           #:play-sample
           #:set-sample-volume

           #:load-music
           #:set-music-volume
           #:play-music
           #:halt-music

           #:color #:color-r #:color-g #:color-b #:color-a #:random-color
           #:+black+ #:+white+ #:+gray+ #:+dark-gray+ #:+light-gray+
           #:+light-green+ #:+light-blue+ #:+red+

           #:v #:vec #:copy-vec #:angle-v #:v-angle #:vx #:vy
           #:v= #:v-round #:v-floor #:v-random
           #:v+ #:v+!  #:v- #:v-! #:v* #:v*! #:v/ #:v/! #:v-max #:v-min #:v-rotate
           #:v-dot #:v-magnitude #:v-normalize #:v-distance
           #:v-truncate #:v-direction
           #:closest-point-to-line #:point-in-line-p #:lines-intersection
           #:distance-from-line #:circle-line-intersection
           #:point-inside-rectangle-p #:rectangles-overlap-p
           #:circles-overlap-p #:point-inside-circle-p))
