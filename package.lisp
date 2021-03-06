(in-package #:CL-USER)

(defpackage #:pal-ffi
  (:use :common-lisp)
  (:export #:+NO-EVENT+
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
           #:+gl-COLOR-BUFFER-BIT+
           #:+gl-CURRENT-BIT+
           #:+gl-DEPTH-BUFFER-BIT+
           #:+gl-ENABLE-BIT+
           #:+gl-LINE-BIT+
           #:gl-push-attrib
           #:gl-pop-attrib
           #:+gl-ALPHA-TEST+
           #:+gl-ALPHA-TEST-FUNC+
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
           #:+gl-ONE-MINUS-DST-ALPHA+
           #:+gl-ONE-MINUS-DST-COLOR+
           #:+gl-ONE-MINUS-SRC-ALPHA+
           #:+gl-ONE-MINUS-SRC-COLOR+
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
           #:SRC
           #:+NO-FADING+
           #:RESIZE-EVENT
           #:QUIT-TTF
           #:UNICODE
           #:+BUTTON-MIDDLE+
           #:MOD
           #:+KEY-DOWN-EVENT+
           #:WARP-MOUSE
           #:KEYBOARD-EVENT
           #:+MUS-WAV+
           #:SCREEN
           #:+INIT-NOPARACHUTE+
           #:BITSPERPIXEL
           #:RW-FROM-FILE
           #:GLOSS
           #:RECTANGLE
           #:+YV12-OVERLAY+
           #:ENABLE-KEYREPEAT
           #:+ANYFORMAT+
           #:+CHANNELS+
           #:RECT
           #:QUIT-EVENT
           #:+ASYNCBLIT+
           #:SURFACE
           #:+MUS-MOD+
           #:UNUSED1
           #:VIDEO-MODE-OK
           #:GET-KEY-NAME
           #:H
           #:+INIT-JOYSTICK+
           #:KEY
           #:BPP
           #:MODE
           #:SET-CAPTION
           #:SET-ALPHA
           #:INIT-SUBSYSTEM
           #:FILL-RECT
           #:YREL
           #:ICON
           #:GET-MOUSE-STATE
           #:SRCRECT
           #:+SRCCOLORKEY+
           #:GMASK
           #:XREL
           #:+OPENGLBLIT+
           #:TITLE
           #:GSHIFT
           #:FLIP
           #:MOUSE-MOTION-EVENT
           #:FMT
           #:WHICH
           #:CHANNELS
           #:BYTESPERPIXEL
           #:+MUS-OGG+
           #:FORMAT-VERSION
           #:+INIT-TIMER+
           #:EVENT
           #:ALOSS
           #:+FULLSCREEN+
           #:+AUDIO-S16+
           #:POLL-EVENT
           #:+AUDIO-S16MSB+
           #:FLAG
           #:+INIT-CDROM+
           #:+INIT-EVERYTHING+
           #:STATE
           #:+MAX-VALUE+
           #:+MOUSE-MOTION-EVENT+
           #:LOCKED
           #:HALT-CHANNEL
           #:+MUS-MID+
           #:+FADING-OUT+
           #:RSHIFT
           #:+AUDIO-S16LSB+
           #:+NOFRAME+
           #:MAP-RBG
           #:RLOSS
           #:+PREALLOC+
           #:HEIGHT
           #:QUIT-SUBSYSTEM
           #:ALPHA
           #:+TTF-STYLE-UNDERLINE+
           #:CONVERT-SURFACE
           #:DSTRECT
           #:INIT-TTF
           #:PITCH
           #:TOGGLE
           #:DISPLAY-FORMAT
           #:W
           #:SDL-KEY
           #:GAIN
           #:+INIT-AUDIO+
           #:GET-EVENT
           #:WIDTH
           #:PALETTE
           #:+OPENGL+
           #:+RESIZE-EVENT+
           #:+TTF-STYLE-NORMAL+
           #:CLIP-RECT
           #:LOAD-IMAGE
           #:+SRCALPHA+
           #:+BUTTON-LEFT+
           #:+INIT-VIDEO+
           #:DISPLAY-FORMAT-ALPHA
           #:+MOUSE-BUTTON-DOWN-EVENT+
           #:FLAGS
           #:+CHANNEL-POST+
           #:MS
           #:+EXPOSE-EVENT+
           #:INIT
           #:GET-CLIP-RECT
           #:DST
           #:+MUS-MP3+
           #:UNUSED
           #:+INIT-EVENTTHREAD+
           #:ACTIVE-EVENT
           #:X
           #:+RESIZABLE+
           #:SYM
           #:+MUS-NONE+
           #:BLIT
           #:free-all-resources
           #:free-resource
           #:+FADING-IN+
           #:+SWSURFACE+
           #:FILE
           #:BMASK
           #:+ACTIVE-EVENT+
           #:+KEY-UP-EVENT+
           #:FREQUENCY
           #:+BUTTON-WHEELDOWN+
           #:SET-VIDEO-MODE
           #:+QUIT-EVENT+
           #:OFFSET
           #:SHOW-CURSOR
           #:ASHIFT
           #:+DEFAULT-CHANNELS+
           #:+YUY2-OVERLAY+
           #:A
           #:+MUS-CMD+
           #:+AUDIO-S8+
           #:COLORKEY
           #:+HWPALETTE+
           #:+HWACCEL+
           #:+TTF-STYLE-BOLD+
           #:GET-TICK
           #:FONT
           #:Y
           #:SET-COLOR-KEY
           #:PIXELFORMAT
           #:UPDATE-RECT
           #:CHANNEL
           #:+DOUBLEBUF+
           #:REFCOUNT
           #:+BUTTON-WHEELUP+
           #:CHUNKSIZE
           #:INTERVAL
           #:+BUTTON-RIGHT+
           #:QUIT
           #:+IYUV-OVERLAY+
           #:R
           #:+RLEACCEL+
           #:surface-map-rgba
           #:surface-map-rgb
           #:create-rgb-surface
           #:create-rgb-surface-from
           #:G
           #:sdl-mod
           #:HWDATA
           #:+UYVY-OVERLAY+
           #:GET-RELATIVE-MOUSE-STATE
           #:PIXELS
           #:+MOUSE-BUTTON-UP-EVENT+
           #:BSHIFT
           #:DELAY
           #:BLOSS
           #:CHUNK
           #:COLOR
           #:SCANCODE
           #:+YVYU-OVERLAY+
           #:font-image
           #:font-glyphs
           #:font-height
           #:B
           #:+DEFAULT-FREQUENCY+
           #:MAP-RGBA
           #:RMASK
           #:OPEN-AUDIO
           #:CLOSE-AUDIO
           #:MAP-RGB
           #:image-p
           #:font-p
           #:AMASK
           #:MOUSE-BUTTON-EVENT
           #:KEYSYM))


(defpackage #:pal
  (:use :common-lisp)
  (:import-from :pal-ffi
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