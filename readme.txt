
Linux gfx card problems


It seems that some people (yours truly included, running Ubuntu 7.04 with ATI
X550 and the OSS drivers) are having problems under Linux when trying to run
PAL applications several times in the same Lisp session. I did some testing and
it _looks_ like the problem is in some graphics cards drivers. Of course it is
possible that there is a bug in PAL, but so far I haven't find it.

Running the following function twice after PAL is loaded should trigger the bug,
if present on your system:

-----------

(defun test-open-close ()
  (pal-ffi::init pal-ffi::+init-video+)
  (pal-ffi::gl-set-attribute pal-ffi::+gl-depth-size+ 0)
  (pal-ffi::gl-set-attribute pal-ffi::+gl-doublebuffer+ 1)
  (when (cffi:null-pointer-p (pal-ffi::set-video-mode
                              800
                              600
                              0
                              (logior pal-ffi::+gl-doublebuffer+ pal-ffi::+opengl+)))
    (error "PAL failed to obtain SDL surface"))
  (pal-ffi::quit))

-----------

This happens on my computer with both SBCL and CLisp, but not with an equivalent
C program or any of the Windows CLs that I have tried. So far I have no idea
what is causing this but if anyone has any clues or more info I'd appreciate
sharing it.

Since this kind of bug causes problems when developing in an incremental, "live"
environment like CL here are some suboptimal workarounds:

- The bug doesn't seem to appear with all drivers/gfx cards. Especially running
X11 without HW acceleration should be safe.

- Never call CLOSE-PAL. I haven't tested it much, but it should be possible to
just call OPEN-PAL when starting your lisp session and never use CLOSE-PAL or
WITH-PAL (which eventually calls CLOSE-PAL). Of course this means that some
parameters like window size can't be changed after initialisation.

- Never return from WITH-PAL. Run your main loop in a separate thread and
install condition handlers that just restart your main loop without closing
down PAL. That way you can incrementally change your functions/classes etc.
while your app is running. I might actually add this as an option to WITH-PAL.

All in all this bug mostly has effect only while developing, applications that
don't need to open/close PAL several times should work fine.

-- tomppa
