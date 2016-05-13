;;
;; chicken-sdl2-image: CHICKEN Scheme bindings to SDL_image 2
;;
;; Copyright © 2015  John Croisant.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


(module sdl2-image
    (init!
     quit!
     current-version
     compiled-version

     load
     load*
     load-rw
     load-rw*
     load-typed-rw
     load-typed-rw*)


  (import (except scheme
                  load)
          chicken foreign lolevel)

  (require-library sdl2 sdl2-internals)
  (import (only sdl2-internals
                define-function-binding
                struct-null?
                %allocate-Uint8
                sdl-failure
                surface?
                unwrap-surface  wrap-surface
                unwrap-rwops    wrap-rwops)
          (only sdl2
                free-surface!
                rw-close!))


  (foreign-declare "#include \"SDL_image.h\"")

  (define-foreign-type Uint8* (c-pointer unsigned-byte))

  (define-foreign-type SDL_Surface*
    (nonnull-c-pointer "SDL_Surface")
    unwrap-surface
    wrap-surface)

  (define-foreign-type SDL_RWops*
    (nonnull-c-pointer "SDL_RWops")
    unwrap-rwops
    wrap-rwops)

  (define-type sdl2:surface (struct sdl2:surface))
  (define-type sdl2:rwops   (struct sdl2:rwops))


  ;; Copied from chicken-sdl2.
  (define-syntax with-temp-mem
    (syntax-rules ()
      ((with-temp-mem ((temp-var alloc-expr) ...)
         body ...)
       (let ((temp-var alloc-expr) ...)
         (receive result-values (begin body ...)
           (free temp-var)
           ...
           (apply values result-values))))))


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; INIT / QUIT

  (define IMG_INIT_JPG (foreign-value "IMG_INIT_JPG" int))
  (define IMG_INIT_PNG (foreign-value "IMG_INIT_PNG" int))
  (define IMG_INIT_TIF (foreign-value "IMG_INIT_TIF" int))

  (: %pack-init-flags ((list-of symbol) symbol -> fixnum))
  (define (%pack-init-flags syms fn-name)
    (apply bitwise-ior
      (map (lambda (sym)
             (case sym
               ((jpg) IMG_INIT_JPG)
               ((png) IMG_INIT_PNG)
               ((tif) IMG_INIT_TIF)
               (else (error fn-name "invalid init flag" sym))))
           syms)))

  (: %unpack-init-flags (fixnum -> (list-of symbol)))
  (define (%unpack-init-flags bitfield)
    (let ((results '()))
      (unless (zero? (bitwise-and bitfield IMG_INIT_TIF))
        (set! results (cons 'tif results)))
      (unless (zero? (bitwise-and bitfield IMG_INIT_PNG))
        (set! results (cons 'png results)))
      (unless (zero? (bitwise-and bitfield IMG_INIT_JPG))
        (set! results (cons 'jpg results)))
      results))


  (: init! ((list-of symbol) -> (list-of symbol)))
  (define (init! #!optional (flags '(jpg png tif)))
    (define-function-binding IMG_Init
      return: (int initted-loaders-bitfield)
      args: ((int flags)))
    (%unpack-init-flags
     (IMG_Init (%pack-init-flags flags 'init!))))


  (: quit! (-> void))
  (define (quit!)
    (define-function-binding IMG_Quit)
    (IMG_Quit))


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; VERSION

  (: current-version (-> (list fixnum fixnum fixnum)))
  (define (current-version)
    (define foreign-getter
      (foreign-lambda*
       void ((Uint8* majorOut) (Uint8* minorOut) (Uint8* patchOut))
       "const SDL_version* v = IMG_Linked_Version();"
       "*majorOut = v->major;"
       "*minorOut = v->minor;"
       "*patchOut = v->patch;"))
    (with-temp-mem ((major-out (%allocate-Uint8))
                    (minor-out (%allocate-Uint8))
                    (patch-out (%allocate-Uint8)))
      (foreign-getter major-out minor-out patch-out)
      (list (pointer-u8-ref major-out)
            (pointer-u8-ref minor-out)
            (pointer-u8-ref patch-out))))


  (: compiled-version (-> (list fixnum fixnum fixnum)))
  (define (compiled-version)
    (define foreign-getter
      (foreign-lambda*
       void ((Uint8* majorOut) (Uint8* minorOut) (Uint8* patchOut))
       "SDL_version v;"
       "SDL_IMAGE_VERSION(&v);"
       "*majorOut = v.major;"
       "*minorOut = v.minor;"
       "*patchOut = v.patch;"))
    (with-temp-mem ((major-out (%allocate-Uint8))
                    (minor-out (%allocate-Uint8))
                    (patch-out (%allocate-Uint8)))
      (foreign-getter major-out minor-out patch-out)
      (list (pointer-u8-ref major-out)
            (pointer-u8-ref minor-out)
            (pointer-u8-ref patch-out))))



  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LOADING

  (define-inline (%nonnull-surface? surf)
    (and (surface? surf) (not (struct-null? surf))))


  (: load (string -> sdl2:surface))
  (define (load filepath)
    (set-finalizer! (load* filepath) free-surface!))


  (: load* (string -> sdl2:surface))
  (define (load* filepath)
    (define-function-binding IMG_Load
      return: (SDL_Surface* surf-or-null)
      args: (((const c-string) filepath)))
    (let ((surf (IMG_Load filepath)))
      (if (%nonnull-surface? surf)
          surf
          (abort (sdl-failure "IMG_Load" #f)))))



  (: load-rw (sdl2:rwops #!optional boolean -> sdl2:surface))
  (define (load-rw rwops #!optional close?)
    (set-finalizer! (load-rw* rwops close?) free-surface!))


  (: load-rw* (sdl2:rwops #!optional boolean -> sdl2:surface))
  (define (load-rw* rwops #!optional close?)
    (define-function-binding IMG_Load_RW
      return: (SDL_Surface* surf-or-null)
      args: ((SDL_RWops* src)
             (bool freesrc)))
    (let ((surf (IMG_Load_RW rwops #f)))
      (when close?
        ;; Properly close and nullify the sdl2:rwops.
        (rw-close! rwops))
      (if (%nonnull-surface? surf)
          surf
          (abort (sdl-failure "IMG_Load_RW" #f)))))



  (: load-typed-rw (sdl2:rwops boolean string -> sdl2:surface))
  (define (load-typed-rw rwops close? type)
    (set-finalizer! (load-typed-rw* rwops close? type) free-surface!))


  (: load-typed-rw* (sdl2:rwops boolean string -> sdl2:surface))
  (define (load-typed-rw* rwops close? type)
    (define-function-binding IMG_LoadTyped_RW
      return: (SDL_Surface* surf-or-null)
      args: ((SDL_RWops* src)
             (bool freesrc)
             (c-string type)))
    (let ((surf (IMG_LoadTyped_RW rwops #f type)))
      (when close?
        ;; Properly close and nullify the sdl2:rwops.
        (rw-close! rwops))
      (if (%nonnull-surface? surf)
          surf
          (abort (sdl-failure "IMG_LoadTyped_RW" #f)))))


  ) ;; end module sdl2-image
