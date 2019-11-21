; BSD 3-Clause License
;
; Copyright (c) 2017-2019, Matti J. Kärki
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; * Redistributions of source code must retain the above copyright notice, this
;   list of conditions and the following disclaimer.
;
; * Redistributions in binary form must reproduce the above copyright notice,
;   this list of conditions and the following disclaimer in the documentation
;   and/or other materials provided with the distribution.
;
; * Neither the name of the copyright holder nor the names of its
;   contributors may be used to endorse or promote products derived from
;   this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#lang racket/base

(require racket/port
         racket/list
         racket/system
         net/url
         ffi/unsafe)

(define URL "https://gfwsl.geforce.com/services_toolkit/services/com/nvidia/services/AjaxDriverService.php?func=DriverManualLookup&psid=101&pfid=859&osID=57&languageCode=1033&beta=0&isWHQL=1&dltype=-1&sort1=0&numberOfResults=10")
(define PROGRAMFILES (getenv "ProgramFiles"))
(define NVIDIASMIPATH "NVIDIA Corporation\\NVSMI\\nvidia-smi.exe")
(define NVIDIASMI (string-append PROGRAMFILES "\\" NVIDIASMIPATH))

(define ERRORTITLE "Error!")
(define ERRORMSG (string-append "Unable to get the driver version!\n\n"
                                "Can't get information by using the program:\n"
                                "~a\n\n"
                                "Maybe the GeForce drivers are not installed?"))

(define VERSIONTITLE "There is a new GeForce driver available")
(define VERSIONMSG (string-append "Current version: ~a\n"
                                  "New version: ~a"))


(define MessageBoxW (get-ffi-obj "MessageBoxW" (ffi-lib "user32.dll")
                                 (_fun _pointer
                                       _string/utf-16
                                       _string/utf-16
                                       _uint32
                                       -> _int32) #f))
(define NULL #f)

(define MB_OK              #x00000000)
(define MB_YESNO           #x00000004)
(define MB_ICONEXCLAMATION #x00000030)
(define MB_ICONINFORMATION #x00000040)
(define MB_ICONQUESTION    #x00000020)
(define MB_ICONSTOP        #x00000010)

(define IDOK       01)
(define IDCANCEL   02)
(define IDABORT    03)
(define IDRETRY    04)
(define IDIGNORE   05)
(define IDYES      06)
(define IDNO       07)
(define IDTRYAGAIN 10)
(define IDCONTINUE 11)

(define (get-page url)
  (port->string (get-pure-port (string->url URL))))

(define (get-version page)
  (string->number (second (regexp-match #rx"\"Version\" : \"(.+?)\"" page))))

(define (get-dl-url page)
  (second (regexp-match #rx"\"DownloadURL\" : \"(.+?)\"" page)))

(define (ask-dl-yes-no installed-version version)
  (MessageBoxW NULL (format VERSIONMSG installed-version version) VERSIONTITLE
               (bitwise-ior MB_YESNO MB_ICONEXCLAMATION)))

(define (get-installed-version)
  (let ((matchresults (regexp-match #rx"Driver Version: ([0-9]+.[0-9]+)"
                                    (port->string (first (process NVIDIASMI))))))
    (if matchresults
        (string->number (second matchresults))
        (begin
          (MessageBoxW NULL (format ERRORMSG NVIDIASMI) ERRORTITLE (bitwise-ior MB_OK MB_ICONSTOP))
          #f))))

(define (get-driver-info)
  (let ((page (get-page URL)))
    (if (string? page)
        (list (get-version page) (get-dl-url page))
        (list #f #f))))

(define (main)
  (let* ((info (get-driver-info))
         (version (first info))
         (dlurl (second info))
         (installed-version (get-installed-version)))
    (when version
      (when installed-version
        (when (> version installed-version)
          (when (= IDYES (ask-dl-yes-no installed-version version))
            (let ((ignored-response (process (string-append "start " dlurl))))
              (void))))))))

(main)
