; BSD 3-Clause License
;
; Copyright (c) 2017, Matti J. Kärki
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


#lang racket
(require racket/system)
(require net/url)
(require ffi/unsafe)

(define URL "https://gfwsl.geforce.com/services_toolkit/services/com/nvidia/services/AjaxDriverService.php?func=DriverManualLookup&psid=85&pfid=653&osID=57&languageCode=1033&beta=0&isWHQL=1&dltype=-1&sort1=0&numberOfResults=10")
(define PROGRAMFILES (getenv "ProgramFiles"))
(define NVIDIASMIPATH "NVIDIA Corporation\\NVSMI\\nvidia-smi.exe")
(define NVIDIASMI (string-append PROGRAMFILES "\\" NVIDIASMIPATH))

(define MessageBoxW (get-ffi-obj "MessageBoxW" (ffi-lib "user32.dll")
                                 (_fun _pointer
                                       _string/utf-16
                                       _string/utf-16
                                       _uint32
                                       -> _int32) #f))
(define NULL #f)
(define MB_OK #x00000000)
(define MB_YESNO #x00000004)
(define MB_ICONEXCLAMATION #x00000030)
(define IDYES 6)
(define IDNO 7)

(define (get-installed-version)
  (string->number (second (regexp-match #rx"Driver Version: ([0-9]+.[0-9]+)"
                                        (port->string (first (process NVIDIASMI)))))))

(define (get-driver-info)
  (let ((page (port->string (get-pure-port (string->url URL)))))
    (list (string->number (second (regexp-match #rx"\"Version\" : \"(.+?)\"" page)))
          (second (regexp-match #rx"\"DownloadURL\" : \"(.+?)\"" page)))))

(let* ((info (get-driver-info))
       (version (first info))
       (dlurl (second info))
       (installed-version (get-installed-version)))
  (when (> version installed-version)
    (when (= IDYES (MessageBoxW NULL
                                (string-append "Current version: "
                                               (number->string installed-version)
                                               "\n"
                                               "New version: "
                                               (number->string version))
                                "There is a new GeForce driver available"
                                (bitwise-ior MB_YESNO MB_ICONEXCLAMATION)))
      (let ((ignored-response (process (string-append "start " dlurl))))
        (void)))))
