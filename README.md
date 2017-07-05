# geforcedrvchk
GeForceDrvChk is a small no-nonsense application for automatically checking Geforce driver updates under Windows.

## Introduction
This little piece of code checks the GeForce website for new driver versions. It only checks for the GeForce GTX series driver for English language and 64-bit Windows desktop (so, no laptop version of the driver).

The main point of the application is to demonstrate the following Racket language features:

- calling a console application and catching the output
- regular expressions
- fetching a page from a WWW server
- calling Win32 API functions via Racket Foreign Function Interface

## Usage

Load the rkt file, use DrRacket to create an executable and put a link to *shell:startup* to execute the application automatically every time Windows starts. Alternatively, if you do not want to install Racket, you can download a ready-made package from [here](https://github.com/mattijk/geforcedrvchk/releases).
