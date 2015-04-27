#! /usr/bin/env gforth

\ pbm2scr.fs
\ Graphic converter from PBM to ZX Spectrum SCR.

\ 2015-04-26: Start.

\ References:
\   - Debian package Netpbm
\   - man pbm

forth definitions

marker --pbm2scr--

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Requirements

require string.fs  \ Gforth dynamic strings

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

: unslurp-file  ( ca1 len1 ca2 len2 -- )
  \ ca1 len1 = content to write to the file
  \ ca2 len2 = filename
  w/o create-file throw >r
  r@ write-file throw
  r> close-file throw
  ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ The ZX Spectrum screen

\ A ZX Spectrum screen has two parts: first, the bitmap: 6144 bytes
\ that represent a 256x192 bitmap, with a special order; second, the
\ attributes: 768 bytes (32x24 character positions) that describe the
\ colors of every 8x8 square of the bitmap.

6144 constant /zxscr-bitmap
 768 constant /zxscr-attributes
/zxscr-bitmap /zxscr-attributes + constant /zxscr

/zxscr allocate throw constant zxscr

zxscr /zxscr-bitmap + constant zxscr-attributes

zxscr /zxscr-bitmap erase
zxscr-attributes /zxscr-attributes 56 fill  \ white paper, black ink

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ The PBM header

\ From the documentation of Netpbm (Debian package):

\ ---------------------------------------------- Each PBM image
\ consists of the following:
\ 
\ - A "magic number" for identifying the file type.  A pbm image's
\ magic number is the two characters "P4".
\ 
\ - Whitespace (blanks, TABs, CRs, LFs).
\ 
\ - The width in pixels of the image, formatted as ASCII characters in
\ decimal.
\ 
\ - Whitespace.
\ 
\ - The height in pixels of the image, again in ASCII decimal.
\ 
\ - Newline or other single whitespace character.
\ 
\ - A  raster of Height rows, in order from top to bottom.  Each row
\ is Width bits, packed 8 to a byte, with don't care bits to fill out
\ the last byte in the row.  Each bit represents a pixel: 1  is black,
\ 0  is white.   The  order of the pixels is left to right.  The order
\ of their storage within each file byte is most significant bit to
\ least significant bit.  The order of the file bytes is from the
\ beginning of the file toward the end of the file.
\ 
\ - Characters  from  a  "#"  to  the  next  end-of-line, before the
\ width/height line, are comments and are ignored.
\
\ There is actually another version of the PBM format, even more more
\ simplistic, more lavishly wasteful  of space than PBM, called Plain
\ PBM.
\
\ The difference is:
\
\ - There is exactly one image in a file.
\
\ - The "magic number" is "P1" instead of "P4".
\
\ - Each pixel in the raster is represented by a byte containing ASCII
\ '1' or '0',  representing  black  and white respectively.  There are
\ no fill bits at the end of a row.
\
\ - White space in the raster section is ignored.
\
\ - You can put any junk you want after the raster, if it starts with
\ a white space character.
\
\ - No line should be longer than 70 characters.
\
\ ----------------------------------------------

\ The chosen approach is to define Forth words, so the PBM file can be
\ interpreted as a Forth source file.

: byte  ( -- b )
  \ Get the next byte from the input file.
  source-id key-file
  ;
: scr-address  ( n -- a )
  \ XXX TODO
  zxscr +
  ;
: p1-bit  ( -- c )
  \ Get the next bit from the P1 input file,
  \ represented by a character: "1" or "0".
  begin   byte dup bl =
  while   drop  \ skip spaces
  repeat
  ;
: p1-byte  ( -- b )
  \ Get the next bitmap byte from the P1 input file,
  \ represented by 8 characters: "1"=black; "0"=white.
  0  \ return value
  8 0 do
    p1-bit [char] 1 = 128 and i rshift or
  loop
  ;
defer bitmap-byte  ( -- b )
: bitmap>scr  ( "<bitmap>" -- )
  \ Get the PBM bitmap and convert it to the SCR bitmap.
  /zxscr-bitmap 0 do
    bitmap-byte i scr-address c!
  loop
  ;

variable width? \ flag: width found in the input file?
variable p1? \ flag: P1 magic number found in the input file?
variable p4? \ flag: P4 magic number found in the input file?

: check-type  ( -- )
  \ Abort if no file type was specified in the file header.
  p1? @ p4? @ or 0=
  abort" File type not supported"
  ;
: check-width  ( -- )
  \ Abort if no width was specified in the file header.
  width? @ 0= abort" The bitmap size must be 256x192"
  ;

wordlist constant pbm-wid  \ words allowed in the PBM file
pbm-wid set-current

' \ alias #  ( "ccc<newline>" -- )
: p1  ( -- )
  \ Set a P1 PBM, the ASCII variant of the format.
  p1? on  ['] p1-byte is bitmap-byte
  ;
: p4  ( -- )
  \ Set a P4 PBM, the binary variant of the format.
  p4? on  ['] byte is bitmap-byte
  ;
: 256  ( -- )
  \ The width of the image.
  \ This is the last but one metadata before the bitmap.
  check-type  width? on
  ;
: 192  ( -- )
  \ The heigth of the image.
  \ This is the last metadata before the bitmap.
  \ If everything is ok, get the bitmap.
  check-type check-width  bitmap>scr
  ;

forth definitions

: save-scr  ( ca len -- )
  \ Save the SCR buffer to the output file.
  \ ca len = input file name
  s" .scr" s+ zxscr /zxscr 2swap unslurp-file
  ;

: (pbm>scr)  ( ca len -- )
  \ ca len = input file name
  2dup
  pbm-wid >order seal  included  only forth
  save-scr
  ;
: usage  ( -- )
  cr ." pbm2scr" cr
  ." Copyright (C) 2015 Marcos Cruz (programandala.net)" cr cr
  ." Usage:" cr
  ."   pbm2scr.fs input_file.pbm" cr
  ." The output file name will have the scr extension added." cr
  ;
: parameter?  ( -- f )
  \ Is there one parameter in the command line?
  argc @ 2 =
  ;
: pbm>scr  ( -- )
  \ Convert a PBM file to a ZX Spectrum SCR file.
  parameter? if  1 arg (pbm>scr)  else  usage  then  bye
  ;

pbm>scr
