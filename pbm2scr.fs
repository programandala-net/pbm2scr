#! /usr/bin/env gforth

\ pbm2scr.fs
\ Graphic converter from PBM to ZX Spectrum SCR.

\ 2015-04-26: Start.

\ References:
\   - Debian package Netpbm
\   - man pbm

forth definitions

marker --pbm2scr--

variable width? \ flag: width found in the input file?
variable p4? \ flag: magic number found in the input file?

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ The ZX Spectrum screen

\ A ZX Spectrum screen consists of two parts: first, a 256x192 bitmap
\ that occupies 6144 bytes; second, and a attributes zone of 768 bytes
\ that describe the color of every 8x8 square of the bitmap.

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

\ ----------------------------------------------
\ Each PBM image consists of the following:
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
\ ----------------------------------------------

\ The chosen approach is to define Forth words, so the PBM file can be
\ interpreted as a Forth stream file.

: byte  ( -- b )
  \ Next byte from the input file.
  source-id key-file
  ;
: raster>scr  ( "<bitmap>" -- )
  /zxscr-bitmap 0 do
    byte
  loop
  ;

wordlist constant pbm-wid
pbm-wid set-current

: p4  ( -- )
  \ The first "word" in a binary PBM file is the magic number "P4".
  ." magic number is ok" cr  \ XXX INFORMER
  p4? on
  ;
' \ alias #  ( "ccc<newline>" -- )
  \ Line comments can be included in the image file, with "#".
: 256  ( -- )
  \ The width of the bitmap must be 256.
  p4? @ 0= abort" File type not supported"
  width? on  \ mark the width was found
  ." width is ok" cr  \ XXX INFORMER
  ;
: 192  ( -- )
  \ The heigth of the bitmap must be 192.
  width? @ 0= abort" The bitmap size must be 256x192"
  ." height is ok" cr  \ XXX INFORMER
  raster>scr
  ;

forth definitions

: (pbm>scr)  ( ca len -- )  \ XXX TMP
  ." filename=" 2dup type cr  \ XXX INFORMER
  pbm-wid >order seal
  included
  only forth
  ;
: usage  ( -- )
  ." Usage:" cr
  ." bpm2scr.fs input.pbm" cr
  ;
: pbm>scr  ( -- )
  \ Convert a PBM file to a ZX Spectrum SCR file.
  argc @ 2 = if  1 arg (pbm>scr)  else  usage  then  bye
  ;

pbm>scr
