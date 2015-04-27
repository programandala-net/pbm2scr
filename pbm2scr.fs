#! /usr/bin/env gforth

\ pbm2scr.fs
\ Graphic converter from PBM to ZX Spectrum SCR.
s" A-00-201504271314" 2constant version

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
\ The bitmap converter

\ XXX TODO

32 constant chars/line                    \ chars per line
/zxscr-bitmap 3 / constant /zxscr-third   \ bytes per third of the bitmap
256 constant chars/third                  \ chars per third

variable third        \ third of the bitmap (0..2)
variable char-row     \ character row (0..23)
variable char-col     \ character row (0..31)
variable char-scan    \ character scan (0..7)
variable pixel-row    \ pixel row (0..191); top row is 0

: scr-address  ( n -- a )
  \ XXX TODO
  \ n = position of the current byte in the input bitmap
  \ a = correspondent address in the output bitmap

  dup chars/line / pixel-row !
  dup chars/line mod char-col !
  dup /zxscr-third / third !
      \ /zxscr-third mod chars/third / char-scan ! \ XXX FIXME
      pixel-row @ 8 / char-scan ! \ XXX FIXME


  \ Calculate the offset into the SCR bitmap
  0
  \ third @ /zxscr-third * +
  \ chars/third char-scan @ * + \ XXX FIXME
  chars/third pixel-row @ * +
  char-col @ +

  \ XXX INFORMER
  [ 1 ] [if]
    ." pixel-row " pixel-row ? cr
    ." char-col " char-col ? cr
    ." third " third ? cr
    ." char-scan " char-scan ? cr
    dup 16384 + ." result address is ZX Spectrum " . cr
    key drop
  [then]

  \ dup /zxscr > abort" Fatal error: out of range"
  /zxscr-bitmap min

  zxscr +

  ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ The PBM format

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

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ The converter

\ The chosen approach is to define Forth words with the name of the
\ expected metadata, so the PBM file can be interpreted as a Forth
\ source file.

: byte  ( -- b )
  \ Get the next byte from the input file.
  source-id key-file
  ;
' byte alias p4-byte  ( -- b )
  \ Get the next bitmap byte from the P4 input file.
: p1-bit  ( -- c )
  \ Get the next bit from the P1 input file,
  \ represented by an ASCII character: 
  \ "1"=black; "0"=white.
  \ XXX TODO check what happens with end of line
  begin   byte dup bl =
  while   drop  \ skip spaces
  repeat
  ;
: p1-byte  ( -- b )
  \ Get the next bitmap byte from the P1 input file,
  \ represented by 8 ASCII characters:
  \ "1"=black; "0"=white.
  0  \ return value
  8 0 do
    p1-bit [char] 1 = 128 and i rshift or
  loop
  ;
defer bitmap-byte  ( -- b )
  \ Get the next bitmap byte from the input file.
  ' false is bitmap-byte  \ default, used for checking
: bitmap>scr  ( "<bitmap>" -- )
  \ Get the PBM bitmap and convert it to the SCR bitmap.
  /zxscr-bitmap 0 do
    bitmap-byte i scr-address c!
  loop
  ;

variable width? \ flag: width found in the input file?

: check-type  ( -- )
  \ Abort if no file type was specified in the file header.
  ['] bitmap-byte defer@ ['] false =  \ `bitmap-byte` not set?
  abort" File type not supported"
  ;
: check-width  ( -- )
  \ Abort if no width was specified in the file header.
  width? @ 0= abort" The bitmap size must be 256x192"
  ;

wordlist constant pbm-wordlist  \ words allowed in the PBM file
pbm-wordlist set-current

\ Only five words are needed to interpret a PBM file: the two possible
\ magic numbers, the line comment character,  the width and the
\ heigth. The heigth is the last one in the file header and it will do
\ the conversion of the bitmap.

: p1  ( -- )
  \ Set a P1 PBM, the ASCII variant of the format.
  ['] p1-byte is bitmap-byte
  ;
: p4  ( -- )
  \ Set a P4 PBM, the binary variant of the format.
  ['] p4-byte is bitmap-byte
  ;
' \ alias #  ( "ccc<newline>" -- )
  \ Line comment.
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

forth-wordlist set-current

: save-scr  ( ca len -- )
  \ Save the SCR buffer to the output file.
  \ ca len = input file name
  s" .scr" s+ zxscr /zxscr 2swap unslurp-file
  ;
: (pbm>scr)  ( ca len -- )
  \ Convert a 256x192 PBM file to a ZX Spectrum SCR file.
  \ ca len = input file name
  2>r  get-order
  pbm-wordlist >order seal  2r@ included
  set-order  2r> save-scr
  ;
: usage  ( -- )
  ." pbm2scr" cr
  ." A PBM to ZX Spectrum SCR graphic converter" cr
  ." Version " version type cr
  ." http://programandala.net/en.program.pbm2scr.html" cr cr
  ." Copyright (C) 2015 Marcos Cruz (programandala.net)" cr cr
  ." Usage:" cr cr
  ."   pbm2scr.fs input_file.pbm" cr cr
  ." The input file must be a 256x192 PBM image," cr
  ." in binary or ASCII variant of the format." cr cr
  ." The output file name will be the input file name" cr
  ." with the .scr extension added." cr
  ;
: parameter?  ( -- f )
  \ Is there exactly one parameter in the command line?
  argc @ 2 =
  ;
: pbm>scr  ( -- )
  \ Convert a 256x192 PBM file to a ZX Spectrum SCR file,
  \ if an input file is provided.
  parameter? if  1 arg (pbm>scr)  else  usage  then
  ;

pbm>scr bye
