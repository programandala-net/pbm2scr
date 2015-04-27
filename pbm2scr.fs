#! /usr/bin/env gforth

\ pbm2scr.fs
\ Graphic converter from PBM to ZX Spectrum SCR.
s" A-01-201504271910" 2constant version

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Development history

\ 2015-04-26: Start. Version A-00.
\
\ 2015-04-27: First working version, A-01; it works fine with P1 and P4
\ variants of the PBM format.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ References

\ The PBM format details were obtained from the manual page of the
\ Netpbm Debian package.

\ The following article was most useful to code the low level
\ calculation of the ZX Spectrum screen addresses:

\ "Cómo manejar la pantalla desde código máquina"
\ by Paco Portalo, published in Microhobby:
\   Issue 63, 1986-02, p. 30:
\     http://www.microhobby.org/numero063.htm

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Requirements

forth definitions

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

\ XXX TMP
: bin.  ( n -- )
  base @ >r 2 base ! s>d <# # # # # # # # # #> r> base ! type space ;

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

 32 constant chars/line   \ chars per line
192 constant heigth       \ pixels

/zxscr-bitmap 3 / constant /zxscr-third   \ bytes per third of the bitmap
256 constant chars/third                  \ chars per third

variable x
variable y

variable third        \ third of the bitmap (0..2)
variable char/third   \ character in the third (0..255)
variable char-row     \ character row (0..23)
variable char-col     \ character row (0..31)
variable char-scan    \ character scan (0..7)
variable pixel-row    \ pixel row (0..191); top row is 0

false constant [echo] immediate

: >zxscr  ( +n -- ca )
  \ XXX TODO
  \ Convert a position in the input file PBM bitmap
  \ to its correspondent address in the SCR bitmap buffer.
  \ +n = position of the current byte in the input PBM bitmap
  \      (0..6143)
  \ ca = correspondent address in the output SCR bitmap buffer

  \ XXX INFORMER
  [echo] [if]
    dup ." +n=" 3 .r space
  [then]

  \ Calculate the required data from the input position.
  dup /zxscr-third / third !
  dup chars/line / y !  \ XXX TMP
      chars/line mod dup char-col !
                         8 * x !

  y @ dup %000111 and char-scan !
          %111000 and 3 rshift pixel-row !
  
  \ XXX INFORMER
  [echo] [if]
    \ ." +n=" 4 .r space
    ." third=" third ? 
    ." x=" x @ 3 .r space
    ." y=" y @ 3 .r space
    ." pixel-row=" pixel-row @ 3 .r space
    ." char-col=" char-col @ 2 .r space
    ." char-scan=" char-scan ? 
  [then]
  
  \ Calculate the correspondent position
  \ in the ZX Spectrum bitmap (0..6143).
  pixel-row @ 32 *  char-col @ +  \ low byte
  [echo] [if]
    dup bin.  \ XXX INFORMER
  [then]
  third @ 8 * char-scan @ +       \ high byte
  [echo] [if]
    dup bin.  \ XXX INFORMER
  [then]
  256 * +                         \ result
  \ XXX INFORMER
  [echo] [if]
    dup 16384 + ." ZX Spectrum address=" . cr
    \ key drop
  [then]

  dup /zxscr > abort" Bitmap bigger than 256x192" \ XXX TMP
  \ /zxscr-bitmap min

  zxscr +  \ actual address in the output buffer

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
: delimiter?  ( c -- f )
  \ Is the given char a delimiter in the P1 bitmap?
  bl <=
  ;
: p1-bit-char  ( -- c )
  \ Get the next bit from the P1 input file,
  \ represented by an ASCII character: 
  \ "1"=black; "0"=white.
  begin   byte dup delimiter?  while  drop  repeat
  ;
: p1-pixel?  ( -- f )
  \ Get the next pixel from the P1 input file.
  \ f = true if black pixel, else white pixel.
  p1-bit-char [char] 1 =
  ;
: p1-byte  ( -- b )
  \ Get the next bitmap byte from the P1 input file,
  \ represented by 8 ASCII characters:
  \ "1"=black; "0"=white.
  0  \ return value
  8 0 do  p1-pixel? 128 and i rshift or  loop
  ;
defer bitmap-byte  ( -- b )
  \ Get the next bitmap byte from the input file.
  ' false is bitmap-byte  \ default, used for checking
: bitmap>scr  ( "<bitmap>" -- )
  \ Get the PBM bitmap and convert it to the SCR bitmap.
  /zxscr-bitmap 0 do  bitmap-byte i >zxscr c!  loop
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
  \ P1 PBM, the ASCII variant of the format.
  ['] p1-byte is bitmap-byte
  ;
: p4  ( -- )
  \ P4 PBM, the binary variant of the format.
  ['] byte is bitmap-byte
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
  ." PBM to ZX Spectrum SCR graphic converter" cr
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
  fpath .path  \ XXX TMP
  parameter? if  1 arg (pbm>scr)  else  usage  then
  ;

pbm>scr bye
