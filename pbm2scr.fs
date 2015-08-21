#! /usr/bin/env gforth

\ pbm2scr

s" A-02-20150821" 2constant version

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Description

\ pbm2scr is a command line tool that converts 256x192 PBM graphics
\ (P1 and P4 versions) to ZX Spectrum SCR graphic files.

\ http://programandala.net/en.program.pbm2scr.html

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Author and license

\ Copyright (C) 2015 Marcos Cruz (programandala.net)

\ You may do whatever you want with this work, so long as you retain
\ the copyright notice(s) and this license in all redistributed copies
\ and derived works. There is no warranty.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Acknowledgements

\ pbm2scr is written in Forth with Gforth 0.7.3 (by Anton Ertl,
\ Bernd Paysan et al.):
\   http://gnu.org/software/gforth

\ The information on the PBM format was obtained from the manual page
\ of the Netpbm Debian package.

\ The following article was most useful to code the low level
\ calculation of the ZX Spectrum screen addresses:
\   "Cómo manejar la pantalla desde código máquina"
\   by Paco Portalo, published in the Microhobby magazine
\   (issue 63, 1986-02, page 30):
\     http://www.microhobby.org/numero063.htm

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Installation

\ XXX TODO Galope library.

\ 1) Install Gforth. It's included with most Linux distros, or you can
\ get it from <http://gnu.org/software/gforth>.
\
\ 2) Make sure <pbm2scr.fs> is executable:
\
\   chmod u+x pbm2scr.fs
\
\ 3) Copy, move or link <pbm2scr.fs> to a directory in your path (e.g.
\ </usr/local/bin/> or <~/bin/>), optionally with the filename
\ extension removed. Example:
\
\   ln pmb2scr.fs /usr/local/bin/fmb2scr

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ History

\ 2015-04-26: Start. Version A-00.
\
\ 2015-04-27: Version A-01, first working version: it converts P1 and
\ P4 variants of the PBM format.  Version A-02: it accepts any number
\ of input files in the command line.
\
\ 2015-04-28: The extension of the output file name is not simply
\ appended any more but substitutes that of the input file name.
\
\ 2015-08-21: Created constant for the default color. Removed the
\ debugging code. Added installation instructions (the Galope library
\ is pending). Improved the text in `about`.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Requirements

forth definitions

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

require galope/unslurp-file.fs
require galope/minus-extension.fs

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ ZX Spectrum screen

\ A ZX Spectrum screen has two parts: first, the bitmap: 6144 bytes
\ that represent a 256x192 bitmap, with a special order; second, the
\ attributes: 768 bytes (32x24 character positions) that describe the
\ colors of every 8x8 square of the bitmap.

6144 constant /zxscr-bitmap
 768 constant /zxscr-attributes
/zxscr-bitmap /zxscr-attributes + constant /zxscr

/zxscr allocate throw constant zxscr

zxscr /zxscr-bitmap + constant zxscr-attributes

%00111000 constant color  \ default color: white paper, black ink
  \ bits 0..2 = ink
  \ bits 3..5 = paper
  \ bit 6     = bright
  \ bit 7     = flash

: init-zxscr  ( -- )
  \ Init the ZX Spectrum screen buffer.
  zxscr /zxscr-bitmap erase
  zxscr-attributes /zxscr-attributes color fill
  ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Bitmap converter

\ The ZX Spectrum screen bitmap is arranged in a very special way.
\ Bit-level calculations are used to calculate the destination address
\ of every input bitmap byte.

 32 constant chars/line   \ chars per line
192 constant heigth       \ pixels

/zxscr-bitmap 3 / constant /zxscr-third   \ bytes per third of the bitmap
256 constant chars/third                  \ chars per third

variable third        \ third of the bitmap (0..2)
variable char/third   \ character in the third (0..255)
variable char-row     \ character row (0..23)
variable char-col     \ character row (0..31)
variable char-scan    \ character scan (0..7)
variable pixel-row    \ pixel row (0..191); top row is 0

: >zxscr  ( +n -- ca )

  \ Convert a position in the input file PBM bitmap (0..6143)
  \ to its correspondent address in the SCR bitmap buffer.

  \ Calculate the required data from the input position.
  dup /zxscr-third / third !                \ bitmap third (0..2)
  dup chars/line /                          \ y row (0..191)
      dup %000111 and char-scan !           \ char scan (0..7)
          %111000 and 3 rshift pixel-row !  \ pixel row (0..7) ? XXX
      chars/line mod char-col !             \ char col (0..31)

  \ Calculate the correspondent position
  \ in the ZX Spectrum bitmap (0..6143).
  pixel-row @ 32 *  char-col @ +  \ low byte
  third @ 8 * char-scan @ +       \ high byte
  256 * +                         \ result

  dup /zxscr > abort" Bitmap bigger than 256x192"
  zxscr +  \ actual address in the output buffer

  ;

\ There are two variants of the PBM format: binary (P4 identifier) and
\ ASCII (P1 identifier).
\
\ The bitmap bytes of the binary variant are ready to be copied into
\ the ZX Spectrum bitmap, though in different positions.
\
\ The bitmap bytes of the ASCII variant are represented by eight
\ characters, so first they have to be calculated.

: byte  ( -- b )
  \ Get the next byte from the input file, currently being
  \ interpreted as a Forth source file.
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
  \ f = true if black pixel, false if white pixel.
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

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ PBM header interpreter

\ The header of a PBM file is ASCII, and their elements are separated
\ by ordinary text delimiters.
\
\ The chosen approach is to define Forth words with the name of the
\ expected header metadata, so the PBM file can be simply interpreted
\ as a Forth source file.

variable width? \ flag: has the width been found in the input file?

: check-type  ( -- )
  \ Abort if no file type was specified in the file header.
  ['] bitmap-byte defer@ ['] false =  \ `bitmap-byte` not set?
  abort" File type not supported"
  ;
: check-width  ( -- )
  \ Abort if the width was not found in the file header.
  width? @ 0= abort" The bitmap size must be 256x192"
  ;

wordlist constant pbm-wordlist  \ words allowed in the PBM file
pbm-wordlist set-current

\ Only five words are needed to interpret a 256x192 PBM file: the two
\ possible magic numbers, the line comment character, the width and
\ the heigth. The heigth is the last one in the file header and it
\ will do the conversion of the bitmap.

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

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ File converter

forth definitions

: working-dir  ( -- ca len )
  \ Current working directory.
  s" PWD" getenv
  ;
: working-dir+  ( ca1 len1 -- ca2 len2 )
  \ Add the current working directory to a file name.
  working-dir s" /" s+ 2swap s+
  ;
: save-scr  ( ca len -- )
  \ Save the SCR buffer to the output file.
  \ ca len = input file name
  -extension s" .scr" s+ zxscr /zxscr 2swap unslurp-file
  ;
: (pbm>scr)  ( ca len -- )
  \ Convert a 256x192 PBM file to a ZX Spectrum SCR file.
  \ ca len = input file name
  2>r  get-order
  init-zxscr pbm-wordlist >order seal
  2r@ working-dir+ included
  set-order  2r> save-scr
  ;
: about  ( -- )
  ." pbm2scr" cr
  ." PBM to ZX Spectrum SCR graphic converter" cr
  ." Version " version type cr
  ." http://programandala.net/en.program.pbm2scr.html" cr cr
  ." Copyright (C) 2015 Marcos Cruz (programandala.net)" cr cr
  ." Usage:" cr
  ."   pbm2scr input_file.pbm" cr
  ." Or (depending on the installation method):" cr
  ."   pbm2scr.fs input_file.pbm" cr cr
  ." The input file must be a 256x192 PBM image," cr
  ." in binary or ASCII variants of the format." cr cr
  ." The output file name will be the input file name" cr
  ." with the '.scr' extension instead of '.pbm'." cr
  ;
: input-files  ( -- n )
  \ Number of input files in the command line.
  argc @ 1-
  ;
: pbm>scr  ( -- )
  \ Convert 256x192 PBM files to ZX Spectrum SCR files.
  input-files ?dup
  if    0 do  i 1+ arg (pbm>scr)  loop
  else  about  then
  ;

pbm>scr bye
