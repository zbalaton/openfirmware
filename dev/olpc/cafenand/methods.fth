\ See license at end of file
purpose: interface methods for CaFe NAND controller

external

: dma-alloc  ( len -- adr )  " dma-alloc" $call-parent  ;

: dma-free  ( adr len -- )  " dma-free" $call-parent  ;

: close  ( -- )
   soft-reset unmap-regs
   dma-buf-va  ?dup  if
      dma-buf-va dma-buf-pa /dma-buf  " dma-map-out" $call-parent
      /dma-buf dma-free
   then
;

: size  ( -- d )  pages/chip /page um*  ;

: open  ( -- okay? )
   map-regs
   init
   configure 0=  if  false exit  then

   /dma-buf dma-alloc to dma-buf-va
   dma-buf-va /dma-buf false " dma-map-in" $call-parent to dma-buf-pa

   " lmove" $find  0=  if  ['] move  then  to do-lmove

   get-bbt

   my-args  dup  if   ( arg$ )
      " jffs2-file-system" find-package  if  ( arg$ xt )
         interpose  true   ( okay? )
      else                 ( arg$ )
         ." Can't find jffs2-file-system package" cr
         2drop  false      ( okay? )
      then                 ( okay? )
   else                    ( arg$ )
      2drop  true          ( okay? )
   then                    ( okay? )
;

\ Establish the NAND timings regardless of whether the device is
\ ever opened, so the OS driver doesn't have to worry about it.
\ Fortunately, for all the NAND chips we have considered so far,
\ the same timing set is appropriate.

: probe  ( -- )  map-regs  timing-configure  unmap-regs  ;
probe

headers

\ LICENSE_BEGIN
\ Copyright (c) 2006 FirmWorks
\ 
\ Permission is hereby granted, free of charge, to any person obtaining
\ a copy of this software and associated documentation files (the
\ "Software"), to deal in the Software without restriction, including
\ without limitation the rights to use, copy, modify, merge, publish,
\ distribute, sublicense, and/or sell copies of the Software, and to
\ permit persons to whom the Software is furnished to do so, subject to
\ the following conditions:
\ 
\ The above copyright notice and this permission notice shall be
\ included in all copies or substantial portions of the Software.
\ 
\ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
\ EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
\ MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
\ NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
\ LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
\ OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
\ WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
\
\ LICENSE_END
