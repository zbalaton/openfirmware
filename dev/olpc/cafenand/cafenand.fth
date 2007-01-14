\ See license at end of file
purpose: Driver for the NAND FLASH section of the OLPC CaFe chip

" nandflash" device-name
" olpc,cafenand" model
" disk" device-type

h# 4000 constant /regs

my-address my-space               encode-phys
    0 encode-int encode+  h# 0 encode-int encode+

my-address my-space h# 200.0010 + encode-phys encode+
    0 encode-int encode+  /regs encode-int encode+

" reg" property


: my-w@  ( offset -- w )  my-space +  " config-w@" $call-parent  ;
: my-w!  ( w offset -- )  my-space +  " config-w!" $call-parent  ;

0 instance value chip

: cl!  ( l adr -- )  chip + rl!  ;
: cl@  ( adr -- l )  chip + rl@  ;
: cw!  ( w adr -- )  chip + rw!  ;
: cw@  ( adr -- w )  chip + rw@  ;
: cb!  ( b adr -- )  chip + rb!  ;
: cb@  ( adr -- b )  chip + rb@  ;

: map-regs ( -- )
   0 0  h# 0200.0010 my-space +  /regs " map-in" $call-parent to chip
   4 my-w@  6 or  4 my-w!
;

: unmap-regs
   chip /regs " map-out" $call-parent
\   4 my-w@  6 invert and  4 my-w!  \ No need to turn it off
;

h#     800 instance value /page
h#  2.0000 instance value /eblock

h# 40 constant /oob
h# e constant /ecc
h# e constant bb-offset  \ Location of bad-block table signature in OOB data

\ This resets the NAND controller in case the DMA gets hung or something
: soft-reset  ( -- )  1 h# 3034 cl!  0 h# 3034 cl!  ;

: timing-configure  ( -- )
   \ The following timing values are calculated from the Hynix and Samsung
   \ datasheets based on a clock cycle time of 10.4 nS.
   h# 1010.0900 h# 24 cl!  \ Timing1
   h#    1.0101 h# 28 cl!  \ Timing2
   h# 1000.0000 h# 2c cl!  \ Timing3
;
[ifdef] notdef
: sloppy-timing  ( -- )
   h# ffff.ffff h# 24 cl!  \ Timing1
   h# ffff.ffff h# 28 cl!  \ Timing2
   h# ffff.ffff h# 2c cl!  \ Timing3
;
[then]

: clr-ints  ( -- )   h# ffff.ffff h# 10 cl!  ;

\ Wait not busy - XXX need timeout
: ctrl-wait  ( -- )  begin  h# c cl@  h# 8000.0000 and 0=  until  ;

\ Think of col as the offset within a disk block, and row as the block#
: set-address  ( row col -- )  h# 1c cl!  h# 20 cl!  ;

: >cmd  ( cmd# #nonmem #address-bytes -- cmdval )
   dup  if  1- d# 27 lshift h# 4000.0000 or  then  ( cmd# #nm adr-field )
   swap 7 and d# 22 lshift  or     ( cmd# nm,adr )
   or  h# 8000.0000 or
;

\   cmd      #nonmem
\              #adr
\ h#        90 4 1 >cmd constant read-id-cmd      \ Not needed
h#   20.0070 1 0 >cmd constant read-status-cmd
h# 0420.0000 0 5 >cmd constant read-cmd
h# 0220.0080 0 5 >cmd constant write-cmd

: wait-dma-done  ( -- )
   begin
      h# 10 cl@  h# 1000.0000 and
   until
   h# 1000.0000 h# 10 cl!   \ Clear DMA done
;

: wait-done  ( -- )
   begin
      h# 10 cl@  h# 8000.0000 and
   until
   h# 8000.0000 h# 10 cl!   \ Clear done
;

\ Control3 - no reset, no BAR write protect
: write-disable  ( -- )  0 8 cl!  ;
: write-enable  ( -- )  h# 4000.0000 8 cl!  ;

: cmd  ( n -- )  0 cl!  wait-done  ;
: cmd2  ( n -- )  4 cl!  ;
: datalen  ( n -- )  h# 18 cl!  ;
: read-status  ( -- b )  read-status-cmd cmd  h# 30 cl@ h# ff and  ;
\ : read-id  ( -- )  0 0 set-address  read-id-cmd cmd  h# 30 cl@  ;
: dma-off  ( -- )  0 h# 40 cl!  ;

: wait-write-done  ( -- )
   0               ( status )
   begin           ( status )
     drop          ( )
     read-status   ( status )
     dup h# 40 and ( status flag )
   until           ( status )
   \ If the value is completely 0 I think it means write protect     
   1 and  if  ." NAND write error" cr  then
;

\ Assumes that the range doesn't straddle a page boundary
: generic-read  ( len page# offset cmd cmd2 -- chip-adr )
   cmd2 >r                   ( len page# offset r: cmd )
   set-address  dma-off      ( len r: cmd )
   datalen                   ( r: cmd )
   r> cmd                    ( )
   chip h# 1000 +            ( adr )
;
: pio-read  ( adr len page# offset -- )
   2 pick >r
   read-cmd h# 130 generic-read        ( adr chip-adr r: len )
   swap r> move                        ( )
;

: pio-write-raw  ( adr len page# offset -- )
   write-enable
   dma-off  set-address  dup datalen   ( adr len )
   chip h# 2000 +  swap  move          ( )
   h# 2000.0110 cmd2  write-cmd  cmd   ( ) \ 2000. 2K page, (No Auto ECC)
   wait-write-done
   write-disable
;

: pio-write-page  ( adr page# -- )
   write-enable  dma-off               ( adr page# )
   0 set-address                       ( adr )
   /page h# e +  dup datalen           ( adr len )
   chip h# 2000 +  swap  move          ( )
   h# 6800.0110 cmd2  write-cmd  cmd   ( ) \ 4000. Auto ECC, 2000. 2K page, 0800 R/S ECC
   wait-write-done
   write-disable
;

: read-oob  ( page# -- adr )
   /oob  swap  h# 800  read-cmd  h# 130 generic-read
;

: read-rst     ( -- )  h# 8000.0000 h# c cl!  ;

0 instance value dma-vadr
0 instance value dma-padr
0 instance value dma-len

: dma-setup  ( adr #bytes #ecc direction-in? -- )
   >r                       ( adr #bytes #ecc )
   datalen                  ( adr #bytes )
   over to dma-vadr         ( adr #bytes )     \ Remember for later
   dup  to dma-len          ( adr #bytes )     \ Remember for later
   tuck false " dma-map-in" $call-parent  ( #bytes padr )  \ Prepare DMA buffer
   dup to dma-padr          ( #bytes padr )           \ Remember for later
   h# 44 cl!  0 h# 48 cl!   ( #bytes )                \ Set address
   r> if  h# a000.0000  else  h# 8000.0000  then  ( bits )
   or h# 40 cl!
;

: dma-release  ( -- )
   dma-vadr dma-padr dma-len  " dma-map-out" $call-parent
;

: slow-dma-read  ( adr len page# offset -- )
   set-address
   dup  true dma-setup                 ( )
   h# 130 cmd2  read-cmd  0 cl!        ( adr chip-adr r: len )
   \ XXX put a 90 us delay here so we don't start hitting the register
   \ until almost done.
   wait-dma-done  \ For DMA reads we wait for DMA completion instead of cmd
   dma-release                         ( )
;

: /dma-buf  ( -- n )  /page /oob +  ;
0 instance value dma-buf-pa
0 instance value dma-buf-va
defer do-lmove

\ XXX should check ECC
: read-page  ( adr page# -- )
   h# 20 cl!   0 h# 1c cl!  
   /page ( /oob + ) h# 18 cl!          ( adr )
   dma-buf-pa h# 44 cl!                ( adr )
   0 h# 48 cl!                         ( adr )
   /page h# a000.0000 or  h# 40 cl!    ( adr )
   h# 130 4 cl!  read-cmd  0 cl!       ( adr )
   wait-dma-done  \ For DMA reads we wait for DMA completion instead of cmd
   dma-buf-va swap /page do-lmove      ( )
;

: dma-write-raw  ( adr len page# offset -- )
   write-enable                          ( adr len page# offset )
   set-address                           ( adr len )
   dup  false dma-setup                  ( )
   h# 2000.0110 cmd2  write-cmd  cmd     ( )
   wait-write-done
   dma-release
   write-disable
;

: dma-write-page  ( adr page# -- )  \ Size is fixed
   write-enable                          ( adr page# )
   0 set-address                         ( adr )
   h# 800 h# 80e  false dma-setup        ( )
   h# 6800.0110 cmd2  write-cmd  cmd     ( )  \ Auto-ECC, 2KB, RS, write cmd
   wait-write-done
\   dma-release
   write-disable
;

\ : read-page    ( adr page# -- )  dma-read-page   ;
: write-page   ( adr page# -- )  dma-write-page  ;
: write-bytes  ( adr len page# offset -- )  pio-write-raw  ;

3 value #erase-adr-bytes  \ Chip dependent
: erase-block  ( page# -- )
   write-enable
   lwsplit swap set-address  \  Fiddle the block number
   h# 1d0 cmd2
   h# 20.0060 0 #erase-adr-bytes >cmd cmd
   wait-write-done
   write-disable
;

: read-id  ( -- adr )  8  0 0  h# c420.0090  0  generic-read  ;

: send-reset-cmd  ( -- )
   ctrl-wait
   h# 8000.00ff  cmd   \ NAND Reset command
;

: init  ( -- )
   0              0 cl!   \ Clear command register
   h# 2000.0000   4 cl!   \ Page 2KB
   write-disable

   send-reset-cmd

   0 h# 14 cl!  \ Interrupts off
   clr-ints
;

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
