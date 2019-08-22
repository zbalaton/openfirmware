purpose: Hardware init for QEMU pegasos2 emulation
\ See license at end of file

hex

f100.0000 constant mv64361-base   \ Needs to be mapped to a VA

: mv-b@  ( offset -- byte )  mv64361-base + rb@  ;
: mv-b!  ( byte offset -- )  mv64361-base + rb!  ;
: mv-w@  ( offset -- byte )  mv64361-base + rw@  ;
: mv-w!  ( byte offset -- )  mv64361-base + rw!  ;
: mv-l@  ( offset -- byte )  mv64361-base + rl@  ;
: mv-l!  ( byte offset -- )  mv64361-base + rl!  ;

." Adding PCI host bus @ 80000000" cr
0 0 " 80000000" " /" begin-package
\ fload ${BP}/cpu/ppc/prep/mappci.fth
: map-pci-phys  ( paddr size io? -- vaddr )
." map-pci-phys:" .s cr
  if  2  else  3  then              ( paddr size parent-space )
  swap " map-in" $call-parent       ( vaddr )
;
: >pci-devaddr  ( root-devaddr -- pci-devaddr )
\   chrp?  0=  if  h# 8000.0000  +  then
;
: pci-devaddr>  ( pci-devaddr -- root-devaddr )
\   chrp?  0=  if  h# 8000.0000  -  then
;

fload ${BP}/dev/pcibus.fth

: config-map  ( config-adr -- port )
  dup  3 invert and  h# 8000.0000 or  h# c78 mv-l!  ( config-adr )
  3 and  h# c7c or
;

: config-l@  ( config-addr -- l )  config-map mv-l@  ;
: config-l!  ( l config-addr -- )  config-map mv-l!  ;
: config-w@  ( config-addr -- w )  config-map mv-w@  ;
: config-w!  ( w config-addr -- )  config-map mv-w!  ;
: config-b@  ( config-addr -- c )  config-map mv-b@  ;
: config-b!  ( c config-addr -- )  config-map mv-b!  ;

\ \  ---PCI Address---  ---Host Addr----   --- size ---
\ \ phys.hi  .mid .low     phys           .hi      .lo
0 0 encode-bytes
  0000.0000 +i 0+i 0+i           0+i     0+i 0+i  \ ISA I/O
  0100.0000 +i 0+i 0+i  fe00.0000 +i     0+i 0100.0000 +i  \ PCI I/O
  0200.0000 +i 0+i 0+i  8000.0000 +i     0+i 8000.0000 +i  \ PCI Mem
" ranges" property
end-package

also forth definitions
" 1,c" dup config-string pci-probe-list
previous definitions

: map-v=p  ( phys size -- )
  2dup 0  mmu-claim drop   ( phys size )
  over swap  -1  mmu-map   ( )
;

stand-init: PCI host bridge
  mv64361-base 10000 map-v=p
  " /pci" " init" execute-device-method drop
;

\ FIXME there's also another pci bus but we don't have anyting on that

." Done" cr

\ LICENSE_BEGIN
\ Copyright (c) 2007 FirmWorks
\ Copyright (c) 2019 BALATON Zoltan
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

