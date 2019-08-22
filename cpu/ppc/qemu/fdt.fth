purpose: Flattened device tree utilities
\ See license at end of file

1 VALUE fdt-debug
0 VALUE fdt-start

struct
  4 field >fdth_magic
  4 field >fdth_tsize
  4 field >fdth_struct_off
  4 field >fdth_string_off
  4 field >fdth_rsvmap_off
  4 field >fdth_version
  4 field >fdth_compat_vers
  4 field >fdth_boot_cpu
  4 field >fdth_string_size
  4 field >fdth_struct_size
constant /fdth

h# d00dfeed constant OF_DT_HEADER
h#        1 constant OF_DT_BEGIN_NODE
h#        2 constant OF_DT_END_NODE
h#        3 constant OF_DT_PROP
h#        4 constant OF_DT_NOP
h#        9 constant OF_DT_END

\ Create some variables early
0 value fdt-start-addr
0 value fdt-struct
0 value fdt-strings


\ FIXME: review the following words which were added to substitue
\ missing words compared to SLOF. Some are copied verbatim from SLOF
\ others may try to implement alternatives but could be wrong
\
\ definitely wrong/missing: claim disabled below in fdt-claim-reserve
\
\ may be wrong: find-node, and other related node words
\               decode-phys returns only one value?

: node>path " ?" ; \ only used for debugging and errors
: get-node current-device ;
: get-parent parent-device ;
: my-#address-cells my-#adr-cells ;

CREATE strcatpad 400 allot
: strcat ( str1 len1 str2 len2 -- str3 len3 )
   >r >r dup >r strcatpad swap move
   r> dup strcatpad + r> swap r@ move
   r> + strcatpad swap ;

: generic-decode-unit ( str len ncells -- addr.lo ... addr.hi )
  dup >r -rot BEGIN r@ WHILE r> 1- >r [char] , left-parse-string
  $number IF 0 THEN r> swap >r >r REPEAT r> 3drop
  BEGIN dup WHILE 1- r> swap REPEAT drop ;

: generic-encode-unit ( addr.lo ... addr.hi ncells -- str len )
  0 0 rot ?dup IF 0 ?DO rot (u.) strcat s" ," strcat LOOP 1- THEN ;

: hex-decode-unit ( str len ncells -- addr.lo ... addr.hi )
  base @ >r hex generic-decode-unit r> base ! ;

: hex-encode-unit ( addr.lo ... addr.hi ncells -- str len )
  base @ >r hex generic-encode-unit r> base ! ;

\ functions to review end here, also see fdt-claim-reserve below


: fdt-init ( fdt-start -- )
    dup to fdt-start-addr
    dup dup >fdth_struct_off l@ + to fdt-struct
    dup dup >fdth_string_off l@ + to fdt-strings
    drop
;

\ Dump fdt header for all to see and check FDT validity
: fdt-check-header ( -- )
    fdt-start-addr dup 0 = IF
        ." No flat device tree !" cr drop -1 throw EXIT THEN
    hex
    fdt-debug IF
        ." Flat device tree header at 0x" dup . s" :" type cr
        ."  magic            : 0x" dup >fdth_magic l@ . cr
        ."  total size       : 0x" dup >fdth_tsize l@ . cr
        ."  offset to struct : 0x" dup >fdth_struct_off l@ . cr
        ."  offset to strings: 0x" dup >fdth_string_off l@ . cr
        ."  offset to rsvmap : 0x" dup >fdth_rsvmap_off l@ . cr
        ."  version          : " dup >fdth_version l@ decimal . hex cr
        ."  last compat vers : " dup >fdth_compat_vers l@ decimal . hex cr
        dup >fdth_version l@ 2 >= IF
            ."  boot CPU         : 0x" dup >fdth_boot_cpu l@ . cr
        THEN
        dup >fdth_version l@ 3 >= IF
            ."  strings size     : 0x" dup >fdth_string_size l@ . cr
        THEN
        dup >fdth_version l@ 11 >= IF
            ."  struct size      : 0x" dup >fdth_struct_size l@ . cr
        THEN
    THEN
    dup >fdth_magic l@ OF_DT_HEADER <> IF
        ." Flat device tree has incorrect magic value !" cr
	drop -1 throw EXIT
    THEN
    dup >fdth_version l@ 10 < IF
        ." Flat device tree has usupported version !" cr
	drop -1 throw EXIT
    THEN

    drop
;

\ Fetch next tag, skip nops and increment address
: fdt-next-tag ( addr -- nextaddr tag )
  0	       	      	 	( dummy tag on stack for loop )
  BEGIN
    drop			( drop previous tag )
    dup l@			( read new tag )
    swap 4 + swap		( increment addr )
  dup OF_DT_NOP <> UNTIL 	( loop until not nop )
;

\ Parse unit name and advance addr
: fdt-fetch-unit ( addr -- addr $name )
  cscount  \ convert cstring:
  2dup + 1 + 3 + fffffffc and -rot
;

\ Update unit with information from the reg property...
\ ... this is required for the PCI nodes for example.
: fdt-reg-unit ( prop-addr prop-len -- )
  decode-phys               ( prop-addr' prop-len' phys.lo ... phys.hi )
  set-my-unit               ( prop-addr' prop-len' )
  2drop
;

\ Lookup a string by index
: fdt-fetch-string ( index -- str-addr str-len )
  fdt-strings + cscount
;

: fdt-create-dec  s" decode-unit" $CREATE , DOES> @ hex-decode-unit ;
: fdt-create-enc  s" encode-unit" $CREATE , DOES> @ hex-encode-unit ;

\ Check whether array contains a zero-terminated ASCII string:
: fdt-prop-is-string?  ( addr len -- string? )
  dup 1 < IF 2drop FALSE EXIT THEN                \ Check for valid length
  1-
  2dup + c@ 0<> IF 2drop FALSE EXIT THEN          \ Check zero-termination
  0 ?DO
    dup i + c@                     \ Get character / byte at current index
    20 7e between not IF           \ Is it out of range 32 to 126 (=ASCII)
      drop FALSE UNLOOP EXIT       \ FALSE means: No ASCII string
    THEN
  LOOP
  drop TRUE    \ Only ASCII found --> it is a string
;

\ Encode fdt property to OF property
: fdt-encode-prop  ( addr len -- )
  2dup fdt-prop-is-string? IF
    1- encode-string
  ELSE
    encode-bytes
  THEN
;

\ Method to unflatten a node
: fdt-unflatten-node ( start -- end )
  \ this can and will recurse
  recursive

  \ Get & check first tag of node
  fdt-next-tag dup OF_DT_BEGIN_NODE <> IF
    " Weird tag 0x" type . " at start of node" type cr
    -1 throw
  THEN drop

  \ Parse name, split unit address
  fdt-fetch-unit
  dup 0 = IF
    2drop " /" root-device extend-package
  ELSE 2dup " memory" $= IF
    2dup locate-device IF new-device ELSE push-device extend-package THEN
  ELSE
    new-device
  THEN THEN
  fdt-debug IF ." Creating node: " 2dup type cr THEN
  [char] @ left-parse-string
  \ Set name to string before @
  device-name

  \ Set preliminary unit address - might get overwritten by reg property
  dup IF
     " #address-cells" get-parent get-package-property IF
        2drop
     ELSE
        decode-int nip nip
	hex-decode-unit
	set-my-unit
     THEN
  ELSE 2drop THEN

  \ Iterate sub tags
  BEGIN
    fdt-next-tag dup OF_DT_END_NODE <>
  WHILE
    dup OF_DT_PROP = IF
      \ Found property
      drop dup			( drop tag, dup addr     : a1 a1 )
      dup l@ dup rot 4 +	( fetch size, stack is   : a1 s s a2)
      dup l@ swap 4 +		( fetch nameid, stack is : a1 s s i a3 )
      rot			( we now have: a1 s i a3 s )
      fdt-encode-prop rot	( a1 s pa ps i)
      fdt-fetch-string		( a1 s pa ps na ns )
      fdt-debug IF ." Adding prop: " 2dup type cr THEN
      2dup " reg" $= IF
          2swap 2dup fdt-reg-unit 2swap
      THEN
      property
      + 8 + 3 + fffffffc and
    ELSE dup OF_DT_BEGIN_NODE = IF
      drop			( drop tag )
      4 -
      fdt-unflatten-node
    ELSE
      drop -1 throw
    THEN THEN
  REPEAT drop \ drop tag

  \ Create encode/decode unit
  " #address-cells" get-node get-package-property IF ELSE
    decode-int dup fdt-create-dec fdt-create-enc 2drop
  THEN

  fdt-debug IF ." End node" cr THEN
  finish-device
;

\ Start unflattening
: fdt-unflatten-tree
    fdt-debug IF ." Unflattening device tree..." cr THEN
    fdt-struct fdt-unflatten-node drop
    fdt-debug IF ." Done !" cr THEN
;

\ Find memory size
: fdt-parse-memory
    \ XXX FIXME Handle more than one memory node, and deal
    \     with RMA vs. full access
    " /memory@0" find-device
    " reg" get-node get-package-property IF
      ." Could not find memory reg property" cr
      throw -1
    THEN

    \ XXX FIXME Assume one entry only in "reg" property for now
    decode-phys
    drop decode-int
    my-#address-cells 1 > IF 20 << or THEN

    fdt-debug IF dup ." Memory size: " . cr THEN
    \ claim.fs already released the memory between 0 and MIN-RAM-SIZE,
    \ so we've got only to release the remaining memory now:
    drop \    MIN-RAM-SIZE swap MIN-RAM-SIZE - release
    2drop device-end
;

\ Claim fdt memory and reserve map
\ : fdt-claim-reserve
\    fdt-debug IF ." Claming reserved areas" cr THEN
\    fdt-start-addr
\    dup dup >fdth_tsize l@ 0 claim drop
\    dup >fdth_rsvmap_off l@ +
\    BEGIN
\        dup dup be-x@ swap 8 + be-x@
\	dup 0 <>
\    WHILE
\	fdt-debug IF 2dup swap ." Reserve map entry: " . ." : " . cr
\	THEN
\	0 claim drop
\	10 +
\    REPEAT drop drop drop
\ ;

\ The following functions are use to replace the FDT phandle and
\ linux,phandle properties with our own OF1275 phandles...

\ This is used to check whether we successfully replaced a phandle value
0 VALUE (fdt-phandle-replaced)

\ Replace phandle value in "interrupt-map" property
: fdt-replace-interrupt-map  ( old new prop-addr prop-len -- old new )
   BEGIN
      dup                    ( old new prop-addr prop-len prop-len )
   WHILE
      \ This is a little bit ugly ... we're accessing the property at
      \ hard-coded offsets instead of analyzing it completely...
      swap dup 10 +          ( old new prop-len prop-addr prop-addr+10 )
      dup l@ 5 pick = IF
          \ it matches the old phandle value!
          3 pick swap l!
          TRUE TO (fdt-phandle-replaced)
      ELSE
          drop
      THEN
      ( old new prop-len prop-addr )
      1c + swap 1c -
      ( old new new-prop-addr new-prop-len )
   REPEAT
   2drop
;

: (fdt-replace-phandles) ( old new propname propnamelen node -- )
    get-package-property IF 2drop EXIT THEN
    BEGIN
        dup
    WHILE                   ( old new prop-addr prop-len )
        over l@
        4 pick = IF
            2 pick 2 pick l! \ replace old with new in place
            TRUE TO (fdt-phandle-replaced)
        THEN
        4 - swap 4 + swap
    REPEAT
    2drop 2drop
;

\ Replace one phandle "old" with a phandle "new" in "node" and recursively
\ in its child nodes:
: fdt-replace-all-phandles ( old new node -- )
   \ ." Replacing in " dup node>path type cr
   >r
   " interrupt-map" r@ get-package-property 0= IF
      ( old new prop-addr prop-len  R: node )
      fdt-replace-interrupt-map
   THEN

   2dup " interrupt-parent" r@ (fdt-replace-phandles)
   2dup " memory-region" r@ (fdt-replace-phandles)

   \ ... add more properties that have to be fixed here ...
   r>
   \ Now recurse over all child nodes:       ( old new node )
   child BEGIN
      dup
   WHILE
      3dup RECURSE
      PEER
   REPEAT
   3drop
;

\ Replace one FDT phandle "val" with a OF1275 phandle "node" in the
\ whole tree:
: fdt-update-phandle ( val node -- )
   >r
   FALSE TO (fdt-phandle-replaced)
   r@ " /" find-node               ( val node root )
   fdt-replace-all-phandles
   (fdt-phandle-replaced) IF
      r@ push-device
      " phandle" delete-property
      " linux,phandle" delete-property
   ELSE
      diagnostic-mode? IF
         cr ." Warning: Did not replace phandle in " r@ node>path type cr
      THEN
   THEN
r> drop
;

\ Check whether a node has "phandle" or "linux,phandle" properties
\ and replace them:
: fdt-fix-node-phandle  ( node -- )
   >r
   " phandle" r@ get-package-property 0= IF
      decode-int nip nip
      fdt-debug IF ." found phandle: " dup . cr THEN
      r@ fdt-update-phandle
   THEN
   r> drop
;

\ Recursively walk through all nodes to fix their phandles:
: fdt-fix-phandles  ( node -- )
   fdt-debug IF ." Fixing up phandles" cr THEN
   \ ." fixing phandles of " dup node>path type cr
   dup fdt-fix-node-phandle
   child BEGIN
      dup
   WHILE
      dup RECURSE
      PEER
   REPEAT
   drop
   device-end
;

: fdt-doit
    \ Bail out if no fdt
    fdt-start 0 = IF
        ." No fdt-start" cr
        -1 throw
    THEN
    fdt-start fdt-init
    fdt-check-header
    fdt-unflatten-tree
fdt-debug IF .s cr THEN
    show-devs
    fdt-parse-memory
fdt-debug IF .s cr THEN
\    fdt-claim-reserve
\    " /" find-node fdt-fix-phandles
fdt-debug IF .s cr THEN
;

\ LICENSE_BEGIN
\ *****************************************************************************
\ * Copyright (c) 2011 IBM Corporation
\ * All rights reserved.
\ * This program and the accompanying materials are made available under the
\ * terms of the BSD License below and is available at
\ * http://www.opensource.org/licenses/bsd-license.php
\ *
\ * Contributors:
\ *     IBM Corporation - initial implementation
\ *     BALATON Zoltan, 2019 - Changes for porting to OpenFirmware
\ ****************************************************************************/
\ * Copyright (c) 2004, 2008 IBM Corporation
\ * All rights reserved.
\ *
\ * Redistribution and use in source and binary forms, with or without
\ * modification, are permitted provided that the following conditions are met:
\ *
\ * Redistributions of source code must retain the above copyright notice,
\ * this list of conditions and the following disclaimer.
\ *
\ * Redistributions in binary form must reproduce the above copyright notice,
\ * this list of conditions and the following disclaimer in the documentation
\ * and/or other materials provided with the distribution.
\ *
\ * Neither the name of IBM nor the names of its contributors may be used to
\ * endorse or promote products derived from this software without specific
\ * prior written permission.
\ *
\ * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
\ * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
\ * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
\ * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
\ * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
\ * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
\ * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
\ * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
\ * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
\ * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
\ * POSSIBILITY OF SUCH DAMAGE.
\ ****************************************************************************/
\ LICENSE_END
