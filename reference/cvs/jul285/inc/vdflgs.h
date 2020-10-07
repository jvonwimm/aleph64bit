C
C!  Bit flag definitions for VDET data banks
C
      INTEGER VB100U,VB050U,VBUNBD,VBUNMP,VBMULT
      INTEGER VBNOIS,VBDEAD,VBUNUS,VBLIN2
      INTEGER VBSUPP,VBOVER,VBHOTC,VBZERO
C
      PARAMETER (VBSUPP = 64)
      PARAMETER (VBOVER = 128)
      PARAMETER (VBMULT = 256)
      PARAMETER (VBZERO = 512)
      PARAMETER (VBLIN2 = 32768)
      PARAMETER (VB100U = 65536)
      PARAMETER (VB050U = 131072)
      PARAMETER (VBUNMP = 262144)
      PARAMETER (VBUNBD = 524288)
      PARAMETER (VBNOIS = 1048576)
      PARAMETER (VBDEAD = 2097152)
      PARAMETER (VBUNUS = 67108864)
      PARAMETER (VBHOTC = 16777216)
C
C  Online parameters; definition of bit fields in VPLH bank
C
      INTEGER OBOVER,OBSUPP,OBPULH,OBFBIT,OBEROR
      PARAMETER (OBOVER = 16384, OBSUPP = 32768, OBEROR = 8192)
      PARAMETER (OBPULH = 16383, OBFBIT = 49152)
#if defined(DOC)
C
C!  Bit assignment of mini-vertex region/strip flags
C       PARAMETER VBOVER  Pulseheight overflow
C       PARAMETER VBSUPP  Online suppressed channel
C       PARAMETER VB100U  100 micron bonding
C       PARAMETER VB200U  050 micron bonding
C       PARAMETER VBZERO  Channel whose pulseheight has been zeroed
C       PARAMETER VBUNMP  unmapped channel
C       PARAMETER VBUNBD  unbonded channel
C       PARAMETER VBNOIS  noisy channel (from offline analysis)
C       PARAMETER VBDEAD  dead channel
C       PARAMETER VBHOTC  Hot channel
C       PARAMETER VBUNUS  'Unuseable' channel
C
#endif
