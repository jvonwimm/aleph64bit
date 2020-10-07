      INTEGER FUNCTION LCSAMP(IPAD)
C--------------------------------------------------------------
C! Find trigger sector (amplifier) for an LC tower
C. - J.D.Hansen & P.Hansen - 860301
C. - modified : P.H.Hansen - 881101
C. - IN : IPAD = Tower addr.[ICOL + 16*(IROW-1) + 512*(MODU-1)]
C. - OUT: LCSAMP  = Amplifier No. [1,24]
C         New numbering scheme (881101):
C
C                       X   ^ Y
C           15 16        \\  |           3  4
C         14     17       \\ |         2      5
C        13       18       \\|        1        6
C   ------------    -------------------------    ------->Z
C        24       19        |\\       12       7
C         23    20          | \\       11     8
C           22 21           |  \\        10 9
C
C. - Called by  LCTRHT                           from this .HLB
C. - Calls      LCAMP                            from this .HLB
C ------------------------------------------------
      SAVE
      EXTERNAL LCAMP
C         macro to decode pad-address
        LCMOD(IXXXX) = (IXXXX-1)/512 + 1
        LCROW(IXXXX) = MOD(IXXXX-1,512)/16 + 1
        LCCOL(IXXXX) = MOD(IXXXX-1,16) + 1
C
C -------------------------------------------------------------
      LCSAMP = 0
C
C - Decode pad number
      MODU = LCMOD(IPAD)
      IFB  = (MODU-1)/2
      IY = LCROW(IPAD)
      IF (IY .LE. 0 .OR. IY .GT. 30) GOTO 999
      ICOL = LCCOL(IPAD)
      IF (ICOL .LE. 0 .OR. ICOL .GT. 16) GOTO 999
C
C - Reflect everything into quadrant 0
      IQUAD = MOD(MODU,2)
      IF (IY .GT. 15) THEN
        IROW  = IY - 15
      ELSE
        IQUAD = 3 - IQUAD
        IROW  = 16 - IY
      ENDIF
      LSAMP   = LCAMP(ICOL,IROW)
      IF (LSAMP .EQ. 0)                              GOTO 999
C
C - Encode amplifier
      IF (IQUAD .EQ. 1 .OR. IQUAD .EQ. 3) THEN
        LSAMP = 4 - LSAMP
      ENDIF
      LCSAMP    = 12*(1-IFB) + 3*IQUAD + LSAMP
  999 RETURN
      END
