*CD lcmacr
C         macro to decode pad-address
        LCMOD(IXXXX) = (IXXXX-1)/512 + 1
        LCROW(IXXXX) = MOD(IXXXX-1,512)/16 + 1
        LCCOL(IXXXX) = MOD(IXXXX-1,16) + 1
C
