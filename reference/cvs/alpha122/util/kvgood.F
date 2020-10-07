      INTEGER FUNCTION KVGOOD(DUMMY)
CKEY VDET / USER
C----------------------------------------------------------------------
C!   Check readout state of VDET.
C!
C!   Author   : HCJ Seywerd            7-Oct-1992
C!   Modified : S Haywood              9-Nov-1992
C!              h.seywerd more data   02-Feb-1994
C!              h.seywerd 95 data     30-Nov-1995
C!
C!   Outputs:  0 if in a period when readout was GOOD; otherwise
C!            +1 if HV appears to be ON; or
C!            -1 if HV appears to be OFF.
C!
C!            If the readout status is GOOD, Vdet information is read
C!            from ALEPH when the HV is ON and is not read out when OFF.
C!            If the readout status is BAD, the Vdet information may be
C!            read. If the Vdet is ON, this is fine; however if it is
C!            OFF, spurious noise hits will be produced.
C!            For MC always in GOOD state.
C!
C!            Periods when the status is BAD and the Vdet is OFF have
C!            been identified from the Vdet noise (number of hits in
C!            VFHL bank).
C!            In these cases, KVGOOD is -1 and FRFT/2 tracks are dubious
C!            - this means the tracks stored on the Mini are dubious.
C!======================================================================
#ifndef DOC
      PARAMETER (NPER=9,NOFF=120)
      DIMENSION LPERF(NPER),LPERL(NPER)
      DIMENSION LRUN(NOFF),LEVTF(NOFF),LEVTL(NOFF)
      DATA LPERF / 10000, 14208, 14952, 15142, 20258, 23029, 37690,
     &             37726, 37779 /
      DATA LPERL / 12089, 14514, 14952, 15144, 21683, 23317, 37690,
     &             37726, 37784/
      DATA LRUN /
     & 10443, 10596, 10608, 10608, 10627, 10768,
     & 10857, 10890, 11012, 11049, 11055, 11064,
     & 11069, 11114, 11162, 11162, 11162, 11165,
     & 11236, 11237, 11237, 11238, 11245, 11252,
     & 11261, 11281, 11281, 11288, 11291, 11294,
     & 11302, 11307, 11311, 11313, 11318, 11361,
     & 11370, 11380, 11381, 11387, 11415, 11420,
     & 11469, 11494, 11500, 11501, 11510, 11521,
     & 11525, 11527, 11530, 11541, 11541, 11548,
     & 11557, 11566, 11569, 11583, 11584, 11590,
     & 11601, 11612, 11614, 11615, 11624, 11626,
     & 11633, 11644, 11644, 11654, 11656, 11656,
     & 11670, 11841, 11880, 11961, 11969, 12022,
     & 14208, 14209, 14214, 14214, 14214, 14270,
     & 14272, 14306, 14311, 14312, 14312, 14336,
     & 14468,
     & 14952, 14952, 14952, 15142, 15143, 15144,
     & 20258, 20600, 20602, 20767, 20858, 21215,
     & 21356, 21577, 21683, 23029, 23029, 23031,
     & 23281, 23317,
     & 37690, 37726, 37726, 37779, 37780, 37781,
     & 37782, 37783, 37784/
      DATA LEVTF /
     &  2413,  1415,  2162,  2944,  3545,  3176,
     &  2019,  2517,     1,     6,    11,   356,
     &  1700,  1669,   246,   675,   736,     9,
     &     3,   272,  2888,    10,  3229,    52,
     &  3432,   776,  1148,  1694,     2,     4,
     &  6328,     1,  5012,  1548,   549,     4,
     &  3448,  4022,    14,  3211,     7,  3454,
     &  1193,     6,     9,     7,     4,     1,
     &     5,     1,    17,     2,  2410,     1,
     &    16,     9,     4,    96,     1,  3650,
     &   225,    14,  4110,     8,     3,   382,
     &     1,  3865,  4320,    17,     3,  3950,
     &     8,    13,    27,  5302,     1,  2086,
     &  1385,  3316,    23,   177,   901,   921,
     &   489,  4334,   350,   461,  4285,  1102,
     &     3,
     &     1,  3814,  5023,  4504,     1,     1,
     &    36,  5876,  1178,     8,   596,   461,
     &     1,   209,  6099,    27,  9943,     5,
     &    40,  3019,
     & 31588,     1, 22165, 99999, 99999, 99999,
     & 99999, 99999, 99999/
      DATA LEVTL /
     &  2416,  1415,  2275,  2945,  3545,  3176,
     &  2019,  2518,   477,     6,  1135,   356,
     &  1702,  1674,   246,   675,   736,   205,
     &   309,   497,  3142,   380,  3234,    53,
     &  3434,   777,  2343,  1700,   377,   409,
     &  6459,   675,  5018,  1590, 99999,   341,
     &  3448,  4957,  2061,  3213,   460,  3456,
     &  1198,    64,    40,    48,   321,   640,
     &    64,    20,    19,   200,  2910,   440,
     &   500,   909,    60,  2464,  1228,  4058,
     &   817,   587,  4612,   155,   913,   383,
     &   400,  3923,  4391,   301,   499,  3950,
     &   440,   188,   281,  5302,   271,  2089,
     &  2280,  4187,    31,   206,  1219,  1092,
     &   688,  4347,  3242,   503,  4286,  2335,
     &    56,
     &   469,  4120,  5173, 99999, 99999, 99999,
     &    66,  5914,  1198,   101,   611,   714,
     &   243,   315,  6219,   227, 10269,    51,
     &    84,  3054,
     & 31811,  2435, 99999, 99999, 99999, 99999,
     & 99999, 99999, 99999/
C
C----------------------------------------------------------------------
      KVGOOD = 0
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.LT.2000) RETURN
C
C++   See if we are in a period of readout problems; if not, return.
C
      DO 10 IPER=1,NPER
   10 IF (IRUN.GE.LPERF(IPER) .AND. IRUN.LE.LPERL(IPER)) KVGOOD = +1
      IF (KVGOOD.EQ.0) RETURN
C
C++   We are in a period of readout problems; see if HV is off.
C
      DO 20 IOFF=1,NOFF
         IF (IRUN.LT.LRUN(IOFF)) RETURN
         IF (IRUN.EQ.LRUN(IOFF)) THEN
            IF (IEVT.GE.LEVTF(IOFF) .AND. IEVT.LE.LEVTL(IOFF))
     &        KVGOOD = -1
         ENDIF
   20 CONTINUE
C
      RETURN
      END
#endif
