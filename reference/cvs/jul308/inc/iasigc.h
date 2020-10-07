      PARAMETER (JTRKIA=2)
      INTEGER NCMNIA,NSMNIA
      REAL SMAXIA,ADJCIA
      COMMON /IASIGC/SMAXIA,ADJCIA,NHMNIA,NCMNIA(JTRKIA),NSMNIA(JTRKIA),
     *               XISFIL,FJUNCU
#if defined(DOC)
C!  Cuts used in reassignment of coords. to tracks - sub. IASIGN
C  SMAXIA : Max. allowed no. of sigmas for inclusion of coord. on trk.
C  ADJCIA : Fraction of cell beyond which to include next cell.
C  NHMNIA : Min. no. of latched cells for reassignment to be tried.
C  NCMNIAi: Min. no of coords. allowed on 1st arc of track after
C           reassignment (for track banks number i)
C  NSMNIAi: Min. no of coords. allowed on spiral of track after
C           reassignment (for track banks number i)
C  XISFIL : Xis for filter cut in ITCCUT
C  FJUNCU : Factor times sum of track sigma to below which throw away
C           hits assigned to 2 tracks in ITCCUT
#endif
