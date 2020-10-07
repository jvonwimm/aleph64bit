*CD itspec
      COMMON /ITSPEC/ITB2BS,ITHCLU,ITTCLU,ITRTHR,ITHTHR(8),ITBTHR(8)
      INTEGER ITB2BS,ITRTHR,ITHTHR,ITBTHR
      LOGICAL ITHCLU,ITTCLU
C
#if defined(DOC)
   Settings for ITC special trigger bits.
      ITB2BS    = Back-to-back spread (0 - 3).
      ITHCLU    = Hit clustering used.
      ITTCLU    = Track clustering used.
      ITRTHR    = Threshold for 'Track bit'
      ITHTHR(I) = Threshold for hits in layer i.
      ITBTHR(I) = Threshold for background layer i.
#endif
