CKEY XLUMOK 
C! XLUMOK package
 ! 981803 correction file no.3 for ALEPHLIB 31.0
     XHVFIX : Fixes to varaiable declarations (A.Waananen)

 ! 981703 correction file no.2 for ALEPHLIB 31.0
     XHVFIX : Bug fix, database was initialised with period number
              instead of run number                                (M.Cattaneo)

 ! 981103 correction file no.1 for ALEPHLIB 31.0
     XHVBIT.H : New. Contains HV bit definitions                   (M.Cattaneo)
     XHVFIX : New. Repairs HV bits when they are known to be wrong (B.Bloch)
     XHVSTA : Add call to XHVFIX.                                  (B.Bloch)
              Take HV bit definitions from XHVBIT.H                (M.Cattaneo)
     XLSLUM : Take HV bit definitions from XHVBIT.H                (M.Cattaneo)

 ! 980715 correction file no.2 for ALEPHLIB 30.8
    XLSLUM : keep trying to get Trigger Enable mask if not present 
             for first event of run                         (B.Bloch)

 ! 980605 correction file no.1 for ALEPHLIB 30.8
    XTGENB : Bug fix, NAXTBN,NAXTOP were incorrectly initialised
                                                           (B.Bloch,G.Taylor)

 ! ALEPHLIB 308
   XLSLUM : For real data: after run 45000 (1998) require VDET ok  (B.Bloch)

 ! ALEPHLIB 307
   New package which brings together various XLUMOK related routines from Julia
   and Alpha. Includes also XVDEOK routines

   XLUMOK adaptations by Brigitte Bloch, XVDEOK by Henry Seywerd
   Code reviewed and optimised by Marco Cattaneo
   
   LLUMOK : Checks HV status, enabled triggers, and t0 synch. for LCAL
   SLUMOK : Checks HV status, enabled triggers, and t0 synch. for SICAL
   VBITGD : Checks readout status of VDET
   XHVBIT : Returns raw HV status bits, without run/detector specific repairs
   XHVSTA : Check HV status bits (from QHVSTA in ALPHA)
   XLSLUM : Checks HV status, enabled triggers, and t0 synchronization
   XLUMOK : Checks HV status, enabled triggers, and t0 synchronization, both
            for SICAL and LCAL with year dependent setup
   XRE133 : Flags laser events taken during LEP 1.5 data (from QRE133 in ALPHA)
   XRE94  : Flags laser events hidden in runs 25520 to 25530 taken in 1994
            (from QRE94 in ALPHA)
   XT0SYN : Get t0 synchronization information
   XTGENB : Check which triggers are enabled
   XVDEOK : Determines HV state of VDET

