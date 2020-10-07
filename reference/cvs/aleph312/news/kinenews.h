CKEY KINE KINGAL
C! KINGAL package
 ! ALEPHLIB 31.1
    KCLEAN : Add XVRT bank to list of possibly user modified banks (B.Bloch)
    KEJOB  : Add bank KSEC to list of end of run banks             (B.Bloch)
    KSECBK : New - BOOK and fill bank KSEC with cross section info (B.Bloch)
    KXLUPR : New - print out jetset switches and parameters        (B.Bloch)

 ! Correction file 6 to ALEPHLIB 30.7
    KP6SHO : New - BOOK and fill bank KSHO with fragmentation info (B.Bloch)
    KSHOBK : New - BOOK and fill bank KSHO with fragmentation info (B.Bloch)

 ! Correction file 3 to ALEPHLIB 30.7
    KXP6CO : Fix a WRITE statement               (B.Bloch)

 ! Correction file 1 to ALEPHLIB 30.7
    KKMOVE : extend NMX from 100 to 200          (B.Bloch)
    KZFRBK : take booking of bank out of do loop (B.Bloch)
    KP6ZFR,KPYZET,KXP6AL,KXP6BR,KXP6CO,KXP6IN,KXP6ST,KXP6TO,PTY6COM.h :
      c.f. KZFRBK,KLUZET,KXL7AL,KXL7BR,KXL7CO,KXL7PA,KXL7ST,KXL7TO,LUN7COM.h
      New - Kingal interface routines for PYTHIA 6.1        (B.Bloch)

 ! ALEPHLIB 30.5
    KBPART : handle properly particles which should be tracked by 
             GEANT (flr)

 ! ALEPHLIB 30.3 correction set 2
    KCLEAN : Allow user to supersede GKBR bank             (B.Bloch)
    KLUZET : New - transfer fragmentation info from JETSET (B.Bloch)
    KXL74A : Extend particle codes from JETSET             (B.Bloch)
    KXL7AL : Handle correctly Tau's from PYTHIA            (B.Bloch)
    KXL7PA : Extend particle codes from JETSET             (B.Bloch)
    KZFRBK : New - book KZFR bank                          (B.Bloch)

 ! ALEPHLIB 21.3
   KCLEAN, KINIT, KSWAP, KXL7AL, KXL7BR, KXL7ST, KXLUAL, KXLUBR :
     split character string stretching over two lines 
   KIPARA : increase the number of particles/event for LEP2 generators
   KXL7AL : fix format statement
   KXL7PA : fix maximum energy to 205. Gev compatible with LEP2
   KXL74A : interface routine to JETSET 7.4

 ! ALEPHLIB 15.8
   KKDROP,KKMOVE,KSWAP,KCLEAN - service routines to overwrite
                                reference cards from DB (B.Bloch-Devaux)
