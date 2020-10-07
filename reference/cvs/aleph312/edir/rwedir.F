#if defined(EDIRTEST)
      PROGRAM RWEDIR
C! Program to write an event directory file
#ifndef DOC
C --
      COMMON/BCS/IW(800000)
C --
      CALL BNAMES(3000)
      CALL BOS(IW,800000)
C --
C   Opens the cards file EVTCLJUL CARDS or whatever cards file name
C   which contain input and output (FILI/FILO) data files.
C --
      CALL AOPEN(5,'ALPHA CARDS *',' ',' ',IER)
      CALL BREADC
C --
C   Open input and/or output files and write event directory file.
C   ALRWEV can also be used to write an EPIO file from an EDIR or
C   Native file and vice versa.
C --
      CALL ALRWEV
C --
      END
#endif
#endif
