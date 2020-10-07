  SUBSCHEMA TRIGGERLEVEL2
   : 'Event record from the Trigger Level 2 of the Aleph detector.'

  AUTHOR   'T. Medcalf,M.Saich'
  REVIEWER 'J. A. Strong'
  VERSION  '3.1'
  DATE     '27/03/90'

     DEFINE ESET

     X2TB
                :      'Polar angle segments of the TPC'
                     STATIC
                     SIZE 63,63

                = ( Start1   = Timebin   : 'Row 1 start-point',
                    End1     = Timebin   : 'Row 1 end-point',
                    Start2   = Timebin   : 'Row 2 start-point',
                    End2     = Timebin   : 'Row 2 end-point',
                    Start3   = Timebin   : 'Row 3 start-point',
                    End3     = Timebin   : 'Row 3 end-point',
                    Start4   = Timebin   : 'Row 4 start-point',
                    End4     = Timebin   : 'Row 4 end-point',
                    Start5   = Timebin   : 'Row 5 start-point',
                    End5     = Timebin   : 'Row 5 end-point',
                    Start6   = Timebin   : 'Row 6 start-point',
                    End6     = Timebin   : 'Row 6 end-point',
                    Start7   = Timebin   : 'Row 7 start-point',
                    End7     = Timebin   : 'Row 7 end-point',
                    Start8   = Timebin   : 'Row 8 start-point',
                    End8     = Timebin   : 'Row 8 end-point'  )
                ;

     X2DF
                :       'Data output from second level trigger'
                      STATIC
                      SIZE 125,*

                =  ( Dataword     = INTE[*,*]: 'Consult expert')
                ;


     X2MS
                :       'TPC trigger mask'
                      STATIC
                      SIZE 60,60

                =  ( YesNo  = INTE[0,1]  : 'Truth value of good track' )
                ;

 X2CO
    :   'Level 2 processors constants. \
              Number of words per set of processors \
              Number of sets of processors'
        STATIC
	SIZE 2,5

        = ( ZOne             = INTE           : 'TPC zone',  
	    NBins	     = INTE	      : 'Number of theta bins',
	    NRoad	     = INTE	      : 'Road width',	 	
	    PadPair	     = INTE	      : 'Pad pair word',
	    THresh	     = INTE	      : 'Theta thresh word',
	    DRift	     = INTE	      : 'Drift velocity',   
	    ZAcc	     = INTE	      : 'Zacceptance',		    
	    ZExs             = INTE	      : ' Z excess ',
	    NIgn	     = INTE	      : 'Number ignored pad rows',
	    IRow(5)	     = INTE           : 'Ignored pad rows')
        ;

 X2LO
    :   'LEVEL2 set of processors with same look-up tables. \ 
              Number of words per set of processors \
              Number of sets of processors'
        STATIC
	SIZE 2,5

	=( Num 	             = INTE           : ' Number of processors in the set ', 
	   Group(12)         = INTE	      : ' list of processor numbers ')
	;   


     END ESET

     DEFINE ATTRIBUTE

        Timebin =  INTE[1,256]
                : 'Digitalized drift time';
                                               END ATTRIBUTE

  END SUBSCHEMA
