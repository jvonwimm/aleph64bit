#
#  Need to execute from $ALBOOK/src/ORACLE
#  as albook
#

include ../UTILITIES/version.h

install: jscanbook/plsql/scanbook_procedures.sql \
         jscanbook/plsql/general_procedures.sql  \
         jscanbook/plsql/scanbook_WWprocedures.sql \
         sqlmisc
	perl install.perl;
	perl install.perl ../UTILITIES/scanbook_version.sql;

jscanbook/plsql/general_procedures.sql:	
	cvs checkout $(VERSION) jscanbook/plsql/general_procedures.sql;

jscanbook/plsql/scanbook_procedures.sql:	
	cvs checkout $(VERSION) jscanbook/plsql/scanbook_procedures.sql;

jscanbook/plsql/scanbook_WWprocedures.sql:
	cvs checkout $(VERSION) jscanbook/plsql/scanbook_WWprocedures.sql;

sqlmisc:
	cvs checkout $(VERSION) jscanbook/sql; \
        cp jscanbook/sql/*.ctl_header \
           /afs/cern.ch/user/a/albook/public_aleph/cerndb1/sql_header/; \
	cvs checkout $(VERSION) jscanbook/sql; \
        cp jscanbook/sql/*.sqlplus_instruct \
           /afs/cern.ch/user/a/albook/public_aleph/cerndb1/sql_header/

clean:
	rm -rf jscanbook
	rm -f *.sql