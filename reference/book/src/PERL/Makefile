#
#  Need to execute from $ALBOOK/src/ORACLE
#  as albook
#

include ../UTILITIES/version.h

install: 
	cvs checkout $(VERSION) jscanbook/perl; \
        cp jscanbook/perl/update_WWtables.perl ./update_WWtables.perl
	cp jscanbook/perl/update_MCPRODtables.perl ./update_MCPRODtables.perl

clean:
	rm -rf jscanbook
