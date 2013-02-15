package com.sun.jbi.ftpbc.ftp;
public class FtpTprops {
    final static String FTP_USER01 = "anonymous";
    final static String FTP_PASSWD = "tester@sun.com";

    //useful site: http://www.ftp-sites.org/
    final static String FTP_TEST_HOST01 = "ftp.sun.com";
    final static String FTP_TEST_HOST02 = "ftp.stc.com";
    final static String FTP_TEST_HOST03 = "ftp.stortek.com";  //same as ftp.stc.com
    final static String FTP_TEST_HOST04 = "ftp.andrew.cmu.edu";
    final static String FTP_TEST_HOST05 = "ftp.apache.com";
    final static String FTP_TEST_HOST06 = "ftp.cs.uoregon.edu";
    final static String FTP_TEST_HOST07 = "ftp.cs.utexas.edu";
    final static String FTP_TEST_HOST08 = "ftp.free.net";
    final static String FTP_TEST_HOST09 = "testcase.boulder.ibm.com";
    final static String FTP_TEST_HOST10 = "ftp.2600.com";
    final static String FTP_TEST_HOST11 = "sunsite.berkeley.edu";
    final static String FTP_TEST_HOST12 = "sunsolve.sun.com";  //this one is EOL 8/08
    final static String FTP_TEST_HOST13 = "supportfiles.sun.com";

    //64 failures:      final static String FTP_TEST_HOST = FTP_TEST_HOST01;
    //works (85s):      final static String FTP_TEST_HOST = FTP_TEST_HOST03;
    //78 failures:      final static String FTP_TEST_HOST = FTP_TEST_HOST05;
    //REAL SLOW:        final static String FTP_TEST_HOST = FTP_TEST_HOST06;
    //HUNG:             final static String FTP_TEST_HOST = FTP_TEST_HOST07;
    //1 FAIL, 1 ERROR:  final static String FTP_TEST_HOST = FTP_TEST_HOST08;
    //1 ERROR:          final static String FTP_TEST_HOST = FTP_TEST_HOST09;
    //71 failures:      final static String FTP_TEST_HOST = FTP_TEST_HOST10;
    //works (135s):     final static String FTP_TEST_HOST = FTP_TEST_HOST11;
    //works (107s)      final static String FTP_TEST_HOST = FTP_TEST_HOST12;
    //64 failures:      final static String FTP_TEST_HOST = FTP_TEST_HOST13;

    final static String FTP_TEST_HOST = FTP_TEST_HOST09;
}
