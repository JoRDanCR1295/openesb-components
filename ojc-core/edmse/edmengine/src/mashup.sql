CREATE DATABASE LINK FLATFILEDB ( DRIVER='org.axiondb.jdbc.AxionDriver' URL='jdbc:axiondb:testdb:C:\\test' USERNAME='sa' PASSWORD='sa')
CREATE EXTERNAL TABLE IF NOT EXISTS T1_TEST (EMP_ID numeric(10) NULL, FNAME varchar(100) NULL) 
ORGANIZATION 
( LOADTYPE='REMOTE' VENDOR='AXION' DBLINK='FLATFILEDB' REMOTETABLE='TEST' )
SELECT 
    S2.&quot;COL1&quot; AS s_column1, 
    s2.&quot;COL2&quot; AS s_column2, 
   
FROM &quot;TABLE2&quot; S2 
    INNER JOIN &quot;TABLE1&quot; S1 
        ON S1.&quot;KEYCOL&quot; = S2.&quot;KEYCOL&quot;


CREATE EXTERNAL TABLE &quot;TEST1&quot; 
(
&quot;SSN&quot; varchar(20) DEFAULT &apos;&apos;, 
&quot;NAME&quot; varchar(20) DEFAULT &apos;&apos;
&quot;AGE&quot; varchar(20) DEFAULT &apos;&apos;
) 
ORGANIZATION (TRIMWHITESPACE=&apos;false&apos;, 
LOADTYPE=&apos;DELIMITED&apos;, ROWSTOSKIP=&apos;0&apos;, 
ISFIRSTLINEHEADER=&apos;true&apos;, MAXFAULTS=&apos;0&apos;, 
QUALIFIER=&apos;&quot;&apos;, 
FILENAME=&apos;c:\test\test1.csv&apos;, 
FIELDDELIMITER=&apos;,&apos;, CREATE_IF_NOT_EXIST=&apos;true&apos;, 
RECORDDELIMITER=&apos;\r\n \n&apos;)