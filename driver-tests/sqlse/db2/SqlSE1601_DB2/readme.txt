join two table using innerjoin

drivertest_emp and drivertest_dept


<?xml version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://schemas.xmlsoap.org/soap/envelope/ http://schemas.xmlsoap.org/soap/envelope/" xmlns="http://com.sun.jbi/sqlse/sqlseengine">
<SOAP-ENV:Header/>
<SOAP-ENV:Body>
  <JoinQueryResponse xmlns="http://com.sun.jbi/sqlse/sqlseengine">
    <EMPID>1</EMPID>
    <ENAME>smith</ENAME>
    <DEPTID>1</DEPTID>
    <DEPTID>1</DEPTID>
    <DEPTNAME>compapps</DEPTNAME>
    <LOCID>1</LOCID>
    <EMPID>2</EMPID>
    <ENAME>john</ENAME>
    <DEPTID>2</DEPTID>
    <DEPTID>2</DEPTID>
    <DEPTNAME>QE</DEPTNAME>
    <LOCID>2</LOCID>
    <EMPID>3</EMPID>
    <ENAME>kerry</ENAME>
    <DEPTID>3</DEPTID>
    <DEPTID>3</DEPTID>
    <DEPTNAME>Dev</DEPTNAME>
    <LOCID>3</LOCID>
  </JoinQueryResponse>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>

