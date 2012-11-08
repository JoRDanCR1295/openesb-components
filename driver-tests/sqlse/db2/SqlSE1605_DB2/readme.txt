execute query which wrong options

query:

INSERT INTO drivertest_emp VALUES ((SELECT MAX(empid) FROM drivertest_emp), 'NEWEMP', (SELECT deptid FROM drivertest_dept WHERE deptid = ?))


value provided : '0'as there is no deptid with value 0
