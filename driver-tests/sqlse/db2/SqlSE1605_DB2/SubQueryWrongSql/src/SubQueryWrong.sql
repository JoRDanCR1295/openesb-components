INSERT INTO drivertest_emp VALUES ((SELECT MAX(empid) FROM drivertest_emp), 'NEWEMP', (SELECT deptid FROM drivertest_dept WHERE deptid = ?))
