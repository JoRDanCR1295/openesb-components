SELECT * FROM drivertest_emp WHERE deptid = (SELECT MAX(deptid) FROM drivertest_dept)
