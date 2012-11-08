INSERT INTO drivertest_emp VALUES ((SELECT MAX(empid) FROM drivertest_emp) + 1, 'newemp', 1)
