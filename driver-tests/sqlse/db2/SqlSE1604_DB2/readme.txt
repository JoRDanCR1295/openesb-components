complex query wrong : 

SELECT * FROM drivertest_emp e, drivertest_dept WHERE e.deptid = d.deptid AND e.empno = ?


and input will be '0' where empno is not present