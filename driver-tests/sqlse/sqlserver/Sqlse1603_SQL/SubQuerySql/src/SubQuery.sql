select * from employee where deptid in (select deptid from department where deptid > 1)
