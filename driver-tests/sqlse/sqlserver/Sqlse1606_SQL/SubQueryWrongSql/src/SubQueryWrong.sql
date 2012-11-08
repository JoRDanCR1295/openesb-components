select * from employee where deptid in (select deptname from department where deptid > 1)
