refer bug https://open-jbi-components.dev.java.net/issues/show_bug.cgi?id=415
refer INF http://inf.central.sun.com/inf/integrationReport.jsp?id=112258

Variables within the OnAlarm were not associated with the approprite scope. The JUnit 
system test to test this persitence is difficult (time consuming), hence adding in the driver project
and hoping to rely on the persistence runs of all the driver projects.

