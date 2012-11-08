/*
 * JavaService1.java
 * 
 * Created on May 17, 2007, 4:39:07 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.recovery.test;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jws.WebService;
import javax.naming.InitialContext;
import javax.sql.DataSource;
import javax.xml.ws.WebServiceRef;
import org.netbeans.j2ee.wsdl.javaservice1.JavaService1PortType;

/**
 *
 * @author pbhagat
 */
@WebService(serviceName = "service1", portName = "port1", endpointInterface = "org.netbeans.j2ee.wsdl.javaservice1.JavaService1PortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/JavaService1", wsdlLocation = "META-INF/wsdl/JavaService1/JavaService1.wsdl")
@Stateless
public class JavaService1 implements JavaService1PortType {
    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/JavaXsltProcess/JavaXsltProcess.wsdl")
    public Service1 service;

    private static Object databaseLock = new Object();
    
    public JavaService1() {
    }

    @TransactionAttribute(TransactionAttributeType.SUPPORTS)
    public void javaService1Operation(org.netbeans.j2ee.wsdl.javaservice1.MessageElement1 partIn) {
        
        //Make sure that no record exists in the table with the current counter
        //Then insert a record with that counter
        
        Connection conn = null;
        PreparedStatement pstmt = null;
        ResultSet resultSet = null;
        
        long receivedCounter = partIn.getCounter();
        
        try {
            
            conn = ((DataSource) new InitialContext().lookup("jdbc/JavaService1Pool")).getConnection();
            
            synchronized(databaseLock) {
                pstmt = conn.prepareStatement("Select * from JAVA_SERVICE1 where COUNTER = ?");
                pstmt.setString(1, Long.toString(receivedCounter));
                resultSet = pstmt.executeQuery();
                if(resultSet.next()){
                    throw new RuntimeException("Table JAVA_SERVICE1 already contains a record with counter value " + receivedCounter);
                }
                resultSet.close();
                
                pstmt = conn.prepareStatement("Insert into JAVA_SERVICE1 values (?,?,?)");
                pstmt.setString(1, getCurrentTime());
                pstmt.setString(2, Long.toString(receivedCounter));
                pstmt.setString(3, partIn.getText());
                pstmt.execute();
            }
            pstmt.close();
            conn.close();
            
            // Call Web Service Operation
            com.sun.jbi.recovery.test.Service1 service = new com.sun.jbi.recovery.test.Service1();
            com.sun.jbi.recovery.test.JavaXsltProcessPortType port = service.getPort1();
            com.sun.jbi.recovery.test.MessageElement1 partOut = new MessageElement1();
            partOut.setCounter(receivedCounter);
            partOut.setText(partIn.getText());
            port.javaXsltProcessOperation(partOut);
            
        } catch (Exception e) {
            if(resultSet!=null) {
                try {
                    resultSet.close();
                } catch (SQLException ex) {
                    ex.printStackTrace();
                }
            }
            if(pstmt != null) {
                try {
                    pstmt.close();
                } catch (SQLException ex) {
                    ex.printStackTrace();
                }
            }
            if(conn != null) {
                try {
                    conn.close();
                } catch (SQLException ex) {
                    ex.printStackTrace();
                }
            }
            
            throw new RuntimeException(e);
        }
    }
    
    private String getCurrentTime(){
        Calendar cal = Calendar.getInstance(TimeZone.getDefault());
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getDefault());
        return sdf.format(cal.getTime()).toString();
    }
}
