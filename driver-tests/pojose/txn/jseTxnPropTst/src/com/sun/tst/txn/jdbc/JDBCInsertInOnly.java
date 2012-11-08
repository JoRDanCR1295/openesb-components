/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.tst.txn.jdbc;

import com.sun.tst.txn.Counter;
import com.sun.tst.txn.TxnTstDriver;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import javax.naming.NamingException;
import javax.sql.DataSource;
import org.glassfish.openesb.pojose.api.ErrorMessage;

/**
 *
 * @author gpatil
 */
@Provider (name="JDBCInsertInOnly",interfaceQN="{http://j2ee.netbeans.org/wsdl/jseTxnPropTst/JDBCInsertInOnly}JMSInPortType",serviceQN="{http://j2ee.netbeans.org/wsdl/jseTxnPropTst/JDBCInsertInOnly}JMSInPortTypeService")
public class JDBCInsertInOnly {
    // Logger
    private static final Logger logger = Logger.getLogger(JDBCInsertInOnly.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;

    private DataSource ds;
    private Connection conn;
    private Statement st;
    
    /**
     * Constructor
     */
    public JDBCInsertInOnly() {
    }

    private void close(Object closable){
        try {
            Method m = closable.getClass().getMethod("close", null);
            m.invoke(closable, null);
        } catch (Exception ex){
            //
        }
    }

    private void closeResouces(){
        close (this.st);
        close (this.conn);
    }

    private void insertRow(String key){
        try {
            this.ds = (DataSource) this.jbiCtx.getNamingContext().lookup(Counter.getJdbcJndiPath());
            this.conn = ds.getConnection();
            this.st = conn.createStatement();
            st.execute("INSERT INTO GMP_TXN_TST1 VALUES ('" + key + "', 'testing,testing')");
        } catch (NamingException ex) {
            Logger.getLogger(JDBCInsertInOut.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SQLException ex) {
            Logger.getLogger(TxnTstDriver.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * POJO Operation
     *
     * @param input input of type String input
     */
    @Operation 
    public void jmsInOperation(String input) throws ErrorMessage{
        try {
            Counter.IN_ONLY_MSG_RCVD.set(true);
            insertRow(input);
            if (Counter.TEST_IN_ONLY_TXN_NEGATIVE.equals(input)){
                throw new ErrorMessage("Testing txn rollback after error.");
            }
        } finally {
            closeResouces();
        }
    }
}