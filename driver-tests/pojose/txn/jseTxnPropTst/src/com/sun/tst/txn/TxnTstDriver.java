/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.tst.txn;

import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import javax.jms.JMSException;
import javax.naming.NamingException;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import javax.jms.ConnectionFactory;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.sql.DataSource;

/**
 *
 * @author gpatil
 */
@Provider 
public class TxnTstDriver {
    // Logger
    private static final Logger logger = Logger.getLogger(TxnTstDriver.class.getName());

    // POJO Context
    @Resource
    private Context jbiCtx;

    private DataSource ds;
    private ConnectionFactory cf;
    private Connection conn;
    private Statement st;
    private Session session;
    private javax.jms.Connection jmsConn;

    /**
     * Constructor
     */
    public TxnTstDriver() {
    }

    private void initInternal(){
        try {
            Counter.prop = null;
            this.ds = (DataSource) this.jbiCtx.getNamingContext().lookup(Counter.getJdbcJndiPath());
            this.cf = (ConnectionFactory) this.jbiCtx.getNamingContext().lookup(Counter.getJMSJndiPath());
            this.conn = ds.getConnection();
            this.st = conn.createStatement();
        } catch (SQLException ex) {
            Logger.getLogger(TxnTstDriver.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NamingException ex) {
            Logger.getLogger(TxnTstDriver.class.getName()).log(Level.SEVERE, null, ex);
        }
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
        close(this.session);
        close(this.jmsConn);
    }
    
    private void doCleanup(){
        try {
            this.st.execute("DELETE from GMP_TXN_TST1");
        } catch (Exception ex){
            //ignore
        }
        
        try {
            this.st.execute("DROP TABLE GMP_TXN_TST1");
            //this.conn.commit();
        } catch (Exception ex){
            //ignore
        }
    }

    private void doInit(){
        try {
            st.execute("CREATE TABLE GMP_TXN_TST1 (TEST_NAME VARCHAR(30), DSCRPTN VARCHAR(30))");
            //this.conn.commit();
        } catch (SQLException ex) {
            Logger.getLogger(TxnTstDriver.class.getName()).log(Level.SEVERE, null, ex);
        }        
    }

    private boolean rowInserted(String key){
        boolean ret = false;

        try {
            ResultSet rs = this.st.executeQuery("SELECT COUNT(*) FROM GMP_TXN_TST1 WHERE TEST_NAME = '" + key + "'");
            if (rs.next()){
                int i = rs.getInt(1);
                if (i == 1){
                    ret = true;
                }
            }
        } catch (SQLException ex) {
            Logger.getLogger(TxnTstDriver.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return ret;
    }

    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://txn.tst.sun.com/TxnTstDriver/}TxnTstDriverOperationResponse")
    public String doTest(String input) {

        try {
            initInternal();
            doCleanup();
            doInit();
            //Do test
            // Send a message:
            jmsConn = cf.createConnection();
            session = jmsConn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            if (Counter.TEST_IN_OUT_TXN_POSITIVE.equals(input) ||
                Counter.TEST_IN_OUT_TXN_NEGATIVE.equals(input)){
                
                Counter.IN_OUT_MSG_RCVD.set(false);

                Queue q = session.createQueue("POJOInOutQueue");

                Queue replyQ = session.createQueue("GMP_TXN_TST1_REPLY");
                TextMessage tm = session.createTextMessage();
                tm.setJMSReplyTo(replyQ);
                tm.setText(input);
                MessageProducer mp = session.createProducer(q);
                mp.send(tm);
                MessageConsumer cons = session.createConsumer(replyQ);
                jmsConn.start();

                if (Counter.TEST_IN_OUT_TXN_POSITIVE.equals(input)){
                    Message m = cons.receive(30 * 1000);
                    if (m != null){
                        TextMessage rm = (TextMessage) m;
                        if (input.equals(rm.getText())){
                            if (rowInserted(input)){
                                return input + ":Ok" ;
                            }
                        }
                    }
                } else if (Counter.TEST_IN_OUT_TXN_NEGATIVE.equals(input)){
                    Message m = cons.receive(3 * 1000);
                    if ((m == null) && !rowInserted(input) && (Counter.IN_OUT_MSG_RCVD.get())){
                        // Inserted row is rolled back.
                        return input + ":Ok" ;
                    }
                }
                
                jmsConn.stop();
            }

            if (Counter.TEST_IN_ONLY_TXN_POSITIVE.equals(input) ||
                Counter.TEST_IN_ONLY_TXN_NEGATIVE.equals(input)){

                Counter.IN_ONLY_MSG_RCVD.set(false);

                Queue q = session.createQueue("POJOInOnlyQueue");
                TextMessage tm = session.createTextMessage();
                tm.setText(input);
                MessageProducer mp = session.createProducer(q);
                mp.send(tm);

                Thread.sleep(3 * 1000);
                
                if (Counter.TEST_IN_ONLY_TXN_POSITIVE.equals(input)){
                    if (rowInserted(input) && (Counter.IN_ONLY_MSG_RCVD.get())){
                        return input + ":Ok" ;
                    }
                } else if (Counter.TEST_IN_ONLY_TXN_NEGATIVE.equals(input)){
                    if (!rowInserted(input) && (Counter.IN_ONLY_MSG_RCVD.get())){
                        // Inserted row is rolled back.
                        return input + ":Ok" ;
                    }
                }
            }
        } catch (JMSException ex) {
            Logger.getLogger(TxnTstDriver.class.getName()).log(Level.SEVERE, null, ex);
        } catch (Exception ex) {
            Logger.getLogger(TxnTstDriver.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            //cleanup
            doCleanup();
            closeResouces();
        }

        return input + ":NOk";
    }
}