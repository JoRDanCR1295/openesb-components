/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.tst.txn;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 *
 * @author gpatil
 */
public class Counter {
    public static final String TEST_IN_OUT_TXN_POSITIVE = "TestInOutTxnPositive" ;
    public static final String TEST_IN_OUT_TXN_NEGATIVE = "TestInOutTxnNegative" ;

    public static final String TEST_IN_ONLY_TXN_POSITIVE = "TestInOnlyTxnPositive" ;
    public static final String TEST_IN_ONLY_TXN_NEGATIVE = "TestInOnlyTxnNegative" ;
    

    private static final String DB_LOOKUP_NAME = "jdbc/iepseDerbyXA" ;
    private static final String JMS_LOOKUP_NAME = "jms/tx/jmq1" ;
    
    public static AtomicBoolean IN_OUT_MSG_RCVD = new AtomicBoolean(false);
    public static AtomicBoolean IN_ONLY_MSG_RCVD = new AtomicBoolean(false);

    static Properties prop = null;
    
    private static synchronized Properties getProperties(){
        if (prop == null){
            prop = new Properties();
            String tmpDir = System.getProperty("java.io.tmpdir");
            if ((tmpDir != null) && (!"".equals(tmpDir.trim()))){
                File file = new File(tmpDir, "pojose_test.properties");
                FileInputStream fr = null;
                if (file.exists()){
                    try {
                        fr = new FileInputStream(file);
                        prop.load(fr);
                        System.out.println("##### Loaded propeties from:" + file.getAbsolutePath());
                        prop.list(System.out);
                    }  catch (Exception ex){
                        //
                    } finally {
                        if (fr != null){
                            try {
                                fr.close();
                            } catch (Exception ex){
                                //ignore.
                            }
                        }
                    }
                }else {
                    System.out.println("##### Test properties file does not exists:" + file.getAbsolutePath());
                }
            }
        }

        return prop;
    }
    public static String getJdbcJndiPath(){
        Properties lp = getProperties();
        String ret = lp.getProperty("pojose.test.txn.xajdbc");
        if ((ret == null) || "".equals(ret.trim())){
            ret = DB_LOOKUP_NAME;
        }
        return ret;
    }

    public static String getJMSJndiPath(){
        Properties lp = getProperties();
        String ret = lp.getProperty("pojose.test.txn.jmsca");
        if ((ret == null) || "".equals(ret.trim())){
            ret = JMS_LOOKUP_NAME;
        }
        return ret;
    }

}
