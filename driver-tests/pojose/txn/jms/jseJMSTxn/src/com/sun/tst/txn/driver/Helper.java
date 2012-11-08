/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.tst.txn.driver;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.util.Properties;

/**
 *
 * @author gpatil
 */
public class Helper {
    public static final String TEST_IN_OUT_TXN_POSITIVE = "TestInOutTxnPositive" ;
    public static final String TEST_IN_OUT_TXN_NEGATIVE_OP_ERROR = "TestInOutTxnNegativeOpError" ;
    public static final String TEST_IN_OUT_TXN_NEGATIVE_OR_ERROR = "TestInOutTxnNegativeOnReplyError" ;
    public static final String TEST_IN_OUT_TXN_NEGATIVE_OD_ERROR = "TestInOutTxnNegativeOnDoneError" ;
    public static final String TEST_IN_OUT_TXN_NEGATIVE_OD_AS_ERROR = "TestInOutTxnNegativeOnDoneASynchError" ;

    private static final String JMS_LOOKUP_NAME = "jms/tx/default" ;
    private static final String PROP_KEY_JMS_CF_JNDI_PATH  = "pojose.test.txn.jmsca" ;
    private static Properties prop = null;

    public static void clearPropCache(){
        prop = null;
    }
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
    
    public static String getJMSJndiPath(){
        Properties lp = getProperties();
        String ret = lp.getProperty(PROP_KEY_JMS_CF_JNDI_PATH);
        if ((ret == null) || "".equals(ret.trim())){
            ret = JMS_LOOKUP_NAME;
        }
        return ret;
    }

}
