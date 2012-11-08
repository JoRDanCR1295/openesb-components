/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 */

package com.sun.jbi.mqbc.mbeans;

import com.sun.jbi.mqbc.extensions.MQBCAddress;
import junit.framework.TestCase;

public class AppConfigAddressVisitorTest extends TestCase {
    private AppConfigAddressVisitor appConfigAddressVisitor;
    private AppConfigUsernameField usernameField;
    private AppConfigPasswordField passwordField;
    private AppConfigQmgrHostField qmgrHostField;
    private AppConfigQmgrNameField qmgrNameField;
    private AppConfigQmgrPortField qmgrPortField;
    private AppConfigCipherSuiteNameField cipherSuiteField;
    private AppConfigSslPeerNameField sslPeerNameField;
    private MQBCAddress mqbcAddress;

    @Override
    protected void setUp() throws Exception {
        mqbcAddress = new MQBCAddress();
        appConfigAddressVisitor = new AppConfigAddressVisitor(mqbcAddress);
        usernameField = new AppConfigUsernameField();
        passwordField = new AppConfigPasswordField();
        qmgrHostField = new AppConfigQmgrHostField();
        qmgrNameField = new AppConfigQmgrNameField();
        qmgrPortField = new AppConfigQmgrPortField();
        cipherSuiteField = new AppConfigCipherSuiteNameField();
        sslPeerNameField = new AppConfigSslPeerNameField();
    }

    public void testVisit() throws Exception {
        String username = "testname";
        String password = "testpassword";
        String hostname = "testhostname";
        String queuemgrName = "testqueuename";
        String cipherSuite = "testciphersuite";
        String sslPeerName = "testSslPeerName";
        Integer port = 10514;
        Object gotValue = null;
        
        usernameField.setValue(username);
        passwordField.setValue(password);
        qmgrHostField.setValue(hostname);
        qmgrNameField.setValue(queuemgrName);
        qmgrPortField.setValue(port);
        cipherSuiteField.setValue(cipherSuite);
        sslPeerNameField.setValue(sslPeerName);
        
        appConfigAddressVisitor.visit(usernameField);
        gotValue = mqbcAddress.getUserName();
        assertTrue("Username result/expected: " + gotValue + "/" + username,
                gotValue.equals(username));

        appConfigAddressVisitor.visit(passwordField);
        gotValue = mqbcAddress.getPassword();
        assertTrue("Password result/expected: " + gotValue + "/" + password,
                gotValue.equals(password));

        appConfigAddressVisitor.visit(qmgrHostField);
        gotValue = mqbcAddress.getHostName();
        assertTrue("Queuemanager hostname result/expected: " + gotValue + "/" + hostname,
                gotValue.equals(hostname));

        appConfigAddressVisitor.visit(qmgrNameField);
        gotValue = mqbcAddress.getQueueManagerName();
        assertTrue("Queuemanager name result/expected: " + gotValue + "/" + queuemgrName,
                gotValue.equals(queuemgrName));

        appConfigAddressVisitor.visit(qmgrPortField);
        gotValue = mqbcAddress.getPortNumber();
        assertTrue("Queuemanager port result/expected: " + gotValue.toString() + "/" + port,
                gotValue.equals(port));
        
        appConfigAddressVisitor.visit(cipherSuiteField);
        gotValue = mqbcAddress.getCipherSuite();
        assertTrue("CipherSuite result/expected: " + gotValue.toString() + "/" + cipherSuite,
                gotValue.equals(cipherSuite));
        
        appConfigAddressVisitor.visit(sslPeerNameField);
        gotValue = mqbcAddress.getSslPeerName();
        assertTrue("SSLPeerName result/expected: " + gotValue.toString() + "/" + sslPeerName,
                gotValue.equals(sslPeerName));
    }
}