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

package com.sun.jbi.hl7bc.configuration;

import com.sun.jbi.hl7bc.extensions.HL7Address;
import junit.framework.TestCase;

public class AppConfigAddressVisitorTest extends TestCase {
    private AppConfigAddressVisitor appConfigAddressVisitor;
    private AppConfigrHostNameField hostField;
    private AppConfigPortField portField;
    private HL7Address hl7Address;

    @Override
    protected void setUp() throws Exception {
        hl7Address = new HL7Address();
        appConfigAddressVisitor = new AppConfigAddressVisitor(hl7Address);
        hostField = new AppConfigrHostNameField();
        portField = new AppConfigPortField();
    }

    public void testVisit() throws Exception {
        String hostname = "testhostname";
        Integer port = 10514;
        Object gotValue = null;
        
		hostField.setValue(hostname);
        portField.setValue(port);
      
		appConfigAddressVisitor.visit(hostField);
        gotValue = hl7Address.getHL7ServerLocation();
        assertTrue("HL7 Service hostName result/expected: " + gotValue + "/" + hostname,
                gotValue.equals(hostname));

        appConfigAddressVisitor.visit(portField);
        gotValue = hl7Address.getHL7ServerPort();
        assertTrue("HL7 Service port result/expected: " + gotValue.toString() + "/" + port,
                gotValue.equals(port));
    }
}