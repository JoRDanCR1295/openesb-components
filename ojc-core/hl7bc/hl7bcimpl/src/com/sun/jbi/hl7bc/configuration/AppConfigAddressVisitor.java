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

/**
 * A Visitor implementation interested in configuring HL7Address objects.
 * 
 * @author Noel.Ang@sun.com
 */
public class AppConfigAddressVisitor implements Visitor {
    private final HL7Address mAddress;

    public AppConfigAddressVisitor(HL7Address address) {
        assert address != null;
        mAddress = address;
    }

    public void visit(Visitable visitable) {
        synchronized (mAddress) {
            if (visitable instanceof AppConfigrHostNameField) {
                AppConfigrHostNameField field = (AppConfigrHostNameField) visitable;
                mAddress.setHL7ServerLocation(field.getValue());
            } else if (visitable instanceof AppConfigPortField) {
                AppConfigPortField field = (AppConfigPortField) visitable;
                mAddress.setHL7ServerPort(field.getValue());
            } else if (visitable instanceof AppConfigTcpRoleField) {
            	AppConfigTcpRoleField field = (AppConfigTcpRoleField) visitable;
            	mAddress.setTcpRoleString(field.getValue());
            }
        }
    }
}
