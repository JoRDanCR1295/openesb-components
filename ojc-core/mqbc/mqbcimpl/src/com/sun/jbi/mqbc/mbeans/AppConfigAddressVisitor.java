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

/**
 * A Visitor implementation interested in configuring MQBCAddress objects.
 * 
 * @author Noel.Ang@sun.com
 */
public class AppConfigAddressVisitor implements Visitor {
    private final MQBCAddress mAddress;

    public AppConfigAddressVisitor(MQBCAddress address) {
        if (address == null) {
            throw new NullPointerException("address");
        }
        mAddress = address;
    }

    public void visit(Visitable visitable) {
        synchronized (mAddress) {
            if (visitable instanceof AppConfigUsernameField) {
                AppConfigUsernameField field = (AppConfigUsernameField) visitable;
                mAddress.setUserName(field.getValue());
            } else if (visitable instanceof AppConfigPasswordField) {
                AppConfigPasswordField field = (AppConfigPasswordField) visitable;
                mAddress.setPassword(field.getValue());
            } else if (visitable instanceof AppConfigQmgrHostField) {
                AppConfigQmgrHostField field = (AppConfigQmgrHostField) visitable;
                mAddress.setHostName(field.getValue());
            } else if (visitable instanceof AppConfigQmgrNameField) {
                AppConfigQmgrNameField field = (AppConfigQmgrNameField) visitable;
                mAddress.setQueueManagerName(field.getValue());
            } else if (visitable instanceof AppConfigQmgrPortField) {
                AppConfigQmgrPortField field = (AppConfigQmgrPortField) visitable;
                mAddress.setPortNumber(field.getValue());
            } else if (visitable instanceof AppConfigCipherSuiteNameField) {
                AppConfigCipherSuiteNameField field = (AppConfigCipherSuiteNameField) visitable;
                mAddress.setCipherSuite(field.getValue());
            } else if (visitable instanceof AppConfigSslPeerNameField) {
                AppConfigSslPeerNameField field = (AppConfigSslPeerNameField) visitable;
                mAddress.setSslPeerName(field.getValue());
            }
        }
    }
}
