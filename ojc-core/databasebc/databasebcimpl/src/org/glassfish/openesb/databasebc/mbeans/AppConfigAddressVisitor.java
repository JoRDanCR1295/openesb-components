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

package org.glassfish.openesb.databasebc.mbeans;

import org.glassfish.openesb.databasebc.extensions.JDBCAddress;

/**
 * A Visitor implementation interested in configuring JDBC Address objects.
 * 
 * @author Noel.Ang@sun.com
 */
public class AppConfigAddressVisitor implements Visitor {
    private final JDBCAddress mAddress;

    public AppConfigAddressVisitor(JDBCAddress address) {
        assert address != null;
        mAddress = address;
    }

    public void visit(Visitable visitable) {
        synchronized (mAddress) {
            if (visitable instanceof AppConfigDBJNDINameField) {
                AppConfigDBJNDINameField field = (AppConfigDBJNDINameField) visitable;
                mAddress.setJndiName(field.getValue());
            } 
        }
    }
}
