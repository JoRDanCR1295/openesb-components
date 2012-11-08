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
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ActivePeriodExImpl.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl;

import javax.xml.namespace.QName;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.BindingOperationEx;

/**
 * Implements the WSDL Operation extension.
 * 
 * @author sunsoabi_edwong
 */
public class BindingOperationExImpl implements BindingOperationEx {
    
    private String mode;

    public String getMode() {
        return mode;
    }

    public void setMode(String mode) {
        this.mode = mode;
    }

    public void setElementType(QName arg0) {}

    public QName getElementType() {
        return ELEM_TYPE;
    }

    public void setRequired(Boolean arg0) {}

    public Boolean getRequired() {
        return Boolean.TRUE;
    }

}
