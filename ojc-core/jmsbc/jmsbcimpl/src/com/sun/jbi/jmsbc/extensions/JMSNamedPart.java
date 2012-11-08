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
 * @(#)JMSNamedPart.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * Represents the jms message part mapping to a jms message's object value that
 * has a name (i.e., a value of MapMessage)
 */
public abstract class JMSNamedPart extends JMSUnNamedPart implements Serializable {

    public static final String ATTR_NAME = "name";

    protected String name;

    /**
     * Get value of name attribute
     * @return The String value of name attribute
     */
    public String getName() {
        return name;
    }

    /**
     * Sets value of name attribute
     * @param val The String value of name attribute
     */
    public void setName(String val) {
        name = val;
    }

}
