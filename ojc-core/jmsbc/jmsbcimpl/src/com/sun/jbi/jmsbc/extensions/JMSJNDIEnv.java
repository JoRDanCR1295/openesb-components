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
 * @(#)JMSJNDIEnv.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.io.Serializable;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 */
public class JMSJNDIEnv implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    QName fieldElementType = JMSConstants.QNAME_JNDIENV;

    Boolean fieldRequired = null;

    Map envEntries = null;  // <envEntryName, JMSJNDEnvEntry>

    public JMSJNDIEnv() {
    }

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Gets the options
     * @return The map of JMSJNDIEnvEntry elements
     */
    public Map getJNDIEnvEntries() {
        return envEntries;
    }

    /**
     * Sets the options
     * @param val The map of JMSOption elements
     */
    public void setJNDIEnvEntries(Map val) {
        envEntries = val;
    }

    /**
     * Returns environment entries as Properties bundle
     */
     public Properties toProperties() {
         Properties props = new Properties();
         if (envEntries != null && envEntries.size() > 0) {
            Iterator iter = envEntries.keySet().iterator();
            while (iter.hasNext()) {
                JMSJNDIEnvEntry entry = (JMSJNDIEnvEntry)envEntries.get(iter.next());
                props.setProperty(entry.getName(), entry.getValue());
            }             
         }
         return props;
     }
     
    /**
     * String format of the object
     * @return The String format of the object
     */
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nJMSJNDIEnv (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nJMSJNDIEnvEntry list =[[" + jndiEnvEntriesMapToString() + "]]");
        strBuf.append("\n");

        return strBuf.toString();
    }
    
    private String jndiEnvEntriesMapToString() {
        StringBuffer strBuff = new StringBuffer();
        if (envEntries != null) {
            Iterator iter = envEntries.keySet().iterator();
            while (iter.hasNext()) {
                JMSJNDIEnvEntry entry = (JMSJNDIEnvEntry)envEntries.get(iter.next());
                strBuff.append(entry.toString());
            }
        }
        return strBuff.toString();
    }    
}
