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
 * @(#)SAPAddressClient.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.impl.WSDLExtensibleElementImpl;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Map;
import java.util.logging.Logger;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

public class SAPAddressClient extends WSDLExtensibleElementImpl implements Serializable {
    
    public SAPAddressClient(final ExtensibilityElement el, final SAPEnvironmentalVars sapEnvVar) throws WSDLException {
        fromXML(el, sapEnvVar);
    }
    
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    public QName getElementType() {
        return fieldElementType;
    }
    
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }
    
    public Boolean getRequired() {
        return fieldRequired;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nsap clientparams " + fieldElementType + ":");
        strBuf.append("\nRequired: " + fieldRequired);
        strBuf.append("\n" + SAPAddressClient.ATTR_USE_LOAD_BALANCING + ": " + useLoadBalancing.toString());
        strBuf.append("\n" + SAPAddressClient.ATTR_APP_SERVER_GROUP + ": " + applicationServerGroup);
        strBuf.append("\n" + SAPAddressClient.ATTR_MESSAGE_SERVER_HOSTNAME + ": " + msgServerHostname);
        return strBuf.toString();
    }
    
    public Boolean useLoadBalancing() {
        return useLoadBalancing;
    }
    
    public String applicationServerGroup() {
        return applicationServerGroup;
    }
    
    public String messageServerHostname() {
        return msgServerHostname;
    }
    
    
    public void toXML(final PrintWriter pw) {
        
        pw.write("            <sap:clientparams");
        
        DOMUtils.printAttribute(
                SAPAddressClient.ATTR_USE_LOAD_BALANCING,
                useLoadBalancing().toString(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddressClient.ATTR_APP_SERVER_GROUP,
                applicationServerGroup(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddressClient.ATTR_MESSAGE_SERVER_HOSTNAME,
                messageServerHostname(),
                pw);
        
        pw.write("/>\n");
    }
    
    public void fromXML(final ExtensibilityElement el, final SAPEnvironmentalVars sapEnvVar) throws WSDLException {
        String val;
        Map attrMap = el.getOtherAttributes();
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddressClient.ATTR_USE_LOAD_BALANCING);
        if (val != null) {
            if ("Yes".equalsIgnoreCase(val)) {
                useLoadBalancing = Boolean.TRUE;
            } else if ("No".equalsIgnoreCase(val)) {
                useLoadBalancing = Boolean.FALSE;
            } else {
                String msg = mMessages.getString(
                        "SAPAddressClient.Unknown_value_for_attr",
                        new Object[] { SAPAddressClient.ATTR_USE_LOAD_BALANCING, val });
                throw new WSDLException("", msg);
            }
        } else {
            String msg = mMessages.getString(
                    "SAPAddressClient.Missing_required_attr",
                    new Object[] { SAPAddressClient.ATTR_USE_LOAD_BALANCING, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddressClient.ATTR_APP_SERVER_GROUP);
        if (val != null) {
            applicationServerGroup = val;
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddressClient.ATTR_MESSAGE_SERVER_HOSTNAME);
        if (val != null) {
            msgServerHostname = val;
        }
         
    }
    
    private static final long serialVersionUID = 1L;
    private static final Messages mMessages = Messages.getMessages(SAPAddressClient.class);
    private static final Logger mLogger = Messages.getLogger(SAPAddressClient.class);
    protected static final String ATTR_USE_LOAD_BALANCING = "useLoadBalancing"; // NO I8N
    protected static final String ATTR_APP_SERVER_GROUP = "applicationServerGroup"; // NO I8N
    protected static final String ATTR_MESSAGE_SERVER_HOSTNAME = "messageServerHostname"; // NO I8N
    
    private QName fieldElementType = TYPE;
    private Boolean fieldRequired = Boolean.FALSE;
    private Boolean useLoadBalancing = Boolean.TRUE;
    private String applicationServerGroup = "";
    private String msgServerHostname = "";
    
    public static final QName TYPE =
            new QName(SAPWsdlConstants.EXT_NAMESPACE_URI, "clientparams", "sap"); // NO I8N
}
