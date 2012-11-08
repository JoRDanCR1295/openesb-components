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
 * @(#)SAPAddress.java 
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
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Logger;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

public class SAPAddress extends WSDLExtensibleElementImpl implements Serializable{
    
    public SAPAddress(final ExtensibilityElement el, final SAPEnvironmentalVars sapEnvVar) throws WSDLException {
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
    
    public void setSAPAddressClient(SAPAddressClient client) {
        mSAPAddressClient = client;
    }
    
    public SAPAddressClient getSAPAddressClient() {
        return mSAPAddressClient;
    }
    
    public void setSAPAddressServer(SAPAddressServer server) {
        mSAPAddressServer = server;
    }
    
    public SAPAddressServer getSAPAddressServer() {
        return mSAPAddressServer;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer();
        strBuf.append("SAPAddress " + fieldElementType + ":");
        strBuf.append("\nRequired: " + fieldRequired);
        strBuf.append("\n" + SAPAddress.ATTR_APPSERVER_NAME + ": " + applicationServer);
        strBuf.append("\n" + SAPAddress.ATTR_CLIENT_NUMBER + ": " + clientNumber);
        strBuf.append("\n" + SAPAddress.ATTR_ENABLE_ABAP_DEBUG + ": " + enableAbapDebugWindow.toString());
        strBuf.append("\n" + SAPAddress.ATTR_GATEWAY_NAME + ": " + gateway);
        strBuf.append("\n" + SAPAddress.ATTR_GATEWAY_SERVICE + ": " + gatewayService);
        strBuf.append("\n" + SAPAddress.ATTR_IS_SAP_UNICODE + ": " + isSapUnicode.toString());
        strBuf.append("\n" + SAPAddress.ATTR_LANGUAGE + ": " + language);
        strBuf.append("\n" + SAPAddress.ATTR_PASSWORD + ": (length " + password.length() + ")");
        strBuf.append("\n" + SAPAddress.ATTR_ROUTER_STRING + ": " + routerString);
        strBuf.append("\n" + SAPAddress.ATTR_SYSTEM_ID + ": " + systemId);
        strBuf.append("\n" + SAPAddress.ATTR_SYSTEM_NUMBER + ": " + systemNumber);
        strBuf.append("\n" + SAPAddress.ATTR_USERNAME + ": " + username);
        return strBuf.toString();
    }
    
    public String applicationServer() {
        return applicationServer;
    }
    
    public String clientNumber() {
        return clientNumber;
    }
    
    public String systemNumber() {
        return systemNumber;
    }
    
    public String systemId() {
        return systemId;
    }
    
    public String username() {
        return username;
    }
    
    public String password() {
        return password;
    }
    
    public String language() {
        return language;
    }
    
    public String gateway() {
        return gateway;
    }
    
    public String gatewayService() {
        return gatewayService;
    }
    
    public String routerString() {
        return routerString;
    }
    
    public Boolean isSapUnicode() {
        return isSapUnicode;
    }
    
    public Boolean enableAbapDebugWindow() {
        return enableAbapDebugWindow;
    }
    
/*
<sap:address applicationServerHostname="sap50uni" clientNumber="800" systemNumber="00" systemID="EUC" user="PS1" password="ONLY4RD" language="EN" enableABAPDebugWindow="No" isSAPSystemUnicode="Yes" gatewayHostname="sap50uni" gatewayService="sapgw00">
    <sap:clientparams useLoadBalancing="No"/>
    <sap:serverparams programID="Provide value for this required attribute"/>
</sap:address>
 */
    
    public void toXML(final PrintWriter pw) {
        
        pw.write("        <sap:address");
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_APPSERVER_NAME,
                applicationServer(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_CLIENT_NUMBER,
                clientNumber(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_SYSTEM_NUMBER,
                systemNumber(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_SYSTEM_ID,
                systemId(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_USERNAME,
                username(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_PASSWORD,
                password(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_LANGUAGE,
                language(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_GATEWAY_NAME,
                gateway(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_GATEWAY_SERVICE,
                gatewayService(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_ROUTER_STRING,
                routerString(),
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_IS_SAP_UNICODE,
                isSapUnicode().booleanValue() == true ? "Yes" : "No",
                pw);
        
        DOMUtils.printAttribute(
                SAPAddress.ATTR_ENABLE_ABAP_DEBUG,
                enableAbapDebugWindow().booleanValue() == true ? "Yes" : "No",
                pw);
        
        pw.write(">\n");
        
        getSAPAddressClient().toXML(pw);
        getSAPAddressServer().toXML(pw);
        
        pw.write("        </sap:address>\n");
    }
    
    public void fromXML(final ExtensibilityElement el, final SAPEnvironmentalVars sapEnvVar) 
            throws WSDLException {
        String val;
        Map attrMap = el.getOtherAttributes();
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_APPSERVER_NAME);
        if (val != null) {
            applicationServer = val;
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_APPSERVER_NAME, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_CLIENT_NUMBER);
        if (val != null) {
            clientNumber = val;
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_CLIENT_NUMBER, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_GATEWAY_NAME);
        if (val != null) {
            gateway = val;
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_GATEWAY_SERVICE);
        if (val != null) {
            gatewayService = val;
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_LANGUAGE);
        if (val != null) {
            language = val;
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_LANGUAGE, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_PASSWORD);
        if (val != null) {
            password = val;
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_PASSWORD, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_ROUTER_STRING);
        if (val != null) {
            routerString = val;
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_SYSTEM_ID);
        if (val != null) {
            systemId = val;
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_SYSTEM_ID, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_SYSTEM_NUMBER);
        if (val != null) {
            systemNumber = val;
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_SYSTEM_NUMBER, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_USERNAME);
        if (val != null) {
            username = val;
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_USERNAME, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_ENABLE_ABAP_DEBUG);
        if (val != null) {
            if ("Yes".equalsIgnoreCase(val)) {
                enableAbapDebugWindow = Boolean.TRUE;
            } else if ("No".equalsIgnoreCase(val)) {
                enableAbapDebugWindow = Boolean.FALSE;
            } else {
                String msg = mMessages.getString(
                        "SAPAddress.Unknown_value_for_attr",
                        new Object[] { SAPAddress.ATTR_ENABLE_ABAP_DEBUG, val });
                throw new WSDLException("", msg);
            }
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_ENABLE_ABAP_DEBUG, val });
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddress.ATTR_IS_SAP_UNICODE);
        if (val != null) {
            if ("Yes".equalsIgnoreCase(val)) {
                isSapUnicode = Boolean.TRUE;
            } else if ("No".equalsIgnoreCase(val)) {
                isSapUnicode = Boolean.FALSE;
            } else {
                String msg = mMessages.getString(
                        "SAPAddress.Unknown_value_for_attr",
                        new Object[] { SAPAddress.ATTR_IS_SAP_UNICODE, val });
                throw new WSDLException("", msg);
            }
        } else {
            String msg = mMessages.getString(
                    "SAPAddress.Missing_required_attr",
                    new Object[] { SAPAddress.ATTR_IS_SAP_UNICODE, val });
            throw new WSDLException("", msg);
        }
        
        /*
        ExtensibilityElement clientElem = el.getDOMUtils.getFirstChildElement(el);
        String subTagName = SAPAddressClient.TYPE.getPrefix() + ":" + SAPAddressClient.TYPE.getLocalPart();
        if (clientElem != null){
            if (clientElem.getTagName().equals(subTagName)) {
                getSAPAddressClient().fromXML(clientElem);
            }
        }
         
        ExtensibilityElement serverElem = DOMUtils.getNextSiblingElement(clientElem);
        subTagName = SAPAddressServer.TYPE.getPrefix() + ":" + SAPAddressServer.TYPE.getLocalPart();
        if (serverElem != null){
            mLogger.info("!!!!!!!!!!! SAP Address  Element fromXML ["+serverElem.getTagName()+"]["+subTagName+"]");
         
            if (serverElem.getTagName().equals(subTagName)) {
                mLogger.info("!!!!!!!!!!! SAP Address before ["+getSAPAddressServer()+"]");
                mLogger.info("!!!!!!!!!!! SAP Address before ["+serverElem+"]");
                getSAPAddressServer().fromXML(serverElem);
                mLogger.info("!!!!!!!!!!! SAP Address before");
            }
        }
         */
        Collection<ExtensibilityElement> extElems = el.getExtensibilityElements();
        if (extElems != null) {
            for (Iterator<ExtensibilityElement> iter = extElems.iterator();iter.hasNext();) {
                ExtensibilityElement e = (ExtensibilityElement) iter.next();
                if (e.getElementType().equals(SAPAddressClient.TYPE)) {
                    setSAPAddressClient(new SAPAddressClient(e, sapEnvVar));
                }
                if (e.getElementType().equals(SAPAddressServer.TYPE)) {
                    setSAPAddressServer(new SAPAddressServer(e, sapEnvVar));
                }
            }
        }
    }
   
    
    private SAPAddressClient mSAPAddressClient = null;
    private SAPAddressServer mSAPAddressServer = null;
    
    private static final long serialVersionUID = 1L;
    private static final Messages mMessages = Messages.getMessages(SAPAddress.class);
    private static final Logger mLogger = Messages.getLogger(SAPAddress.class);
    protected static final String ATTR_APPSERVER_NAME = "applicationServerHostname"; // NO I8N
    protected static final String ATTR_CLIENT_NUMBER = "clientNumber"; // NO I8N
    protected static final String ATTR_SYSTEM_NUMBER = "systemNumber"; // NO I8N
    protected static final String ATTR_SYSTEM_ID = "systemID"; // NO I8N
    protected static final String ATTR_USERNAME = "user"; // NO I8N
    protected static final String ATTR_PASSWORD = "password"; // NO I8N
    protected static final String ATTR_LANGUAGE = "language"; // NO I8N
    protected static final String ATTR_ENABLE_ABAP_DEBUG = "enableABAPDebugWindow"; // NO I8N
    protected static final String ATTR_GATEWAY_NAME = "gatewayHostname"; // NO I8N
    protected static final String ATTR_GATEWAY_SERVICE = "gatewayService"; // NO I8N
    protected static final String ATTR_ROUTER_STRING = "routerString"; // NO I8N
    protected static final String ATTR_IS_SAP_UNICODE = "isSAPSystemUnicode"; // NO I18N
    
    private QName fieldElementType = TYPE;
    private Boolean fieldRequired = Boolean.FALSE;
    private String applicationServer = "";
    private String clientNumber = "";
    private String systemNumber = "";
    private String systemId = "";
    private String username = "";
    private String password = "";
    private String language = "";
    private String gateway = "";
    private String gatewayService = "";
    private String routerString = "";
    private Boolean isSapUnicode = Boolean.TRUE;
    private Boolean enableAbapDebugWindow = Boolean.FALSE;
    
    public static final QName TYPE =
            new QName(SAPWsdlConstants.EXT_NAMESPACE_URI, "address", "sap"); // NO I8N
    
}
