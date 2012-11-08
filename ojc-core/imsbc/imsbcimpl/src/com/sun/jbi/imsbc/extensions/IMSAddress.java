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

package com.sun.jbi.imsbc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;

/**
 * @author Sun Microsystems
 */
public class IMSAddress implements ExtensibilityElement, Serializable {
	
	// Attribute name - IMS Server Location URL
    public static final String ATTR_LOCTN = "imsServerLocation";

    private static final long serialVersionUID = 1L;

    QName fieldElementType = IMSConstants.QNAME_ADDRESS;

    private Boolean fieldRequired = null;

    private String serverName = null;

    private Integer serverPort = null;
	
    private String serverLocation = null; 
    
    private static final Messages mMessages = Messages.getMessages(IMSAddress.class);

    public IMSAddress() {
    }

    /**
     * Get the extensibility element type
     * 
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
     * 
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
     * Get the server name
     * 
     * @return server name
     */
    public String getServerName() {
        return serverName;
    }

    public void setServerName(String val) {
        serverName = val;
    }

	
    public Integer getServerPort() {
        return serverPort;
    }

    public void setServerPort(Integer val) {
        serverPort = val;
    }

    public String getServerLocation(){
    	return serverLocation;
    }

    public void setServerLocation(String url) {
    	serverLocation = url;
        String[] stArr = url.split(":");
        if(stArr.length == 3 && stArr[0].equals("ims") && stArr[1].indexOf("//") 
                == 0 && Integer.parseInt(stArr[2])<= 65535){
            setServerName(stArr[1].substring(2, stArr[1].length()));
            setServerPort(Integer.valueOf(stArr[2]));
        }            
    }
    
	public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nIMSAddress " + fieldElementType + ":");
        strBuf.append("\nRequired=" + fieldRequired);
	    strBuf.append("\nServerName=" + serverName);
	    strBuf.append("\nServerPort=" + serverPort.toString());
	    strBuf.append("\nServerLocation=" + serverLocation);
        return strBuf.toString();
    }

}
