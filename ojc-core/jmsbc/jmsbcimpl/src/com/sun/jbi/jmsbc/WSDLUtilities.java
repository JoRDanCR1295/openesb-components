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
 * @(#)WSDLUtilities.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.util.HashMap;
import java.util.Map;

import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;

/*
 * WSDL utility class
 */
public class WSDLUtilities {
    private static HashMap<String, String> excludedTypes = new HashMap<String, String>();
    static {
        excludedTypes.put("anyType", "");
    }
    
    public static boolean isBuiltInType(QName typename) {
    	if(typename == null)
    		return false;
    	
    	String uri = typename.getNamespaceURI();
    	if(uri == null)
    		return false;
    	return ( (uri.equals("http://www.w3.org/2001/XMLSchema") ||
	        uri.equals("http://www.w3.org/1999/XMLSchema") ) &&
	        !excludedTypes.containsKey(typename.getLocalPart()));
    }
    
    public static boolean isBuiltInBinaryType(QName typename) {
    	if(typename == null)
    		return false;
    	
    	String uri = typename.getNamespaceURI();
    	if(uri == null)
    		return false;
    	return (uri.equals("http://www.w3.org/2001/XMLSchema") || uri
				.equals("http://www.w3.org/1999/XMLSchema"))
				&& "base64Binary".equals(typename.getLocalPart());
    }
    
    public static String getLocalPart(String qname) {
    	String localPart = qname;
        if (qname != null && !qname.equals("")) {
            if (qname.indexOf(":") != -1) {
                localPart = qname.substring(qname.indexOf(":") + 1);
            }
        }
        return localPart;
    }

    public static boolean isXsdAnyType(QName typename) {
    	return ("http://www.w3.org/2001/XMLSchema".equals(typename.getNamespaceURI()) ||
    			"http://www.w3.org/1999/XMLSchema".equals(typename.getNamespaceURI()) ) &&
                "anyType".equals(typename.getLocalPart());
    }
    
//    public static String makePartName(String typename) {
//    	String name = "value";
//    	
//        if (builtInTypes.containsKey(typename)) {
//            if (typename.indexOf(".") > 0) {
//                name = toLowerCase(typename.substring(typename.lastIndexOf(".") + 1)) + name;
//            } else {
//                name = toLowerCase(typename) + name;
//            }
//        }
//        
//        return name;
//    }
    
    public static String normalize(String name) {
        return Character.toUpperCase(name.charAt(0)) + toLowerCase(name.substring(1));
    }
    
    private static String toLowerCase(String name) {
        StringBuffer sb = new StringBuffer();
        for (int ii = 0; ii < name.length(); ii++) {
            sb.append(Character.toLowerCase(name.charAt(ii))); 
        }
        
        return sb.toString();
    }
    
    public static String getUniqueTargetNamespace(String adaptername) {
        StringBuffer sb = new StringBuffer();

        sb.append("urn:");
        sb.append(toLowerCase(adaptername));
        sb.append("service");
        
        return sb.toString();
   }
    
   public static Part findWSDLMessagePart(String partName, 
                                          Message wsdlMessage) {
        Map parts = wsdlMessage.getParts();
        Part aPart = (Part)parts.get(partName);
        return aPart;
    }

}
