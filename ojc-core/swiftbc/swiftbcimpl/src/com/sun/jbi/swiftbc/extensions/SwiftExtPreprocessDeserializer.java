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
 * @(#)HL7ExtPreprocessDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extensions;

import java.util.Map;

import com.sun.jbi.internationalization.Messages;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.swiftbc.SAGConstants;

import org.w3c.dom.Element;

/**
 * 
 * @author Sun Microsystems
 *
 */

public class SwiftExtPreprocessDeserializer extends SwiftExtSerializer {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Messages mMessages = Messages.getMessages(SwiftExtPreprocessDeserializer.class);

    // default constructor
    public SwiftExtPreprocessDeserializer() {
        super();
    }

    public SwiftExtPreprocessDeserializer(Map envVariableMap) {
        super(envVariableMap);
    }

    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
                                                                 QName elementType,
                                                                 Element el,
                                                                 Definition def,
                                                                 ExtensionRegistry extReg) throws WSDLException {

        ExtensibilityElement returnValue = null;
        
        if (SAGConstants.QNAME_ADDRESS.equals(elementType)) {
            SwiftAddress swiftAddress = new SwiftAddress();
            
            String location = DOMUtils.getAttribute(el, SwiftAddress.ATTR_SWIFT_SVR_LOCATIONURL);
            if (nonEmptyString(location)) {
                try {
                    if (isAToken(location)) {
                        String envVariableName = getEnvVariableName(location);
                        if (!mEnvVariableMap.containsKey(envVariableName)) {
                            mEnvVariableMap.put(envVariableName, null);
                        }
                    }
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }
            }
            returnValue = swiftAddress;

        }
        return returnValue;

    }

}
