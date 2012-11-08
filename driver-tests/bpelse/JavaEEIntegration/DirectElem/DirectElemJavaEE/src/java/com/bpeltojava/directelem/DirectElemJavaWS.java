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
 * @(#)DirectElemJavaWS.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.bpeltojava.directelem;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author pvarghese
 */
@Stateless
@WebService(serviceName = "CallJavaEEService", portName = "CallJavaEEPort", endpointInterface = "org.netbeans.j2ee.wsdl.directelem.CallJavaEEPT", targetNamespace = "http://j2ee.netbeans.org/wsdl/DirectElem", wsdlLocation = "META-INF/wsdl/DirectElemJavaWS/DirectElem.wsdl")
public class DirectElemJavaWS implements org.netbeans.j2ee.wsdl.directelem.CallJavaEEPT {
    
    /** Creates a new instance of DirectElemJavaWS */
    public DirectElemJavaWS() {
    }

    public org.netbeans.xml.schema.directelemschema.MsgType callJavaEEOper(org.netbeans.xml.schema.directelemschema.MsgType input) {
        //return null;
        int id = input.getIntElem() + 100;
        String msg = input.getStrElem() + " Added in the JavaEE SE and EJB:";
        input.setIntElem(id);
        input.setStrElem(msg);
        return input;
        
    }
    
}
