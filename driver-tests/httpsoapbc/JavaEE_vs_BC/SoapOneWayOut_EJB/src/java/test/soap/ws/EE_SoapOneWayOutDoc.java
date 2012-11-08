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
 * @(#)EE_SoapOneWayOutDoc.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package test.soap.ws;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author harry.liu (harry.liu@sun.com)
 */
@Stateless
@WebService(serviceName = "SoapOneWayOutDocService", portName = "SoapOneWayOutDocPort", endpointInterface = "org.netbeans.j2ee.wsdl.soaponewayoutdoc.SoapOneWayOutDocPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapOneWayOutDoc", wsdlLocation = "META-INF/wsdl/EE_SoapOneWayOutDoc/SoapOneWayOutDoc.wsdl")
public class EE_SoapOneWayOutDoc implements org.netbeans.j2ee.wsdl.soaponewayoutdoc.SoapOneWayOutDocPortType {
    
    /** Creates a new instance of EE_SoapOneWayOutDoc */
    public EE_SoapOneWayOutDoc() {
    }

    public void soapOneWayOutDocOperation(org.netbeans.xml.schema.elementschema.RequestElement part1) {
        String data = part1.getRequest();
        System.out.println(data + "\r\n... EE_SoapOneWayOutDoc.soapOneWayOutDocOperation: called\r\n");
    }
    
}
