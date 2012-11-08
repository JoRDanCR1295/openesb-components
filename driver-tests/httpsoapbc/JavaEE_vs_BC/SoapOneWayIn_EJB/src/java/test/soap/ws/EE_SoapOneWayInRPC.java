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
 * @(#)EE_SoapOneWayInRPC.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package test.soap.ws;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.soaponewayinrpc.SoapOneWayInRPCPortType;
import org.netbeans.xml.schema.typeschema.RequestType;
import test.soap.ejb.EJB_UtilLocal;

/**
 *
 * @author harry.liu (harry.liu@sun.com)
 */
@Stateless
@WebService(serviceName = "SoapOneWayInRPCService", portName = "SoapOneWayInRPCPort", endpointInterface = "org.netbeans.j2ee.wsdl.soaponewayinrpc.SoapOneWayInRPCPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapOneWayInRPC", wsdlLocation = "META-INF/wsdl/EE_SoapOneWayInRPC/SoapOneWayInRPC.wsdl")
public class EE_SoapOneWayInRPC implements SoapOneWayInRPCPortType {

    @EJB
    private EJB_UtilLocal eJB_UtilBean;
    
    /** Creates a new instance of EE_SoapOneWayInRPC */
    public EE_SoapOneWayInRPC() {
    }

    public void soapOneWayInRPCOperation(RequestType part1) {
        String log = "\r\n... EE_SoapOneWayInRPC.soapOneWayInRPCOperation: Data was read in\r\n";
        eJB_UtilBean.setTrace(log);

        System.out.println(log + "\r\n... Data is: " + part1.getRequest());
    }
    
}
