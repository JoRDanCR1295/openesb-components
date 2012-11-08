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
 * @(#)EE_SoapOneWayInDoc.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package test.soap.ws;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.soaponewayindoc.SoapOneWayInDocPortType;
import org.netbeans.xml.schema.elementschema.RequestElement;
import test.soap.ejb.EJB_UtilLocal;

/**
 *
 * @author harry.liu (harry.liu@sun.com)
 */
@Stateless
@WebService(serviceName = "SoapOneWayInDocService", portName = "SoapOneWayInDocPort", endpointInterface = "org.netbeans.j2ee.wsdl.soaponewayindoc.SoapOneWayInDocPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapOneWayInDoc", wsdlLocation = "META-INF/wsdl/EE_SoapOneWayInDoc/SoapOneWayInDoc.wsdl")
public class EE_SoapOneWayInDoc implements SoapOneWayInDocPortType {

    @EJB
    private EJB_UtilLocal eJB_UtilBean;
    
    /** Creates a new instance of EE_SoapOneWayInDoc */
    public EE_SoapOneWayInDoc() {
    }

    public void soapOneWayInDocOperation(RequestElement part1) {
        String log = "\r\n... EE_SoapOneWayInDoc.soapOneWayInDocOperation: Data was read in\r\n";
        eJB_UtilBean.setTrace(log);

        System.out.println(log + "\r\n... Data is: " + part1.getRequest());
    }
    
}
