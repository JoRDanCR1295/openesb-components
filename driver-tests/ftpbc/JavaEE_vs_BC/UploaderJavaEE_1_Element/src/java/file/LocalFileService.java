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
 * @(#)LocalFileService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package file;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author jfu
 */
@Stateless
@WebService(serviceName = "FILEService", portName = "FILEPort", endpointInterface = "org.netbeans.j2ee.wsdl.file.FILEPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/FILE", wsdlLocation = "META-INF/wsdl/LocalFileService/FILE.wsdl")
public class LocalFileService implements org.netbeans.j2ee.wsdl.file.FILEPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/FTP_CLIENT/FTP_CLIENT.wsdl")
    private client.Service1 service;
    
    /** Creates a new instance of LocalFileService */
    public LocalFileService() {
    }

    public org.netbeans.xml.schema.responsemessageschema.ResponseType fileOperation(org.netbeans.xml.schema.requestmessageschema.RequestType part1) {
        org.netbeans.xml.schema.responsemessageschema.ResponseType response = new org.netbeans.xml.schema.responsemessageschema.ResponseType();
        try { // Call Web Service Operation
            client.FTPPortType port = service.getPort1();
            // TODO initialize WS operation arguments here
            client.RequestType part1c = new client.RequestType();
            part1c.setRequestElement(part1.getRequestElement());
            port.ftpOperation(part1c);
            response.setResponseElement("UPLOADED MESSAGE in fileOperation(part1)...");
        } catch (Exception ex) {
            // TODO handle custom exceptions here
            response.setResponseElement("Exception in fileOperation(part1), e=" + ex.getLocalizedMessage());
        }
        return response;
    }
    
}
