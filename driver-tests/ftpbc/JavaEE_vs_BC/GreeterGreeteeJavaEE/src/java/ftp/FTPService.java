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
 * @(#)FTPService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package ftp;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author jfu
 */
@Stateless
@WebService(serviceName = "service1", portName = "port1", endpointInterface = "org.netbeans.j2ee.wsdl.ftp_duke.FTPPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/FTP_DUKE", wsdlLocation = "META-INF/wsdl/FTPService/FTP_DUKE.wsdl")
public class FTPService implements org.netbeans.j2ee.wsdl.ftp_duke.FTPPortType {
    
    /** Creates a new instance of FTPService */
    public FTPService() {
    }

    public org.netbeans.xml.schema.responsemessageschema.ResponseType ftpOperation(org.netbeans.xml.schema.requestmessageschema.RequestType part1) {
        org.netbeans.xml.schema.responsemessageschema.ResponseType response = new org.netbeans.xml.schema.responsemessageschema.ResponseType();
        String msg = part1.getRequestElement();
        if ( msg != null && msg.length() > 0 ) {
            if ( msg.startsWith("Hello Duke, this is ") ) {
                response.setResponseElement("Hello " + msg.substring("Hello Duke, this is ".length()) + ", this is Duke");
            }
            else {
                response.setResponseElement("What was that?");
            }
        }
        else {
            response.setResponseElement("Empty or NULL input message in ftpOperation(part1)");
        }
        return response;
    }
    
}
