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
 * @(#)HelloPartner.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package partner;

import javax.jws.WebService;

/**
 *
 * @author Sujit Biswas
 */
@WebService(serviceName = "helloPartnerService", portName = "helloPartnerPort", endpointInterface = "org.netbeans.j2ee.wsdl.hellopartner.HelloPartnerPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/helloPartner", wsdlLocation = "WEB-INF/wsdl/HelloPartner/helloPartner.wsdl")
public class HelloPartner implements org.netbeans.j2ee.wsdl.hellopartner.HelloPartnerPortType {
    
    /** Creates a new instance of HelloPartner */
    public HelloPartner() {
    }

    public String helloPartnerOperation(String part1) {
        return "Hello, " + part1;
    } 
    
}
