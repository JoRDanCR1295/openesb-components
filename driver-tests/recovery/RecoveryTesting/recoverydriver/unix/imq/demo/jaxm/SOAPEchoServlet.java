/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.  
 *
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 */

/*
 * @(#)SOAPEchoServlet.java	1.5 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import javax.xml.messaging.JAXMServlet;
import javax.xml.messaging.ReqRespListener;

import javax.xml.soap.SOAPMessage;

/**
 * This example echos the SOAP message received back to the sender.
 */
public class SOAPEchoServlet extends JAXMServlet implements ReqRespListener {

    /**
     * SOAP Message received is echoed back to the sender.
     */
    public SOAPMessage onMessage (SOAPMessage soapMessage) {
        return soapMessage;
    }

}
