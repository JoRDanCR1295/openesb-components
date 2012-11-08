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
 * @(#)MessageNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.wsdl.model.WSDLMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.WSDLException;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

/**
 * This is the class that normalizes messages for the SAP BC.
 */
public class MessageNormalizer {
    
    public NormalizedMessage normalize(NormalizedMessage newmsg,
            WSDLMessage wsdlModelMsg,
            Document doc)
            throws MessageProcessingException {
        
        try {
            WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
            wrapperBuilder.initialize(null, SAPWSDLUtilities.getMessage(wsdlModelMsg), null);
            
            String messageURI = wsdlModelMsg.getQName().getNamespaceURI();
            String messageName = wsdlModelMsg.getQName().getLocalPart();
            
            // Assumption: SAP-generated Message definitions only have one part
            String wsdlPartName = wsdlModelMsg.getPart(0).getName();
            //System.out.println("Number of parts ["+wsdlModelMsg.getPartSize()+ "]");
            
            NodeList nodeList = doc.getElementsByTagNameNS(messageURI, messageName);
            if (nodeList.getLength() > 0) {
                wrapperBuilder.addPart(wsdlPartName, nodeList);
            }
            
            newmsg.setContent(new DOMSource(wrapperBuilder.getResult()));
            
        } catch (WrapperProcessingException ex) {
            throw new MessageProcessingException(
                    mMessages.getString(
                    "MessageDenormalizer.Failed_unwrap",
                    new Object[] {
                wsdlModelMsg.getName(),
            }),
                    ex
                    );
            
        } catch (MessagingException ex) {
            
            throw new MessageProcessingException(
                    mMessages.getString(
                    "MessageDenormalizer.messageprocessingexception",
                    new Object[] {
                wsdlModelMsg.getName(),
            }),
                    ex
                    );
        } catch (WSDLException ex) {
            throw new MessageProcessingException(
                    mMessages.getString(
                    "MessageDenormalizer.wsdlexception",
                    new Object[] {
                wsdlModelMsg.getName(),
            }),
                    ex
                    );
        }
        return newmsg;
    }
    private static final Messages mMessages = Messages.getMessages(MessageNormalizer.class);
}
