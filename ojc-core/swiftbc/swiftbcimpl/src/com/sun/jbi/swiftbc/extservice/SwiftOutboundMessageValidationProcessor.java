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
 * @(#)SwiftOutboundMessageValidationProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2006, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.swiftbc.extservice;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.List;
import java.util.Map;

import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.jbi.messaging.NormalizedMessage;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.w3c.dom.NodeList;


import com.sun.jbi.swiftbc.extservice.ValidationInfo;
import com.sun.jbi.swiftbc.Endpoint;
import com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.ApplicationException;

/**
 * This class validates the Swift Outbound message
 * 
 * @author Raghunadh
 */

public class SwiftOutboundMessageValidationProcessor {

    private String logMsg;
    private static final Messages mMessages = Messages.getMessages(SwiftOutboundMessageValidationProcessor.class);

    private static final Logger mLog = Messages.getLogger(SwiftOutboundMessageValidationProcessor.class);

    private boolean sftEnabled = false;

    private ValidationInfo mValidationInfo;

    public ValidationInfo messageValidationProcess(Endpoint destination, NormalizedMessage normalizedMsg)
            throws ApplicationException, Exception {

        SwiftProtocolProperties protocolProperties = destination.getSwiftProtocolProperties();
        mValidationInfo = new ValidationInfo();
        // ensure that the message received from the queue is not null
        if (normalizedMsg == null) {
            mLog.log(Level.SEVERE, "SwiftOutboundMessageValidationProcessor_Invalid_NMSG");
            logMsg = mMessages.getString("SwiftOutboundMessageValidationProcessor_Invalid_NMSG");
            throw new ApplicationException(logMsg);
        }
        Source source = normalizedMsg.getContent();
            // TODO ..do some checkings here..

        return mValidationInfo;

    }

    private void validateSwiftMessage(SwiftProtocolProperties protocolProperties,
                                    NormalizedMessage normalizedMsg,
                                    ValidationInfo validationInfo) throws Exception {

        if (mLog.isLoggable(Level.INFO)) {
            mLog.log(Level.INFO, "SwiftOutboundMessageValidationProcessor_Inside_Validate_Message");
        }
    }

    private Document getDocument(Source src) throws Exception {

        DOMResult result = new DOMResult();
        if (src != null) {
            TransformerFactory fact = TransformerFactory.newInstance();
            Transformer transformer = fact.newTransformer();
            transformer.transform(src, result);
        }
        Node node = result.getNode();
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }

        return normalizedDoc;

    }

}
