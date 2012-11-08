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
 * @(#)MessageUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime;

import java.util.List;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import com.sun.jbi.common.qos.messaging.WrapperUtil;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.transform.engine.runtime.impl.JbiMessage;

/**
 * Utility to construct and parse JBI WSDL 1.1 message wrapper element.
 */
public class MessageUtil {
    /**
     * Creates a {@link WSMessage} from the specified XML and WSDL message definition.
     * 
     * @param src The XML content of the message.
     * @param model The WSDL message definition.
     * @return a service message.
     * @throws Exception if an error occurs creating message.
     */
    public static WSMessage createWSMessage(Source src, Message model) throws Exception {
        DOMSource dom = XmlUtil.toDOMSource(src);
        JbiMessage msg = new JbiMessage(model);
        Node node = dom.getNode();
        List parts = model.getOrderedParts(null);
        
        if (node instanceof Document) {
            Document doc = (Document) node;
            // the document element is expected to be a jbi:message element or empty doc
            if (WrapperUtil.isMessageWrapped(doc)) {
                for (int i = 0, n = parts.size(); i < n; i++) {
                    msg.setPart(((Part) parts.get(i)).getName(), 
                                WrapperUtil.getPartElement(doc, i));
                }
            }
            else if (parts.size() == 1) {  // source from stylesheet invoke
                // we only support one-part wsdl messages from invoke extension fxn
                Element elem = doc.getDocumentElement();
                msg.setPart(((Part) parts.get(0)).getName(), elem);
            }
        }
        
        return msg;
    }
}
