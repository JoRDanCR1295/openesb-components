/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.pojo.tst;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.api.annotation.POJOResource;
import org.glassfish.openesb.pojose.api.res.POJOContext;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * @author gpatil
 */
@POJO
public class EchoME2ME {
     @POJOResource
     POJOContext ctx;    
     
    @Operation(outMessageType="EchoME2MEOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoME2ME/")
    public MessageExchange receive(MessageExchange me) {
        NormalizedMessage in = null;
        NormalizedMessage out = null;
        try {
            out = me.createMessage();
            in = me.getMessage("in");
            Source src = in.getContent();
            out.setContent(src);
            me.setMessage(out, "out");

            if (src instanceof DOMSource) {
                DOMSource ds = (DOMSource) src;
                Node n = ds.getNode();
                if (n.getNodeType() == Node.DOCUMENT_NODE) {
                    n = ((Document) n).getDocumentElement();
                }
                //n is jbi:message node.
                Node typeAttr = n.getAttributes().getNamedItem("type");
                String val = typeAttr.getNodeValue();
                val = val.replaceAll("Request", "Response");
                typeAttr.setNodeValue(val);
            }
            
        } catch (MessagingException ex) {
            Logger.getLogger(EchoNM2NM.class.getName()).log(Level.SEVERE, null, ex);
        }
        return me;        
    }
}
