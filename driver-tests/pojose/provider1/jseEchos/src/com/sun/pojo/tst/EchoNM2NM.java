/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.pojo.tst;

import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.api.annotation.POJOResource;
import org.glassfish.openesb.pojose.api.res.POJOContext;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 *
 * @author gpatil
 */
 @POJO
public class EchoNM2NM {
     @POJOResource
     POJOContext ctx;
             
    @Operation(outMessageType="EchoNM2NMOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoNM2NM/")
    public NormalizedMessage  receive(NormalizedMessage input) {
        NormalizedMessage out = null;        
        try {
            out = ctx.getMessageExchange().createMessage();
            Source src = input.getContent();
            out.setContent(src);
            
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
        return out;        
    }
    
    static class MyNamespaceContext implements NamespaceContext {

        public String getNamespaceURI(String prefix) {
            if (prefix == null) {
                throw new NullPointerException("Null prefix");//NOI18N
            } else if ("jbi".equals(prefix)) {
                return "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper"; //NOI18N
            }
            return XMLConstants.NULL_NS_URI;
        }

        // This method isn't necessary for XPath processing.
        public String getPrefix(String uri) {
            throw new UnsupportedOperationException();
        }

        // This method isn't necessary for XPath processing either.
        public Iterator getPrefixes(String uri) {
            throw new UnsupportedOperationException();
        }
    }     
}
