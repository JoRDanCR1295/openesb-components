/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.tst;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.glassfish.openesb.pojose.api.FaultMessage;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

/**
 *
 * @author gpatil
 */
@Provider(serviceQN = "{http://tst.com/FaultTster/}FaultTsterService", interfaceQN = "{http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleRPCLiteral}SimpleRPCLiteralPortType", name = "FaultTster")
public class FaultTster {
    @Resource
    private Context jbiCtx;

    @Operation(outMessageTypeQN = "{http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleRPCLiteral}SimpleRPCLiteralOperationResponse")
    public String doTest(String inp) throws FaultMessage {
        if ("ThrowFault".equalsIgnoreCase(inp)){
            Fault f = null;
            try {
                f = this.jbiCtx.getMessageExchange().createFault();
                DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
                dbf.setNamespaceAware(true);
                Document doc = dbf.newDocumentBuilder().newDocument();

                Element m = doc.createElementNS("http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper", "jbi:message");
                m.setAttribute("version", "1.0");
                m.setAttribute("xmlns:msgns", "http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleRPCLiteral");
                m.setAttribute("type", "msgns:SimpleRPCLiteralOperationFault");
                Element p = doc.createElementNS("http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper", "jbi:part");
                doc.appendChild(m);
                m.appendChild(p);
                Element e1 = doc.createElementNS("http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleRPCLiteral", "ns1:theFault");
                p.appendChild(e1);
                Element e2 = doc.createElementNS(null, "appMsg");
                e1.appendChild(e2);
                Text tn = doc.createTextNode("Hello");
                e2.appendChild(tn);
                Source s = new DOMSource(m);
                //doc.
                f.setContent(s);
                FaultMessage fm = new FaultMessage(f);
                throw fm;
            } catch (ParserConfigurationException ex) {
                Logger.getLogger(FaultTster.class.getName()).log(Level.SEVERE, null, ex);
            } catch (MessagingException ex) {
                Logger.getLogger(FaultTster.class.getName()).log(Level.SEVERE, null, ex);
            }
        } else if ("ThrowFaultUsingUtilMethod".equalsIgnoreCase(inp)){
            QName msgType = new QName("http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleRPCLiteral", "SimpleRPCLiteralOperationFault");
            String abstractPayload = "<ns1:theFault xmlns:ns1=\"http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleRPCLiteral\"><appMsg>Hello</appMsg></ns1:theFault>" ;
            FaultMessage fm = this.jbiCtx.createFaultMessage(abstractPayload, msgType);
            throw fm;
        }
        return inp;
    }


}
