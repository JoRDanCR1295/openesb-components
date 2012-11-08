/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.glassfish.openesb.pojo.cbr;

import java.util.Iterator;
import java.util.logging.Level;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.Consumer.MessageObjectType;
import org.glassfish.openesb.pojose.api.MessageException;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.w3c.dom.Node;

/**
 *
 * @author gpatil
 */
@Provider 
public class WWOrderRouterStatic {
    
    /**
     * Constructor
     */
    public WWOrderRouterStatic() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type Node input
     * @return Node
     */
    @Operation (outMessageTypeQN="{http://cbr.pojo.openesb.glassfish.org/WWOrderRouterStatic/}WWOrderRouterStaticOperationResponse")
    public Node receive(Node input) {
        try {
            XPathFactory xpFactory = XPathFactory.newInstance();
            XPath xpath = xpFactory.newXPath();
            xpath.setNamespaceContext(new MyNamespaceContext());
            XPathExpression expr = xpath.compile("//wwd:order/fulfillmentLocation/text()"); //NOI18N
            String cntry = (String) expr.evaluate(input, XPathConstants.STRING);
            System.out.println("Location:" + cntry);
            Node outputMsg = null;
            Consumer cons = null;
            
            if ("Asia".equals(cntry)) {
                cons = asiaEp;
            } else {
                cons = europeEp;
            }

            outputMsg = (Node) cons.sendSynchInOut(input, MessageObjectType.Node);
            return outputMsg;
        } catch (MessageException ex) {
            Logger.getLogger(WWOrderRouterStatic.class.getName()).log(Level.SEVERE, null, ex);
        } catch (XPathExpressionException ex) {
            Logger.getLogger(WWOrderRouterStatic.class.getName()).log(Level.SEVERE, null, ex);
        }
        return input;
    }


    static class MyNamespaceContext implements NamespaceContext {

        public String getNamespaceURI(String prefix) {
            if (prefix == null) {
                throw new NullPointerException("Null prefix");//NOI18N
            } else if ("wwd".equals(prefix)) {
                return "wwOrderProcessNS"; //NOI18N
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

//    private String endpointName = "wwOrderProcessPortTypeRole_myRole";
//    private QName asiaSvcName = QName.valueOf("{bplAsiaFulfillmentProcess}AsiaPartnerLink");
//    private QName europeSvcName =  QName.valueOf("{bplEuropeFulfillmentProcess}EuropePartnerLink");
//    private QName consInMsgType = QName.valueOf("{wwOrderProcessNS}wwOrderProcessOperationRequest");
//    private QName consOpName = QName.valueOf("{wwOrderProcessNS}wwOrderProcessOperation");

    @ConsumerEndpoint(name="asiaBPELProcess", 
            serviceQN="AsiaSvc",     
            interfaceQN="{wwOrderProcessNS}wwOrderProcessPortType",
            operationQN="{wwOrderProcessNS}wwOrderProcessOperation", 
            inMessageTypeQN="{wwOrderProcessNS}wwOrderProcessOperationRequest")
    private Consumer asiaEp;

    @ConsumerEndpoint(name="europeBPELProcess", 
            serviceQN="EuropeSvc",     
            interfaceQN="{wwOrderProcessNS}wwOrderProcessPortType",            
            operationQN="{wwOrderProcessNS}wwOrderProcessOperation", 
            inMessageTypeQN="{wwOrderProcessNS}wwOrderProcessOperationRequest")
    private Consumer europeEp;
    
    @Resource
    private Context ctx;
}