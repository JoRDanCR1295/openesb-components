/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.glassfish.openesb.pojo.cbr;

import java.util.Iterator;
import java.util.logging.Level;
import org.glassfish.openesb.pojose.api.annotation.*;
import java.util.logging.Logger;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.Consumer.MessageObjectType;
import org.glassfish.openesb.pojose.api.MessageException;
import org.glassfish.openesb.pojose.api.res.Context;
import org.w3c.dom.Node;

/**
 *
 * @author gpatil
 */
@Provider
public class WWOrderRouter {
    
    /**
     * Constructor
     */
    public WWOrderRouter() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation(outMessageTypeQN="{http://cbr.pojo.openesb.glassfish.org/WWOrderRouter/}WWOrderRouterOperationResponse")
    public Node receive(Node input) {
        try {
            XPathFactory xpFactory = XPathFactory.newInstance();
            XPath xpath = xpFactory.newXPath();
            xpath.setNamespaceContext(new MyNamespaceContext());
            XPathExpression expr = xpath.compile("//wwd:order/fulfillmentLocation/text()"); //NOI18N
            String cntry = (String) expr.evaluate(input, XPathConstants.STRING);
            System.out.println("Location:" + cntry);
            Node outputMsg = null;
            QName svc2use = null;
            if ("Asia".equals(cntry)) {
                svc2use = this.asiaSvcName;
            } else {
                svc2use = this.europeSvcName;
            }

            ServiceEndpoint se = this.ctx.getEndpoint(svc2use, this.endpointName);
            Consumer cons = this.ctx.getConsumer(se, this.consOpName, this.consInMsgType);
            outputMsg = (Node) cons.sendSynchInOut(input, MessageObjectType.Node);
            return outputMsg;
        } catch (MessageException ex) {
            Logger.getLogger(WWOrderRouter.class.getName()).log(Level.SEVERE, null, ex);
        } catch (XPathExpressionException ex) {
            Logger.getLogger(WWOrderRouter.class.getName()).log(Level.SEVERE, null, ex);
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

    private String endpointName = "wwOrderProcessPortTypeRole_myRole";
    private QName asiaSvcName = QName.valueOf("{bplAsiaFulfillmentProcess}AsiaPartnerLink");
    private QName europeSvcName =  QName.valueOf("{bplEuropeFulfillmentProcess}EuropePartnerLink");
    private QName consInMsgType = QName.valueOf("{wwOrderProcessNS}wwOrderProcessOperationRequest");
    private QName consOpName = QName.valueOf("{wwOrderProcessNS}wwOrderProcessOperation");

    @Resource
    private Context ctx;
}