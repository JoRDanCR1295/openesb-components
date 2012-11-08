/*
 * XACMLProcessor.java
 *
 * Created on 27 février 2006, 07:36
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 * 
 * NOTE: This code is provided without any support
 * stated, intended or otherwise.
 * 
 */
package xacmlse;

import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.component.ComponentContext;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import com.sun.xacml.finder.impl.FilePolicyModule;
import com.sun.xacml.finder.impl.CurrentEnvModule;
import com.sun.xacml.finder.PolicyFinder;
import com.sun.xacml.finder.AttributeFinder;
import com.sun.xacml.ctx.RequestCtx;
import com.sun.xacml.ctx.ResponseCtx;
import com.sun.xacml.PDP;
import com.sun.xacml.PDPConfig;

import java.util.HashSet;
import java.util.logging.Logger;
import java.util.Set;
import java.util.ArrayList;
import java.util.List;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import javax.jbi.messaging.InOut;
import javax.xml.transform.Source;
import org.w3c.dom.Node;

/**
 * Processes the XACML file, received through the JBI bus.
 * @author Serge Blais (100725)
 */
public class XACMLService {

    /**
     * Control point so that we do the configuration only once. Used in DoWork in the 
     * EngineSUDeployment.
     */
    public boolean isConfig = false;
    private PDP mPolicyDecisionPoint = null;
    private DocumentBuilder mDocumentBuilder = null;
    private Transformer mXMLTransformer = null;
    private ByteArrayOutputStream mXMLOutstream = null;
    private ByteArrayInputStream mXMLInstream = null;
    private Logger mLgr = null;
    private ComponentContext mContext = null;

    /**
     * Creates a new instance of XACMLProcessor
     * @param ctx component contextual information holder.
     * @param theLogger logger handle
     */
    public XACMLService(ComponentContext ctx, Logger theLogger) {
        this.mLgr = theLogger;
        this.mContext = ctx;
    }

    static public XACMLService getInstance(ComponentContext ctx, Logger theLogger) {
        return new XACMLService(ctx, theLogger);
    }

    /**
     * processes the message exchange
     * @param exchange message exchange to be processed.
     */
    public void doWork(InOut exchange) {
        this.evaluateRaw(exchange);

    }

    /**
     * Configs the XACML engine with the policy to be used.
     * @param policyFilename file name of the policy to use in this xacml engine.
     * @return true if it worked.
     */
    public boolean config(String policyFilename) {
        try {
            this.configXACMLProcessor(policyFilename);
            this.configDocumentReceiver();
            return true;
        } catch (ParserConfigurationException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return false;
        } catch (TransformerConfigurationException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return false;
        }
    }

    /**
     * Configure the document receiver part of this engine.
     * @throws javax.xml.parsers.ParserConfigurationException thrown if the parser configuration has a problem.
     * @throws javax.xml.transform.TransformerConfigurationException xml transformer problems.
     */
    public void configDocumentReceiver() throws ParserConfigurationException, TransformerConfigurationException {
        DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        docBuilderFactory.setValidating(false);
        docBuilderFactory.setNamespaceAware(true);
        mXMLOutstream = new ByteArrayOutputStream();

        mDocumentBuilder = docBuilderFactory.newDocumentBuilder();
        TransformerFactory tFactory = TransformerFactory.newInstance();
        mXMLTransformer = tFactory.newTransformer();
        mXMLTransformer.setOutputProperty(javax.xml.transform.OutputKeys.OMIT_XML_DECLARATION, "yes");
    }

    /**
     * Configure XACML processor
     * @param policyFilename Filename of the policy to use with this engine's instance.
     */
    public void configXACMLProcessor(String policyFilename) {
        FilePolicyModule policyModule = null;
        CurrentEnvModule envModule = null;
        PolicyFinder policyFinder = null;
        AttributeFinder attrFinder = null;

        policyModule = new FilePolicyModule();
        policyModule.addPolicy(policyFilename);
        envModule = new CurrentEnvModule();

        policyFinder = new PolicyFinder();

        Set policyModules = new HashSet();
        policyModules.add(policyModule);
        policyFinder.setModules(policyModules);

        attrFinder = new AttributeFinder();
        List attrModules = new ArrayList();
        attrModules.add(envModule);
        attrFinder.setModules(attrModules);

        mPolicyDecisionPoint = new PDP(new PDPConfig(attrFinder, policyFinder, null));
    }
    /*
    <Request>
    <Subject>
    <Attribute AttributeId="group"
    DataType="http://www.w3.org/2001/XMLSchema#string"
    Issuer="admin@bea.com">
    <AttributeValue>Deployer
    </Attribute>
    </Subject>
    <Resource>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:resource:resource-id"
    DataType="http://www.w3.org/2001/XMLSchema#anyURI">
    <AttributeValue>http://bea.com/deploy/control.html
    </Attribute>
    </Resource>
    <Action>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:action:action-id"
    DataType="http://www.w3.org/2001/XMLSchema#string">
    <AttributeValue>redeploy
    </Attribute>
    </Action>
    </Request>
     */
    /*<Request>
    <Subject>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:subject:subject-id"
    DataType="urn:oasis:names:tc:xacml:1.0:data-type:rfc822Name">
    <AttributeValue>seth@users.example.com</AttributeValue>
    </Attribute>
    <Attribute AttributeId="group"
    DataType="http://www.w3.org/2001/XMLSchema#string"
    Issuer="admin@users.example.com">
    <AttributeValue>developers</AttributeValue>
    </Attribute>
    </Subject>
    <Resource>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:resource:resource-id"
    DataType="http://www.w3.org/2001/XMLSchema#anyURI">
    <AttributeValue>http://server.example.com/code/docs/developer-guide.html</AttributeValue>
    </Attribute>
    </Resource>
    <Action>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:action:action-id"
    DataType="http://www.w3.org/2001/XMLSchema#string">
    <AttributeValue>read</AttributeValue>
    </Attribute>
    </Action>
    </Request>*/

    /* TO BE DONE EVENTUALLY...
    private boolean evaluateJBI(MessageExchange exchange){
    javax.jbi.messaging.InOut inExchange =(javax.jbi.messaging.InOut)(exchange);
    NormalizedMessage inMsg = inExchange.getInMessage();
    Set subjects=inMsg.g;
    Set resource=null;
    Set action=null;
    Set environment=null;
    String resourceContent=null;
    ResponseCtx response=null;
    if (inMsg == null) return false;
    if (inMsg.getContent() == null) return false;
    this.mLgr.fine(exchange.getPattern().toASCIIString());
    try {
    DOMSource domIn=(DOMSource)(inMsg.getContent());
    mXMLOutstream.reset();
    StreamResult result = new StreamResult(mXMLOutstream);
    mXMLTransformer.transform(inMsg.getContent(), result);
    String contentXML = mXMLOutstream.toString();
    mXMLInstream = new ByteArrayInputStream(contentXML.getBytes());
    this.mLgr.finer(contentXML);
    RequestCtx request =RequestCtx.getInstance(mXMLInstream);
    response= mPolicyDecisionPoint.evaluate(request);
    mXMLOutstream.reset();
    response.encode(mXMLOutstream);
    mXMLInstream = new ByteArrayInputStream(mXMLOutstream.toByteArray());
    // There must be a more direct way to go from an OutStream to a Document....
    org.w3c.dom.Document doc = mDocumentBuilder.parse(mXMLInstream);
    doc.normalize();
    DOMSource dom = new DOMSource(doc);
    this.mLgr.finest(dom.toString());
    NormalizedMessage outMsg = exchange.createMessage();
    outMsg.setContent(dom);
    inExchange.setOutMessage(outMsg);
    } catch (javax.xml.transform.TransformerConfigurationException ex) {
    System.out.println(ex.getLocalizedMessage());
    ex.printStackTrace();
    return false;
    } catch (javax.xml.transform.TransformerException ex) {
    System.out.println(ex.getLocalizedMessage());
    ex.printStackTrace();
    return false;
    }catch (com.sun.xacml.ParsingException ex){
    System.out.println(ex.getLocalizedMessage());
    ex.printStackTrace();
    return false;
    } catch (java.io.IOException ex){
    System.out.println(ex.getLocalizedMessage());
    ex.printStackTrace();
    return false;
    } catch (javax.jbi.messaging.MessagingException ex){
    System.out.println(ex.getLocalizedMessage());
    ex.printStackTrace();
    return false;
    } catch (SAXParseException ex){
    System.out.println(ex.getLocalizedMessage());
    ex.printStackTrace();
    return false;
    }   catch (SAXException ex) {
    System.out.println(ex.getLocalizedMessage());
    ex.printStackTrace();
    return false;
    }
    return true;
    }
     */
    private Source evaluateRaw(InOut exchange) {
        ResponseCtx response = null;
        NormalizedMessage inMsg = exchange.getInMessage();
        if (inMsg == null) {
            return null;
        }
        if (inMsg.getContent() == null) {
            return null;
        }
        this.mLgr.fine(exchange.getPattern().toASCIIString());
        DOMSource domIn = (DOMSource) (inMsg.getContent());
        try {
            mXMLOutstream.reset();
            StreamResult result = new StreamResult(mXMLOutstream);
            mXMLTransformer.transform(inMsg.getContent(), result);
            String requestXML = mXMLOutstream.toString();
            this.mLgr.finer(requestXML);
            mXMLInstream = new ByteArrayInputStream(requestXML.getBytes());
            RequestCtx request = RequestCtx.getInstance(mXMLInstream);
            response = mPolicyDecisionPoint.evaluate(request);

            mXMLOutstream.reset();
            response.encode(mXMLOutstream);
            mXMLInstream = new ByteArrayInputStream(mXMLOutstream.toByteArray());
            // There must be a more direct way to go from an OutStream to a Document....
            org.w3c.dom.Document doc = mDocumentBuilder.parse(mXMLInstream);
            doc.normalize();
            DOMSource dom = new DOMSource(doc);
            this.mLgr.finest(dom.toString());
            NormalizedMessage outMsg = exchange.createMessage();
            outMsg.setContent(dom);
            exchange.setOutMessage(outMsg);

        } catch (javax.xml.transform.TransformerConfigurationException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return null;
        } catch (javax.xml.transform.TransformerException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return null;
        } catch (com.sun.xacml.ParsingException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return null;
        } catch (java.io.IOException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return null;
        } catch (javax.jbi.messaging.MessagingException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return null;
        } catch (SAXParseException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return null;
        } catch (SAXException ex) {
            System.out.println(ex.getLocalizedMessage());
            ex.printStackTrace();
            return null;
        }

        return null;
    }

    public Source evaluatePolicy(InOut exchange)
            throws javax.xml.transform.TransformerConfigurationException,
            javax.xml.transform.TransformerException,
            com.sun.xacml.ParsingException,
            java.io.IOException,
            javax.jbi.messaging.MessagingException,
            SAXParseException,
            SAXException {
        ResponseCtx response = null;
        NormalizedMessage inMsg = exchange.getInMessage();
        if (inMsg == null) {
            return null;
        }
        if (inMsg.getContent() == null) {
            return null;
        }
        this.mLgr.fine(exchange.getPattern().toASCIIString());
        mXMLOutstream.reset();
        DOMSource domIn = (DOMSource) (inMsg.getContent());
        Node n = domIn.getNode(); //jbi.message
        n = n.getFirstChild(); //jbi.part
        n = n.getFirstChild(); //xac:Request
        n = n.getFirstChild(); //xac:Request
        
        StreamResult result = new StreamResult(mXMLOutstream);
        //mXMLTransformer.transform(inMsg.getContent(), result);
        
        /* sblais there seems to be a limitation of the request processor.
         * that the data must be unspecified. Consequently, this removes all 
         * namespaces in the xml structure.
         * nov 27th, 2007
         */
        this.removeNamespace(n);        
        domIn.setNode(n);
        mXMLTransformer.transform(domIn, result);
        String requestXML = mXMLOutstream.toString();
        this.mLgr.finer("*********");
        this.mLgr.finer(requestXML);
        this.mLgr.finer("*********");
        mXMLInstream = new ByteArrayInputStream(requestXML.getBytes());
        RequestCtx request = RequestCtx.getInstance(mXMLInstream);

        response = mPolicyDecisionPoint.evaluate(request);

        mXMLOutstream.reset();
        response.encode(mXMLOutstream);
        mXMLInstream = new ByteArrayInputStream(mXMLOutstream.toByteArray());
        // There must be a more direct way to go from an OutStream to a Document....

        org.w3c.dom.Document doc = mDocumentBuilder.parse(mXMLInstream);
        doc.normalize();
        DOMSource dom = new DOMSource(doc);
        this.mLgr.finest(dom.toString());

        /* sblais 12 nov 2007 commenting out because of refactoring.
         * if it works, delete this code.
        NormalizedMessage outMsg = exchange.createMessage();
        outMsg.setContent(dom);
        exchange.setOutMessage(outMsg);
         */
        return dom;
    }
    private void removeNamespace(Node n){
        if (n==null) return;
        if (n.getNamespaceURI() != null)
            n.setPrefix(null);
        removeNamespace(n.getFirstChild());
        removeNamespace(n.getNextSibling());
    }
}
