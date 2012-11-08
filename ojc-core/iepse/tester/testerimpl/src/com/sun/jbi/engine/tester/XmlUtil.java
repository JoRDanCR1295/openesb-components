/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 *
 * @author radval
 */
public class XmlUtil {

    static Logger mLogger = Logger.getLogger(XmlUtil.class.getName());
    
    /**
     * Description of the Method
     *
     * @param namespaceAware  Description of the Parameter
     * @return                Description of the Return Value
     * @exception Exception   Description of the Exception
     */
    public static Document createDocument(boolean namespaceAware)
            throws Exception {
        
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        
        factory.setNamespaceAware(namespaceAware);
        
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.newDocument();
        
        return document;
    }
    
    /**
     * Description of the Method
     *
     * @param namespaceAware  Description of the Parameter
     * @param source          Description of the Parameter
     * @return                Description of the Return Value
     * @exception Exception   Description of the Exception
     */
    public static Document createDocument(boolean namespaceAware,
            InputSource source)
            throws Exception {
        
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        
        factory.setNamespaceAware(true);
        //factory.setValidating();
        
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(source);
        
        document.normalize();
        
        return document;
    }
    
    public static Document createDocument(boolean namespaceAware,
            InputStream source)
            throws Exception {
        
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        
        factory.setNamespaceAware(true);
        //factory.setValidating();
        
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(source);
        
        document.normalize();
        
        return document;
    }
    
    
     /**
     * Description of the Method
     *
     * @param namespaceAware  Description of the Parameter
     * @param xml             Description of the Parameter
     * @return                Description of the Return Value
     * @exception Exception   Description of the Exception
     */
    public static Document createDocumentFromXML(boolean namespaceAware,
            String xml)
            throws Exception {
        return createDocument(namespaceAware,
                new InputSource(new StringReader(xml)));
    }
    
    
    public static Document createDocumentFromSource(Source src)
            throws TransformerFactoryConfigurationError, TransformerException {
        DOMResult result = new DOMResult();
        if (src != null) {
            Transformer transformer = TransformerFactory.newInstance().newTransformer();
            try {
                transformer.transform(src, result);
            } finally {
               
            }
        }
        
        return (Document)result.getNode();
    }
    
    /**
     * Gets the text attribute of the DOMUtil class
     *
     * @param node  Description of the Parameter
     * @return      The text value
     */
    public static String getText(Node node) {
        StringBuffer buf = new StringBuffer();
        NodeList children = node.getChildNodes();
        for (int i = 0, I = children.getLength(); i < I; i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.TEXT_NODE) {
                buf.append(child.getNodeValue());
            }
        }
        return buf.toString();
    }
    
    public static String toXml(Node node, String encoding, boolean omitXMLDeclaration) {
        String ret = null;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            Transformer trans = TransformerFactory.newInstance().newTransformer();
            trans.setOutputProperty(OutputKeys.ENCODING, encoding);
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            trans.setOutputProperty(OutputKeys.METHOD, "xml");
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration? "yes":"no");
            trans.transform(new DOMSource(node), new StreamResult(baos));
            ret = baos.toString(encoding);
            
                mLogger.log(Level.FINEST, "XmlUtil.Result", ret);
            
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, "XmlUtil.Fail_to_turn_xml_Node_into_text", e);
        }
        return ret;
    }
    
}
