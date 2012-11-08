/*
 * Util.java
 * 
 * Created on Apr 25, 2007, 7:04:09 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.workflow.xml;

import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

/**
 *
 * @author radval
 */
public class Util {

    public Util() {
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
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, omitXMLDeclaration ? "yes"
                    : "no");
            trans.transform(new DOMSource(node), new StreamResult(baos));
            ret = baos.toString(encoding);
        //mLogger.debug("ret: " + ret);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return ret;
    }

    public static String formatData(String input) {
        if (input == null || input.equals("")) {
            return input;
        }

        String rep = java.util.regex.Pattern.compile("\n").matcher(input).replaceAll("");
        rep = java.util.regex.Pattern.compile("\r").matcher(rep).replaceAll("");
        return rep;
    }

    /**
     * Converts the specified {@link InputSource} into a <code>Document</code>.
     * @param src An <code>InputSource</code> containing xml data.
     * @return A DOM Document.
     * @throws Exception if an error occurs reading stream or parsing xml.
     */
    public static Document readXml(InputSource src) throws Exception {
        if (src == null) {
            return null;
        }

        Document expectedDoc = null;
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder docBuilder = factory.newDocumentBuilder();
        expectedDoc = docBuilder.parse(src);


        return expectedDoc;
    }
    
    public static Element toNode(String xmlString) throws Exception {
        if (xmlString == null) {
            return null;
        }
        Document expectedDoc = readXml (new InputSource(new StringReader(xmlString)));
        return expectedDoc.getDocumentElement();
    }    
}
