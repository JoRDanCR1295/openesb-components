package address;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class AddressSearchByCity {
    private static final XPathFactory XPATH_FACT = XPathFactory.newInstance();

    /**
     * @param args
     * @throws XPathExpressionException
     * @throws TransformerException 
     * @throws ParserConfigurationException 
     */
    public static void main(String[] args) throws XPathExpressionException, TransformerException, ParserConfigurationException {
        Document document = parseXmlFile(
                "C:\\Documents and Settings\\mpottlapelli\\workspace\\addressSearch\\bin\\input.xml",
                false);
        String cityName = "Monrovia";
        Node node = addressSearchByCity(document.getDocumentElement(), cityName);

        System.out.println("output is: " + createXmlString(node));
    }

    public static Node addressSearchByCity(Node node, String cityName)
            throws XPathExpressionException, ParserConfigurationException {
        XPath xpath = XPATH_FACT.newXPath();
        MyNamespaceContext nsContext = new MyNamespaceContext();
        nsContext.registerNamespace("per",
                "http://xml.netbeans.org/schema/personAddress");
        xpath.setNamespaceContext(nsContext);
        String xpathExpr = "per:address[per:city='" + cityName + "']";
        NodeList nodeList = (NodeList) xpath.evaluate(xpathExpr, node, XPathConstants.NODESET);
        
        //DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document doc = node.getOwnerDocument(); //builder.newDocument();
        Element docElement = doc.createElementNS("http://xml.netbeans.org/schema/personAddress", "per:searchResultAddress");
        //doc.appendChild(docElement);
        
        Element countElement = doc.createElementNS("http://xml.netbeans.org/schema/personAddress", "per:count");
        countElement.setTextContent(String.valueOf(nodeList.getLength()));
        docElement.appendChild(countElement);
        
        for(int i=0, nodeCount=nodeList.getLength(); i<nodeCount; i++){
            Node currentNode = nodeList.item(i);
            currentNode = doc.importNode(currentNode, true);
            docElement.appendChild(currentNode);
        }

        return docElement;
    }

    static String createXmlString(Node node) throws TransformerException {
        DOMSource source = new DOMSource(node);
        return createXmlString(source);
    }

    private static final TransformerFactory mFactory = TransformerFactory
            .newInstance();

    static String createXmlString(Source source)
            throws TransformerException {
        StringWriter writer = new StringWriter();
        StreamResult result = new StreamResult(writer);

        Transformer transformer = mFactory.newTransformer();

        transformer.transform(source, result);

        String xmlString = writer.toString();
        return xmlString;
    }
    private static Document parseXmlFile(String filename, boolean validating) {
        try {
            // Create a builder factory
            DocumentBuilderFactory factory = DocumentBuilderFactory
                    .newInstance();
            factory.setValidating(validating);
            factory.setNamespaceAware(true);

            // Create the builder and parse the file
            Document doc = factory.newDocumentBuilder().parse(
                    new File(filename));
            return doc;
        } catch (SAXException e) {
            // A parsing error occurred; the xml input is not valid
        } catch (ParserConfigurationException e) {
        } catch (IOException e) {
        }
        return null;
    }
}

class MyNamespaceContext implements NamespaceContext {
    private Map map;

    public MyNamespaceContext() {
        map = new HashMap();
    }

    public void registerNamespace(String prefix, String namespaceURI) {
        map.put(prefix, namespaceURI);
    }

    public String getNamespaceURI(String prefix) {
        String ns = (String) map.get(prefix);
        return ns;
    }

    public String getPrefix(String namespaceURI) {
        Set keys = map.keySet();
        for (Iterator iterator = keys.iterator(); iterator.hasNext();) {
            String prefix = (String) iterator.next();
            String uri = (String) map.get(prefix);
            if (uri.equals(namespaceURI))
                return prefix;
        }
        return null;
    }

    public Iterator getPrefixes(String namespaceURI) {
        List prefixes = new ArrayList();
        Set keys = map.keySet();
        for (Iterator iterator = keys.iterator(); iterator.hasNext();) {
            String prefix = (String) iterator.next();
            String uri = (String) map.get(prefix);
            if (uri.equals(namespaceURI))
                prefixes.add(prefix);
        }
        return prefixes.iterator();
    }
}