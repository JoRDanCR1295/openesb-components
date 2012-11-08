/*
 * WLMProcessor.java
 *
 * Created on April 9, 2007, 1:48 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.workflow.processors;

import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import java.util.List;
import java.util.Map;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.dom4j.DocumentException;
import org.orbeon.oxf.pipeline.api.PipelineContext;
import org.orbeon.oxf.processor.ProcessorInput;
import org.orbeon.oxf.processor.ProcessorOutput;
import org.orbeon.oxf.processor.SimpleProcessor;
import org.orbeon.oxf.processor.ProcessorInputOutputInfo;
import org.orbeon.oxf.processor.ProcessorImpl.KeyValidity;
import org.orbeon.oxf.processor.ProcessorImpl.ProcessorOutputImpl;
import org.orbeon.oxf.xml.dom4j.LocationData;
import org.orbeon.oxf.xml.dom4j.LocationSAXWriter;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;
import org.dom4j.Document;
import org.dom4j.QName;
import org.dom4j.io.SAXReader;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author radval
 */
public class WLMProcessor extends SimpleProcessor {

    /** Creates a new instance of WLMProcessor */
    private static final String pattern = "<redirect-url><path-info>/xforms-jsp/taskDetails.jsp</path-info><parameters><parameter><name>taskId</name><value>?taskId</value></parameter><parameter><name>claimedBy</name><value>?claimedBy</value></parameter></parameters></redirect-url>";

    public WLMProcessor() {
        addInputInfo(new ProcessorInputOutputInfo("instance"));
        addInputInfo(new ProcessorInputOutputInfo("request"));
        addOutputInfo(new ProcessorInputOutputInfo("data"));
    }

    public void generateData(PipelineContext context, ContentHandler contentHandler) throws SAXException {
        String taskIdStr = null;
        String userId = null;
        String claimedBy = null;

        org.w3c.dom.Document instanceDomDocument = readInputAsDOM(context, "instance");
        if (instanceDomDocument != null && instanceDomDocument.getDocumentElement() != null) {
            try {
                Document taskIdDocument = readInputAsDOM4J(context, "request");
                if (taskIdDocument != null && taskIdDocument.getRootElement() != null) {
                    taskIdStr = (String) taskIdDocument.selectObject("string(/request/parameters/parameter[name = 'taskId']/value)");
                    userId = (String) taskIdDocument.selectObject("string(/request/parameters/parameter[name = 'userId']/value)");
                    claimedBy = (String) taskIdDocument.selectObject("string(/request/parameters/parameter[name = 'claimedBy']/value)");
                    if (taskIdStr != null && !taskIdStr.trim().equals("")) {
                        long taskId = Long.parseLong(taskIdStr);
                        try {
                            Element rootElement = toOutputXml(instanceDomDocument.getDocumentElement(), "UTF-8", true);
                            setTaskOutput(taskId, rootElement, userId);
                        } catch (ParserConfigurationException ex) {
                            throw new SAXException(ex);
                        }
                    }
                }
                String redirect = pattern.replace("?taskId", taskIdStr);
                redirect = redirect.replace("?claimedBy", claimedBy);
                Document redirectDoc = new SAXReader().read(new StringReader(redirect));

                LocationSAXWriter saxWriter = new LocationSAXWriter();
                saxWriter.setContentHandler(contentHandler);
                saxWriter.write(redirectDoc);
            } catch (DocumentException ex) {
                Logger.getLogger(WLMProcessor.class.getName()).log(Level.SEVERE, null, ex);
                throw new SAXException(ex);
            }


        }
    }

    public void generateDouble(PipelineContext context,
            ContentHandler contentHandler)
            throws SAXException {

        // Get number from input using DOM4J
        Document numberDocument = readInputAsDOM4J(context, "number");
        String numberString = (String) numberDocument.selectObject("string(/number)");
        int number = Integer.parseInt(numberString);
        String doubleString = Integer.toString(number * 2);

        // Generate output document with SAX
        contentHandler.startDocument();
        contentHandler.startElement("", "number", "number",
                new AttributesImpl());
        contentHandler.characters(doubleString.toCharArray(), 0,
                doubleString.length());
        contentHandler.endElement("", "number", "number");
        contentHandler.endDocument();
    }

    private static Element toOutputXml(Node node, String encoding, boolean omitXMLDeclaration) throws ParserConfigurationException {
        org.w3c.dom.Document doc = createDocument(true);
        Element rootElement = doc.createElement("outputMsg");
        doc.appendChild(rootElement);

        NodeList children = node.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            Node newNode = doc.importNode(child, true);
            rootElement.appendChild(newNode);
        }


        return rootElement;
    }

    public static org.w3c.dom.Document createDocument(boolean namespaceAware)
            throws ParserConfigurationException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        factory.setNamespaceAware(namespaceAware);

        DocumentBuilder builder = factory.newDocumentBuilder();
        org.w3c.dom.Document document = builder.newDocument();

        return document;
    }

    private static String toXml(Node node, String encoding, boolean omitXMLDeclaration) {
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

    private void setTaskOutput(long taskId, Object output, String userId) {


        try { // Call Web Service Operation
            com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
            com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();

            com.sun.workflow.client.ResultCodeType result = port.setTaskOutput(taskId, output, userId);
        //System.out.println("Result = "+result);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    public ProcessorOutput createOutput(final String name) {
        return super.createOutput(name);
    }

    public ProcessorInput createInput(String name) {
        return super.createInput(name);
    }

    public void addInput(String name, ProcessorInput input) {
        super.addInput(name, input);
    }

    protected void addInputInfo(ProcessorInputOutputInfo inputInfo) {
        super.addInputInfo(inputInfo);
    }

    public void addOutput(String name, ProcessorOutput output) {
        super.addOutput(name, output);
    }

    protected void addOutputInfo(ProcessorInputOutputInfo outputInfo) {
        super.addOutputInfo(outputInfo);
    }

    public void deleteInput(ProcessorInput input) {
        super.deleteInput(input);
    }

    public void deleteOutput(ProcessorOutput output) {
        super.deleteOutput(output);
    }

    public String getId() {
        return super.getId();
    }

    public QName getName() {
        return super.getName();
    }

    public Map getConnectedInputs() {
        return super.getConnectedInputs();
    }

    public Map getConnectedOutputs() {
        return super.getConnectedOutputs();
    }

    public ProcessorInput getInputByName(String name) {
        return super.getInputByName(name);
    }

    public ProcessorInputOutputInfo getInputInfo(String name) {
        return super.getInputInfo(name);
    }

    public List getInputsByName(String name) {
        return super.getInputsByName(name);
    }

    public List getInputsInfo() {
        return super.getInputsInfo();
    }

    public ProcessorInputOutputInfo getOutputInfo(String name) {
        return super.getOutputInfo(name);
    }

    public LocationData getLocationData() {
        return super.getLocationData();
    }

    public ProcessorOutput getOutputByName(String name) {
        return super.getOutputByName(name);
    }

    public List getOutputsInfo() {
        return super.getOutputsInfo();
    }

    protected void removeInputInfo(ProcessorInputOutputInfo inputInfo) {
        super.removeInputInfo(inputInfo);
    }

    protected void removeOutputInfo(ProcessorInputOutputInfo outputInfo) {
        super.removeOutputInfo(outputInfo);
    }

    public void reset(PipelineContext pipelineContext) {
        super.reset(pipelineContext);
    }

    public Object getState(PipelineContext context) {
        return super.getState(context);
    }

    protected boolean hasState(PipelineContext context) {
        return super.hasState(context);
    }

    protected Object getOutputObject(PipelineContext pipelineContext, ProcessorOutputImpl processorOutput, String keyName) {
        return super.getOutputObject(pipelineContext, processorOutput, keyName);
    }

    @Override
    protected Object getOutputObject(PipelineContext pipelineContext, ProcessorOutputImpl processorOutput, String keyName, KeyValidity outputKeyValidityImpl) {
        return super.getOutputObject(pipelineContext, processorOutput, keyName,
                outputKeyValidityImpl);
    }
}
