/*
 * Util.java
 *
 * Created on November 16, 2006, 10:15 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.engine.workflow.clientapi;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.ResourceBundle;

import javax.naming.InitialContext;
import javax.sql.DataSource;
import javax.wsdl.Definition;
import javax.wsdl.PortType;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.EnginePropertyConstants;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.WorkflowMapEntryTable;
import com.sun.jbi.engine.workflow.WorkflowModelManager;
import com.sun.jbi.engine.workflow.db.test.DBUtil;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactoryTest;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.model.ModelFactory;
import com.sun.jbi.workflow.model.Task;

/**
 * 
 * 
 */
public class Util {

    private static String JBI_NAMESPACE = "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper";

    private static QName portTypeTaskCommonQName = new QName(
            "http://jbi.com.sun/wfse/wsdl/TaskCommon", "TaskCommonPortType");

    private static QName portTypeTaskDynamicQName = new QName(
            "http://jbi.com.sun/wfse/wsdl/WorkflowApp16_wf_client", "ApprovePurchasePT");

    private static String taskStaticWsdlFileName = "/com/sun/jbi/engine/workflow/clientapi/TaskCommon.wsdl";

    private static String taskDynamicWsdlFileName = "/com/sun/jbi/engine/workflow/clientapi/WorkflowApp16_wf_client.wsdl";

    private static String claimTaskInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/ClaimTaskInput.xml";

    private static String completeTaskInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/CompleteTaskInput.xml";

    private static String taskListInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListInput.xml";
    
    private static String taskListInputQuery1File = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListInputQuery1.xml";

    private static String taskListInputPaginationFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListPagination.xml";
    
    private static String getTaskInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptInputInput.xml";
    
    private static String revokeTaskInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/RevokeTaskInput.xml";
    
    private static String getTaskInputXFormFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptXFormInput.xml";
    
    private static String getTaskOutputXFormInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptOutputXFormInput.xml";
    
    private static String getTaskInputXFormInstanceInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptInputXFormInputInstance.xml";;

    private static String getTaskOutputXFormInstanceInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptOutputXFormInputInstance.xml";;
    
    private static String getTaskOutputInputFile = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptOutputInput.xml";

    private static ResourceBundle rb = ResourceBundle.getBundle("com.sun.jbi.engine.workflow.clientapi.config");

    
    /** Creates a new instance of Util */
    public Util() {
    }

    public static PortType loadTaskCommonPortType() throws Exception {
        Definition definition = loadTaskCommonDefinition();
        PortType portType = definition.getPortType(portTypeTaskCommonQName);

        return portType;
    }

    public static Definition loadTaskCommonDefinition() throws Exception {

        Definition definition;

        URL url = Util.class.getResource(taskStaticWsdlFileName);
        URI uri = url.toURI();

        WSDLReader reader = WSDLFactory.newInstance().newWSDLReader();
        definition = reader.readWSDL(uri.toString());

        return definition;

    }

    public static PortType loadTaskDynamicPortType() throws Exception {
        Definition definition = loadTaskDynamicDefintion();
        PortType portType = definition.getPortType(portTypeTaskDynamicQName);

        return portType;
    }

    public static Definition loadTaskDynamicDefintion() throws Exception {

        URL url = Util.class.getResource(taskDynamicWsdlFileName);
        URI uri = url.toURI();

        WSDLReader reader = WSDLFactory.newInstance().newWSDLReader();
        Definition definition = reader.readWSDL(uri.toString());

        return definition;
    }

    public static Task loadTask(String fileName) throws Exception {
        URL url = TaskManagerFactoryTest.class.getResource(fileName);
        URI uri = url.toURI();

        File wfFile = new File(uri);

        Task task = ModelFactory.getInstance().getTaskModel(wfFile.getAbsolutePath());

        return task;
    }
    

    public static Task loadTask(Node taskNode) throws Exception {
        Task task = ModelFactory.getInstance().getTaskModel(taskNode);

        return task;
    }

    public static String readFile(String filename) throws FileNotFoundException, IOException {
        BufferedReader in = null;

        try {
            in = new BufferedReader(new InputStreamReader(new FileInputStream(filename), "UTF-8"));

        } catch (java.io.FileNotFoundException e) {
            throw e;

        }

        StringBuffer sb = new StringBuffer();

        // read into string
        try {
            String s = null;

            while ((s = in.readLine()) != null) {
                sb.append(s);
            }

        } catch (java.io.IOException e) {
            throw e;
        } finally {
            if (in != null)
                in.close();
        }

        return sb.toString();
    }

    public static Source loadTaskInput(String fileName) throws Exception {
        URL url = TaskManagerFactoryTest.class.getResource(fileName);
        URI uri = url.toURI();

        File taskInputFile = new File(uri);
        StreamSource streamSource = new StreamSource(taskInputFile);
        DOMResult result = new DOMResult();

        Transformer transformer = TransformerFactory.newInstance().newTransformer();
        transformer.transform(streamSource, result);

        Source s = new DOMSource(result.getNode());
        return s;
    }

    public static Element loadInputMessageElement(String fileName) throws Exception {
        URL url = StaticOperationFactoryTest.class.getResource(fileName);
        URI uri = url.toURI();

        File file = new File(uri);

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document doc = factory.newDocumentBuilder().parse(file);
        doc.normalize();
        return doc.getDocumentElement();
   }
    
    public static Element loadInputMessageElement(String fileName, Class fromClass) throws Exception {
        URL url = fromClass.getResource(fileName);
        URI uri = url.toURI();

        File file = new File(uri);

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document doc = factory.newDocumentBuilder().parse(file);
        doc.normalize();
        return doc.getDocumentElement();
   }    
    
   public static Element loadInputMessageElement (String fileName, String[] tokens, String[] replaces) throws Exception {
       URL url = StaticOperationFactoryTest.class.getResource(fileName);
       URI uri = url.toURI();

       File file = new File(uri);
       String fileContent = readFile(file.getAbsolutePath());
       
       if (tokens != null && replaces !=null && tokens.length == replaces.length ) {
           for (int i = 0; i < tokens.length; i++) {
               fileContent = fileContent.replaceAll(tokens[i], replaces[i]);
           }
       }
       
       return loadString(fileContent);
       
   }
    
    public static Element loadElement(String fileName) throws Exception {
        URL url = StaticOperationFactoryTest.class.getResource(fileName);
        URI uri = url.toURI();

        File file = new File(uri);

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document doc = factory.newDocumentBuilder().parse(file);
        doc.normalize();

        return doc.getDocumentElement();
    }
    
    public static Element loadString(String xmlStr) throws Exception {
        Document doc = XmlUtil.createDocumentFromXML(true, xmlStr);
        return doc.getDocumentElement();
    }

    public static Element getElement(DOMSource source) throws Exception {
        Node node = source.getNode();
        if (node instanceof Document) {
            return ((Document) node).getDocumentElement();
        } else {
            return (Element) node;
        }
    }

    public static Element loadClaimTaskInputElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, claimTaskInputFile);
        return element;
    }

    public static Element loadCompleteTaskInputElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, completeTaskInputFile);
        return element;
    }

    public static Element loadGetTaskListInputElement() throws Exception {
        Element element = loadInputMessageElement(taskListInputFile);
        return element;
    }
    public static Element loadGetTaskListInputElement(int startIndex, int pageSize) throws Exception {
        
        Element element = loadInputMessageElement(taskListInputPaginationFile, new String[] {"\\$start", "\\$pageSize"}, new String [] {new Integer (startIndex).toString(), new Integer (pageSize).toString()});
        return element;
    }    

    public static Element loadGetTaskListQuery1InputElement() throws Exception {
        Element element = loadInputMessageElement(taskListInputQuery1File);
        return element;
    }    
    public static Element loadGetTaskInputElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, getTaskInputFile);
        return element;
    }
    
    public static Element loadGetTaskXformElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, getTaskInputXFormFile);
        return element;
    }
    
    public static Element loadGetTaskOutputXformElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, getTaskOutputXFormInputFile);
        return element;
    }

    public static Element loadGetTaskOutputElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, getTaskOutputInputFile);
        return element;
    }
    
    public static Element loadRevokeTaskInputElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, revokeTaskInputFile);
        return element;
    }
            
    private static Element loadTaskIdInput(long taskId, String fileName) throws Exception {
        Element returnedElement = loadInputMessageElement(fileName);
        NodeList decendents = returnedElement.getElementsByTagNameNS(JBI_NAMESPACE, "part");
        if (decendents.getLength() > 0) {
            // get first jbi part which is for taskid
            Node node = decendents.item(0);
            node.setTextContent("" + taskId);
        }

        return returnedElement;
    }

    /**
     * replace the text content of an element. All the NodeList obtained by running elementXPath
     * will have their text content replaced
     * 
     * @source source element or any ancestor of the element where the content will be replace.
     * @elementXPath QName name of the element whose text content is to be replace.
     * @prefixToNS optional map of prefix to namespace which is used to resolve QName in
     *             elementXPath
     */
    public static void replaceTextContent(Element source, String elementXPath,
            String newTextContent, Map prefixToNS) throws Exception {
        XPathFactory factory = XPathFactory.newInstance();
        XPath xPath = factory.newXPath();
        NamespaceContext context = null;
        if (prefixToNS != null) {
            context = new DefaultNamespaceContext(prefixToNS);
            xPath.setNamespaceContext(context);
        }

        NodeList nodeList = (NodeList) xPath.evaluate(elementXPath, source, XPathConstants.NODESET);

        if (nodeList != null && nodeList.getLength() > 0) {
            for (int i = 0; i < nodeList.getLength(); i++) {
                Node node = nodeList.item(i);
                node.setTextContent("" + newTextContent);
            }
        } else {
            throw new Exception("xpath " + elementXPath
                    + " did not return a valid nodelist in element " + source.getNodeName());
        }

    }

    public static String extractTextContent(Element source, String elementXPath, Map prefixToNS)
            throws Exception {
        String textContent = null;
        XPathFactory factory = XPathFactory.newInstance();
        XPath xPath = factory.newXPath();
        NamespaceContext context = null;
        if (prefixToNS != null) {
            context = new DefaultNamespaceContext(prefixToNS);
            xPath.setNamespaceContext(context);
        }

        Node node = (Node) xPath.evaluate(elementXPath, source, XPathConstants.NODE);

        if (node != null) {
            textContent = node.getTextContent();
        } else {
            throw new Exception("xpath " + elementXPath
                    + " did not return a valid node in element " + source.getNodeName());
        }

        return textContent;
    }

    /**
     * Convert the Element to a string representing XML
     * 
     * @param node
     *            The DOM Node
     * @return The string representing XML
     */
    public static String createXmlString(Node node) {

        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            DOMSource source = new DOMSource(node);
            StreamResult result = new StreamResult(baos);

            Transformer transformer = TransformerFactory.newInstance().newTransformer();

            try {
                transformer.transform(source, result);
            } catch (TransformerException ex) {
                throw ex;
            }

            byte[] bytes = baos.toByteArray();
            String xmlString = new String(bytes, "UTF-8");
            return xmlString;

        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static boolean compareElements(Element expected, Element result) throws Exception {
//        XMLUnit.setControlParser("org.apache.xerces.jaxp.DocumentBuilderFactoryImpl");
//        XMLUnit.setTestParser("org.apache.xerces.jaxp.DocumentBuilderFactoryImpl");
//        XMLUnit.setSAXParserFactory("org.apache.xerces.jaxp.SAXParserFactoryImpl");

        XMLUnit.setIgnoreWhitespace(true);
        org.custommonkey.xmlunit.Diff diff = XMLUnit.compareXML(expected.getOwnerDocument(), result
                .getOwnerDocument());
        if (!diff.identical()) {
            System.err.println("Diffs:" + diff);
            System.out.println("Expected--");
            System.out.println(createXmlString(expected));
            System.out.println("Actual--");
            System.out.println(createXmlString(result));
            System.out.println(expected.getChildNodes().getLength());
        }
        return diff.identical();
    }

    public static String getRandomME() {
        return new Integer(new Random().nextInt()).toString();
    }

    static class DefaultNamespaceContext implements NamespaceContext {

        private Map<String, String> mPrefixToNSMap;

        DefaultNamespaceContext(Map<String, String> prefixToNSMap) {
            this.mPrefixToNSMap = prefixToNSMap;
        }

        public String getNamespaceURI(String prefix) {
            return mPrefixToNSMap.get(prefix);
        }

        public String getPrefix(String namespaceURI) {
            Iterator<String> it = mPrefixToNSMap.keySet().iterator();
            while (it.hasNext()) {
                String prefix = it.next();
                String ns = mPrefixToNSMap.get(prefix);
                if (namespaceURI.equals(ns)) {
                    return prefix;
                }
            }

            return null;
        }

        public Iterator getPrefixes(String namespaceURI) {
            List prefixes = new ArrayList();
            Iterator<String> it = mPrefixToNSMap.keySet().iterator();
            while (it.hasNext()) {
                String prefix = it.next();
                String ns = mPrefixToNSMap.get(prefix);
                if (namespaceURI.equals(ns)) {
                    prefixes.add(prefix);
                }
            }

            return prefixes.iterator();
        }

    }

    public static void initContext(boolean removeSchema) throws Exception {
        String key = TaskManagerFactory.class.getName();
        if (System.getProperty(key).equals ("com.sun.jbi.engine.workflow.runtime.model.impl.MemoryTaskManagerFactoryImpl")) {
            return;
        }
        // TODO Auto-generated method stub
        EngineContext engineContext = new EngineContext();
        InitialContext initialContext = null;

        initialContext = new TestContext();
        DataSource ds = (DataSource) initialContext.lookup("jdbc/__workflow");
        assert (ds != null);

        engineContext.setInitialContext(initialContext);

        WorkflowMapEntryTable workflowMapEntry = new WorkflowMapEntryTable();
        engineContext.setWorkflowMapEntryTable(workflowMapEntry);
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        
        Task task = Util.loadTask(wfFileName);

//        Tasks tasks = (Tasks) task.getParent();
//        QName qName = new QName(tasks.getTargetNamespace(), task.getName());
//        Map<QName, Tasks> tasksMap = new HashMap<QName, Tasks>();
//        tasksMap.put(qName, tasks);
//        workflowMapEntry.setTaskModelMap(tasksMap);

        

        Properties props = DBUtil.getDBProperties();
        props.put(EnginePropertyConstants.DATASOURCE_JNDI, "jdbc/__workflow");
        props.put(EnginePropertyConstants.DATASOURCE_TYPE, props.getProperty("DB_Type"));
        
        props.put(EnginePropertyConstants.TEST_MODE,  removeSchema? "true" : "false");

        engineContext.setConfig(props);
        TaskManagerFactory.getInstance().getTaskManager().setContext(engineContext);
        setModelInContext(task);
        
        WorkflowModelManager modelManager = new WorkflowModelManager();
        
        String taskXFormName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseXform.xhtml";
        Util.setXform(task, modelManager, taskXFormName);
       
        String taskInputXFormInstanceName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseInputInstance.xml";
        Util.setXformInstance(task, modelManager, taskInputXFormInstanceName);
        String taskOutputXFormInstanceName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseOutputInstance.xml";
        Util.setXformInstance(task, modelManager, taskOutputXFormInstanceName);                
        

    }
    
    public static void initTaskManager() {
        initTaskManager(true);        
    }
    
    public static void initTaskManager(boolean removeSchema) {
        String key = TaskManagerFactory.class.getName();
        String factoryImpl = rb.getString(key);
        if(factoryImpl == null) {
            //default is Memory for now
            factoryImpl = "com.sun.jbi.engine.workflow.runtime.model.impl.MemoryTaskManagerFactoryImpl";
        }
        
        System.setProperty(key, factoryImpl);
        try {
            initContext (removeSchema);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
    }    
    
    public static void setModelInContext (Task task ) throws Exception {
        EngineContext engineContext = TaskManagerFactory.getInstance().getTaskManager().getContext();
        if (engineContext != null) {
            WorkflowMapEntryTable workflowMapEntryTable = engineContext.getWorkflowMapEntryTable();
            QName qName = task.getQName();
            WorkflowMapEntry entry = new WorkflowMapEntry (null, null, null, task, WorkflowMapEntry.EntryType.ENTRY_PROVIDE);            
//            Map<QName, Tasks> tasksMap = workflowMapEntryTable.getTaskModelMap();
            List<WorkflowMapEntry> entries = workflowMapEntryTable.getEntryListByServiceUnitName("TEST");
            for (WorkflowMapEntry oldEntry : entries) {
                workflowMapEntryTable.removeEntry(oldEntry, "TEST");
            }
            workflowMapEntryTable.addEntry(entry, "TEST");
        }
    }
    
    public static void setXform (Task task, WorkflowModelManager manager, String fileName) throws Exception {
        URL url = TaskManagerFactoryTest.class.getResource(fileName);
        
        URI uri = url.toURI();
        File xForm = new File(uri);
        Map<QName, WorkflowMapEntry> entryMap = new HashMap<QName, WorkflowMapEntry> ();
        EngineContext engineContext = TaskManagerFactory.getInstance().getTaskManager().getContext();
        if (engineContext != null) {
            WorkflowMapEntryTable workflowMapEntryTable = engineContext.getWorkflowMapEntryTable();
            WorkflowMapEntry entry = workflowMapEntryTable.findWorkflowEntry(task);
            entryMap.put(new QName(task.getTargetNamespace(), task.getName()), entry);
        }        
        manager.loadXform(xForm,entryMap);
    }
    
    public static void setXformInstance (Task task, WorkflowModelManager manager, String fileName) throws Exception {
        URL url = TaskManagerFactoryTest.class.getResource(fileName);
        
        URI uri = url.toURI();
        File instance = new File(uri);
        Map<QName, WorkflowMapEntry> entryMap = new HashMap<QName, WorkflowMapEntry> ();
        EngineContext engineContext = TaskManagerFactory.getInstance().getTaskManager().getContext();
        if (engineContext != null) {
            WorkflowMapEntryTable workflowMapEntryTable = engineContext.getWorkflowMapEntryTable();
            WorkflowMapEntry entry = workflowMapEntryTable.findWorkflowEntry(task);
            entryMap.put(new QName(task.getTargetNamespace(), task.getName()), entry);
        }          
        manager.loadXFormInstance(instance, entryMap);
        
//        EngineContext engineContext = TaskManagerFactory.getInstance().getTaskManager().getContext();
//        if (engineContext != null) {
//            WorkflowMapEntryTable workflowMapEntryTable = engineContext.getWorkflowMapEntryTable();
//            WorkflowMapEntry entry = workflowMapEntryTable.findWorkflowEntry(task);
//            entry.setInputXformInstance(manager.getInputXformInstance(new QName(task.getTargetNamespace(), task.getName())));
//            entry.setOutputXformInstance(manager.getOutputXformInstance(new QName(task.getTargetNamespace(), task.getName())));            
//        }

    }

    public static Element loadGetTaskInputXformInstanceElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, getTaskInputXFormInstanceInputFile);
        return element;
    }
    
    public static Element loadGetTaskOutputXformInstanceElement(long taskId) throws Exception {
        Element element = loadTaskIdInput(taskId, getTaskOutputXFormInstanceInputFile);
        return element;
    }
}
