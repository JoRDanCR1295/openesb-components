package test.jbi.integration.testbc.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.UUID;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.Properties;

import javax.activation.DataHandler;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.Status;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

final public class JbiHelper {

	private static TransformerFactory sTransformerFactory = TransformerFactory
			.newInstance();
	private static DocumentBuilder sBuilder;
	private static String JBI_WSDL_11_WRAPPER_BEGIN1 = "<jbi:message version='1.0' xmlns:jbi='http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper' "
			+ "xmlns:msgns='NS' xmlns:xop='http://www.w3.org/2004/08/xop/include'";
	private static String JBI_WSDL_11_WRAPPER_BEGIN2 = " type='msgns:MSG'>"; 
	private static String JBI_WSDL_11_WRAPPER_PART = "<jbi:part>VALUE</jbi:part>";
	private static String JBI_WSDL_11_WRAPPER_PART_AS_ATTACHMENT = "<jbi:part><xop:Include href='CID'/></jbi:part>";
	private static String JBI_WSDL_11_WRAPPER_END = "</jbi:message>";

	private static Hashtable<String, Object> mCache = new Hashtable<String, Object>();
	private static ThreadPoolExecutor sThreadPool;

	private static MessageProcessor sMessageProcessor;
	private static ComponentContext sComponentContext;

	static void start(ComponentContext componentContext)  throws MessagingException{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		try {
			sBuilder = factory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new RuntimeException(
					"Could not instantiate document builder.");
		}
		sThreadPool = new ThreadPoolExecutor(10, 10, 30, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());

		sMessageProcessor = new MessageProcessor(componentContext);
		sMessageProcessor.start(); 
		sComponentContext = componentContext;
	}
	
	static void stop(){
		if(sMessageProcessor != null){
			sMessageProcessor.stop();
			sMessageProcessor = null;
		}
		
		if(sThreadPool != null){
			sThreadPool.shutdownNow();
			sThreadPool = null;
		}
		sBuilder = null;
		sComponentContext = null;
	}
	
	public static ComponentContext getComponentContext(){
		return sComponentContext;
	}
	
	public static Object getSavedObject(String key){
		return mCache.get(key);
	}
	
	public static Object saveObject(String key, Object obj){
		return mCache.put(key, obj);
	}

	public static Transaction startTransaction(ComponentContext context)
			throws Exception {
		TransactionManager txManager = (TransactionManager) context
				.getTransactionManager();
		txManager.begin();
		Transaction tx = txManager.getTransaction();
		if (tx == null) {
			throw new Exception("Could not start strasaction");
		}
		return tx;
	}

	public static Transaction getTransaction(ComponentContext context)
			throws Exception {
		Transaction tx = null;

		TransactionManager txManager = (TransactionManager) context
				.getTransactionManager();
		tx = txManager.getTransaction();
		return tx;
	}
	
	public static Transaction suspendThreadTx(ComponentContext context)
			throws Exception {
		return ((TransactionManager) context.getTransactionManager()).suspend();
	}

	public static void resumeTransaction(ComponentContext context,
			Transaction currentTx) throws Exception {

		if(currentTx == null)
			return;
		
		Transaction tx = ((TransactionManager)context.getTransactionManager()).getTransaction();
		if(tx == null){
				((TransactionManager) context.getTransactionManager())
						.resume(currentTx);
		}else if(tx != currentTx){
			throw new Exception("Thread is associated with different transaction context");
			} else {
				//Transaction is already associated with the current thread
			}
		}

	public static DOMSource fromXMLToDOMSource(String xml) throws SAXException,
			IOException {
		Document doc = sBuilder.parse(new InputSource(new StringReader(xml)));
		return new DOMSource(doc);
	}

	public static String transformToString(Source src, String encoding)
			throws Exception {
		Transformer trans = sTransformerFactory.newTransformer();
		String xmlData = null;
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		resetOuputProperties(trans); // reset anything previously set
		trans.setOutputProperty(OutputKeys.ENCODING, encoding);
		trans.setOutputProperty(OutputKeys.INDENT, "yes");
		trans.setOutputProperty(OutputKeys.METHOD, "xml");
		trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		trans.transform(src, new StreamResult(baos));
		xmlData = baos.toString(encoding);
		return xmlData;
	}

	public static String wrapIntoJBIMessage(QName message, String[] parts) {
		StringBuffer buf = new StringBuffer();
		buf.append(JBI_WSDL_11_WRAPPER_BEGIN1.replaceFirst("NS", message
				.getNamespaceURI()));
		buf.append(JBI_WSDL_11_WRAPPER_BEGIN2.replaceFirst("MSG", message
				.getLocalPart()));
		for (int i = 0; i < parts.length; ++i) {
			buf
					.append(JBI_WSDL_11_WRAPPER_PART.replaceFirst("VALUE",
							parts[i]));
		}
		buf.append(JBI_WSDL_11_WRAPPER_END);
		return buf.toString();
	}

	public static String wrapIntoJBIMessageAsAttachment(QName message, String cid) {
		StringBuffer buf = new StringBuffer();
		buf.append(JBI_WSDL_11_WRAPPER_BEGIN1.replaceFirst("NS", message
				.getNamespaceURI()));
		buf.append(JBI_WSDL_11_WRAPPER_BEGIN2.replaceFirst("MSG", message
				.getLocalPart()));
		buf.append(JBI_WSDL_11_WRAPPER_PART_AS_ATTACHMENT.replaceFirst("CID", cid));
		buf.append(JBI_WSDL_11_WRAPPER_END);
		return buf.toString();
	}
	
	public static Object[] unwrapFromJBIMessage(Node root) {
		ArrayList<Node> result = new ArrayList<Node>();
		NodeList nodeList = root.getChildNodes();
		for (int i = 0; i < nodeList.getLength(); ++i) {
			Node node = nodeList.item(i);
			if(node.getFirstChild() != null){
				result.add(node.getFirstChild());
			}
		}
		return result.toArray();
	}

	public static Object[] unwrapFromJBISource(Source source) throws Exception {
		DOMResult result = new DOMResult();
		Transformer trans = sTransformerFactory.newTransformer();
		resetOuputProperties(trans); // reset anything previously set
		trans.setOutputProperty(OutputKeys.METHOD, "xml");
		trans.transform(source, result);
		Node root = result.getNode();
		if (root instanceof Document) {
			root = root.getFirstChild();
		}
		return unwrapFromJBIMessage(root);
	}

	public static DOMResult transformToDOMResult(Source source)
			throws Exception {
		DOMResult result = new DOMResult();
		Transformer trans = sTransformerFactory.newTransformer();
		resetOuputProperties(trans); // reset anything previously set
		trans.setOutputProperty(OutputKeys.METHOD, "xml");
		trans.transform(source, result);
		return result;
	}
	
	public static void execute(Runnable cmd){
		sThreadPool.execute(cmd);
	}
	
	public static void registerEndPoint(QName serviceName, String endpointName){
		sMessageProcessor.registerEndPoint(serviceName, endpointName);
	}

	public static void registerEndPoint(QName serviceName, String endpointName, MessageConsumer c){
		sMessageProcessor.registerEndPoint(serviceName, endpointName, c);
	}

	public static Object unregisterEndPoint(QName serviceName, String endpointName){
		return sMessageProcessor.unregisterEndPoint(serviceName, endpointName);
	}
	
	public static MessageExchange getNextMessage(QName serviceName, String endpointName){
		return sMessageProcessor.getNextMessage(serviceName, endpointName);
	}
	
	public static MessageExchange peekNextMessage(QName serviceName, String endpointName){
		return sMessageProcessor.peekNextMessage(serviceName, endpointName);
	}

	public static void activateEndpoint(ComponentContext context, QName serviceName, String endpointName) throws JBIException{
		sMessageProcessor.registerEndPoint(serviceName, endpointName);
		context.activateEndpoint(serviceName, endpointName);
	}

	public static void deactivateEndpoint(ComponentContext context, QName serviceName, String endpointName) throws JBIException{
		ServiceEndpoint ep = context.getEndpoint(serviceName, endpointName);
		sMessageProcessor.unregisterEndPoint(serviceName, endpointName);
		context.deactivateEndpoint(ep);
	}
	
	public static byte[] getAttachment(NormalizedMessage nm, String cid) throws IOException{
		DataHandler dh = nm.getAttachment(cid);
		InputStream in = dh.getInputStream();
		ByteArrayOutputStream bout = new ByteArrayOutputStream();
		byte[] bytes = new byte[128];
		int len;
		while((len = in.read(bytes))!=-1){
			bout.write(bytes, 0, len);
		}
		return bout.toByteArray();
	}
	
    /**
     * reset transformer output properties.
     * Sun JDK Implementation com.sun.org.apache.xalan.internal.xsltc.trax.TransformerImpl reverts the output properties 
     * to properties set in the Templates with xsl:output on calling setOutputProperties(null),
     * but apache implementaion sets it to null and causes nullpointer exception on next call to set Outputproperty.
     * Added this check to support ibm implementation on AIX.
     */
    private static void resetOuputProperties(Transformer trans) {
        if(trans.getClass().getName().startsWith("com.sun"))
        	trans.setOutputProperties(null); // reset anything previously set
        else
        	trans.setOutputProperties(new Properties()); // setting 
    }	
}
