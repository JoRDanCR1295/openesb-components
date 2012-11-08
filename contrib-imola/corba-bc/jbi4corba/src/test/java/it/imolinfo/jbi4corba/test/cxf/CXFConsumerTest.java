 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.cxf;

import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.jbi.cxf.Jbi4CorbaBareOutInterceptor;
import it.imolinfo.jbi4corba.jbi.cxf.Jbi4CorbaConsumerExceptionInterceptor;
import it.imolinfo.jbi4corba.jbi.cxf.Jbi4CorbaWrappedOutInterceptor;
import it.imolinfo.jbi4corba.jbi.processor.transform.SourceTransformer;
import it.imolinfo.jbi4corba.jbi.processor.transform.StringSource;
import it.imolinfo.jbi4corba.test.webservice.generator.testclasses.TestEchoBean;
import it.imolinfo.jbi4corba.test.webservice.generator.testclasses.TestUserProfile;
import it.imolinfo.jbi4corba.test.webservice.generator.testclasses.TestUserProfileConsumer;
import it.imolinfo.jbi4corba.test.webservice.generator.testclasses.UserProfileException;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceInvoker;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import junit.framework.TestCase;

import org.apache.cxf.binding.soap.SoapBindingConstants;
import org.apache.cxf.binding.soap.interceptor.RPCOutInterceptor;
import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.frontend.MethodDispatcher;
import org.apache.cxf.helpers.DOMUtils;
import org.apache.cxf.interceptor.BareInInterceptor;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.interceptor.StaxInInterceptor;
import org.apache.cxf.interceptor.StaxOutInterceptor;
import org.apache.cxf.interceptor.WrappedInInterceptor;
import org.apache.cxf.jaxws.JAXWSMethodDispatcher;
import org.apache.cxf.jaxws.JaxWsClientFactoryBean;
import org.apache.cxf.jaxws.interceptors.HolderInInterceptor;
import org.apache.cxf.jaxws.interceptors.HolderOutInterceptor;
import org.apache.cxf.jaxws.interceptors.WrapperClassInInterceptor;
import org.apache.cxf.jaxws.interceptors.WrapperClassOutInterceptor;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.ExchangeImpl;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageImpl;
import org.apache.cxf.phase.PhaseInterceptorChain;
import org.apache.cxf.phase.PhaseManager;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.model.BindingOperationInfo;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.service.model.MessageInfo;
import org.apache.cxf.service.model.MessagePartInfo;
import org.apache.cxf.staxutils.FragmentStreamReader;
import org.apache.cxf.staxutils.StaxUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * Test class for the Provider using CXF. The test, from the extended WSDL, creates the service and invokes-it
 * @author marco 
 */
public class CXFConsumerTest extends TestCase {      

    public static final String wsdlDirExt = "src/test/etc/wsdlext"; 
    
    public static final String serviceUnitName = "testServiceUnit";
    
    public static final String rootPath = "target/test-wsdl-ext";
    
    public static final String workingPath = "target/test-wsdl-ext/temp";
    
    private static final TransformerFactory TRANSFORMER_FACTORY = TransformerFactory.newInstance();
    
    public void setUp() {
        File dir = new File(rootPath);        
        dir.mkdir();
        File dirTemp = new File(workingPath);
        dirTemp.mkdir();
    }
    
    
    /**
     * Consumer test with simple parameters.
     */
    public void testSimpleEcho() {
        // String wsdlName = "EchoCXFTest.wsdl";
        QName interfaceName = new QName("urn:it.jb4corba.userprofile", "EchoOperations");
        
        Method methodCalled = null;
        try {
            methodCalled = TestEchoBean.class.getMethod("echo", String.class);            
        } catch (SecurityException e) {
            e.printStackTrace();
            fail(e.getMessage());
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
        
        // Adds the parameters
        List params = new ArrayList();
        params.add("testConsumer");
        
        // Creates the service
        Endpoint  ep = serviceCreation(interfaceName, new TestEchoBean());
        
        String jbiXmlRequest = fromObjectToXML(ep, methodCalled, params);
        System.out.println("xmlMessage:" + jbiXmlRequest);
        
        // Here put the JBI Call...returning:
        
        String jbiXmlExpected = "<ns1:echoResponse xmlns:ns1=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">"+
        "<return>test</return></ns1:echoResponse>";
                          
        Object returnedObj = fromXMLToObject(ep, jbiXmlExpected, methodCalled);
        System.out.println("Returned: " + returnedObj);            
        assertEquals("test", returnedObj);
    }
    
    
    /**
     * Consumer test with complex parameters.
     */
    public void testUserProfile() {
                                               
        QName interfaceName = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "UserProfileConsumerCorbaInterfaceOperations");
        
        Method methodCalled = null;
        try {
            methodCalled = TestUserProfile.class.getMethod("getUserProfile", String.class);            
        } catch (SecurityException e) {
            e.printStackTrace();
            fail(e.getMessage());
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
        
        // Adds the parameters
        List params = new ArrayList();
        params.add("testConsumerGetUserProfile");
        
        // Creates the service
        Endpoint  ep = serviceCreation(interfaceName, new TestUserProfileConsumer());
        
        String jbiXmlRequest = fromObjectToXML(ep, methodCalled, params);
        
        System.out.println("xmlMessage produced:" + jbiXmlRequest);
        /*
        // Here put the JBI Call...returning:
        
        String jbiXmlExpected = "<ns1:getUserProfileResponse xmlns:ns1=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
        		"<return xmlns=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\"><address>via Selice</address><age>31</age><name>Marco</name></return></ns1:getUserProfileResponse>";
                          

        Object returnedObj = fromXMLToObject(ep, jbiXmlExpected, methodCalled);
        System.out.println("Returned: " + returnedObj);
        UserProfile userProf = (UserProfile)returnedObj;
        System.out.println("Name: " + userProf.getName());
        System.out.println("Address: " + userProf.getAddress());
        System.out.println("Age: " + userProf.getAge());
        assertEquals(userProf.getName(), "Marco");
        assertEquals(userProf.getAddress(), "via Selice");
        assertEquals(userProf.getAge(), 31);
        */
    }    
    
    /**
     * Consumer test with complex Fault.
     */
    public void _testUserProfileFault() {
                                               
        QName interfaceName = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "UserProfileConsumerCorbaInterfaceOperations");
        
        Method methodCalled = null;
        try {
            methodCalled = TestUserProfile.class.getMethod("getUserProfile", String.class);            
        } catch (SecurityException e) {
            e.printStackTrace();
            fail(e.getMessage());
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
        
        // Adds the parameters
        List params = new ArrayList();
        params.add("testConsumerGetUserProfile");
        
        // Creates the service
        Endpoint  ep = serviceCreation(interfaceName, new TestUserProfileConsumer());
        
        String jbiXmlRequest = fromObjectToXML(ep, methodCalled, params);
        System.out.println("xmlMessage:" + jbiXmlRequest);
        
        // Here put the JBI Call...returning:
        
//        String jbiXmlExpected =
//            "<detail><ns1:UserProfileException xmlns:ns1=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
//            "<reason xmlns:ns2=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">myreason</reason>"+
//            "<userCode xmlns:ns2=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">myuserCode</userCode>" +
//        "<profiles xmlns:ns2=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " +
//        "xsi:type=\"ns2:userProfileArray\"><item><ns2:address>via vai</ns2:address><ns2:age>23</ns2:age><ns2:name>Marco</ns2:name></item></profiles>" +
//            "</ns1:UserProfileException>" + 
//            "</detail>";
        
        String jbiXmlExpected =
            "<ns1:UserProfileException xmlns:ns1=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
            "<reason xmlns:ns2=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">myreason</reason>"+
            "<userCode xmlns:ns2=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">myuserCode</userCode>" +
        "<profiles xmlns:ns2=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " +
        "xsi:type=\"ns2:userProfileArray\"><item><ns2:address>via vai</ns2:address><ns2:age>23</ns2:age><ns2:name>Marco</ns2:name></item></profiles>" +
            "</ns1:UserProfileException>" + 
            "";        
                                   
        // FAULT Path 
        Throwable returnedObj = fromXMLToException(ep, jbiXmlExpected, methodCalled);
        // Object returnedObj = fromXMLToObject(ep, jbiXmlExpected, methodCalled);
        
        System.out.println("Returned: " + returnedObj.getClass().getName());        
        UserProfileException userProfEx = (UserProfileException)returnedObj;
        System.out.println("UserCode: " + userProfEx.getUserCode());
        System.out.println("Reason: " + userProfEx.getReason());
        System.out.println("USerProfile: " + userProfEx.getProfiles()[0]);      
        assertEquals(userProfEx.getProfiles()[0].getName(), "Marco");
        assertEquals(userProfEx.getProfiles()[0].getAge(), 23);
        assertEquals(userProfEx.getProfiles()[0].getAddress(), "via vai");              
    }       
    
    /**
     * Converts the method call in an XML message
     * @param ep
     * @param methodCalled
     * @param params
     * @return
     */
    public String fromObjectToXML(Endpoint ep, Method methodCalled, List params) {
    
        // Gets the BindingOperationInfo from the method
        JAXWSMethodDispatcher md = (JAXWSMethodDispatcher) 
        ep.getService().get(MethodDispatcher.class.getName());        
        BindingOperationInfo bio = md.getBindingOperation(methodCalled, ep);        
        String parameterStyle = getParameterStyle(bio);
        String bindingStyle = getBindingStyle(bio);
        QName operation = bio.getOperationInfo().getName();
        System.out.println("operation to call:" + operation);           

        // Creates the message
        Message message = new MessageImpl();
        Exchange exchange = new ExchangeImpl();                                    
        exchange.setOneWay(false);
        exchange.put(BindingOperationInfo.class, bio);
        exchange.put(Service.class, ep.getService());            
        exchange.put(Endpoint.class, ep);            
        message.setExchange(exchange);            
        exchange.setOutMessage(message);                  
        exchange.put(StaxOutInterceptor.FORCE_START_DOCUMENT, Boolean.TRUE);
      
        // We have to map the request (input operation).
        message.put(Message.REQUESTOR_ROLE, Boolean.TRUE);        
       
        // Sets the in content
        message.setContent(List.class, params);
                      
        // Sets the output stream 
        OutputStream out = new ByteArrayOutputStream();        
        message.setContent(OutputStream.class, out);
                
        
        // The interceptor order is important! First the Stream is created, than the message converted in xml
        // and then is wrapped. Notice that with a custom binding, we could fix the interceptor execution order statically        
        PhaseInterceptorChain outInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getOutPhases());
        CXFUtils.populateOutInterceptors(outInterceptorChain, parameterStyle, bindingStyle, true);                       
        // Process the message
        message.setInterceptorChain(outInterceptorChain);
        outInterceptorChain.doIntercept(message);
                
        return out.toString();
    }
    
    
    /**
     * Converts the method call in an XML message
     * @param ep
     * @param methodCalled
     * @param params
     * @return
     */
    public Object fromXMLToObject(Endpoint ep, String xml, Method methodCalled) {
    
        // Gets the BindingOperationInfo from the method
        JAXWSMethodDispatcher md = (JAXWSMethodDispatcher) 
        ep.getService().get(MethodDispatcher.class.getName());       
        BindingOperationInfo bio = md.getBindingOperation(methodCalled, ep);  
        String parameterStyle = getParameterStyle(bio);
        String bindingStyle = getBindingStyle(bio);
        
        QName operation = bio.getOperationInfo().getName();
        System.out.println("operation to call:" + operation); 
        // System.out.println(bio.getOperationInfo().getOutput().getMessagePartByIndex(0).getName());
                          
        // Creates the message
        Message message = new MessageImpl();
        Exchange exchange = new ExchangeImpl();                                    
        exchange.setOneWay(false);
        exchange.put(BindingOperationInfo.class, bio);
        exchange.put(Service.class, ep.getService());            
        exchange.put(Endpoint.class, ep);            
        message.setExchange(exchange);            
        
        exchange.setOutMessage(message);
        
        // Important! If not set, maps the object in the input message (the opposite as the objectToXml method)..
        message.put(Message.REQUESTOR_ROLE, Boolean.TRUE);        
                                   
        // Sets the Source as In message
        try {
            message.setContent(InputStream.class, convertMessageToInputStream(new StringSource(xml)));
        } catch (TransformerConfigurationException e) {
            e.printStackTrace();
            fail(e.getMessage());
        } catch (IOException e) {
            e.printStackTrace();
            fail(e.getMessage());
        } catch (TransformerException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
                
        // The interceptor order is important! First the Stream is created, than the message is unwrapped and then 
        // converted into objects. Notice that with a custom binding, we could fix the interceptor execution order statically
        PhaseInterceptorChain inInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getInPhases());

        
        message.setInterceptorChain(inInterceptorChain);                    
        
        this.populateInInterceptors(inInterceptorChain, parameterStyle, bindingStyle);
                                 
        // Process the message
        inInterceptorChain.doIntercept(message);

        // Gets the returned objects
        List outObjs = (List)message.getContent(List.class);        
        
        return outObjs.get(0);
    }
    
    /**
     * Converts the method call in an XML message
     * @param ep
     * @param methodCalled
     * @param params
     * @return
     */
    public Throwable fromXMLToException(Endpoint ep, String xml, Method methodCalled) {                
    
        // Gets the BindingOperationInfo from the method
        JAXWSMethodDispatcher md = (JAXWSMethodDispatcher) 
        ep.getService().get(MethodDispatcher.class.getName());       
        BindingOperationInfo bio = md.getBindingOperation(methodCalled, ep);  
        String parameterStyle = getParameterStyle(bio);
        
        QName operation = bio.getOperationInfo().getName();
        System.out.println("operation to call:" + operation); 
        // System.out.println(bio.getOperationInfo().getOutput().getMessagePartByIndex(0).getName());
                          
        // Creates the message
        Message message = new MessageImpl();
        Exchange exchange = new ExchangeImpl();                                    
        exchange.setOneWay(false);
        exchange.put(BindingOperationInfo.class, bio);
        exchange.put(Service.class, ep.getService());            
        exchange.put(Endpoint.class, ep);            
        message.setExchange(exchange);            
        exchange.setInMessage(message);
        exchange.setOutMessage(new MessageImpl());

        
                
        // Preapres the CXF fault to be processed
        // Mandatory to pass a message...
        String msg =  "ERROR_IN_JBI_PROCESSING";
        Fault fault = new Fault(new org.apache.cxf.common.i18n.Message(msg, (ResourceBundle) null));
                                
        StringSource bodySource = new StringSource(xml);
        XMLStreamReader xmlReader = StaxUtils.createXMLStreamReader(bodySource);
        Element detailContent = null;
        try {
            detailContent = StaxUtils.read(new FragmentStreamReader(xmlReader)).getDocumentElement();
        } catch (XMLStreamException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
                
        Document detailDocument = DOMUtils.createDocument();
        Element detail = detailDocument.createElement("detail");
        detailDocument.adoptNode(detailContent);
        detail.appendChild(detailContent);
                      
        fault.setDetail(detail);
        
        SourceTransformer sourceTransformer = new SourceTransformer();
        String xmlException;
        try {
            xmlException = sourceTransformer.toString(new DOMSource(detail));
            System.err.println("xmlException: " + xmlException);
        } catch (TransformerException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }               
                       
        // Sets the fault on the message
        message.setContent(Exception.class, fault);
                                      
        // Handle the message using the ClientFaultConverter;
        PhaseInterceptorChain faultInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getInPhases());
        message.setInterceptorChain(faultInterceptorChain);     
        //faultInterceptorChain.add(new StaxOutInterceptor());
        //faultInterceptorChain.add(new FaultOutInterceptor());	
        faultInterceptorChain.add(new Jbi4CorbaConsumerExceptionInterceptor());
                               
        // Process the message
        faultInterceptorChain.doIntercept(message);       

        // The ClientFaultConverter replaces the fault with the Exception object in the message for the exception class...
        Throwable th = message.getContent(Exception.class);
        // Gets the returned objects
        // Throwable th = fault.getCause();     
           
        return th;
    }
    
    

      
    /**
     * Creates the service.
     * @param interfaceName
     * @param fakeServant
     * @return
     */
    public Endpoint serviceCreation(QName interfaceName, Object fakeServant) {       
               
        try {       
            // Creates the service from the service interface.       
            JaxWsClientFactoryBean factory = CXFUtils.getJaxWsClientFactoryBean();    
            factory.setServiceClass(fakeServant.getClass());               
            factory.create();
            
            // Gets the service model
            Service service = factory.getServiceFactory().getService();
                        
            // Sets the invoker
            service.setInvoker(new ProviderServiceInvoker(null));
            ((ProviderServiceInvoker)service.getInvoker()).setServiceObject(fakeServant);
                                  
            // Gets the CXF Endpoint
            EndpointInfo ei = CXFUtils.getEndpointInfo(service);              
            Endpoint ep = CXFUtils.getEndpoint(service, ei);
            
            ByteArrayOutputStream baos = new ByteArrayOutputStream();        
            CXFUtils.writeDefinitionOnOutputStream(service, baos);        
            System.err.println(baos.toString());                         
            
            return ep;
 
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
            return null;
        } finally {
            deleteDir(new File(rootPath));
            deleteDir(new File(workingPath));            
        }
        
    }        
    
    private static InputStream convertMessageToInputStream(Source src) throws IOException,
    TransformerConfigurationException, TransformerException {

        final Transformer transformer = TRANSFORMER_FACTORY.newTransformer();

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        StreamResult result = new StreamResult(baos);
        transformer.transform(src, result);

        return new ByteArrayInputStream(baos.toByteArray());
    }    
       
    /**
     * recursively removes all the directory
     * @param dir
     * @return
     */
    public static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
                boolean success = deleteDir(new File(dir, children[i]));
                if (!success) {
                    return false;
                }
            }
        }    
        // The directory is now empty so delete it
        return dir.delete();
    }  
    
    /**
     * Return the correct parameter style for the binding, Wrapped or Bare 
     * (notice that the WSDL is always Document/literal).
     * @param bio
     * @return <code>SoapBindingConstants.PARAMETER_STYLE_WRAPPED</code> or <code>SoapBindingConstants.PARAMETER_STYLE_BARE</code>
     */
    private static String getParameterStyle(BindingOperationInfo bio) {
            
        String parameterStyle = SoapBindingConstants.PARAMETER_STYLE_WRAPPED;
        if (bio!= null) {
            if (bio.getUnwrappedOperation() == null) {
                parameterStyle = SoapBindingConstants.PARAMETER_STYLE_BARE;
            }
        }        
        return parameterStyle;
    }
    
    /**
     * This method creates the CXF OUT Interceptors chain and process the message.
     * 
     * @param parameterStyle
     * @param message
     */
  	private void populateOutInterceptors(PhaseInterceptorChain outInterceptorChain, 
  			String parameterStyle, String bindingStyle) {
  		
        outInterceptorChain.add(new StaxOutInterceptor());
        
        outInterceptorChain.add(new HolderOutInterceptor());
        // For Jax-ws
        outInterceptorChain.add(new WrapperClassOutInterceptor());  
        
  		// RPC  vs Document
  		if (bindingStyle.equals(SoapBindingConstants.BINDING_STYLE_RPC)) {
  			outInterceptorChain.add(new RPCOutInterceptor());
  		} else if (SoapBindingConstants.BINDING_STYLE_DOC.equalsIgnoreCase(bindingStyle)
                && SoapBindingConstants.PARAMETER_STYLE_BARE.equalsIgnoreCase(parameterStyle)) {            
            outInterceptorChain.add(new Jbi4CorbaBareOutInterceptor()); 
  		} else {            
  			// Wrapped
  		    outInterceptorChain.add(new Jbi4CorbaWrappedOutInterceptor());
  		    outInterceptorChain.add(new Jbi4CorbaBareOutInterceptor());
  		} 	
  	} 
	
    /**
     * This method creates the CXF OUT Interceptors chain and process the message.
     * 
     * @param parameterStyle
     * @param message
     */
	private void populateInInterceptors(PhaseInterceptorChain inInterceptorChain, 
			String parameterStyle, String bindingStyle) {
		
		  inInterceptorChain.add(new StaxInInterceptor());        
			inInterceptorChain.add(new WrapperClassInInterceptor());			
			
			inInterceptorChain.add(new HolderInInterceptor());
	      
	      if (parameterStyle.equals(SoapBindingConstants.PARAMETER_STYLE_WRAPPED)) {
	          inInterceptorChain.add(new WrappedInInterceptor());
	      }
	      inInterceptorChain.add(new BareInInterceptor());
       
	} 	
	
    /**
     * Return the correct binding style for the binding, Document o RPC.
     * We don'thave  SOAP binding extensions, so we have to calculate that 
     * from the message part. See Section 7 of the SOAP 1.1 specification.
     * (notice that the WSDL is always Document/literal).
     * @param bio
     * @return <code>SoapBindingConstants.PARAMETER_STYLE_WRAPPED</code> or <code>SoapBindingConstants.PARAMETER_STYLE_BARE</code>
     */
    private static String getBindingStyle(BindingOperationInfo bio) {                	
        String bindingStyle = SoapBindingConstants.BINDING_STYLE_DOC;     
        if (bio.getOperationInfo() != null) {
           MessageInfo mi = bio.getOperationInfo().getInput();
            MessagePartInfo mpi = mi.getMessagePart(0);
            // There
            if ((mi.getMessageParts().size() != 1)  || 
            	(!mpi.isElement()) ) {
            	bindingStyle = SoapBindingConstants.BINDING_STYLE_RPC;        	
            } 
        }
        return bindingStyle;
    }   
            
}