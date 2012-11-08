 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.cxf;

import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaLifeCycle;
import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaRuntime;
import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaSUManager;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.jbi.cxf.Jbi4CorbaBareOutInterceptor;
import it.imolinfo.jbi4corba.jbi.cxf.Jbi4CorbaServiceInvokerInterceptor;
import it.imolinfo.jbi4corba.jbi.cxf.Jbi4CorbaWrappedOutInterceptor;
import it.imolinfo.jbi4corba.jbi.endpoint.ProviderEndpoint;
import it.imolinfo.jbi4corba.jbi.processor.transform.SourceTransformer;
import it.imolinfo.jbi4corba.jbi.processor.transform.StringSource;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.test.mock.MockComponentContext;
import it.imolinfo.jbi4corba.test.webservice.generator.WSDLGeneratorTest;
import it.imolinfo.jbi4corba.test.webservice.generator.testclasses.TestEchoBean;
import it.imolinfo.jbi4corba.test.webservice.generator.testclasses.TestInOutEchoBean;
import it.imolinfo.jbi4corba.test.webservice.generator.testclasses.TestUserProfile;
import it.imolinfo.jbi4corba.utils.HelperFileUtil;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.Util;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceCreator;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceInvoker;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.util.List;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import junit.framework.TestCase;
import net.java.hulp.measure.Probe;

import org.apache.cxf.binding.soap.SoapBindingConstants;
import org.apache.cxf.binding.soap.interceptor.RPCInInterceptor;
import org.apache.cxf.binding.soap.interceptor.RPCOutInterceptor;
import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.interceptor.DocLiteralInInterceptor;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.interceptor.FaultOutInterceptor;
import org.apache.cxf.interceptor.StaxInInterceptor;
import org.apache.cxf.interceptor.StaxOutInterceptor;
import org.apache.cxf.jaxws.interceptors.HolderInInterceptor;
import org.apache.cxf.jaxws.interceptors.HolderOutInterceptor;
import org.apache.cxf.jaxws.interceptors.WrapperClassInInterceptor;
import org.apache.cxf.jaxws.interceptors.WrapperClassOutInterceptor;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.ExchangeImpl;
import org.apache.cxf.message.FaultMode;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageImpl;
import org.apache.cxf.phase.PhaseInterceptorChain;
import org.apache.cxf.phase.PhaseManager;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.model.BindingInfo;
import org.apache.cxf.service.model.BindingOperationInfo;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.service.model.MessageInfo;
import org.apache.cxf.service.model.MessagePartInfo;
import org.apache.cxf.service.model.ServiceInfo;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;



/**
 * Test class for the Provider using CXF. The test, from the extended WSDL, creates the service and invokes-it
 * @author marco 
 */
public class CXFProviderTest extends TestCase {      

    public static final String wsdlDirExt = "src/test/etc/wsdlext"; 
    
    public static final String serviceUnitName = "testServiceUnit";
    
    public static final String rootPath = "target/test-wsdl-ext";
    
    public static final String workingPath = "target/test-wsdl-ext/temp";
    
    public static final String suBaseDir = "src/test/etc/sus";      
    
    private static final TransformerFactory TRANSFORMER_FACTORY = TransformerFactory.newInstance();
    
    private String repodir = null;
    
    @Override
    public void setUp() {
    	
		repodir = System.getProperty("localRepository");		

		assertNotNull(repodir, "The localRepositry variable must be set");
		assertFalse("".equals(repodir));
		
        File dir = new File(rootPath);        
        dir.mkdir();
        File dirTemp = new File(workingPath);
        dirTemp.mkdir();
    }
    
    /**
     * Provider test with simple parameters
     */
    public void testSimpleEcho() {
        // String wsdlName = "EchoCXFTest.wsdl";
        String suName = "echo-su-cxf-provider";
        QName interfaceName = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "EchoOperations");
        QName operation = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "echo");
        
        String jbiMessage = 
            "<urn:echo xmlns:urn=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
            "<urn:arg0>test</urn:arg0>" +                 
            "</urn:echo>" ;
        String result = null;
        try {
           result = testServiceCreationAndInvoke(suName, interfaceName, operation, jbiMessage, TestEchoBean.class.getName());
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        }         
        String jbiResultExpected = "<ns1:echoResponse xmlns:ns1=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\"><return xmlns=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">test</return></ns1:echoResponse>";
        System.err.println("********************************");
        System.err.println("RESULT: " + result);
        System.err.println("********************************");
        assertEquals(result, jbiResultExpected);
    }

    
    /**
     * Provider test with simple inout parameters
     */
    public void testSimpleInOutEcho() {
        
        // String wsdlName = "EchoCXFTest.wsdl";
        String suName = "echo-su-inout-cxf-provider";
        QName interfaceName = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "EchoInOutOperations");
        QName operation = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "echo");
        
        String jbiMessage = 
            "<urn:echo xmlns:urn=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
            "<urn:msg>test</urn:msg>" +
            "<urn:msgstr>teststr</urn:msgstr>" +
            "</urn:echo>" ;
        String result = null;
        try {
        	result = testServiceCreationAndInvoke(suName, interfaceName, operation, jbiMessage, TestInOutEchoBean.class.getName());
        } catch (Exception e) {

            e.printStackTrace();
            fail(e.getMessage());
        }
        
        System.err.println("********************************");
        System.err.println("RESULT: " + result);
        System.err.println("********************************");
    }
    
    /**
     * ProviderTest with complex parameters
     */
    public void testUserProfile() {
        
        String suName = "userprofile-su-cxf-provider";
                        
        QName interfaceName = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "UserProfileCorbaInterfaceOperations");
        
        QName operation = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "getUserProfile");

        String jbiMessage = 
        	"<urn:getUserProfile xmlns:urn=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
        	"<urn:code>testUserProfile</urn:code>" +                 
        	"</urn:getUserProfile>";
                                        
        String jbiResultExpected =  "<ns1:getUserProfileResponse xmlns:ns1=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\"><return xmlns=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\"><address>via Selice</address><age>31</age><name>Marco</name><testArray>2</testArray><testArray>3</testArray></return></ns1:getUserProfileResponse>";
        
        String result = null;
        try {
            result = testServiceCreationAndInvoke(suName, interfaceName, operation, jbiMessage, "it.imolinfo.jbi4corba.test.webservice.generator.testclasses.TestUserProfile");
        } catch (Exception e) {            
            e.printStackTrace();
            fail(e.getMessage());
        }       
        System.err.println("********************************");
        System.err.println("RESULT: " + result);
        System.err.println("********************************");
        assertEquals(result, jbiResultExpected);
    }    
    
    /**
     * ProviderTest with complex parameters
     */
    public void testArrayUserProfile() {
        
        String suName = "userprofile-su-cxf-provider";
                        
        QName interfaceName = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "UserProfileCorbaInterfaceOperations");
        
        // QName operation = new QName("http://it.imolinfo.jbi4corba.test.webservice.generator.testclasses/", "getUserProfile");
        QName operation = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "setUserProfileArray");

        String jbiMessage = 
        "<urn:setUserProfileArray xmlns:urn=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
        "<urn:es>testUserProfile</urn:es>" +                 
        "</urn:setUserProfileArray>";
                                
        String jbiResultExpected =  
        	"<ns1:setUserProfileArrayResponse xmlns:ns1=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\"><return xmlns=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">1</return><return xmlns=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">2</return></ns1:setUserProfileArrayResponse>";                                     
        
        String result = null;
        try {
            result = testServiceCreationAndInvoke(suName, interfaceName, operation, jbiMessage, TestUserProfile.class.getName());
        } catch (Exception e) {            
            e.printStackTrace();
            fail(e.getMessage());
        }       
        System.err.println("********************************");
        System.err.println("RESULT: " + result);
        System.err.println("********************************");
        assertEquals(result, jbiResultExpected);
    }    
    
 
    /**
     * ProviderTest with complex fault
     */
    public void testUserProfileFault() {
        
        String suName = "userprofile-su-cxf-provider";
        QName interfaceName = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "UserProfileCorbaInterfaceOperations");
        QName operation = new QName("http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/", "getUserProfile");
        
        String jbiMessage = 
            "<urn:getUserProfile xmlns:urn=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
            "<urn:code>fault</urn:code>" +                 
            "</urn:getUserProfile>";                
        
        String jbiResultExpected =
        	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        	"<ns1:UserProfileException xmlns:ns1=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
        	"<userCode xmlns=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">myuserCode</userCode>" +
        	"<reason xmlns=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">myreason</reason>" +
        	"<profiles xmlns=\"http://testclasses.generator.webservice.test.jbi4corba.imolinfo.it/\">" +
        	"<address>via vai</address><age>23</age><name>Marco</name>" +
        	"</profiles></ns1:UserProfileException>";
              
        
        String result = null;
        try {
            result = testServiceCreationAndInvoke(suName, interfaceName, operation, jbiMessage, TestUserProfile.class.getName());
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
        
        System.err.println("********************************");
        System.err.println("RESULT: " + result);
        System.err.println("********************************");
        
        boolean testRet = compareStringWithDOM(result, jbiResultExpected);

        assertTrue(testRet);        

    }
      
    /**
     * Creates and invoke a service. REturn the XML from invocation, in String form.
     * 
     * @param suNameDir
     * @param interfaceName
     * @param operation
     * @param jbiMessage
     * @param fakeServant
     * @return
     * @throws JBIException
     * @throws DeploymentException
     */
    public String testServiceCreationAndInvoke(String suNameDir, QName interfaceName, 
                                               QName operation, String jbiMessage, String fakeServantClassName) throws JBIException, DeploymentException {
        
    	    	
        // Deploy simulation...             
        String suPath = prepareWorkingPath(suBaseDir, suNameDir);
        
        // Copy the needed jar for the classes compile.
        JarListHelper.copyJarToTargetDir(repodir, suPath);                
        
        Jbi4CorbaRuntime corbaComponent = new Jbi4CorbaRuntime();          
        Jbi4CorbaSUManager suManager = new Jbi4CorbaSUManager(corbaComponent);
        Jbi4CorbaLifeCycle lifeCycle = new Jbi4CorbaLifeCycle(corbaComponent);
        
        MockComponentContext context = new MockComponentContext();
        context.setInstallRoot(suPath);
        RuntimeContext.getInstance().setComponentContext(context);   
        
        lifeCycle.init(context);
        suManager.setLifeCycle(lifeCycle);
        context.setInstallRoot(suPath);
        
        suManager.init(serviceUnitName, suPath);
        suManager.deploy(serviceUnitName, suPath);           
        
        ProviderEndpoint endpoint = null;
                
        try {
                   
            List endpoints = suManager.getDeployedEndpoints();            
            endpoint = (ProviderEndpoint)endpoints.get(0);
            ProviderServiceDescriptor descriptor  = endpoint.getServiceDescriptor();                        

            // Service creation
            ProviderServiceCreator serviceCreator = new ProviderServiceCreator();
            
            Service service = serviceCreator.createService(descriptor, interfaceName);
            
            // Copy the fake servant class to the classloader directory 
            String filePath = "." + File.separator +"target" +File.separator 
            	+ "test-classes" + File.separator 
            	+ Util.replaceDotWithSeparator(fakeServantClassName) + ".class";            
            File classFile = new File(filePath);
            File targetClass = new File(descriptor.getOriginalClassLoader().getURLs()[0].getFile() + File.separator 
                	+ Util.replaceDotWithSeparator(fakeServantClassName) + ".class");            
            HelperFileUtil.copyFile(classFile, targetClass);           
            
            // Change the classLoader and instances the fake servant           
            ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();            
            Thread.currentThread().setContextClassLoader(descriptor.getOriginalClassLoader());            
            Class fakeServantClass  = descriptor.getOriginalClassLoader().loadClass(fakeServantClassName);             
            Object fakeServant = fakeServantClass.newInstance();                                                
                       
            // Sets the "FAKE" Corba servant on the invoker
            ProviderServiceInvoker createdServiceInvoker = (ProviderServiceInvoker)service.getInvoker();
            ProviderServiceInvoker fakeServiceInvoker = 
                new ProviderServiceInvoker(createdServiceInvoker.getServiceDescriptor(), false);
            service.setInvoker(fakeServiceInvoker);                             

            ((ProviderServiceInvoker)service.getInvoker()).setServiceObject(fakeServant);
            
            // Gets the CXF Endpoint
            EndpointInfo ei = CXFUtils.getEndpointInfo(service);              
            Endpoint ep = CXFUtils.getEndpoint(service, ei);

            // Same as:
            // OperationInfo op = service.getServiceInfo().getOperation(exchange.getOperation().getLocalPart());
            ServiceInfo si = service.getServiceInfos().get(0);
            BindingInfo bi = (BindingInfo)si.getBindings().iterator().next();
            BindingOperationInfo bio = bi.getOperation(operation);           
                        
            String parameterStyle = getParameterStyle(bio);
            String bindingStyle = getBindingStyle(bio);
            
            // Creates and sets the Exchange
            Exchange exchange = new ExchangeImpl();
            // exchange.put(Message.SCHEMA_VALIDATION_ENABLED,Boolean.TRUE);
            Probe mMeasurement = Probe.fine(getClass(), endpoint.getUniqueName(),"Denormalization");
            //Add Measurement object to Context, as it will need to be stopped in a different class                
            exchange.put("Measure-deN", mMeasurement);            
            //Add endpoint name also.
            exchange.put("EndpointName", endpoint.getUniqueName());
            
            Message message = new MessageImpl();                                    
            exchange.setOneWay(false);
            exchange.put(BindingOperationInfo.class, bio);
            exchange.put(Service.class, service);            
            exchange.put(Endpoint.class, ep);            
            message.setExchange(exchange);            
            exchange.setInMessage(message);           
            exchange.setOutMessage(new MessageImpl());
            exchange.setOutFaultMessage(new MessageImpl());            
                                                
            // Sets the Source as In message
            message.setContent(InputStream.class, convertMessageToInputStream(new StringSource(jbiMessage)));
            
            PhaseInterceptorChain outInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getOutPhases());
            PhaseInterceptorChain inInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getInPhases());
            PhaseInterceptorChain faultInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getOutPhases());
            
            populateInInterceptors(inInterceptorChain, parameterStyle, bindingStyle);
            populateOutInterceptors(outInterceptorChain, parameterStyle, bindingStyle);
            populateFaultInterceptors(faultInterceptorChain, parameterStyle, bindingStyle);

            message.setInterceptorChain(inInterceptorChain);
    		inInterceptorChain.doIntercept(message);
    		
    		Thread.currentThread().setContextClassLoader(oldClassLoader);
            
            // Tests if Fault are present
            Fault fault = (Fault)exchange.getOutFaultMessage().getContent(Exception.class);
            
            if (fault == null) {            
            	            	
                // OUT message
                Message outMessage = exchange.getOutMessage();                 
                outMessage.setInterceptorChain(outInterceptorChain);  
                
                OutputStream out = new ByteArrayOutputStream();
                outMessage.setContent(OutputStream.class, out);  
                
                // Call the interceptor chain
                outInterceptorChain.doIntercept(outMessage);  
      
              
                return out.toString();                
            } else {
                // FAULT
                
                // Gets the generated fault
                Fault f = (Fault)message.getContent(Exception.class);
                // Gets the fault mode
                // FaultMode can be: CHECKED_APPLICATION_FAULT / UNCHECKED_APPLICATION_FAULT
                FaultMode faultMode = exchange.getOutMessage().get(FaultMode.class);
                System.err.println("Fault received, fault mode: " + faultMode);
                                                               
                // Call the interceptor chain                
                faultInterceptorChain.doIntercept(message);
                
                // Gets the details element:
                Element detail  = f.getDetail();   
                                
                // Converts the Element to a StreamSource
                OutputStream out2 = new ByteArrayOutputStream();
                StreamResult result2 = new StreamResult(out2);        
                SourceTransformer transformer2 = new SourceTransformer();
                transformer2.toResult(new DOMSource(detail), result2);

                System.err.println("***** DETAIL **************************");
                System.err.println(out2.toString());
                System.err.println("**************************************");
                
                Node exceptionDoc = unwrapDetail(detail);
                
                // Converts the Element to a StreamSource
                OutputStream out = new ByteArrayOutputStream();
                StreamResult result = new StreamResult(out);        
                SourceTransformer transformer = new SourceTransformer();
                transformer.toResult(new DOMSource(exceptionDoc), result);

                System.err.println("***** FAULT **************************");
                System.err.println(out.toString());
                System.err.println("**************************************");
                                
                return out.toString();                
            }                        
                            
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
            return null;
        } finally {            
            deleteDir(new File(rootPath));
            deleteDir(new File(workingPath));   
            if (endpoint != null) {
                try {
                    endpoint.unregisterService();
                } catch (Jbi4CorbaException e) {
                    e.printStackTrace();
                }
            }
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
     * Unwraps the detail element that contains the Exception in XML.
     * @param detail
     * @return
     * @throws IOException
     */
    private static Node unwrapDetail(Element detail) {
        return detail.getFirstChild();        
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
     * Copy the WSDL on the working path.
     * @param fileName
     * @return the SU directory
     */
    public String prepareWorkingPath(String wsdlSrcDir, String suName) {
        String wsdlOrig= wsdlSrcDir + File.separator + suName;
        String wsdlDest = rootPath + File.separator + suName;
        File wsdlOrigDir = new File(wsdlOrig);
        File wsdlDestDir = new File(wsdlDest);
        try {
        	JarListHelper.copyDirectory(wsdlOrigDir, wsdlDestDir);
        } catch (Exception e1) {
            e1.printStackTrace();
            fail(e1.getMessage());
        }  
        return wsdlDestDir.getAbsolutePath();
    }
    
   
    
    /**
     * Compares two XML String using DOM.
     * @param expected
     * @param actual
     * @return
     * @throws Exception
     */
    public static boolean compareStringWithDOM( final String actual, final String expected) {
      
        
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder mBuilder;        
        
        boolean domEquals = false;
        
        try {            
            mBuilder = factory.newDocumentBuilder();                        
            Document expectedDoc = mBuilder.parse(new InputSource(new StringReader(expected)));
            Document actualDoc = mBuilder.parse(new InputSource(new StringReader(actual)));   
            expectedDoc.normalize();
            expectedDoc.normalizeDocument();
            actualDoc.normalize();
            actualDoc.normalizeDocument();
            
            String actualDocStr = WSDLGeneratorTest.documentToString(actualDoc, "UTF-8", false);
            String expectedDocStr = WSDLGeneratorTest.documentToString(expectedDoc, "UTF-8", false);           
            
            System.err.println("Expected:" + expectedDocStr);
            System.err.println("Returned:" + actualDocStr);
            
            domEquals = actualDocStr.equals(expectedDocStr);
            
        } catch (ParserConfigurationException ex) {
            ex.printStackTrace();
            return false;
        } catch (SAXException ex) {
            ex.printStackTrace();
            return false;
        } catch (IOException ex) {
            ex.printStackTrace();
            return false;
        } catch (Exception ex) {
            ex.printStackTrace();
            return false;
        }        
        
        return domEquals;       
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
     * Return the correct binding style for the binding, Document o RPC.
     * We don'thave  SOAP binding extensions, so we have to calculate that 
     * from the message part. See Section 7 of the SOAP 1.1 specification.
     * (notice that the WSDL is always Document/literal).
     * @param bio
     * @return <code>SoapBindingConstants.PARAMETER_STYLE_WRAPPED</code> or <code>SoapBindingConstants.PARAMETER_STYLE_BARE</code>
     */
    private static String getBindingStyle(BindingOperationInfo bio) {                	
        String bindingStyle = SoapBindingConstants.BINDING_STYLE_DOC;     
        
        MessageInfo mi = bio.getOperationInfo().getInput();
        MessagePartInfo mpi = mi.getMessagePart(0);
        // There
        if ((mi.getMessageParts().size() != 1)  || 
        	(!mpi.isElement()) ) {
        	bindingStyle = SoapBindingConstants.BINDING_STYLE_RPC;        	
        } 
        return bindingStyle;
    }    
    

    /**
     * This method creates the CXF Interceptors chain and process the message.
     *                
     * @param parameterStyle
     * @param message
     */
	private void populateInInterceptors(PhaseInterceptorChain inInterceptorChain, 
			String parameterStyle, String bindingStyle) { 
		
		inInterceptorChain.add(new StaxInInterceptor());
		// For jax-ws
		inInterceptorChain.add(new WrapperClassInInterceptor());			
		
		inInterceptorChain.add(new HolderInInterceptor());
		inInterceptorChain.add(new Jbi4CorbaServiceInvokerInterceptor());
		
		if (bindingStyle.equals(SoapBindingConstants.BINDING_STYLE_RPC)) {
			inInterceptorChain.add(new RPCInInterceptor());
		} else {            	
		    inInterceptorChain.add(new DocLiteralInInterceptor());
		} 		
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

		// RPC vs Document
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
	private void populateFaultInterceptors(PhaseInterceptorChain faultInterceptorChain, 
			String parameterStyle, String bindingStyle) {
		
        // The FaultOutInterceptor creates and fills the XML details from the cause Exception                 
        faultInterceptorChain.add(new StaxOutInterceptor());
        faultInterceptorChain.add(new FaultOutInterceptor());					
	} 		
	
    
    
	
}
