/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * RuntimeHelper.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

//import it.imolinfo.jbi4ejb.Logger;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.logging.Level;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Helper class that have easy accessors for the common functions in the component
 * runtime.
 *
 * @author Sun Microsystems, Inc.
 */
public final class RuntimeHelper {
    public static Logger getLogger() {
        return RuntimeContext.getInstance().getLogger();
    }
    
    public static void logInfo(String msg) {
        System.out.println(msg);
    }
    public static void logVerbose(String msg) {
        System.out.println(msg);
        
    }
    
    public static void logWarning(Object logObj) {
        System.out.println(logObj);
        
        if ( logObj instanceof Throwable) {
            getLogger().log(Level.WARNING, ((Throwable)logObj).getMessage(), (Throwable)logObj);
        } else {
            getLogger().warning(logObj.toString());
        }
    }
    
    public static void logError(Object logObj) {
        
        System.out.println(logObj);
        
        if ( logObj instanceof Throwable) {
            getLogger().log(Level.SEVERE, ((Throwable)logObj).getMessage(), (Throwable)logObj);
        } else {
            getLogger().severe(logObj.toString());
        }
    }
    
    public static void logDebug(Object logObj) {
        
        System.out.println(logObj);
        
        if ( logObj instanceof Throwable) {
            getLogger().log(Level.FINER, ((Throwable)logObj).getMessage(), (Throwable)logObj);
        } else {
            getLogger().finer(logObj.toString());
        }
    }
    
    public static ComponentContext getComponentContext() {
        return RuntimeContext.getInstance().getComponentContext();
    }
    
    public static DeliveryChannel getDeliveryChannel() {
        return RuntimeContext.getInstance().getDeliveryChannel();
    }
    
    public static String getComponentName() {
        return RuntimeContext.getInstance().getComponentName();
    }
    
    /**
     * helper method to find the active ServiceEndpiont for the service decribed with the
     * serviceDescriptor. This method looks for the Active ServiceEndpoint using interface or
     * servicename or service name and the endpointname.
     */
    public static ServiceEndpoint findServiceEndpoint(ServiceDescriptor serviceDescriptor) {
        
        QName serviceType = serviceDescriptor.getServiceType();
        QName serviceName = serviceDescriptor.getServiceName();
        String endpointName = serviceDescriptor.getEndpointName();
        
        ServiceEndpoint [] refs = null;
        ServiceEndpoint serviceEndpoint = null;
        
        ComponentContext compContext = RuntimeHelper.getComponentContext();
        
        if ( compContext == null ) {
            RuntimeHelper.logDebug("Component context is not yet initialized to Looking for ServiceEndpoint");
            return null;
        }
        
        // first, find the ServiceEndpoint using the abstract service ( serviceType - interface or portType )
        // this way, consumer don't have to worry about who implemented the service.
        // if ServiceEndpoint can not be found with abstract service, then search for it using
        // service name or the service nam and the endpoint to look for specific implemenation.
        
        if ( serviceName != null && endpointName != null ) {
            RuntimeHelper.logDebug(
                "Looking for ServiceEndpoint with service name and endpiont" +
                "ServiceName: " + serviceName +
                "EndpointName: " + endpointName);
            serviceEndpoint =  compContext.getEndpoint(serviceName, endpointName);
        }
        
        if ( serviceEndpoint == null && serviceName != null && endpointName == null) {
            
            RuntimeHelper.logDebug("Looking for ServiceEndpoint with service name " + serviceName);
            refs = compContext.getEndpointsForService(serviceName);
            if ( refs != null && refs.length > 0 ) {
                serviceEndpoint = refs[0];
            }
        }
        
        if ( serviceEndpoint == null && serviceType != null && serviceName == null && endpointName == null) {
            RuntimeHelper.logDebug("Looking for ServiceEndpoint with service type " + serviceType);
            refs = compContext.getEndpoints(serviceType);
            if ( refs != null && refs.length > 0 ) {
                serviceEndpoint = refs[0];
            }
        }
        
        
        
        return serviceEndpoint;
    }
    /**
     * this method creates a InOutMessageExchange Object and sets the required
     * data on the MessageExchange object including the create and set the Normalized
     * message object to hold the input message on the MessageExchange object.
     */
    public static InOut createInOutMessageExchange(QName operation, ServiceEndpoint serviceEndpoint)
    throws MessagingException {
        
        InOut inOutME = null;
        DeliveryChannel channel = RuntimeHelper.getDeliveryChannel();
        
        // create message exchange factory for the endpiont
        MessageExchangeFactory factory =  channel.createExchangeFactory(serviceEndpoint);
        
        // create INOUT Message Exchange
        inOutME = factory.createInOutExchange();
        
        inOutME.setOperation(operation);
        
        // create IN Nomralized Message
        NormalizedMessage inMsg = inOutME.createMessage();
        
        // set IN Normalized message on message exchange
        inOutME.setInMessage(inMsg);
        
        return inOutME;
    }
    
    /**
     * return the DOM Document
     * @param xmlReader Reader
     * @return dom document
     * @throws Exception on parser exception or any other exception
     */
    public static Document buildDOMDocument(Reader xmlReader) throws Exception {
        Document xmlDoc = null;
        DocumentBuilderFactory docBuilderFactory =
            DocumentBuilderFactory.newInstance();
        docBuilderFactory.setValidating(false);
        DocumentBuilder docBuilder =
            docBuilderFactory.newDocumentBuilder();
        docBuilder.setErrorHandler( new DefaultHandler() {
            public void fatalError(SAXParseException e)
            throws SAXException {
                throw new SAXException(e.getMessage());
            }
        });
        
        docBuilder.setEntityResolver(new EntityResolver() {
            public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException {
                StringReader reader = new StringReader("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"); // NOI18N
                InputSource source = new InputSource(reader);
                source.setPublicId(publicId);
                source.setSystemId(systemId);
                return source;
            }
        });
        
        InputSource is = new InputSource(xmlReader);
        xmlDoc = docBuilder.parse(is);
        
        return xmlDoc;
    }
    /**
     * reads xml text from DOMSource to StringBuffer
     */
    public static StringBuffer readFromDOMSource(DOMSource domSource) {
        
        StringWriter writer = new StringWriter();
        
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer trans = null;
        try {
            trans = tFactory.newTransformer();
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION,
                "yes");
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            StreamResult result = new StreamResult(writer);
            trans.transform(domSource, result);
        } catch (TransformerConfigurationException ex) {
            ex.printStackTrace();
        } catch (TransformerException ex) {
            ex.printStackTrace();
        }
        
        return writer.getBuffer();
    }
    /**
     * reads the xml text from InputSource into a StringBuffer
     */
    public  static StringBuffer readFromInputSource(InputSource inSource) {
        
        StringWriter writer = new StringWriter();
        PrintWriter out = new PrintWriter(writer);
        InputStream inStream = inSource.getByteStream();
        Reader reader = inSource.getCharacterStream();
        if ( reader == null ) {
            reader = new InputStreamReader(inStream);
        }
        BufferedReader buff = new BufferedReader(reader);
        try {
            
            for ( String line = null; (line = buff.readLine()) != null ; ) {
                out.println(line);
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
        return writer.getBuffer();
    }
    /**
     * reads xml from from DOM, SAX or Stream Source into a string buffer
     */
    public static StringBuffer readFromSource(Source source) {
        if ( source instanceof DOMSource ) {
            return readFromDOMSource((DOMSource)source);
        } else {
            InputSource inSource = SAXSource.sourceToInputSource(source);
            if ( inSource != null ) {
                return readFromInputSource(inSource);
            } else {
                return null;
            }
        }
    }
    /**
     * creates a DOMSource from the xml text read from the reader.
     */
    public static DOMSource createDOMSource(Reader xmlReader) {
        Document doc = null;
        try {
            doc = buildDOMDocument(xmlReader);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return new DOMSource(doc);
    }
    /**
     * converts the ex stracktrace to string.
     */
    public static StringBuffer getExceptionStackTrace(Exception ex) {
        StringWriter strWriter = new StringWriter();
        if ( ex != null ) {
            PrintWriter out = new PrintWriter(strWriter);
            ex.printStackTrace(out);
        }
        return strWriter.getBuffer();
    }
    /**
     * may be used to set the exception as fault content.
     */
    public static String getExceptionAsXmlText(Exception ex) {
        String message = ex.getMessage();
        String stackTrace = getExceptionStackTrace(ex).toString();
        String exXmlText =
            "<exception>" +
            "<message>" + message + "</message>" +
            "<stack-trace>" + stackTrace + "</stack-trace>" +
            "</exception>" ;
        return exXmlText;
    }
    
}