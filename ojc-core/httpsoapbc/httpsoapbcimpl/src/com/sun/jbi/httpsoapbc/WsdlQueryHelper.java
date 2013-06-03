/**
 *
 */
package com.sun.jbi.httpsoapbc;

import com.ibm.wsdl.Constants;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.Import;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.Types;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.extensions.schema.SchemaReference;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.wsdl.extensions.soap12.SOAP12Address;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.coyote.tomcat5.CoyoteRequest;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.ibm.wsdl.extensions.schema.SchemaConstants;
import com.ibm.wsdl.extensions.soap.SOAPConstants;
import com.ibm.wsdl.extensions.soap12.SOAP12Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;
import com.sun.jbi.internationalization.Messages;

/**
 * @author Sujit Biswas
 *
 */
public class WsdlQueryHelper {

    private String lbHost;
    private String lbPort;
 //   private Definition definition;
    private boolean isWsdl;
    private String localhostFQDN;
    private Document doc;
    private ByteBuffer byteBuffer;
    /**
     * this is the port on which http-bc is running
     */
    String httpbcPort;
    private CoyoteRequest request;
    private static Messages mMessages = Messages.getMessages(WsdlQueryHelper.class);
    private static Logger mLog = Messages.getLogger(WsdlQueryHelper.class);

    /**
     *
     */
    public WsdlQueryHelper(CoyoteRequest request, int rPort, ByteBuffer def, boolean wsdl) {

        this.isWsdl = wsdl;

        this.request = request;

        this.byteBuffer = def;

        lbHost = request.getServerName();
        lbPort = Integer.toString(request.getServerPort());

        httpbcPort = Integer.toString(rPort);

        try {

            localhostFQDN = InetAddress.getLocalHost().getCanonicalHostName().toString();

            /*
            if (isWsdl) {
                WSDLFactoryImpl wsdlFactory = new WSDLFactoryImpl();
                WSDLReader reader = (WSDLReader) wsdlFactory.newWSDLReader();
                reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);

                InputSource source = new InputSource(new ByteArrayInputStream(def.array()));

                this.definition = reader.readWSDL("", source);
            } else {
            * */
                DocumentBuilderFactory builderF = DocumentBuilderFactory.newInstance();

                builderF.setNamespaceAware(true);
                DocumentBuilder builder = builderF.newDocumentBuilder();

                doc = builder.parse(new ByteArrayInputStream(def.array()));

            //}

        } catch (Exception e) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "init query helper failed.", e);
            }
        }

    }

    public WsdlQueryHelper(CoyoteRequest request, int rPort, ByteArrayOutputStream baos, boolean wsdl) {

        this.isWsdl = wsdl;

        this.request = request;

     //   this.definition = def;

        lbHost = request.getServerName();
        lbPort = Integer.toString(request.getServerPort());

        httpbcPort = Integer.toString(rPort);

        try {
            localhostFQDN = InetAddress.getLocalHost().getCanonicalHostName().toString();
            
            DocumentBuilderFactory builderF = DocumentBuilderFactory.newInstance();

                builderF.setNamespaceAware(true);
                DocumentBuilder builder = builderF.newDocumentBuilder();

                doc = builder.parse(new ByteArrayInputStream(baos.toByteArray()));
        } catch (Exception e) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "the proxy host may be unknown.", e);
            }
        }

    }

    /*
    public Definition getServiceDescriptor() {

        try {
            modifyWSDL(definition);

        } catch (Exception e) {
            // TODO need to update some of the exception to warning where
            // suitable
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "modify wsdl failed, this will return the original wsdl.", e);
            }
        }
        return definition;
    }*/

    public ByteBuffer getServiceDescriptorAsByteBuffer() {

        /**
         * check if the request is a direct request, return the byte-buffer if
         * true
         */
        if (isDirectRequest()) {
            return byteBuffer;
        }

        if (isWsdl) {
            try {
                updateImportDom(doc.getDocumentElement());
                
                // ESBCOMP-34 : Bad location URL in soap address
                updateSoapAddressDom(doc.getDocumentElement());
            } catch (Exception ex) {
                ex.printStackTrace();
                System.out.println("Exception : " + ex);
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "unable to modify the wsdl.", ex);
                }
            }
        } else {// this is xsd
            try {
                updateSchemaDom(doc.getDocumentElement());
            } catch (Exception e) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "unable to modify the xsd.", e);
                }
            }
        }

        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            String encoding = doc.getXmlEncoding();

            Transformer trans = TransformerFactory.newInstance().newTransformer();
            //trans.setOutputProperty(OutputKeys.ENCODING, encoding);
            if (encoding == null) {
                trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            } else {
                trans.setOutputProperty(OutputKeys.ENCODING, encoding);
            }
            //changes ends here
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            trans.setOutputProperty(OutputKeys.METHOD, "xml");
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, false ? "yes" : "no");
            trans.transform(new DOMSource(doc), new StreamResult(baos));

            return ByteBuffer.wrap(baos.toByteArray());
        } catch (Exception e) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "not able to transform.", e);
            }
        }

        /*
         if (isWsdl) {
         try {
         WSDLFactory wsdlFactory = (WSDLFactory) WSDLFactory.newInstance();
         WSDLWriter writer = (WSDLWriter) wsdlFactory.newWSDLWriter();
         ByteArrayOutputStream baos = new ByteArrayOutputStream();
         writer.writeWSDL(getServiceDescriptor(), baos);
         return ByteBuffer.wrap(baos.toByteArray());
         } catch (Exception ex) {
         if (mLog.isLoggable(Level.FINE)) {
         mLog.log(Level.FINE, "unable to write the wsdl.", ex);
         }
         }
         } else {// this is xsd
         try {
         updateSchemaDom(doc.getDocumentElement());

         ByteArrayOutputStream baos = new ByteArrayOutputStream();
         String encoding = doc.getXmlEncoding();

         Transformer trans = TransformerFactory.newInstance().newTransformer();
         //trans.setOutputProperty(OutputKeys.ENCODING, encoding);
         if(encoding == null){
         trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
         }else{
         trans.setOutputProperty(OutputKeys.ENCODING, encoding);
         }
         //changes ends here
         trans.setOutputProperty(OutputKeys.INDENT, "yes");
         trans.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
         trans.setOutputProperty(OutputKeys.METHOD, "xml");
         trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, false ? "yes" : "no");
         trans.transform(new DOMSource(doc), new StreamResult(baos));

         return ByteBuffer.wrap(baos.toByteArray());

         } catch (Exception e) {
         if (mLog.isLoggable(Level.FINE)) {
         mLog.log(Level.FINE, "not able to transform.", e);
         }
         }
         }
         */
        return null;
    }

    private boolean isDirectRequest() {

        if (isLocalHost()) {
            if (lbPort.equalsIgnoreCase(httpbcPort)) {
                return true;
            }
        }

        return false;
    }

    private boolean isLocalHost() {
        boolean b = "localhost".equalsIgnoreCase(lbHost) || localhostFQDN.equalsIgnoreCase(lbHost);

        if (!b) {
            try {
                String s = InetAddress.getByName(lbHost).getCanonicalHostName();
                if (s.equalsIgnoreCase(localhostFQDN)) {
                    return true;
                }
            } catch (UnknownHostException e) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "unknown host.", e);
                }
            }
        }
        return b;

    }

    @SuppressWarnings("unchecked")
    private void modifyWSDL(Definition def) throws Exception {

        // Search all wsdl imports for the resourceName
        Iterator importLists = def.getImports().values().iterator();
        while (importLists.hasNext()) {
            Iterator imports = ((List) importLists.next()).iterator();
            while (imports.hasNext()) {
                Import anImport = (Import) imports.next();

                String location = anImport.getLocationURI();
                String result = getResultUrl(location);
                anImport.setLocationURI(result);

            }
        }

        // Search all XSD imports and includes for the resourceName
        Types types = def.getTypes();
        if (types != null) {
            Iterator schemas = types.getExtensibilityElements().iterator();
            while (schemas.hasNext()) {
                ExtensibilityElement element = (ExtensibilityElement) schemas.next();
                if (element instanceof Schema) {
                    Schema schema = (Schema) element;

                    handleSchema(schema);

                    updateSchemaDom(schema.getElement());

                }
            }
        }

        Map services = def.getAllServices();
        Iterator iter = services.keySet().iterator();
        while (iter.hasNext()) {
            Service s = (Service) services.get(iter.next());

            Map ports = s.getPorts();
            Iterator iter1 = ports.keySet().iterator();

            while (iter1.hasNext()) {
                Port port = (Port) ports.get(iter1.next());

                List address = port.getExtensibilityElements();
                for (Iterator iterator = address.iterator(); iterator.hasNext();) {
                    Object object = (Object) iterator.next();

                    if (object instanceof SOAPAddress) {

                        SOAPAddress sa = (SOAPAddress) object;
                        String result = getResultUrl(sa.getLocationURI());
                        sa.setLocationURI(result);

                    } else if (object instanceof SOAP12Address) {
                        SOAP12Address sa = (SOAP12Address) object;
                        String result = getResultUrl(sa.getLocationURI());
                        sa.setLocationURI(result);
                    }

                }

            }

        }

    }

    @SuppressWarnings("unchecked")
    private void handleSchema(Schema schema) {
        Map imports = schema.getImports();

        Iterator iter = imports.keySet().iterator();

        while (iter.hasNext()) {
            Collection refs = (Collection) imports.get(iter.next());

            Iterator iterator = refs.iterator();

            while (iterator.hasNext()) {
                SchemaReference si = (SchemaReference) iterator.next();

                si.getSchemaLocationURI();
                String location = si.getSchemaLocationURI();
                String result = getResultUrl(location);
                si.setSchemaLocationURI(result);
            }

        }

        List includes = schema.getIncludes();

        Iterator iter1 = includes.iterator();

        while (iter1.hasNext()) {
            SchemaReference sr = (SchemaReference) iter1.next();
            sr.getSchemaLocationURI();
            String location = sr.getSchemaLocationURI();
            String result = getResultUrl(location);
            sr.setSchemaLocationURI(result);

        }

        List redefines = schema.getRedefines();

        Iterator iter2 = redefines.iterator();

        while (iter2.hasNext()) {
            SchemaReference sr = (SchemaReference) iter2.next();
            sr.getSchemaLocationURI();
            String location = sr.getSchemaLocationURI();
            String result = getResultUrl(location);
            sr.setSchemaLocationURI(result);

        }

    }

    private String getResultUrl(String location) {
        String result = "", hostPort = "", host = "", port = "", rest = "";

        boolean isSecure = request.isSecure();

        if (location.startsWith("http://") && !isSecure) {
            String s = location.substring("http://".length());
            int i = s.indexOf("/");

            rest = s.substring(i);

            hostPort = s.substring(0, i);
            hostPort = hostPort.trim();
            int index = hostPort.indexOf(':');

            if (index == -1) {
                return location;
            }

            host = hostPort.substring(0, index);
            port = hostPort.substring(index + 1);

            if (host.equalsIgnoreCase(getlocalhostFQDN())) {
                host = lbHost;
                if (port.equalsIgnoreCase(httpbcPort)) {
                    port = lbPort;
                }

            }

            result = "http://" + host + ":" + port + rest;

        } else if (location.startsWith("https://") && isSecure) {

            String s = location.substring("https://".length());
            int i = s.indexOf("/");

            rest = s.substring(i);

            hostPort = s.substring(0, i);
            hostPort = hostPort.trim();
            int index = hostPort.indexOf(':');

            if (index == -1) {
                return location;
            }

            host = hostPort.substring(0, index);
            port = hostPort.substring(index + 1);

            if (host.equalsIgnoreCase(getlocalhostFQDN())) {
                host = lbHost;
                if (port.equalsIgnoreCase(httpbcPort)) {
                    port = lbPort;
                }

            }

            result = "https://" + host + ":" + port + rest;
            //Fix for [ bug #102 ] schemaLocation in xsd is empty
        } else {
            // Don't change location for relative paths
            result = location;
        }
        return result;
    }

    private void updateImportDom(Element el) throws Exception {
        Element tempEl = DOMUtils.getFirstChildElement(el);

        for (; tempEl != null; tempEl = DOMUtils.getNextSiblingElement(tempEl)) {
            QName tempElType = QNameUtils.newQName(tempEl);

            if (Constants.Q_ELEM_IMPORT.equals(tempElType)) {
                updateImportDomReference(tempEl);
            }
        }
    }
    
    private void updateSoapAddressDom(Element el) throws Exception {
        Element tempEl = DOMUtils.getFirstChildElement(el);

        for (; tempEl != null; tempEl = DOMUtils.getNextSiblingElement(tempEl)) {
            QName tempElType = QNameUtils.newQName(tempEl);

            if (Constants.Q_ELEM_SERVICE.equals(tempElType)) {
                Element tempPortEl = DOMUtils.getFirstChildElement(tempEl);
                
                for (; tempPortEl != null; tempPortEl = DOMUtils.getNextSiblingElement(tempPortEl)) {
                    QName tempPortElType = QNameUtils.newQName(tempPortEl);
                    
                    if (Constants.Q_ELEM_PORT.equals(tempPortElType)) {
                        
                        Element tempSoapEl = DOMUtils.getFirstChildElement(tempPortEl);
                        QName tempSoapElType = QNameUtils.newQName(tempSoapEl);
                        
                        if (SOAPConstants.Q_ELEM_SOAP_ADDRESS.equals(tempSoapElType) ||
                                SOAP12Constants.Q_ELEM_SOAP_ADDRESS.equals(tempSoapElType)) {
                            updateSoapAddressDomReference(tempSoapEl);
                        }
                    }
                }
            }
        }
    }

    private void updateSchemaDom(Element el) throws Exception {
        Element tempEl = DOMUtils.getFirstChildElement(el);

        for (; tempEl != null; tempEl = DOMUtils.getNextSiblingElement(tempEl)) {
            QName tempElType = QNameUtils.newQName(tempEl);

            if (SchemaConstants.XSD_IMPORT_QNAME_LIST.contains(tempElType) || SchemaConstants.XSD_INCLUDE_QNAME_LIST.contains(tempElType)
                    || SchemaConstants.XSD_REDEFINE_QNAME_LIST.contains(tempElType)) {
                updateSchemaDomReference(tempEl);
            }
        }
    }

    private void updateImportDomReference(Element tempEl) throws Exception {
        String locationURI = DOMUtils.getAttribute(tempEl, Constants.ATTR_LOCATION);
        if (locationURI != null) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "nlocationURI = " + locationURI);
            }
            String result = getResultUrl(locationURI);

            setAttribute(tempEl, Constants.ATTR_LOCATION, result);

        }
    }

    private void updateSchemaDomReference(Element tempEl) throws Exception {
        String locationURI = DOMUtils.getAttribute(tempEl, SchemaConstants.ATTR_SCHEMA_LOCATION);
        if (locationURI != null) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "nlocationURI = " + locationURI);
            }
            String result = getResultUrl(locationURI);

            setAttribute(tempEl, SchemaConstants.ATTR_SCHEMA_LOCATION, result);

        }
    }
    
    private void updateSoapAddressDomReference(Element tempEl) throws Exception {
        String locationURI = DOMUtils.getAttribute(tempEl, Constants.ATTR_LOCATION);
        if (locationURI != null) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "nlocationURI = " + locationURI);
            }
            String result = getResultUrl(locationURI);

            setAttribute(tempEl, Constants.ATTR_LOCATION, result);

        }
    }

    private void setAttribute(Element el, String attrName, String attrValue) {
        Attr attr = el.getAttributeNode(attrName);

        if (attr != null) {
            attr.setValue(attrValue);
        }
    }

    private String getlocalhostFQDN() {
        return localhostFQDN;
    }
}
