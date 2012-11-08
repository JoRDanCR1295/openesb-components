/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)SAPWSDLUtilities.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.util;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.Part;
import com.sun.wsdl.model.Service;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLMessage;
import com.ibm.wsdl.PartImpl;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.Endpoint;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.extensions.SAPEnvironmentalVars;
import com.sun.jbi.sapbc.packaging.EndpointDataImpl;
import com.sun.jbi.sapbc.packaging.WSDLConfigurations;
import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import javax.wsdl.Message;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.EntityResolver;

/**
 * Utility class for the examination of Web service definitions generated
 * using SAP NetWeaver conventions.
 *
 * @author Julie Knight (julie.knight@sun.com)
 */
public class SAPWSDLUtilities {
    
    static Logger mLogger = Messages.getLogger(SAPWSDLUtilities.class);
    
    /**
     * Returns a reference to the binding definition in the specified document,
     * that is associated to the specified endpoint (service + port).
     * @param def          The subject document
     * @param serviceName  Name of the endpoint's service definition
     * @param endpointName Name of the port definition associated with the
     *                     sought binding
     *
     * @return Binding object or null
     *
     * @throws NullPointerException if def is null.
     * @throws IllegalArgumentException if serviceName or endpointName is null.
     */
    public static Binding findBindingDefinition(
            final WSDLDefinitions def,
            final QName serviceQName,
            final QName endpointQName) {
        
        Binding binding = null;
        
        final Service service = findServiceDefinition(def, serviceQName);
        if (service != null) {
            final ServicePort port = findServicePortDefinition(service, endpointQName);
            if (port != null) {
                binding = port.getWSDLBinding();
                if (binding == null) {
                    mLogger.warning("No wsdl:binding was found for service ["+serviceQName.toString()+"] endpoint ["+endpointQName.toString()+"] for targetNamespace ["+def.getTargetNamespace()+"]");
                }
            } else {
                mLogger.warning("No service port was found for service ["+serviceQName.toString()+"] endpoint ["+endpointQName.toString()+"] for targetNamespace ["+def.getTargetNamespace()+"]");
            }
        } else {
            mLogger.warning("No service found for ["+serviceQName.toString()+"] for targetNamespace ["+def.getTargetNamespace()+"]");
        }
        return binding;
    }
    
    /**
     * Search for an endpoint's Port definition in the WSDL document.
     *
     * @param service      Service definition to search
     * @param endpointName Name of endpoint to find
     *
     * @return Port object for the specified endpoint, or null if it
     *         is not found.
     *
     * @throws NullPointerException if service is null.
     * @throws IllegalArgumentException if endpointName is null.
     */
    public static ServicePort findServicePortDefinition(final Service service, final QName endpointQName) {
        ServicePort servicePort = null;
        
        final Collection<ServicePort> ports = service.getPorts();
        
        for (Iterator<ServicePort> port = ports.iterator(); port.hasNext(); ) {
            servicePort = port.next();
            servicePort.getName().equals(endpointQName.getLocalPart());
        }
        return servicePort;
    }
    
    /**
     * Search for the specified service in the WSDL definition.
     *
     * @param def         WSDL definition to search
     * @param serviceName Name of service to find
     *
     * @return Service object for the specified service name, or null if it
     *         is not found.
     *
     * @throws NullPointerException if def is null.
     * @throws IllegalArgumentException if serviceName is null.
     */
    public static Service findServiceDefinition(final WSDLDefinitions def, final QName serviceQName) {
        /* debug
        Iterator it = def.getServices().iterator();
        /mLogger.info("###debug number of service is ["+def.getServices().size()+"]");
        while (it.hasNext()) {
            Object obj = it.next();
            mLogger.info("Class is ["+obj.getClass().getName()+"]");
            Service service = (Service) obj;
            mLogger.info("###debug service is ["+service.getQName().toString()+"]");
        }
         **/
        //If the SAP WSDL is imported into another file then def.getServcie will find
        //the service definition even though it does not exist in the current wsdl
        //To avoid duplicate services being found the number of services is checked first.
        if (def.getServices().size() == 0)
            return null;
        else
            return def.getService(serviceQName);
    }
    
    /**
     * Recursively browses the specified directory, looking for files with
     * the specified extensions. All such files are collected and returned in
     * a list.
     *
     * @param dir The directory to browse
     * @param ext The extension to use to match files
     *
     * @return A non-modifiable list of zero or more File objects
     *
     * @throws NullPointerException if dir is null, or is not a directory,
     *         or if an I/O exception occurs
     */
    public static List<File> listResourceFiles(final File dir, final String ext) {
        final List<File> results = new ArrayList<File>();
        final File[] files = dir.listFiles();
        for (int i = 0; i < files.length; ++i) {
            if (files[i].isFile()) {
                if (files[i].getName().toLowerCase().endsWith(ext)) {
                    mLogger.info("Adding file ["+files[i].getName()+"] from ["+dir.getAbsoluteFile()+"]");
                    results.add(files[i]);
                }
            } else if (files[i].isDirectory()) {
                results.addAll(listResourceFiles(files[i], ext));
            }
        }
        return Collections.unmodifiableList(results);
    }
    
    /**
     * Search for the specified service in the WSDL definition.
     *
     * @param wsdlFile         
     * @param interfaceQName         
     * @param serviceQName Name of service to find
     * @param endpointName 
     * @param direction 
     * @param envVars
     *
     * @return Service object for the specified service name, or null if it
     *         is not found.
     *
     * @throws Exception
     */
    public static Endpoint getWSDLEndpointByName(final File wsdlFile, 
            final QName interfaceQName, 
            final QName serviceQName, 
            final String endpointName,
            final EndpointType direction,
            final SAPEnvironmentalVars envVars) 
    throws Exception {
        WSDLConfigurations WSDLConfigs = new WSDLConfigurations( null);
        EntityResolver resolver = new CatalogResolver( new CatalogManager() );
        
        List interfaces = new ArrayList();
        interfaces.add( new EndpointDataImpl( interfaceQName, 
                                              serviceQName, 
                                              endpointName,
                                              EndpointType.INBOUND
                                            ) 
                      );
        WSDLConfigs.setSAPEnvVars(envVars);
        List endpoints = WSDLConfigs.parseWSDL( wsdlFile, resolver, interfaces);
        return (Endpoint) endpoints.get( 0 );
     }
  
    
    /**
     * converts from wsdlmodel message to wsdl4j message
     *
     * @param message wsdlmodel WSDLMessage
     *
     * @return Message wsdl4j message
     *
     * @throws WSDLException WSDLException
     */
    public static Message getMessage(WSDLMessage message)
    throws WSDLException {
        
        // CR6403914 Create a WSDL4J style wrapper around the WSDLMessage instance
        Message msgRet = new MessageWrapper(message);
        
        // Moving this into a lazy evalutation in the MessageWrapper implementation
        // would mean popluation could be avoided unless needed
        QName msgName = message.getQName();
        msgRet.setQName(msgName);
        
        Iterator it = message.getParts().iterator();
        
        while (it.hasNext()) {
            Part part = (Part) it.next();
            javax.wsdl.Part p = new PartImpl();
            p.setName(part.getName());
            msgRet.addPart(p);
        }
        
        return msgRet;
    }
    
    public static void printQualifiedAttribute(QName name, String value, WSDLDefinitions def, PrintWriter pw)
    throws WSDLException {
        if(name != null) {
            printAttribute(getQualifiedValue(name.getNamespaceURI(), name.getLocalPart(), def), value, pw);
        }
    }
    
    private static void printAttribute(String name, String value, PrintWriter pw) {
        if(value != null)
            pw.print((new StringBuilder()).append(' ').append(name).append("=\"").append(cleanString(value)).append('"').toString());
    }
    private static String getQualifiedValue(String namespaceURI, String localPart, WSDLDefinitions def)
    throws WSDLException {
        String prefix = null;
        if(namespaceURI != null && !namespaceURI.equals(""))
            prefix = getPrefix(namespaceURI, def);
        return (new StringBuilder()).append(prefix == null || prefix.equals("") ? "" : (new StringBuilder()).append(prefix).append(":").toString()).append(localPart).toString();
    }
    
    private static String getPrefix(String namespaceURI, WSDLDefinitions def)
    throws WSDLException {
        String prefix = def.getNamespacePrefix(namespaceURI);
        if(prefix == null)
            throw new WSDLException("OTHER_ERROR", (new StringBuilder()).append("Can't find prefix for '").append(namespaceURI).append("'. Namespace prefixes must be set on the").append(" Definition object using the ").append("addNamespace(...) method.").toString());
        else
            return prefix;
    }
    
    public static String cleanString(String orig) {
        if(orig == null)
            return "";
        StringBuffer strBuf = new StringBuffer();
        char chars[] = orig.toCharArray();
        boolean inCDATA = false;
        for(int i = 0; i < chars.length; i++) {
            if(!inCDATA) {
                switch(chars[i]) {
                    case 38: // '&'
                        strBuf.append("&amp;");
                        continue;
                        
                    case 34: // '"'
                        strBuf.append("&quot;");
                        continue;
                        
                    case 39: // '\''
                        strBuf.append("&apos;");
                        continue;
                        
                    case 60: // '<'
                        if(chars.length >= i + 9) {
                            String tempStr = new String(chars, i, 9);
                            if(tempStr.equals("<![CDATA[")) {
                                strBuf.append(tempStr);
                                i += 8;
                                inCDATA = true;
                            } else {
                                strBuf.append("&lt;");
                            }
                        } else {
                            strBuf.append("&lt;");
                        }
                        break;
                        
                    case 62: // '>'
                        strBuf.append("&gt;");
                        break;
                        
                    default:
                        strBuf.append(chars[i]);
                        break;
                }
                continue;
            }
            strBuf.append(chars[i]);
            if(chars[i] == '>' && chars[i - 1] == ']' && chars[i - 2] == ']')
                inCDATA = false;
        }
        
        return strBuf.toString();
    }
    
    public static void doctostring(Document doc) {
        
        OutputFormat format = new OutputFormat(doc, "ISO-8859-1", true);
        format.setStandalone(true);
        format.setIndenting(true);
        
        StringWriter stringOut = new StringWriter();
        XMLSerializer serial = new XMLSerializer(stringOut, format);
        
        try {
            serial.asDOMSerializer();
            serial.serialize(doc.getDocumentElement());
        } catch (Exception ex) {
            ex.printStackTrace();
            String errmsg = "doctostring: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            System.out.println(errmsg);
        }
        
        System.out.println("DOC XML CONTENT \n"+stringOut.toString());
    }
    
    public static void nodetostring(Node node) {
        DOMImplementationLS domImplementation = (DOMImplementationLS) node.getOwnerDocument().getImplementation();
        LSSerializer writer = domImplementation.createLSSerializer();
        writer.setNewLine("\n");
        System.out.println("");
        System.out.println(writer.getClass().getName());
        
        String res = writer.writeToString(node);
        System.out.println("NODE XML CONTENT \n"+res);
    }
    
}
