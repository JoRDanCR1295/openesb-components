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
 * @(#)WSDLConfigurations.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc.packaging;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.smtpbc.Endpoint;
import com.sun.jbi.smtpbc.Endpoint.EndpointType;
import com.sun.jbi.smtpbc.EndpointImpl;
import com.sun.jbi.smtpbc.extensions.SMTPAddress;
import com.sun.jbi.smtpbc.extensions.SMTPBinding;
import com.sun.jbi.smtpbc.extensions.SMTPConstants;
import com.sun.jbi.smtpbc.extensions.SMTPExtPreprocessDeserializer;
import com.sun.jbi.smtpbc.extensions.SMTPExtensionRegistry;
import com.sun.jbi.smtpbc.extensions.SMTPOperation;
import com.sun.jbi.smtpbc.extensions.SMTPOperationInput;
import com.sun.jbi.smtpbc.extensions.SMTPOperationOutput;
import com.sun.jbi.smtpbc.packaging.EndpointConfiguration.PortMap;
import com.sun.xsd.model.FastSchema;
import com.sun.xsd.model.FastSchemaFactory;

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints
 * based on the portmap list.
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class WSDLConfigurations {

    private static final Messages mMessages = 
        Messages.getMessages(WSDLConfigurations.class);

    private static final Logger mLogger = Messages.getLogger(WSDLConfigurations.class); 
    
    private String mRootPath;
    private List mXsds = new ArrayList();

    private final Map mEncoderMap = new HashMap();
    
    public WSDLConfigurations (final String rootPath) {
        mRootPath = rootPath;
    }

    public Collection parse(final Collection portMaps,Map envVarMap)
        throws Exception {

    	Map envVariables = (Map)((HashMap)envVarMap).clone();
        final File dir = new File(mRootPath);

        final CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() +
                                       File.separator +
                                       "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        final EntityResolver resolver = new CatalogResolver(catalogManager);

        mXsds = listResourceFiles(dir, ".xsd");
        final ArrayList endpoints = new ArrayList();
        final Iterator wsdls = listResourceFiles(dir,".wsdl").iterator();
        while (wsdls.hasNext()) {
            endpoints.addAll(parseWSDL((File)wsdls.next(), resolver, portMaps, envVariables));
        }

        return endpoints;
    }

    public Collection parseWSDL(final File wsdlFile,
                                final EntityResolver resolver,
                                final Collection portMaps, Map envVarMap) 
        throws Exception {

        final Definition def = readWSDL(wsdlFile, resolver, envVarMap);
        final ArrayList endPoints = new ArrayList();
        final Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            final PortMap pm = (PortMap)it.next();

            // Check the Definition if it has an SMTPBinding.  If not,
            // continue
            final SMTPBinding binding = getSMTPBinding(def,
                                                 pm.getService(),
                                                 pm.getEndpoint());
            if (binding == null) {
                continue;
            }

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint.  If we find one,
            // extract file details

            // If we have an SMTPBinding, we must have an SMTPAddress.
            final SMTPAddress address =
                getSMTPAddress(def, pm.getService(), pm.getEndpoint());
            if (address == null) {
                final String msg = WSDLConfigurations.mMessages.getString(
                    "WCF_Missing_addr") + pm.getService();                
                throw new Exception(msg);
            }
            
            // If we have an SMTPBinding, we must have operations
            final Map smtpOperations =
                getSMTPOperations(def, pm.getService(), pm.getEndpoint());

            if ((smtpOperations == null) || (smtpOperations.size() == 0)) {
                final String msg =
                    WSDLConfigurations.mMessages.getString("WCF_Missing_ops") +
                    pm.getService();                
                throw new Exception(msg);
            }
                    
            // Create an Endpoint for each Port.  The endpoint should have the
            // correct SMTPBinding, the associated Operations, and the
            // associated OperationInput and OperationOutput
            final Endpoint endpoint = new EndpointImpl();

            // Store the endpoint name
            endpoint.setEndpointName(pm.getEndpoint());
            
            // Store the Service name
            endpoint.setServiceName(QName.valueOf(pm.getService()));

            // Store the Definition
            endpoint.setDefinition(def);
                        
            // Store the type of Endpoint
            endpoint.setEndpointType(pm.getDirection());

            // Set the description
            final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
            final DocumentBuilder documentBuilder = docBuilderFactory.newDocumentBuilder();
            final Document result = documentBuilder.parse(wsdlFile);
            endpoint.setServiceDescription(result);

            // Store our extensibility elements
            endpoint.setSMTPAddress(address);
            endpoint.setSMTPBinding(binding);
            endpoint.setSMTPOperations(smtpOperations);
            setInputsOutputs(def, pm.getService(), pm.getEndpoint(),
                             smtpOperations.values(), endpoint);

            // Now add the Endpoint to our list of Endpoints
            endPoints.add(endpoint);
        }

        return endPoints;
    }


    /**
     * List all wsdl files in the currentDir and below
     */    
    protected List listResourceFiles(final File currentDir, final String extension) {
        final List cumulativeResults = new ArrayList();
        final File[] filesInCurrentDir = currentDir.listFiles();
        for (final File element : filesInCurrentDir) {

		if (element.isFile()) {
		    if (element.getName().toLowerCase().endsWith(extension)) {
		        cumulativeResults.add(element);
		    }
		} else if (element.isDirectory()) {
		    final List wsdlsInSubDirectories =
		        listResourceFiles(element, extension);
		    cumulativeResults.addAll(wsdlsInSubDirectories);
		}
      }
        return cumulativeResults;
    }    



    protected Definition readWSDL(final File f, final EntityResolver resolver, Map envVarMap)
        throws javax.wsdl.WSDLException {
        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        final WSDLReader reader =
            ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new SMTPExtensionRegistry(envVarMap));
        final Definition def = reader.readWSDL(f.getAbsolutePath());
        
        return def;
    }
 
        
    protected Binding getBinding(final Definition def, final String serviceName,
                                 final String endpointName) {
        final Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        final Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }
        
    protected SMTPAddress getSMTPAddress(final Definition def, final String serviceName,
                                         final String endpointName) {
        SMTPAddress address = null;
        final Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        final Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            final List extElems = port.getExtensibilityElements();
                
            //Look for file:address
                
            final Iterator extIter = extElems == null ? null : extElems.iterator();
            while ((extIter != null) && extIter.hasNext() && (address == null)) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SMTPAddress.class.isInstance(ee)) {
                    address = (SMTPAddress) ee;
                }
            }
        }
        return address;
    }
        
    protected SMTPBinding getSMTPBinding(final Definition def, final String serviceName, final String endpointName) {
        SMTPBinding fileBinding = null;
        final Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            final List extElems = binding.getExtensibilityElements();
                
            //Look for file:binding
                
            final Iterator extIter = extElems == null ? null : extElems.iterator();
            while ((extIter != null) && extIter.hasNext() && (fileBinding == null)) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SMTPBinding.class.isInstance(ee)) {
                    fileBinding = (SMTPBinding) ee;
                }
            }
        }
        return fileBinding;
    }
        
    protected Map getSMTPOperations(final Definition def,
                                    final String serviceName,
                                    final String endpointName) {
        final Map smtpOperations = new HashMap();
        final Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            final List bindingOperations = binding.getBindingOperations();
            final Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while ((operIter != null) && operIter.hasNext()) {
                final BindingOperation oper = (BindingOperation) operIter.next();
                final List extElems = oper.getExtensibilityElements();
                // Look for file:operation entries
                    
                final Iterator extIter = extElems == null ? null : extElems.iterator();
                while ((extIter != null) && extIter.hasNext()) {
                    final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (SMTPOperation.class.isInstance(ee)) {
                    	final SMTPOperation smtpOperation = (SMTPOperation)ee;
                        final BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            final Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                final ExtensibilityElement inputExt = (ExtensibilityElement) inputIter.next();
                                if (inputExt instanceof SMTPOperationInput) {
                                	final SMTPOperationInput smtpOperationInput = (SMTPOperationInput)inputExt;
                                	smtpOperation.setSmtpInput(smtpOperationInput);
                                }
                            }
                        }                    	
                        smtpOperations.put(QName.valueOf(oper.getName()),
                                           ee);
                    }
                }
            }
        }
        return smtpOperations;
    }

    protected void setInputsOutputs (final Definition def,
                                     final String serviceName,
                                     final String endpointName,
                                     final Collection operations,
                                     final Endpoint endpoint) {

        final Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            final List bindingOperations = binding.getBindingOperations();
            final Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while ((operIter != null) && operIter.hasNext()) {
                final BindingOperation oper = (BindingOperation) operIter.next();
                final List extElems = oper.getExtensibilityElements();
                // Look for file:operation entries
                    
                SMTPOperation smtpOperation = null;
                final Iterator extIter = extElems == null ? null : extElems.iterator();
                while ((extIter != null) && extIter.hasNext()) {
                    final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (operations.contains(ee)) {
                        smtpOperation = (SMTPOperation)ee;
                    }
                }

                if (smtpOperation != null) {
                    final BindingInput bindingInput = oper.getBindingInput();
                    if (bindingInput != null) {
                        final Iterator it = bindingInput.getExtensibilityElements().iterator();
                        while (it.hasNext()) {
                            final ExtensibilityElement ee = (ExtensibilityElement)it.next();
                            if (ee instanceof SMTPOperationInput) {
                                endpoint.setSMTPOperationInput(smtpOperation,
                                                               (SMTPOperationInput)ee);
                            }
                        }
                    }
                    
                    final BindingOutput bindingOutput = oper.getBindingOutput();
                    if (bindingOutput != null) {
                        final Iterator it2 = bindingOutput.getExtensibilityElements().iterator();
                        while (it2.hasNext()) {
                            final ExtensibilityElement ee = (ExtensibilityElement)it2.next();
                            if (ee instanceof SMTPOperationOutput) {
                                endpoint.setSMTPOperationOutput(smtpOperation,
                                                            (SMTPOperationOutput)ee);
                            }
                        }
                    }

                }
            }
        }
    }
    
    public Map getPartEncoderMapping(final Definition def,
						            final String serviceName,
						            final String endpointName,
						            final EndpointType endpointType,
						            final Map smtpOperations) throws Exception {
    	

		
		final Map partMapping = new HashMap();
		
		// Don't support inline encoder schemas
		if (mXsds.size() <= 0) {
			return partMapping;
		}
		
		final Service service = def.getService(QName.valueOf(serviceName));
		final Port port = service.getPort(endpointName);
		final PortType portType = port.getBinding().getPortType();
		

		//javax.wsdl.Message wsdlMessage = null;
		//Map parts = new HashMap();
        for (final Iterator opnameIter = smtpOperations.keySet().iterator(); opnameIter.hasNext();) {
            final QName operationName = (QName) opnameIter.next();

            final SMTPOperation smtpOperation = (SMTPOperation) smtpOperations.get(operationName);
            final SMTPOperationInput smtpOperationInput = smtpOperation.getSmtpInput();
            
    		if (smtpOperationInput == null) {
 			   throw new Exception(WSDLConfigurations.mMessages.getString("WCF_Missing_SMTPInput"));
			}
    		
    		String encodingStyle = null;
    		
            if (SMTPConstants.SMTP_USE_TYPE_ENCODED.equals(smtpOperationInput.getSmtpUseType())) {
                encodingStyle = smtpOperationInput.getEncodingStyle();

    	           if ((encodingStyle == null) || encodingStyle.equals("")) {
    	               throw new Exception(WSDLConfigurations.mMessages.getString("WCF_Missing_EncodingStyle_SMTPInput"));
    	           }
            
                    for (final Iterator operIter = portType.getOperations().iterator(); operIter.hasNext();) {
                        final Operation op = (Operation) operIter.next();
                        if (op.getName().equals(operationName.toString()) || op.getName().equals(operationName.getLocalPart())) {
                                /**
                                * There is nothing in the WSDL spec that says that the part name has to be
                                * unique within the WSDL document, so we need to prefix the part name with the
                                * message name.
                                */

                                final Input input = op.getInput();
                                if (input != null) {

                                        final javax.wsdl.Message wsdlMessage = input.getMessage();
                                        final Map parts = wsdlMessage.getParts();
                                        for (final Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                                           final Part aPart = (Part) partIter.next();
                                           if(aPart.getName().equals(smtpOperationInput.getMessage()))
                                           {

                                                   final QName type = (aPart.getElementName() != null) ? aPart.getElementName()
                                                           : aPart.getTypeName();
                                                   final String partName = aPart.getName();

                                                   // locate the XSD file based on the part type namespace
                                                   final String namespace = type.getNamespaceURI();
                                                   final String xsdFileLoc = getXsdFileLocation(namespace);
                                                   if (xsdFileLoc != null) {


                                                       Encoder encoder = null;
                                                       final MetaRef metaRef = new MyMetaRef(xsdFileLoc, type);
                                                       if (mEncoderMap.get(metaRef) != null) {
                                                           encoder = (Encoder) mEncoderMap.get(metaRef);
                                                       } else {
                                                           final EncoderFactory encoderFactory = EncoderFactory.newInstance();
                                                           encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle), metaRef);
                                                           mEncoderMap.put(metaRef, encoder);
                                                       }
                                                       partMapping.put(wsdlMessage.getQName() + partName, encoder);
                                                   }
                                                   break;
                                                }
                                        }
                                }
                    }

                }
            }
        }
		return partMapping;
	}

    public void clearEncoderCache() {
        mEncoderMap.clear();
    }    
    
    private String getXsdFileLocation(final String aNamespace) {
        String xsdFileLoc = null;
        File aXsdFile = null;

        try {
            for (int ii = 0; ii < mXsds.size(); ii++) {
                aXsdFile = (File) mXsds.get(ii);
                final FastSchemaFactory factory = FastSchemaFactory.getInstance();
                final FastSchema schema = factory.newFastSchema(aXsdFile.getAbsolutePath());
                if (aNamespace.equals(schema.getTargetNamespace())) {
                    xsdFileLoc = aXsdFile.getAbsolutePath();
                    break;
                }
            }
        } catch (final Exception e) {
            WSDLConfigurations.mLogger.log(Level.SEVERE, "WCF_cannot_parse_xsd", aXsdFile.getName());
        }

        return xsdFileLoc;
    }
    
    /**
     * An implementation of the MetaRef interface
     */
    private class MyMetaRef implements MetaRef {
        private final String mXsdPath;

        private final QName mRootElemName;

        private final String mToString;

        /**
         * Constructor
         */
        protected MyMetaRef(final String xsdLoc) {
            this(xsdLoc, null);
        }

        /**
         * Alternative constructor that constructs a MetaRef object with the file path location of
         * the main XSD and qualified name of the root element
         */
        protected MyMetaRef(final String xsdLoc, final QName rootElemName) {
            mXsdPath = xsdLoc;
            mRootElemName = rootElemName;
            mToString = toString();
        }

        /**
         * Gets the URL of the main metadata file.  This URL should point to an
         * XSD file somewhere.  If this method returns a value other than
         * <code>null</code>, the return value of <code>getPath()</code> will
         * be ignored.  To load encoder metadata from a jar file, a URL in form
         * "jar:&lt;url&gt;!/{entry}" can be used.
         * 
         * @return the URL of the main meta file
         */
        public URL getURL() {
            return null;
        }        
        /**
         * Return the file path location of the main XSD
         * 
         * @return the path of the main meta file
         */
        public String getPath() {
            return mXsdPath;
        }

        /**
         * Return the QName of the root element.
         * 
         * @return the QName of the root element
         */
        public QName getRootElemName() {
            return mRootElemName;
        }

        @Override
		public String toString() {
            return mXsdPath + mRootElemName.toString();
        }

        @Override
		public boolean equals(final Object obj) {
            if (!(obj instanceof MyMetaRef)) {
                return false;
            }
            return mToString.equals(((MyMetaRef) obj).mToString);
        }

        @Override
		public int hashCode() {
            return mToString.hashCode();
        }
    }
    
    public Map parseForEnvironmentVariables(List portMaps, Map envVariableMap) throws Exception {
    	Map envVariables = (Map)((HashMap)envVariableMap).clone();
        File dir = new File(mRootPath);
        
        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() +
                                       File.separator +
                                       "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
        while (wsdls.hasNext()) {
            envVariables.putAll(readWSDLForEnvVariables((File)wsdls.next(), resolver, envVariables));
        }

        return envVariables;
    }
    
    private Map readWSDLForEnvVariables(File f, EntityResolver resolver, Map envVariableMap) 
    	throws WSDLException {
		    WSDLFactory wsdlFactory = WSDLFactory.newInstance();
		    WSDLReader reader =
		        ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
		    SMTPExtPreprocessDeserializer preProcessDeserializer = new SMTPExtPreprocessDeserializer(envVariableMap);
		    reader.setExtensionRegistry(new SMTPExtensionRegistry(preProcessDeserializer));
		    reader.readWSDL(f.getAbsolutePath());
		    return preProcessDeserializer.getEnvVariableMap();
    }


}
