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
 * @(#)ServiceUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.extensions.schema.SchemaConstants;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;

// common-util and qos imports
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.httpsoapbc.descriptors.HttpSoapEndpointIdentifier;
import com.sun.jbi.httpsoapbc.descriptors.HttpSoapSUDescriptorParser;
import com.sun.jbi.httpsoapbc.extensions.SOAPExtSerializer;
import com.sun.jbi.httpsoapbc.security.api.ServiceUnitSecurityConfig;
import com.sun.jbi.httpsoapbc.security.impl.ServiceUnitSecurityConfigImpl;
import com.sun.jbi.httpsoapbc.security.impl.CredentialValidatorManager;
import com.sun.jbi.httpsoapbc.validator.HttpSoapValidatorRegistry;
import com.sun.jbi.httpsoapbc.util.HttpUrlResolverUtil;
import com.sun.jbi.httpsoapbc.util.TransformerPool;
import com.sun.jbi.httpsoapbc.validator.WSDLExtensionValidator;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.ValidatingWSDLReader;
import com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.extensions.schema.SchemaReference;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.Import;
import javax.wsdl.Types;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.Transformer;

import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * ServiceUnitImpl represents a ServiceUnit deployed in a ServiceUnitManager.
 * <p>
 * The initialization of a ServiceUnit and its respective Endpoints depends
 * on two main configuration items: the jbi.xml
 * file and the WSDL(s) representing the Service Unit.  Initialization of the
 * ServiceUnit is done through the Observer pattern when parsing each of the
 * WSDL files.  This class only cares about WSDLs containing the extensibility
 * elements pertinent to this Binding Component.
 * <p>
 * By encapsulating the initialization logic away from
 * model of ServiceUnitManagers, ServiceUnits, and Endpoints, we can more
 * easily change how models are initialized without breaking too much. 
 * <p>
 *
 */
public class ServiceUnitImpl implements ServiceUnit {

    private static final Logger mLogger =
        Messages.getLogger(ServiceUnitImpl.class);
    private static final Messages mMessages =
        Messages.getMessages(ServiceUnitImpl.class);
    
    private String mId;
    private String mComponentName;
    private ComponentContext mContext;
    private RuntimeConfigurationMBean mRuntimeConfig;
    private boolean endpointsCreated;
    private String mRootPath;
    private Map mEndpoints;
    private Collection mEndpointChangeListeners;
    private Map mModifiedWSDLs;
    private Map mModifiedSchemas;
    private Map mDefs;
    private TransformerPool mTransformerPool;
    private CredentialValidatorManager mCredValidatorMgr;
    private MessagingChannel mChannel;
    private CatalogManager mCatalogManager;
    private EntityResolver mResolver;
    
    public ServiceUnitImpl(String id, 
    			   ComponentContext context,
    			   RuntimeConfigurationMBean runtimeConfig,
                           Collection endpointChangeListeners,
                           CredentialValidatorManager credValidatorMgr) {
        mId = id;
        mContext = context;
        mRuntimeConfig = runtimeConfig;
        mEndpoints = new HashMap();
        mEndpointChangeListeners = endpointChangeListeners;
        mModifiedWSDLs = new HashMap();
        mModifiedSchemas = new HashMap();
        mTransformerPool = new TransformerPool();
        mComponentName = context.getComponentName();
        mCredValidatorMgr = credValidatorMgr;
        mChannel = HttpSoapComponentContext.getInstance().getBindingChannel();
    }
    
    ServiceUnitImpl(String id, 
                    ComponentContext context,
                    RuntimeConfigurationMBean runtimeConfig,
                    Collection endpointChangeListeners,
                    CredentialValidatorManager credValidatorMgr,
                    Map endpoints) {
        this (id, context, runtimeConfig, endpointChangeListeners, credValidatorMgr);
        mEndpoints = endpoints;
    }

    ServiceUnitImpl(String id, 
                    ComponentContext context,
                    RuntimeConfigurationMBean runtimeConfig,
                    Collection endpointChangeListeners,
                    CredentialValidatorManager credValidatorMgr,
                    Map endpoints,
                    MessagingChannel channel) {
        this (id, context, runtimeConfig, endpointChangeListeners, credValidatorMgr);
        mChannel = channel;
        mEndpoints = endpoints;
    }
    
    ////////
    //
    //  ServiceUnit Interface Methods
    //
    ////////

    /**
     * Retrieves the Id of this ServiceUnit.
     *
     * @return       the name of the Service as a QName
     */
    public String getServiceUnitId() {
        return mId;
    }

    /**
     * Deploy the ServiceUnit.  Useful for validating the ServiceUnit during
     * the deployment phase
     *
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void deploy(String serviceUnitRootPath) throws JBIException {
        createEndpoints(serviceUnitRootPath, false);
        
    }


    /**
     * Initializes the ServiceUnit.  Parses the serviceUnitRootPath
     * to create the Endpointobjects.  A ServiceUnit may have
     * mutiple WSDLs, each with multiple endpoints that we care about.
     * <p>
     * This method will initialize the WSDLs, transforming it into a
     * set of Endpoints
     *
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void init(String serviceUnitRootPath) throws JBIException {
    	// get the map of QoS attributes associated with the endpoints defined
        // in the service unit
        mRootPath = serviceUnitRootPath;
        // look up and install the QoS configurations
        mChannel.installServiceQualities(mId, serviceUnitRootPath);
        
        // set up the entity resolver here
        File catalog = new File(mRootPath +
            File.separator + "META-INF" + File.separator +
            "catalog.xml");
        if (catalog.exists()) {
            mCatalogManager = new CatalogManager();
            mCatalogManager.setIgnoreMissingProperties(true);
            mCatalogManager.setCatalogFiles(catalog.getAbsolutePath());
            mCatalogManager.setRelativeCatalogs(true);
            mCatalogManager.setUseStaticCatalog(false);
            mResolver = new CatalogResolver(mCatalogManager);
        }
        
        //init can be called during the reboot of the appserver or
        //during the deploy/start/stop/shutdown of the SU.
        //To optimize performance, make sure endpoints are created 
        //and validated only once during the life cycle of the SU.
        if (!endpointsCreated) {
            createEndpoints(serviceUnitRootPath, true);
        }
        
        Iterator it = mEndpoints.values().iterator();
        Endpoint currEndpoint = null;
        EndpointChangeListener listener;
        Iterator it2 = null;
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                
                // start provisioning
                currEndpoint.activate(mContext);
                
                // Let all our EndpointChangeListeners know what's happening
                it2 = mEndpointChangeListeners.iterator();
                while (it2.hasNext()) {
                    listener = (EndpointChangeListener)it2.next();
                    listener.endpointInitialized(currEndpoint);
                }
            }
        } catch (Exception ex) {
            throw new JBIException(ex);
        }
    }

    /**
     * Starts this ServiceUnit.  This involves activating
     * all Endpoints that are part of this ServiceUnit.
     * <p>
     * TODO: What should happen if not all the Endpoints
     * can be activated?  Should I deactivate them or just leave
     * them?  For now, I'm going to assume that this method is
     * transactional.  Either all the Endpoints activate or none.
     * If any one fails to activate, the other activated Endpoints
     * will be deactivated.
     *
     * @exception    JBIException if a any Endpoint fails
     * to activate
     */
    public void start() throws JBIException {

        HashSet activatedEndpoints = new HashSet();
        
        Iterator it = mEndpoints.values().iterator();
        Endpoint currEndpoint = null;
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                
                //currEndpoint.activate(mContext);
                /*
                List wsdlNamespaces = currEndpoint.getImportedWSDLNamespaces();
                List xsdNamespaces = currEndpoint.getImportedXSDNamespaces();
                for (int ii = 0; ii < wsdlNamespaces.size(); ii++) {
                    String aNamespace = (String)wsdlNamespaces.get(ii);
                    if (mModifiedWSDLNamespacesMap.get(aNamespace) != null) {
                        currEndpoint.addImportedWSDLDefinition(aNamespace, (Definition)mModifiedWSDLNamespacesMap.get(aNamespace));
                    }
                }
                
                for (int ii = 0; ii < xsdNamespaces.size(); ii++) {
                    String aNamespace = (String)xsdNamespaces.get(ii);
                    if (mModifiedXSDNamespacesMap.get(aNamespace) != null) {
                        currEndpoint.addImportedXSDSchema(aNamespace, (Element)mModifiedXSDNamespacesMap.get(aNamespace));
                    }
                }*/
                
                activatedEndpoints.add(currEndpoint);
                
                // Let all our EndpointChangeListeners know what's happening
                Iterator it2 =
                    mEndpointChangeListeners.iterator();
                while (it2.hasNext()) {
                    EndpointChangeListener listener =
                        (EndpointChangeListener)it2.next();
                    listener.endpointActivated(currEndpoint);
                }
            }
        } catch (Exception ex) {
            if (currEndpoint != null) {
                String text = mMessages.getString("HTTPBC-E00301.Endpoint_activate_failed",
                        new Object[] { currEndpoint.getUniqueName(), ex.getLocalizedMessage() });
                mLogger.log(Level.SEVERE, text, ex);
                // remove the "bad" endpoint from the list
                mEndpoints.remove(currEndpoint.getUniqueName());
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "HTTPBC-R00305.Deactivating_endpoints_incomplete_activation");
            }
            Iterator it2 = activatedEndpoints.iterator();
            while (it2.hasNext()) {
                ((Endpoint)it2.next()).deactivate(mContext);
            }
            throw new JBIException(ex);
        }
        
    }

    /**
     * Stops this ServiceUnit.  This involves deactivating
     * all Endpoints that are part of this ServiceUnit;
     * <p>
     * TODO: What should happen if not all Endpoints deactivate?
     * Unlike the activate() method, I'm NOT going to assume
     * this is transactional.  It seems silly to deactivate a number of
     * Endpoint, and if one fails, re-activate them.  I'll just throw
     * an error, and have the user decide how to deal with it.
     *
     * @exception    JBIException if any Endpoint fails
     * to deactivate
     */
    public void stop() throws JBIException {
        try {
            Iterator it = mEndpoints.values().iterator();
            while (it.hasNext()) {
                Endpoint endpoint = (Endpoint)it.next();
                //endpoint.deactivate(mContext);
                
                // Let all our EndpointChangeListeners know what's happening
                Iterator it2 =
                        mEndpointChangeListeners.iterator();
                while (it2.hasNext()) {
                    EndpointChangeListener listener =
                            (EndpointChangeListener)it2.next();
                    listener.endpointDeactivated(endpoint);
                }
            }
        } catch (Exception ex) {
            throw new JBIException(ex);
        }
    }

    /**
     * Shuts down this ServiceUnit
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {
        try {
            // remove the QoS attributes for the undeployed endpoints
            mChannel.uninstallServiceQualities(mId);
            
            Iterator it = mEndpoints.values().iterator();
            Endpoint currEndpoint;
            
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                
                // stop provisioning
                currEndpoint.deactivate(mContext);
                
                // Let all our EndpointChangeListeners know what's happening
                Iterator it2 =
                        mEndpointChangeListeners.iterator();
                while (it2.hasNext()) {
                    EndpointChangeListener listener =
                            (EndpointChangeListener)it2.next();
                    listener.endpointShutdown(currEndpoint);
                }
            }
        } catch (Exception ex) {
            throw new JBIException(ex);
        } finally {
            endpointsCreated = false;
            // clean up all the cached endpoints 
            mEndpoints.clear();
        }
        
    }


    /**
     * Undeploy the ServiceUnit.  Useful for cleaning up any lingering state.
     *
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void undeploy(String serviceUnitRootPath) throws JBIException {
    }

    /**
     * Retrieves the Endpoints handled by this ServiceUnit.
     *
     * @return  the map of Endpoints
     */
    public Map getEndpoints() {
        return mEndpoints;
    }
    
    private void createEndpoints(String serviceUnitRootPath, boolean resolveTokens) 
            throws JBIException {
        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
        try {
            HttpSoapSUDescriptorParser descSupport = 
                new HttpSoapSUDescriptorParser(serviceUnitRootPath);
            HttpSoapEndpointIdentifier[] svcs = descSupport.getServices();
            

            if (svcs.length == 0) {
                mLogger.log(Level.WARNING, "HTTPBC-W00306.No_endpoints_to_create",
                        new Object[] { mComponentName, mId } );
                return;
            }
            
            Map appConfigObjectMap = mRuntimeConfig.retrieveApplicationConfigurationsMap();
            // Read our WSDLS
            File dir = new File(serviceUnitRootPath);
            
            ValidatingWSDLFactory wsdlFactory = new ValidatingWSDLFactory();
            ValidatingWSDLReader reader =
                (ValidatingWSDLReader)wsdlFactory.newWSDLReader(dir);

            // Suppress WSDL4j System.out logs and control the logging based on the current
            // logger logging level setting
            if (mLogger.isLoggable(Level.FINE)) {
                reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);                         
            } else {
                reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
            }  
            
            // Populate our reader with the right extensionRegistry
            ExtensionRegistry registry =
                wsdlFactory.newPopulatedExtensionRegistry();
            SOAPExtSerializer serializer = new SOAPExtSerializer(mRuntimeConfig, resolveTokens);
            serializer.registerSerializer(registry);
            reader.setExtensionRegistry(registry);
            reader.setValidatorRegistry(new HttpSoapValidatorRegistry(mRuntimeConfig, resolveTokens));
            mDefs = reader.readWSDL(dir);
            WSDLExtensionValidator extValidator = new WSDLExtensionValidator(mDefs, svcs);
            extValidator.validate();
            
            if (!resolveTokens) {
                // we are done with loading the tokens
                
                // Now we will add the application configuration names defined 
                // in jbi.xml to the application configuration composite data
                // structure in the MBean, if they are not already there.
                
                for (int ii = 0; ii < svcs.length; ii++) {
                    HttpSoapEndpointIdentifier epDesc = svcs[ii];
                    String configName = epDesc.getApplicationConfigurationName();
                    
                    // this application configuration name is not yet defined in the config MBean,
                    // add it to the application configuration data structure
                    if (configName != null && !"".equals(configName) &&
                        !appConfigObjectMap.containsKey(configName)) {
                        appConfigObjectMap.put(configName, "");    
                    }
                }
                return;
            }
			
            //Load the security configuration
            ServiceUnitSecurityConfig suSecurityConfig = new ServiceUnitSecurityConfigImpl(serviceUnitRootPath, mId);
            HttpUrlResolverUtil urlResolverUtil = new HttpUrlResolverUtil(mRuntimeConfig.retrieveApplicationVariablesMap());
            
            Iterator it = mDefs.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry entry = (Map.Entry)it.next();
                String path = (String)entry.getKey();
                Definition def = (Definition)entry.getValue();
                for (int ii = 0; ii < svcs.length; ii++) {
                    boolean hasAppObject = false;
                    HttpSoapEndpointIdentifier epDesc = svcs[ii];
                    if (epDesc == null) continue;
                
                    String configName = epDesc.getApplicationConfigurationName();
                    URL urlLocation = null;
                    
                    if (configName != null && !"".equals(configName)) {
                        // there is a config object defined for the endpoint
                        // use the URL defined for the config object instead
                        // of the one defined in the WSDL
                        hasAppObject = true;
                        String location = (String) appConfigObjectMap.get(configName);
                        if (location == null) {
                            throw new JBIException(mMessages.getString("HTTPBC-E00360.Application_config_object_not_defined", configName));
                        }
                    
                        try {
                            String newLocation = urlResolverUtil.resolveEmbeddedTokensInURL(
                                location, mRuntimeConfig.getHttpDefaultPort(), mRuntimeConfig.getHttpsDefaultPort());       // resolve any tokens in the URL location
                            newLocation = urlResolverUtil.resolveHostNameInUrl(newLocation);			            // resolve "localhost" in the URL location
                            urlResolverUtil.validateLocationURI(newLocation);
                            urlLocation = new URL(newLocation);
                        } catch (Exception e) {
                            throw new JBIException(mMessages.getString("HTTPBC-E00361.Invalid_url_defined_in_application_config_object", 
                                                   new Object[] { configName, location, e.getLocalizedMessage() }));
                        }
                    }
                    Endpoint bcEndpoint = 
                        EndpointFactory.createEndpoint(def,
                             epDesc.getServiceName(),
                             epDesc.getEndpointName(),
                             epDesc.getInterfaceName(),
                             epDesc.isProvider(),
                             new HttpClientConnectionProperties(epDesc.getHostnameVerification(), epDesc.getConnectTimeout(), epDesc.getReadTimeout()),
                             epDesc.getPropagateSoapHeader(),
                             path,
                             suSecurityConfig.getEndpointSecurityConfig(epDesc.getEndpointName()),
                             mId,
                             serviceUnitRootPath,
                             mCredValidatorMgr);

                    if (bcEndpoint != null) {
                        getQOSConfigurations (bcEndpoint, epDesc);
                        
                        bcEndpoint.setValidHostnames(mRuntimeConfig.getValidHostnames());
                        bcEndpoint.init();
                        // add the handler list
                        bcEndpoint.setHandlers(epDesc.getHandlers());
                        // save the handler libraries and dependencies
                        bcEndpoint.setHandlerLibPaths(listResourceFiles(new File(serviceUnitRootPath), ".jar"));
                        
                        // enable ?wsdl ?
                        bcEndpoint.setEnableWsdlQuery(epDesc.getEnableWsdlQuery()); 
                        
                        //Let the listeners perform the one time deployment
                        //specific logic such as endpoint validations.
                        Iterator it2 = mEndpointChangeListeners.iterator();
                        while (it2.hasNext()) {
                            EndpointChangeListener listener =
                                (EndpointChangeListener)it2.next();
                            listener.endpointDeployed(bcEndpoint);
                        }
                        
                        bcEndpoint.setOriginalWSDL(new File(serviceUnitRootPath + File.separatorChar + path));
                        if (hasAppObject) {
                            bcEndpoint.setEndpointUrl(urlLocation);
                        }
                        mEndpoints.put(bcEndpoint.getUniqueName(), bcEndpoint);

                        if (!epDesc.isProvider() && mModifiedWSDLs.get(path) == null) {
                            // First convert path to a valid context string
                            path = path.replace(File.separatorChar, '/');
                            
                            String dirPath = "";
                            if (path.contains("/")) {
                                dirPath = path.substring(0, path.lastIndexOf("/"));
                            }
                            modifyWSDL(bcEndpoint.getEndpointUrl().getProtocol(), bcEndpoint.getEndpointUrl().getPort(), dirPath, def, bcEndpoint);
                            //mModifiedWSDLs.put(path, def);
                            mModifiedWSDLs.put(path, new File(serviceUnitRootPath + File.separatorChar + path).getCanonicalPath());
                        }
                        svcs[ii] = null;
                    }
                }
            }
            
            endpointsCreated = true;            
       } catch (Exception ex) {
           throw new JBIException(ex);
       }        
    }

    public ByteBuffer getResource(String resourcePath, Endpoint endpoint) {
        Transformer transformer = null;
        ByteBuffer result = null;
        byte[] data = null;
        try {
            Object defRef = mModifiedWSDLs.get(resourcePath);
            if (defRef != null && defRef instanceof String) {
                File defFile = new File ((String) defRef);
                if (defFile.exists()) {
                    FileInputStream fileStream = new FileInputStream(defFile);
                    data = new byte[ (int) defFile.length() ];
                    int size = fileStream.read(data);
                    fileStream.close();
                    if (size < 0) {
                        return null;
                    }
                    result = ByteBuffer.wrap(data);                
                }
            }
            
            Object schemaRef = mModifiedSchemas.get(resourcePath);
            if (schemaRef != null) {
            	if (schemaRef instanceof String) {
                    // read the schema file content
                    File schemaFile = new File((String)schemaRef);
                    if (schemaFile.exists()) {
                        FileInputStream fileStream = new FileInputStream(schemaFile);
                        data = new byte[ (int) schemaFile.length() ];
                        int size = fileStream.read(data);
                        fileStream.close();
                        if (size < 0) {
                            return null;
                        }
                    } else { // a location URI to be resolved by catalog.xml entry
                        if (mResolver == null) {
                            return null;
                        }
                       
                        InputSource inputSource = mResolver.resolveEntity (null, ((String) schemaRef));
                        InputStream schemaStream = inputSource.getByteStream();
                        StreamSource schemaSource = new StreamSource(schemaStream);
                        transformer = mTransformerPool.retrieve();
                        ByteArrayOutputStream baos = new ByteArrayOutputStream();
                        StreamResult dest = new StreamResult(baos);
                        
                        transformer.transform(schemaSource, dest);
                        data = baos.toByteArray();
                    }
                }
                if (data == null) {
                    return null;
                } 
                result = ByteBuffer.wrap(data);
                
            }
        } catch (Exception ex) {
            // fall-thru to account for new 'finally' block
            //return null;
        } finally {
            mTransformerPool.relinquish(transformer);
        }
        return result;
    }
    
    /**
     * Modifies the WSDL definition to use absolute paths based on the
     * URL.  This method will change an imported XSDs and WSDL
     * locations to absolute URLs.
     * <p>
     * The change follows the following format:
     * <p>
     * http://&lt;endpoint url&gt;?WSDL=&lt;XSD or WSDL path&gt;
     * <p>
     * where
     * <ul>
     * <li>&ltendpoint url&gt; is the endpoint url</li>
     * <li>&lt;XSD or WSDL path&gt; is the relative path originally in the
     * location attribute in the WSDL.
     * <p>
     * There are possibly multiple endpoints defined in the WSDL.  We choose
     * just one to be the base of our WSDL URL.  The retrieval mechanism must also
     * decode the
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public void modifyWSDL(String protocol, int port, String path, Definition def, Endpoint endpoint) throws Exception {
        
        // Search all wsdl imports for the resourceName
        Iterator importLists = def.getImports().values().iterator();
        while (importLists.hasNext()) {
            Iterator imports = ((List)importLists.next()).iterator();
            while (imports.hasNext()) {
                Import anImport = (Import)imports.next();
                String relativeLoc = anImport.getLocationURI();
//TODO: This check does not seem sufficient to check for relative URL - what about ftp for example?
                if (relativeLoc != null && !relativeLoc.toLowerCase().startsWith("http://") && !relativeLoc.toLowerCase().startsWith("https://")) {
                    // We have a relative location]
                    String updatedLocationURI = relativeLoc;
                    if (!path.equals("")) {
                        updatedLocationURI = path + "/" + relativeLoc;
                    }
                    ResourceLocator locator = new ResourceLocator(protocol, port, mId, updatedLocationURI);
                    anImport.setLocationURI(locator.toString());
//TODO: it does not seem safe to assume it's file here
//TODO: if we keep this, combine with addNewWSDL logic to normalize file name
                    // Keep a list of imported WSDLs for JAX-WS use
                    File importedWSDL = new File(mRootPath, updatedLocationURI);
                    if (!importedWSDL.exists()) {
                        // try to resolve the physical location of the import using catalog.xml
                        updatedLocationURI = mCatalogManager.getCatalog().resolveSystem(relativeLoc);
                        URL locationURL = new URL(updatedLocationURI);
                        if (locationURL != null && locationURL.getProtocol().equals("file")) {
                            importedWSDL = new File(locationURL.getPath());
                        }
                    } 
                    endpoint.addWSDLImport(importedWSDL.toURL());
                    
                    // Should we be adding this WSDL to our list of modified WSDLs?
                    // The caller of this method is iterating through all the WSDLs
                    // handled by this ServiceUnit.  It may be possible that we are
                    // importing a WSDL that won't be provisioned by us; if that
                    // happens, we should be traversing this URI and adding it to
                    // the mModifiedWSDLs data structure
                    addNewWSDL(protocol, port, path, relativeLoc, endpoint);
                } else {
                    // Keep a list of imported WSDLs for JAX-WS use                
                    endpoint.addWSDLImport(new URL(relativeLoc));
                }
            }
        }
        
        // Search all XSD imports and includes for the resourceName
        Types types = def.getTypes();
        if (types != null) {
            Iterator schemas = types.getExtensibilityElements().iterator();
            while (schemas.hasNext()) {
                ExtensibilityElement element = (ExtensibilityElement)schemas.next();
                if (element instanceof Schema) {
                    Schema schema = (Schema)element;
                    
                    // Update all the Schema objects to have the URLs
                    updateSchemaWSDL(protocol, port, path, schema, endpoint);
                    // Update the underlying DOM object too.  wsdl4j representation
                    // doesn't take care of this
                    updateSchemaDom(protocol, port, path, schema.getElement());
                    
                }
            }
        }
    }
    
    private void updateSchemaWSDL(String protocol, int port, String path, Schema schema, Endpoint endpoint) throws Exception {
    	Iterator importURIList = schema.getImports().keySet().iterator();
    	while (importURIList.hasNext()) {
    	    endpoint.addImportedXSDNamespace((String) importURIList.next());   
    	}
        Iterator importList = schema.getImports().values().iterator();
        while (importList.hasNext()) {
            updateSchemaReferences(protocol, port, path, schema, (Collection)importList.next());
        }
        updateSchemaReferences(protocol, port, path, schema, schema.getIncludes());
        updateSchemaReferences(protocol, port, path, schema, schema.getRedefines());
        
    }
    
    private void updateSchemaReferences(String protocol, int port, String path, Schema schema, Collection references) throws Exception {
        Iterator includesList = references.iterator();
        while (includesList.hasNext()) {
            SchemaReference schemaref = (SchemaReference)includesList.next();
            String locationURI = schemaref.getSchemaLocationURI();
            if (locationURI != null && !locationURI.toLowerCase().startsWith("http://") && !locationURI.toLowerCase().startsWith("https://")) {
                // We have a relative location
                String updatedLocationURI = locationURI;
                if (!path.equals("")) {
                    updatedLocationURI = path + "/" + locationURI;
                }
                ResourceLocator locator = new ResourceLocator(protocol, port, mId, updatedLocationURI);
                schemaref.setSchemaLocationURI(locator.toString());
            }
        }
        
    }
    
    private void updateSchemaDom(String protocol, int port, String path, Element el) throws Exception {
        Element tempEl = DOMUtils.getFirstChildElement(el);
        
        for (; tempEl != null; tempEl = DOMUtils.getNextSiblingElement(tempEl)) {
            QName tempElType = QNameUtils.newQName(tempEl);
     
            if (SchemaConstants.XSD_IMPORT_QNAME_LIST.contains(tempElType) ||
                SchemaConstants.XSD_INCLUDE_QNAME_LIST.contains(tempElType) ||
                SchemaConstants.XSD_REDEFINE_QNAME_LIST.contains(tempElType)) {                
                updateSchemaDomReference(protocol, port, path, tempEl);               
            }
        }
    }
    
    private void updateSchemaDomReference(String protocol, int port, String path, Element tempEl) throws Exception {
        String locationURI = DOMUtils.getAttribute(tempEl, SchemaConstants.ATTR_SCHEMA_LOCATION);
        if (locationURI != null && !locationURI.toLowerCase().startsWith("http://") && !locationURI.toLowerCase().startsWith("https://")) {
            // We have a relative location
            String updatedLocationURI = locationURI;
            if (!path.equals("")) {
                updatedLocationURI = path + "/" + locationURI;
            }
            ResourceLocator locator = new ResourceLocator(protocol, port, mId, updatedLocationURI);
            setAttribute(tempEl, SchemaConstants.ATTR_SCHEMA_LOCATION, locator.toString());
            addNewSchema(protocol, port, path, locationURI);
        }
    }
    
    private void addNewWSDL(String protocol, int port, String path, String locationURI, Endpoint endpoint) throws Exception {
        // Read in the new schema
        // First convert paths and locationURI to OS-specific File separators
//TODO: is this replacement necessary or even desirable before constructing File?
        String filePath = path.replace('/', File.separatorChar);
        String fileLocationURI = locationURI.replace('/', File.separatorChar);
        String relativePath = filePath + File.separator + fileLocationURI;
        // Normalize it so that it's the same
        File wsdlFile = new File(mRootPath, filePath + File.separator + fileLocationURI);
        File root = new File(mRootPath);
        String normalizedRoot = root.getCanonicalPath();
        String completePath = wsdlFile.getCanonicalPath();
        Definition wsdlDef = null;
        String normalizedRelativePath = null;
         
        if (completePath.startsWith(normalizedRoot)) {
            normalizedRelativePath = completePath.substring(normalizedRoot.length(), completePath.length());
            while (normalizedRelativePath.startsWith(File.separator)) {
                normalizedRelativePath = normalizedRelativePath.substring(1, normalizedRelativePath.length());
            }
            wsdlDef = (Definition)mDefs.get(normalizedRelativePath);
            if (wsdlDef == null) {
                // Big trouble here
                // This is an internal error that the user cannot correct for.
                // Moving it to FINE level. -nang
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "WARNING: Service Unit " + mId
                            + " deployment unable to find an internal map entry for the WSDL object "
                            + normalizedRelativePath + ". Ignoring it.");
                }
                return;
            }
        } else {
            // Big trouble here
            // Internal error not correctable by the user
            // Moving it to FINE level. -nang
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Service Unit " + mId
                        + " deployment encountered a WSDL object (" + completePath
                        + ") outside its jurisdiction (" + null + ").  Ignoring it.");
            }
            return;
        }

        String locatorPath = normalizedRelativePath.replace(File.separatorChar, '/');
        if (mModifiedWSDLs.get(locatorPath) == null) {
            // Modify the schema
            // Use the path of the current schema File
            String currentPath = null;
            if (locationURI.indexOf("/") != -1) {
                currentPath = locationURI.substring(0, locationURI.lastIndexOf("/"));
            }
            if (currentPath == null || currentPath.equals("")) {
                currentPath = path;
            } else {
                if (!path.equals(""))
                    currentPath = path + "/" + currentPath;
            }
            modifyWSDL(protocol, port, currentPath, wsdlDef, endpoint);
            
            // Add the schema to our mModifiedSchemas map
            // First convert path to a valid context string
            
            //mModifiedWSDLs.put(locatorPath, wsdlDef);
            mModifiedWSDLs.put(locatorPath, completePath);
        }
    }

    private void addNewSchema(String protocol, int port, String path, String locationURI) throws Exception {
        // Read in the new WSDL.  We should get it from the list of defs that we're currently
        // traversing.
        // First convert paths and locationURI to OS-specific File separators
        String filePath = path.replace('/', File.separatorChar);
        String fileLocationURI = locationURI.replace('/', File.separatorChar);
        File schemaFile = new File(mRootPath, filePath + File.separator + fileLocationURI);
        Document schema = null;
        //InputSource schemaSource = null;
        if (schemaFile.exists()) {
            schema = getDocument(schemaFile);
        } else {
            // try to get it from the catalog.xml if the schema file cannot be found at the location
            if (mResolver != null) {
            	//schemaSource = mResolver.resolveEntity(null, locationURI);
                //schema = getDocument(schemaSource);
                schema = getDocument(mResolver.resolveEntity(null, locationURI));
            }
        }
        
        if (schema == null) {
            throw new Exception(mMessages.getString("HTTPBC-E00362.Cannot_resolve_schema", locationURI));
        }
        
        // Modify the WSDL
        // Use the path of the current WSDL File
        String currentPath = null;
        if (locationURI.indexOf("/") != -1) {
            currentPath = locationURI.substring(0, locationURI.lastIndexOf("/"));
        }
        if (currentPath == null || currentPath.equals("")) {
            currentPath = path;
        } else {
            if (!path.equals(""))
                currentPath = path + "/" + currentPath;
        }


        // Add the schema to our mModifiedSchemas map
        // First convert path to a valid context string
        if (currentPath.equals("")) {
            if (!mModifiedSchemas.containsKey(schemaFile.getName())) {
                //mModifiedSchemas.put(schemaFile.getName(), schema.getDocumentElement());
                if (schemaFile.exists()) {
                    mModifiedSchemas.put(schemaFile.getName(), schemaFile.getCanonicalPath());
                } else {
                    //mModifiedSchemas.put(schemaFile.getName(), mResolver.resolveEntity(null, locationURI));
                    mModifiedSchemas.put(schemaFile.getName(), locationURI);  // let's resolve the schema resource later when needed
                }
                updateSchemaDom(protocol, port, currentPath, schema.getDocumentElement());
            }
        } else {
            String normalizedPath = new URI(currentPath + "/" + schemaFile.getName()).normalize().getPath();
            if (!mModifiedSchemas.containsKey(normalizedPath)) {
                //mModifiedSchemas.put(normalizedPath, schema.getDocumentElement());
                if (schemaFile.exists()) {
                    mModifiedSchemas.put(normalizedPath, schemaFile.getCanonicalPath());
                } else {
                    //mModifiedSchemas.put(normalizedPath, mResolver.resolveEntity(null, locationURI));
                    mModifiedSchemas.put(normalizedPath, locationURI);  // let's resolve the schema resource later when needed
                }
                updateSchemaDom(protocol, port, currentPath, schema.getDocumentElement());
            }
        }

    }
    

    private Document getDocument(File input) throws Exception {
    	if (input == null) {
    	    return null;
    	}
    	
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        
        factory.setNamespaceAware(true);
        factory.setValidating(false);
        DocumentBuilder builder = factory.newDocumentBuilder();
        // The parse method doesn't do well with file names that have ../.. in it.
        // Pass a file that has a canonical path as it's main file path
        File updatedFile = new File(input.getCanonicalPath());
        
        
        // set up catalog resolver
        if (mResolver != null) {
            builder.setEntityResolver(mResolver);
        }
        
        return builder.parse(updatedFile);
    }
    
    private Document getDocument(InputSource input) throws Exception {
    	if (input == null) {
    	    return null;
    	}
    	
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        
        factory.setNamespaceAware(true);
        factory.setValidating(false);
        DocumentBuilder builder = factory.newDocumentBuilder();
        // set up catalog resolver
        if (mResolver != null) {
            builder.setEntityResolver(mResolver);
        }
        return builder.parse(input);
    }
      
    private void setAttribute(Element el, String attrName, String attrValue) {
        Attr attr = el.getAttributeNode(attrName);

        if (attr != null) {
            attr.setValue(attrValue);
        }
    }

    private void getQOSConfigurations (Endpoint httpbcEndpoint, HttpSoapEndpointIdentifier endpointIdentifier) 
        throws DeploymentException {
        // get the QoS map
        EndpointInfo endpointInfo = new EndpointInfo (false, 
                                                      endpointIdentifier.getEndpointName(), 
                                                      null,
                                                      endpointIdentifier.getServiceName(),
                                                      null);
        List<ServiceQuality> qoss = mChannel.getServiceQualitiesForEndpoint(endpointInfo);
        if (qoss == null) {
            // there is no QoS configuration on this endpoint
            return;
        }
        Iterator<ServiceQuality> qossIter = qoss.iterator();
        while (qossIter.hasNext()) {
            ServiceQuality qos = qossIter.next();
            // Gather throttling config
            if (qos instanceof ThrottlingConfig) {
                ThrottlingConfig throttleConfig = (ThrottlingConfig)qos;
                httpbcEndpoint.setMaxConcurrencyLimit(throttleConfig.getMaxConcurrencyLimit());
            } else if (qos instanceof RedeliveryConfig) {   // get the redelivery configuration
                RedeliveryConfig redeliveryConfig = (RedeliveryConfig)qos;
                httpbcEndpoint.setRedeliveryConfiguration(redeliveryConfig);
            }
            // no other QoS services at the moment...
        }
    }

    private List listResourceFiles(File currentDir, String extension) {
        List cumulativeResults = new ArrayList();
        File[] filesInCurrentDir = currentDir.listFiles();
        for (int fileCount = 0; fileCount < filesInCurrentDir.length;
        fileCount++) {
            
            if (filesInCurrentDir[fileCount].isFile()) {
                if (filesInCurrentDir[fileCount].getName().toLowerCase().endsWith(extension)) {
                    cumulativeResults.add(filesInCurrentDir[fileCount]);
                }
            } else if (filesInCurrentDir[fileCount].isDirectory()) {
                List wsdlsInSubDirectories =
                        listResourceFiles(filesInCurrentDir[fileCount], extension);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }
}
