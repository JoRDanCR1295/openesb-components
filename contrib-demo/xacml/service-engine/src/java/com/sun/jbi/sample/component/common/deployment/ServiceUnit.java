/*
 * ServiceUnit.java
 *
 */

package com.sun.jbi.sample.component.common.deployment;

import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Consumes;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Provides;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Service;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;

/**
 * This is an abstract class that implements the service unit functionality in the component which
 * processes the service unit artifacts and implement the actual service unit lifecycle functionality.
 * The AbstractServiceUnitManager and its extended classes that implement the ServiceUnitManager
 * functionality creates and maintains the reference to the concrete implementation of this class to
 * invoke the functionality implemented by this class and its concrete implementation during the
 * execution of the ServiceUnitMangers lifecycle methods by the jbi runtime.
 * <p>
 * The main lifecycle methods of this class that will be called by the ServiceUnitManger implementation
 * during the service unit deployment lifecycle are  {@link #doload}, {@link #doDeploy}, {@link #doUndeploy},
 * {@link #doInit}, {@link #doStart}, {@link #doStop} and {@link #doShutdown}
 * <p>
 * Service unit processing supported by this implementation is based on wsdl 1.1 deployments where the
 * services provided and consumed in this service unit are described using wsdl 1.1 definitions.
 * <p>
 * The main service unit artifacts that will be processed during deployment lifecycle of the service
 * unit are 1.Service unit descriptor(jbi.xml) that describes the services provided and consumed by
 * this service unit. 2.WSDL 1.1 document that describes the service definition 3.Component specific
 * configurations related to services provided and consumed by this service unit.
 * <p>
 * When the service unit is for Binding Component, the component specific configurations are read
 * from the WSDL1.1 extensions defined in the WSDL document corresponding to the service provider
 * or consumer deployed with this service unit.
 * <p>
 * When the service unit is for Service Engine, the component specific configurations are read
 * from the deployment artifacts such as xslt files and mapping files in the service unit zip file
 * along with the WSDL document corresponding to the service provider or consumer deployed with this
 * service unit.
 *
 * @see SUDescriptor
 * @see Endpoint
 * @see ProviderEndpoint
 * @see ConsumerEndpoint
 * @see WSDLProcessor
 * @author chikkala
 */
public abstract class ServiceUnit {
    /** Service Unit Name */
    private String mSUName;
    /** Service Unit Root path passed by jbi runtime */
    private String mSURootPath;
    /** service unit descriptor model unmarshalled from service unit jbi.xml */
    private SUDescriptor mSUDescriptor;
    /** Map of Endpoint Key to ProviderEndpoint configurations in this service unit  */
    private Map<String, ProviderEndpoint> mProviderEndpointMap;
    /** Map of Endpoint Key to ConsumerEndpoint configurations in this service unit  */
    private Map<String, ConsumerEndpoint> mConsumerEndpointMap;
    /** Map of Service Key to the WSDL Definition in this service unit */
    private Map<String, Definition> mWSDLMap;
    /** WSDLProcessor configured for this service unit to process wsdls in the service unit */
    private WSDLProcessor mWSDLProcessor;
    /**
     *
     * @param suName
     * @param suRootPath
     */
    protected ServiceUnit(String suName, String suRootPath) {
        this.mSUName = suName;
        this.mSURootPath = suRootPath;
        this.mSUDescriptor = null;
        this.mWSDLMap = new HashMap<String,Definition>();
        this.mProviderEndpointMap = new HashMap<String, ProviderEndpoint>();
        this.mConsumerEndpointMap = new HashMap<String, ConsumerEndpoint>();
    }
    /** returns service unit name
     * @return service unit name
     */
    public String getName() {
        return this.mSUName;
    }
    /** returns service unit root path where the su artifacts are unzipped by the jbi runtime
     * @return path to the service unit root directory.
     */
    public String getSURootPath() {
        return this.mSURootPath;
    }
    protected SUDescriptor createSUDescriptor() throws Exception {
        File jbiXmlFile = new File(this.getSURootPath(), "META-INF/jbi.xml");
        String jbiXmlPath = jbiXmlFile.getAbsolutePath();
        return SUDescriptor.SUDescriptorFactory.getSUDescriptor(jbiXmlPath);
    }
    /** return the Service unit descriptor model that was read from the jbi.xml
     * @return SUDescriptor
     */
    public SUDescriptor getSUDescriptor() throws Exception {
        if ( this.mSUDescriptor == null ) {
            this.mSUDescriptor = createSUDescriptor();
        }
        return this.mSUDescriptor;
    }
    public ProviderEndpoint getProviderEndpoint(String providerID) {
        return this.mProviderEndpointMap.get(providerID);
    }
    public Collection<ProviderEndpoint> getProviderEndpoints() {
        return Collections.unmodifiableCollection(this.mProviderEndpointMap.values());
    }
    public ConsumerEndpoint getConsumerEndpoint(String cosumerID) {
        return this.mConsumerEndpointMap.get(cosumerID);
    }
    
    public Collection<ConsumerEndpoint> getConsumerEndpoints() {
        return Collections.unmodifiableCollection(this.mConsumerEndpointMap.values());
    }
    /**
     * @return Logger
     */
    protected Logger getLogger() {
        return RuntimeHelper.getLogger();
    }
    /**
     * extended classes implement this method to create the su specific WSDLProcessor. for examples,
     * a su supporting binding component may have wsdl extensions that it want to registry for reading
     * the configurations from the wsdl file.
     * @return WSDLProcessor
     * @see com.sun.jbi.sample.component.common.wsdl.WSDLProcessor
     * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionRegistry
     */
    protected WSDLProcessor createWSDLProcessor() {
        return new WSDLProcessor(this.getSURootPath());
    }
    /**
     * return the WSDLProcessor reference by creates if it is not yet created.
     * @return WSDLProcessor.
     */
    public final WSDLProcessor getWSDLProcessor() {
        if ( this.mWSDLProcessor == null ) {
            this.mWSDLProcessor = createWSDLProcessor();
        }
        return this.mWSDLProcessor;
    }
    /**
     * loads the service unit artifacts into the SU model. AbstractServiceUnitManager implementation
     * calls this method during the during deploy and init lifecycle methods when the service unit
     * object is newly created.
     */
    public void doLoad() throws DeploymentException {
        try {
            SUDescriptor suDesc = getSUDescriptor(); // load jbi.xml
            loadServiceDefinitions();  // check if the wsdls are valid for corresponding services.
            loadOtherArtifacts(); // additional validations specific to component deployment features.
            loadEndpoints(); // create endpoints
        } catch ( DeploymentException jbiEx) {
            throw jbiEx;
        } catch (Exception ex) {
            throw new DeploymentException(ex);
        }
    }
    /** extended classes implement this method to perform the su specific deployment related tasks in
     * this method implementation.
     * @throws DeploymentException on error.
     */
    public void doDeploy() throws DeploymentException {
        // NOOP. doLoad has done it all.
        this.getLogger().fine("ServiceUnit.doDeploy");
    }
    /** extended classes implement this method to perform the su specific undeployment related tasks in
     * this method implementation.
     * @throws DeploymentException on error.
     */
    public void doUndeploy() throws DeploymentException {
        //NOOP
        this.getLogger().fine("ServiceUnit.doUndeploy");
    }
    /** extended classes implement this method to perform the su specific initialization tasks in
     * this method implementation.
     * @throws DeploymentException on error.
     */
    public void doInit() throws DeploymentException {
        this.getLogger().fine("ServiceUnit.doInit");
        this.doInitEndpoints();
    }
    /** extended classes implement this method to perform the su specific tasks on start in
     * this method implementation.
     * @throws DeploymentException on error.
     */
    public void doStart() throws DeploymentException {
        this.getLogger().fine("ServiceUnit.doStart");
        this.doActivateEndpoints();
    }
    /** extended classes implement this method to perform the su specific tasks on stop in
     * this method implementation.
     * @throws DeploymentException on error.
     */
    public void doStop() throws DeploymentException {
        this.getLogger().fine("ServiceUnit.doStop");
        this.doDeactivateEndpoints();
    }
    /** extended classes implement this method to perform the su specific tasks on shutdown in
     * this method implementation.
     * @throws DeploymentException on error.
     */
    public void doShutdown() throws DeploymentException {
        this.getLogger().fine("ServiceUnit.doShutdown");
        this.doCleanEndpoints();
    }
    /**
     * create the ProviderEndpoint that implement the service provider implementation specific to this su.
     * @return ProviderEndpoint or null if the SU does not support the service provider access
     */
    protected ProviderEndpoint createProviderEndpoint(Provides provides, Definition wsdlDef) {
        return null;
    }
    /**
     * create the ProviderEndpoint that implement the service consumer implementation specific to this su.
     * @return ConsumerEndpoint or null if the SU does not support the service consumer access
     */
    protected ConsumerEndpoint createConsumerEndpoint(Consumes consumes, Definition wsdlDef) {
        return null;
    }
    /**
     * generates the key based on the service to store the wsdl definitions .
     */
    protected String getServiceKey(Service service) {
        StringBuffer strBuff = new StringBuffer();
        strBuff.append(service.getInterface()).append("+");
        strBuff.append(service.getServiceName()).append("+");
        strBuff.append(service.getEndpointName());
        return strBuff.toString();
    }
    /**
     * looks up the wsdl definition loaded for this service.
     */
    protected Definition findWSDLFor(Service service) throws WSDLException {
        Definition wsdlDef = null;
        String key = this.getServiceKey(service);
        wsdlDef = this.mWSDLMap.get(key);
        return wsdlDef;
    }
    
    protected Definition findWSDL(List<Definition> wsdlList, Service service, boolean ignoreEndpointLookup) {
        Definition foundDef = null;
        if (wsdlList.size() == 0)
            this.getLogger().warning("There is no WSDL deployed in this service engine ["+this.getName()+"]");
        for ( Definition def : wsdlList ) {
            if (ignoreEndpointLookup)
                this.getLogger().fine("will ignore endpoint");
            this.getLogger().fine("Got the namespace ["+def.getTargetNamespace()+"] from the list.");
            this.getLogger().finest("Got the WSDL ["+def+"]");
            if ( WSDLProcessor.isWSDLFor(def, service.getInterface(), service.getServiceName(),
                ((ignoreEndpointLookup) ? null : service.getEndpointName()) ) ) {
                foundDef = def;
                break;
            }
        }
        return foundDef;
    }
    /**
     * loads the WSDL definitions corresponds to the service providers and consumers defined in the
     * service unit descriptor.
     */
    protected void loadServiceDefinitions() throws Exception {
        
        this.mWSDLMap = new HashMap<String,Definition>();
        
        WSDLProcessor wsdlProcessor = getWSDLProcessor();
        List<Definition> wsdlList = wsdlProcessor.readWSDLs(this.getSURootPath());
        this.getLogger().fine("Number of wsdl definitions in service unit " + wsdlList.size());
        
        List<Service> services = new ArrayList<Service>();
        services.addAll(this.getSUDescriptor().getProvidedServices());
        services.addAll(this.getSUDescriptor().getConsumedServices());
        
        boolean isForBinding = this.getSUDescriptor().isForBindingComponent();
        this.getLogger().fine("Is this service unit for Binding? " + isForBinding);
        
        for ( Service service : services ) {
            this.getLogger().fine("Looking up WSDL for service " + service);
            boolean ignoreEndpointLookup = false;
            boolean providerAtEngine = false;
            if ( !isForBinding && service instanceof Provides ) {
                ignoreEndpointLookup = true;
                providerAtEngine = true;
            }
            Definition def = findWSDL(wsdlList, service, ignoreEndpointLookup);
            if ( def == null ) {
                throw new Exception("WSDL Definition not found for " + service);
            }
            this.mWSDLMap.put(getServiceKey(service), def);
            if ( providerAtEngine ) {
                // provider at engine. so add engine binding and endpoint to the wsdl
                wsdlProcessor.createServiceEngineBinding(def,
                    service.getInterface(), service.getServiceName(), service.getEndpointName());
            }
        }
    }
    /** extended classes implement this method to perform the su specific artifacts validation in
     * this method implementation.
     * @throws DeploymentException on error.
     */
    protected void loadOtherArtifacts() throws DeploymentException {
        // nothing to validate further.
        this.getLogger().fine("ServiceUnit.loadOtherArtifacts");
    }
    /**
     * creates ProviderEndpoint and ConsumerEndpoint objects corresponding to the service providers
     * and consumers described in the su descriptor ( jbi.xml )
     */
    protected void loadEndpoints() throws Exception {
        
        this.mProviderEndpointMap = new HashMap<String, ProviderEndpoint>();
        this.mConsumerEndpointMap = new HashMap<String, ConsumerEndpoint>();
        
        Collection<Provides> providesList = this.getSUDescriptor().getProvidedServices();
        for ( Provides provides : providesList ) {
            Definition wsdlDef = findWSDLFor(provides);
            if ( wsdlDef == null ) {
                throw new DeploymentException("WSDL Definitions not found for " + provides);
            }
            ProviderEndpoint provider = createProviderEndpoint(provides, wsdlDef);
            this.mProviderEndpointMap.put(provider.getID(), provider);
        }
        
        Collection<Consumes> consumesList = this.getSUDescriptor().getConsumedServices();
        for ( Consumes consumes : consumesList ) {
            Definition wsdlDef = findWSDLFor(consumes);
            if ( wsdlDef == null ) {
                throw new DeploymentException("WSDL Definitions not found for " + consumes);
            }
            ConsumerEndpoint consumer = createConsumerEndpoint(consumes, wsdlDef);
            this.mConsumerEndpointMap.put(consumer.getID(), consumer);
        }
    }
    /**
     * initializes the Endpoint objects created corresponding to the consumer and providers defined
     * in the su descriptor
     */
    protected void doInitEndpoints() throws DeploymentException {
        // init endpoints. if any initialization fails, rollback the already inited endpoints
        List<Endpoint> initedEndpoints = new ArrayList<Endpoint>();
        List<Endpoint> allEndpoints = new ArrayList<Endpoint>();
        allEndpoints.addAll(this.getProviderEndpoints());
        allEndpoints.addAll(this.getConsumerEndpoints());
        
        for ( Endpoint endpoint : allEndpoints ) {
            try {
                endpoint.init();
                initedEndpoints.add(endpoint);
            } catch ( Exception initEx) {
                doCleanEndpoints(initedEndpoints);
                throw new DeploymentException(initEx);
            }
        }
        
    }
    /**
     * invokes activates method of all provider and consumer endpoint object in this su. if there is
     * and error activating any one the endpoints, it deactivates the already activated ones and throws
     * the error
     */
    protected void doActivateEndpoints() throws DeploymentException {
        // activate providers first and then consumers
        List<Endpoint> activatedEndpoints = new ArrayList<Endpoint>();
        List<Endpoint> allEndpoints = new ArrayList<Endpoint>();
        allEndpoints.addAll(this.getProviderEndpoints());
        allEndpoints.addAll(this.getConsumerEndpoints());
        
        for ( Endpoint endpoint : allEndpoints ) {
            try {
                endpoint.activate();
                activatedEndpoints.add(endpoint);
            } catch ( Exception actEx) {
                doDeactivateEndpoints(activatedEndpoints);
                throw new DeploymentException(actEx);
            }
        }
    }
    /**
     * invokes deactivate method on the list of Endpoint objects passed to this method
     */
    protected void doDeactivateEndpoints(List<Endpoint> endpoints) {
        for ( Endpoint endpoint : endpoints )  {
            try {
                endpoint.deactivate();
            } catch(Exception ex) {
                // ignore the exception and log it.
                this.getLogger().log(Level.FINE, ex.getMessage(), ex);
            }
        }
    }
    /**
     * invokes deactivate method on the all consumer and provider endpoint objects
     */
    protected void doDeactivateEndpoints() {
        // deactivate consumers first and then the providers
        List<Endpoint> allEndpoints = new ArrayList<Endpoint>();
        allEndpoints.addAll(this.getConsumerEndpoints());
        allEndpoints.addAll(this.getProviderEndpoints());
        doDeactivateEndpoints(allEndpoints);
    }
    /**
     * invokes clean method on the list of endpoint objects
     */
    protected void doCleanEndpoints(List<Endpoint> endpoints) {
        for ( Endpoint endpoint : endpoints )  {
            try {
                endpoint.clean();
            } catch(Exception ex) {
                // ignore the exception and log it.
                this.getLogger().log(Level.FINE, ex.getMessage(), ex);
            }
        }
    }
    /**
     * invokes clean method on the all consumer and provider endpoint objects in this su.
     */
    protected void doCleanEndpoints() {
        
        List<Endpoint> allEndpoints = new ArrayList<Endpoint>();
        allEndpoints.addAll(this.getConsumerEndpoints());
        allEndpoints.addAll(this.getProviderEndpoints());
        doCleanEndpoints(allEndpoints);
    }
    /**
     * prints the service unit description
     */
    protected final String printDetails() {
        
        StringWriter writer = new StringWriter();
        PrintWriter out = new PrintWriter(writer);
        
        out.println("ServiceUnit Name : " + this.getName());
        out.println("ServiceUnit Root : " + this.getSURootPath());
        
        SUDescriptor suDesc = null;
        try {
            suDesc = this.getSUDescriptor();
            for ( SUDescriptor.Consumes consumer : suDesc.getConsumedServices()) {
                out.println(consumer);
            }
            for ( SUDescriptor.Provides provides : suDesc.getProvidedServices()) {
                out.println(provides);
            }
        } catch (Exception ex) {
            ex.printStackTrace(out);
        }
        return writer.getBuffer().toString();
    }
    
    
}
