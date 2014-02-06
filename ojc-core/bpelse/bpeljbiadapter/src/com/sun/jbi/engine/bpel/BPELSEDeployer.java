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
 * @(#)BPELSEDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.io.File;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.naming.InitialContext;
import javax.sql.DataSource;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

import net.sf.hulp.measure.Measurement;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine.TransformEngine;
import com.sun.jbi.engine.bpel.core.bpel.event.EventsConfigurationHelper;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.AppVarValidationResult;
import com.sun.jbi.engine.bpel.core.bpel.util.RApplicationVariablesHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.ValidationResult;
import com.sun.jbi.engine.bpel.core.bpel.util.XslCache;
import com.sun.jbi.engine.bpel.util.I18n;
import com.sun.jbi.engine.bpel.util.SUArtifacts;
import com.sun.jbi.util.ManagementMessage;
import com.sun.transform.api.Xslt2Support;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class BPELSEDeployer implements ServiceUnitManager {
    private static final Logger LOGGER = Logger.getLogger(BPELSEDeployer.class.getName());
    private Engine mEngine;
    private HashMap mDeployedSUvsSEMap;
    private HashMap mDeployedSEvsSUMap;
    private Map<String, XslCache> mXslCachesBySU;
    private ComponentContext mContext;
    private DeploymentBindings mDepBindings;
    private BPELSELifeCycle component;

    static final String CLASSIDENTIFIER_FORMEASUREMENT = "BPEL-SE.ServiceUnitManager";
    /** a map of {[SUName] -> [collection of BEPL Ids]} */
    private HashMap mBPELIdsSUMap;
    
    /**
     * Creates a new instance of BPELSEDeployer
     *
     * @param engine Engine instance
     * @param context JBI component context
     * @param channel JBI engine channel
     * @param epsHelper Endpoint statistics helper class
     * @param depBindings deployment bindings
     */
    public BPELSEDeployer(Engine engine, ComponentContext context, 
                          DeploymentBindings depBindings, BPELSELifeCycle component) 
            throws DeploymentException {
        mEngine = engine;
        mDeployedSUvsSEMap = new HashMap();
        mDeployedSEvsSUMap = new HashMap();
        mXslCachesBySU = new HashMap<String, XslCache>();
        mContext = context;
        mDepBindings = depBindings;
        mBPELIdsSUMap = new HashMap();
        this.component = component;
    }

    /**
     * Initiate a Service Deployment.
     *
     * @param suName service unit id.
     * @param suPath service unit path.
     *
     * @return status message.
     *
     * @throws DeploymentException DeploymentException
     */
    public synchronized String deploy(String suName, String suPath) throws DeploymentException {
        // store SU archive in DB
    	if ( isMonitorEnabled() ){
            storeSUArchive(suName, suPath);
        }
    	
        return ManagementMessage.createDeployMessage(
            mContext.getComponentName(), I18n.loc("BPJBI-4000: Deploy"),
            ManagementMessage.STATUS_SUCCESS, ManagementMessage.TYPE_INFO,
            I18n.loc("BPJBI-4001: BPELSE"), null, null, null
        );
       
    }

    private boolean isMonitorEnabled(){
        return mEngine.isMonitorEnabled();
    }


    private void storeSUArchive(String suName, String suPath) {
    	Connection dbcon = null;
    	boolean origValOfAutoCommit = false;
    	try{          
            String dbJndiName = mEngine.getConnectionConfiguration().getConnectionProperties().getProperty(Engine.DB_NON_XA_JNDI_NAME);
            InitialContext namingContext = mContext.getNamingContext();
            DataSource ds = (DataSource) namingContext.lookup(dbJndiName);
            
            try {
                // If we are in the context of GFv2, please use getNonTxConnection
                // in place of getConnection (for leak reason)
                dbcon = (Connection) ds.getClass().getMethod("getNonTxConnection",
                    new Class[] {}).invoke(ds, new Object[] {});
            } catch (NoSuchMethodException nsme) {
                dbcon = (Connection) ds.getClass().getMethod("getConnection",
                    new Class[] {}).invoke(ds, new Object[] {});
            }
            
            origValOfAutoCommit = dbcon.getAutoCommit();
            dbcon.setAutoCommit(true);
            
            BPELSEDeployerUtil.storeSUToDB(dbcon, suName, suPath);
            
        } catch (Exception ex){
        	LOGGER.log(Level.WARNING, 
        			I18n.loc("BPJBI-6000: Exception while trying to store service unit archive to database {0}", 
        					ex.getMessage()), ex);
        	//TODO: do you want to throw Exception ??
        } finally {
        	if (dbcon != null) {
        		try {
        			dbcon.setAutoCommit(origValOfAutoCommit);
        			dbcon.close();
        		} catch (SQLException sqlExcp) {
                	LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6000: Exception while trying to store service unit " + 
                			"archive to database {0}", sqlExcp.getMessage()), sqlExcp);
        		}
        	}
        }
        
    }
    
    /**
     * This method is called by the framework when the deployment has to be \ initialised , just
     * before starting it.
     * CR 6527110: VALIDATIONS
	 * 1. In a SU if there are duplicate endpoints throw a deployment exception when the SU is being deployed.
	 * 2. If a SU tries to activate an endpoint during deployment that is already activated by another SU  for this component
	 * then throw a deployment exception.      
	 * 
     * @param suName service unit id.
     * @param suPath service unit path.
     *
     * @throws DeploymentException DeploymentException
     */
    public synchronized void init(String suName, String suPath) throws DeploymentException {
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5009: SU.init for {0} is started, Business Process packaged in this SU will be deployed", suName));
        Measurement m1 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, "SU.init");
        BPELTraceManager.getInstance().alertSUStatusChange(mEngine.getId(), suName, NotificationEvent.OPERATIONAL_STATE_STARTING);

        if (mDeployedSUvsSEMap.containsKey(suName)) {
            String[] params = { suName };
            String msg = ManagementMessage.createDeployMessage(
                    mContext.getComponentName(),
                    I18n.loc("BPJBI-4002: Init"),
                    ManagementMessage.STATUS_FAILED,
                    ManagementMessage.TYPE_ERROR,
                    I18n.loc("BPJBI-4001: BPELSE"),
                    I18n.loc("BPJBI-7003: Deployment Unit name already exists. Service unit failed to load deployment"),
                    params,
                    new Exception(
                    		I18n.loc("BPJBI-7003: Deployment Unit name already exists. Service unit failed to load deployment")));
            throw new DeploymentException(msg);
        }
        
        Measurement m2 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.processArtifacts");
        SUArtifacts suArtifacts = null; 
        try {
            suArtifacts = EngineHelper.deploy(suName, new File(suPath), 
                    mEngine.getDeploymentLookup());
        } catch (Throwable ex) {
            LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7004: Service unit {0} failed to load deployment {1} : {2}", 
            		suName, suPath, ex.getMessage()), ex);

            String msg = ManagementMessage.createDeployMessage(mContext.getComponentName(),
            		I18n.loc("BPJBI-4002: Init"), ManagementMessage.STATUS_FAILED,
                    ManagementMessage.TYPE_ERROR, I18n.loc("BPJBI-4001: BPELSE"),
                    I18n.loc("BPJBI-7004: Service unit {0} failed to load deployment {1} : {2}", 
                    		suName, suPath, ex.getMessage()),
                    null, ex);

            throw new DeploymentException(msg, ex);
        } finally {
            m2.end();
        }

        // Validation 1. In the returned list of Activation Entries for this SU check to see if there are duplicates
        EngineHelper.ActivationEntry duplicateEntry = checkForDuplicateEntries(suArtifacts.getProviderEndpoint());
        if (duplicateEntry != null) {
        	LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7005: Duplicate activation entry found for service {0} and endpoint {1}", 
        			duplicateEntry.getServiceName(), duplicateEntry.getEndpointName()));
        	
            Exception exep = new Exception(I18n.loc("BPJBI-7006: Duplicate activation entry is not allowed"));
            String msg = ManagementMessage.createDeployMessage(mContext.getComponentName(),
            		I18n.loc("BPJBI-4002: Init"), ManagementMessage.STATUS_FAILED,
                    ManagementMessage.TYPE_ERROR, I18n.loc("BPJBI-4001: BPELSE"),
                    I18n.loc("BPJBI-7007: Service unit: {0} failed to load deployment: {1} for: {2}:  Service name: {3} : Endpoint name: {4}. {5}", 
                    		suName, suPath, duplicateEntry.getServiceName(), duplicateEntry.getEndpointName(), exep), null, exep);
            
            throw new DeploymentException(msg);
        }
        // Validation 2. Check to see if there exist an activated endpoint(activated by another SU) for each of the
        // entries in the activation list for this SU being deployed.
        EngineHelper.ActivationEntry duplicateEndpoint = checkIfEndpointActivatedByAnotherSU(suArtifacts.getProviderEndpoint());
        if (duplicateEndpoint != null) {
        	LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7008: An activated endpoint already exits for service {0} and endpoint {1}",
        		duplicateEndpoint.getServiceName(), duplicateEndpoint.getEndpointName()));
        	
            Exception exep = new Exception(I18n.loc("BPJBI-7009: An activated endpoint already exits"));
            
            String msg = ManagementMessage.createDeployMessage(mContext.getComponentName(),
            		I18n.loc("BPJBI-4002: Init"), ManagementMessage.STATUS_FAILED,
                    ManagementMessage.TYPE_ERROR, I18n.loc("BPJBI-4001: BPELSE"),
                    I18n.loc("BPJBI-7007: Service unit: {0} failed to load deployment: {1} for: {2}:  Service name: {3} : Endpoint name: {4}. {5}", 
                    		suName, suPath, duplicateEndpoint.getServiceName(), duplicateEndpoint.getEndpointName(), exep), null, exep);
            
            throw new DeploymentException(msg);
        }
        
        m2 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.installQoSMessagingChannel");
        //set QOS on consuming EPs on MessagingChannel (decorator on DeliveryChannel)
        mEngine.installQosParams(suName, suPath);
        m2.end();

        // set SU class loader with jar files found in the SU root. This class
        // loader is used for loading classes referenced in xpath expressions
        mEngine.getClassLoaderContext().registerServiceClassLoader(suName, suPath);

        List startedBPELIds = null; 
        try {
            // compile...
            compileXsl(suArtifacts);
            // ... and cache XSL stylesheets
            installXslCache(suName, suArtifacts.getXslCache());
            // add application variables
            initApplicationVariables(suArtifacts);
            // register BPs
            startedBPELIds = registerBPsWithEngine(suArtifacts);
        } 
        catch (Throwable ex) {
            LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7004: Service unit {0} failed to load deployment {1} : {2}", 
                    suName, suPath, ex.getMessage()), ex);

            String msg = ManagementMessage.createDeployMessage(mContext.getComponentName(),
                    I18n.loc("BPJBI-4002: Init"), ManagementMessage.STATUS_FAILED,
                    ManagementMessage.TYPE_ERROR, I18n.loc("BPJBI-4001: BPELSE"),
                    I18n.loc("BPJBI-7004: Service unit {0} failed to load deployment {1} : {2}", 
                            suName, suPath, ex.getMessage()),
                    null, ex);

            mEngine.uninstallQosParams(suName);
            mEngine.getClassLoaderContext().unregisterServiceClassLoader(suName);
            cleanupXslCache(suName);
            
            throw new DeploymentException(msg);
        }

        m2 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, "SU.init.activateEndpoint");
        EngineHelper.ActivationEntry actEntry = null;
        List serviceEndpointList = new ArrayList();
        try {
            List actList = suArtifacts.getProviderEndpoint();
            for (int i = 0; i < actList.size(); i++) {
                actEntry = (EngineHelper.ActivationEntry) actList.get(i);
                /*NOTE: If this activation entry corresponds to an already activated endpoint by another component in the 
                 * JBI runtime this call to activateEndpoint would throw a javax.jbi.messaging.MessagingException(JBIException) 
                 */ 
                ServiceEndpoint se = mContext.activateEndpoint(actEntry.getServiceName(),
                        actEntry.getEndpointName());
                serviceEndpointList.add(se);
                LOGGER.log(Level.INFO, I18n.loc("BPJBI-5008: Activated an endpoint. Service name {0}, endpoint name {1}",
                        actEntry.getServiceName(), actEntry.getEndpointName()));
            }
        } catch (JBIException ex) {
            String msg = ManagementMessage.createDeployMessage(mContext.getComponentName(),
            		I18n.loc("BPJBI-4002: Init"), ManagementMessage.STATUS_FAILED,
                    ManagementMessage.TYPE_ERROR, I18n.loc("BPJBI-4001: BPELSE"),
                    I18n.loc("BPJBI-7011: Service unit: {0} failed to load deployment: {1} as it failed to activate " + 
                    		"endpoints: {2}. Service name: {3} : Endpoint name: {4}. {5}", 
                    		suName, suPath, actEntry.getServiceName(), actEntry.getEndpointName(), ex), null, ex);

            unRegisterBPsWithEngine(startedBPELIds);
            mEngine.uninstallQosParams(suName);
            mEngine.getClassLoaderContext().unregisterServiceClassLoader(suName);
            cleanupXslCache(suName);
            mDepBindings.removeInComingEventModel(suName);

            throw new DeploymentException(msg);
        } finally{
            m2.end();
        }

        mDeployedSUvsSEMap.put(suName, serviceEndpointList);
        for (int i = 0; i < serviceEndpointList.size(); i++) {
            ServiceEndpoint se = (ServiceEndpoint) serviceEndpointList.get(i);
            List listSUForSE = (List) mDeployedSEvsSUMap.get(se);
            if (listSUForSE == null) {
                listSUForSE = new ArrayList();
                mDeployedSEvsSUMap.put(se, listSUForSE);
            }
            listSUForSE.add(suName);
        }
        mBPELIdsSUMap.put(suName, startedBPELIds);
        mEngine.setSUState(suName, Engine.SUStates.Stop);
        m1.end();
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5010: SU.init for {0} is completed, Business Process packaged in this SU are deployed", suName));

    }

    /**
     * Compiles and caches XSL stylesheets for use by <code>DoXslTransformFunction</code>.
     * @param su The service unit artifact.
     * @throws Exception if an error occurs during compilation.
     */
    private void compileXsl(SUArtifacts su) throws Exception {
        boolean v1 = mEngine.getTransformEngine() == TransformEngine.XSLT_1_0;
        Xslt2Support support = v1 ? null : Xslt2Support.newInstance();
        TransformerFactory fac = v1 ? TransformerFactory.newInstance() : null;
        
        mEngine.getClassLoaderContext().switchClassLoader(
                su.getUnitName(), SwitchType.service_classloader);
        try {
            for (String loc: su.getXslCache().locations()) {
                try {
                    StreamSource src = new StreamSource(new File(loc));
                    if (v1) {
                        su.getXslCache().addStylesheet(loc, fac.newTemplates(src));
                    }
                    else {
                        su.getXslCache().addStylesheet(loc, support.compile(src));
                    }
                }
                catch (Exception e) {
                    // logged when caught
                    throw e;
                }
            }
        }
        finally {
            mEngine.getClassLoaderContext().switchClassLoader(
                    su.getUnitName(), SwitchType.context_classloader);
        }
    }

    private void validateAppVarsAvailability(SUArtifacts su) {
        assert su.getApplicationVariables().keySet() != null;

        StringBuilder errorMsgs = new StringBuilder();
        Iterator bpItr = su.getBPs().iterator();
        while (bpItr.hasNext()) {
            RBPELProcess bpelProcess = (RBPELProcess) bpItr.next();
            AppVarValidationResult appVarValResult = RApplicationVariablesHelper.
                    validateAppVarsAvailability(bpelProcess, su.getApplicationVariables());

            String tmpErrorMsg = appVarValResult == null
                    ? null : appVarValResult.toString();
            if (tmpErrorMsg != null && tmpErrorMsg.length() > 0) {
                if (errorMsgs.length() > 0) {
                    errorMsgs.append(ValidationResult.COLUMN);
                }
                errorMsgs.append(tmpErrorMsg);
            }
        }

        if (errorMsgs.length()> 0 ) {
            //errorMessages are localized
            throw new IllegalStateException(errorMsgs.toString());
        }
    }

    private void initApplicationVariables(SUArtifacts su) {

        Map<String, Object> appVars = su.getApplicationVariables();
        assert appVars != null;
        appVars.putAll(mEngine.getApplicationVariables());
        validateAppVarsAvailability(su);
    }

    /**
     * Starts   a deployment.
     *
     * @param suName service unit id.
     *
     * @throws DeploymentException DeploymentException
     */
    public synchronized void start(String suName) throws DeploymentException {
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5011: SU.start for {0} is started", suName));
        Measurement m1 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, "SU.start");
        
        if (!mDeployedSUvsSEMap.containsKey(suName)) {
            String[] params = {suName};
            String msg = ManagementMessage.createDeployMessage(
                    mContext.getComponentName(), I18n.loc("BPJBI-4003: Start"),
                    ManagementMessage.STATUS_FAILED, ManagementMessage.TYPE_ERROR,
                    I18n.loc("BPJBI-4001: BPELSE"), 
                    I18n.loc("BPJBI-7012: Cannot perform operation. Service Unit {0} does not exist", suName),
                    params, 
                    new Exception(I18n.loc("BPJBI-7012: Cannot perform operation. Service Unit {0} does not exist", suName))
                );
            throw new DeploymentException(msg);
        }
        
        //update SU status on engine.
        mEngine.setSUState(suName, Engine.SUStates.Started);

        //this releases threads blocked on delivery channel to recover the instances for a given SU.
        if(mEngine.isPersistenceEnabled()){
            component.notifyThreads();
        }
        BPELTraceManager.getInstance().alertSUStatusChange(mEngine.getId(), suName, NotificationEvent.OPERATIONAL_STATE_STARTED);
        
        m1.end();
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5012: SU.start for {0} is completed", suName));
    }

    /**
     * Stops the service unit.
     *
     * @param suName service unit id.
     *
     * @throws DeploymentException DeploymentException
     */
    public synchronized void stop(String suName) throws DeploymentException {
        if (!mDeployedSUvsSEMap.containsKey(suName)) {
            String[] params = {suName};
            String msg = ManagementMessage.createDeployMessage(
                    mContext.getComponentName(), I18n.loc("BPJBI-4004: Stop"),
                    ManagementMessage.STATUS_FAILED, ManagementMessage.TYPE_ERROR,
                    I18n.loc("BPJBI-4001: BPELSE"), 
                    I18n.loc("BPJBI-7012: Cannot perform operation. Service Unit {0} does not exist", suName),
                    params, 
                    new Exception(I18n.loc("BPJBI-7012: Cannot perform operation. Service Unit {0} does not exist", suName))
                );
            throw new DeploymentException(msg);
        }
        //update SU status on engine.
        mEngine.setSUState(suName, Engine.SUStates.Stop);
        BPELTraceManager.getInstance().alertSUStatusChange(mEngine.getId(), suName, NotificationEvent.OPERATIONAL_STATE_STOPPED);        
    }
    
    /**
     * Shuts down the deplyment.
     * 
     * @param suName deployment id.
     * @throws DeploymentException deploy exception.
     */
    public synchronized void shutDown(String suName) throws DeploymentException {
        if (!mDeployedSUvsSEMap.containsKey(suName)) {
            String[] params = {suName};
            String msg = ManagementMessage.createDeployMessage(
                    mContext.getComponentName(), I18n.loc("BPJBI-4005: ShutDown"),
                    ManagementMessage.STATUS_FAILED, ManagementMessage.TYPE_ERROR,
                    I18n.loc("BPJBI-4001: BPELSE"), 
                    I18n.loc("BPJBI-7012: Cannot perform operation. Service Unit {0} does not exist", suName),
                    params, 
                    new Exception(I18n.loc("BPJBI-7012: Cannot perform operation. Service Unit {0} does not exist", suName))
            );
            throw new DeploymentException(msg);
        }

        // remove the registered QoS parameters for the endpoints 
        mEngine.uninstallQosParams(suName);
        //destroy SU class loader
        mEngine.getClassLoaderContext().unregisterServiceClassLoader(suName);
        // cleanup xsl cache
        cleanupXslCache(suName);
        
        cleanUpAfterDeployment(suName, true);

        //update SU status on engine.
        mEngine.setSUState(suName, Engine.SUStates.ShutDown);
        BPELTraceManager.getInstance().alertSUStatusChange(mEngine.getId(), suName, NotificationEvent.OPERATIONAL_STATE_SHUTDOWN);  
    }

    /**
     * Undeploys the service unit.
     *
     * @param suName service unit id.
     * @param suPath service unit path.
     *
     * @return status message.
     *
     * @throws DeploymentException DeploymentException
     */
    public synchronized String undeploy(String suName, String suPath)
        throws DeploymentException {
    	
    	if (mEngine.isMonitorEnabled()) {
    		// If monitoring is enabled the DBConnectionFactory will be initialized.
    		EventsConfigurationHelper.deleteEventsGenerationFlag(mEngine.getDBConnectionFactory(), suName);
    	}

    	String msg = ManagementMessage.createDeployMessage(
                mContext.getComponentName(), I18n.loc("BPJBI-4006: Undeploy"),
                ManagementMessage.STATUS_SUCCESS, ManagementMessage.TYPE_INFO,
                I18n.loc("BPJBI-4001: BPELSE"), null, null, null
            );

        return msg;
    }
    
    private void installXslCache(String suName, XslCache cache) {
        mXslCachesBySU.put(suName, cache);
        if (cache != null) {
            component.getNotificationEmitter()
                    .addNotificationListener(cache, null, null);
        }
    }
    
    private void cleanupXslCache(String suName) {
        XslCache cache = mXslCachesBySU.remove(suName);
        if (cache != null) {
            try {
                component.getNotificationEmitter()
                        .removeNotificationListener(cache, null, null);
            }
            catch (Exception e) { /* ignore, emitter may be null */ }
            cache.cleanup();
        }
    }
    
    private void cleanUpAfterDeployment(String suName, boolean throwException) throws DeploymentException {
        List serviceEndpointList = (List) mDeployedSUvsSEMap.remove(suName);
        for (int i = 0; i < serviceEndpointList.size(); i++) {
            ServiceEndpoint se = (ServiceEndpoint) serviceEndpointList.get(i);
            List listSUForSE = (List) mDeployedSEvsSUMap.get(se);
            if ((listSUForSE.size() > 1) && listSUForSE.contains(suName)) {
                listSUForSE.remove(suName);
            } else if ((listSUForSE.size() == 1) && listSUForSE.contains(suName)) {
                mDeployedSEvsSUMap.remove(se);
                try {
                    mContext.deactivateEndpoint(se);
                } catch (JBIException ex) {
                    if (throwException) {
                        String[] params = { suName,
                        		I18n.loc("BPJBI-4007: Service name: {0}", se.getServiceName()),
                        		I18n.loc("BPJBI-4008: Endpoint name: {0}", se.getEndpointName()) };
                        
                        String msg = ManagementMessage.createDeployMessage(mContext.getComponentName(),
                        		I18n.loc("BPJBI-4005: ShutDown"), ManagementMessage.STATUS_FAILED,
                                ManagementMessage.TYPE_ERROR, I18n.loc("BPJBI-4001: BPELSE"),
                                I18n.loc("BPJBI-7013: Service unit failed to deactivate endpoints"),
                                params, ex);
                        throw new DeploymentException(msg);
                    }
                }
            } else {
                if (throwException) {
                    String[] params = { suName,
                    		I18n.loc("BPJBI-4007: Service name: {0}", se.getServiceName()),
                    		I18n.loc("BPJBI-4008: Endpoint name: {0}", se.getEndpointName()) };
                    String msg = ManagementMessage.createDeployMessage(mContext.getComponentName(),
                    		I18n.loc("BPJBI-4005: ShutDown"), ManagementMessage.STATUS_FAILED,
                            ManagementMessage.TYPE_ERROR, I18n.loc("BPJBI-4001: BPELSE"),
                            "BPELSEDeployer.service_endpoint_data_structure_is_corrupted", params, null);
                    throw new DeploymentException(msg);
                }
            }
        }
        unRegisterBPsWithEngine(suName);
        mDepBindings.removeInComingEventModel(suName);
    }
    private void unRegisterBPsWithEngine(String suName){
        Collection startedBPELIds = (Collection) mBPELIdsSUMap.remove(suName);
        unRegisterBPsWithEngine(startedBPELIds);
    }
    
    private void unRegisterBPsWithEngine(Collection startedBPELIds) {
        //String id = null;
        QName id = null;
        if (startedBPELIds != null && startedBPELIds.size() > 0) {
            for (Iterator itr = startedBPELIds.iterator(); itr.hasNext();) {
                //id = (String) itr.next();
                id = (QName) itr.next();
                mEngine.removeModel(id);
            }
        }
        
    }
    
    private List registerBPsWithEngine(SUArtifacts suArtifacts) throws Exception{
        List startedBPELIds = new ArrayList();
        Iterator bpItr = suArtifacts.getBPs().iterator();
        while (bpItr.hasNext()) {
            RBPELProcess bpelProcess = (RBPELProcess) bpItr.next();
            try{
                mEngine.addModel(bpelProcess, suArtifacts.getAssemblyName(), suArtifacts.getUnitName());
                mEngine.getBPELProcessManager(bpelProcess.getBPELId()).setXslCache(suArtifacts.getXslCache());
                mEngine.getBPELProcessManager(bpelProcess.getBPELId()).setApplicationVariables(suArtifacts.getApplicationVariables());
            } 
            catch (Exception exp){
                unRegisterBPsWithEngine(startedBPELIds);
                throw exp;
            }
            QName bpelId = bpelProcess.getBPELId();
            startedBPELIds.add(bpelId);
            int maxInstanceCount = suArtifacts.getMaxInstanceCountByBP(bpelId);
            mEngine.getBPELProcessManager(bpelProcess.getBPELId()).setMaxInstances(maxInstanceCount);
        }
        
        Set inEventModelSet = suArtifacts.getInComingEventModel().entrySet();
        Iterator inEventModelIter = inEventModelSet.iterator();

        while (inEventModelIter.hasNext()) {
            Map.Entry inEventModelMapEntry = (Map.Entry) inEventModelIter.next();
            InComingKey key = (InComingKey) inEventModelMapEntry.getKey();
            InComingEventModel model = (InComingEventModel) inEventModelMapEntry.getValue();
            try{
                mDepBindings.addInComingEventModel(suArtifacts.getUnitName(), key, model);
                mEngine.addStartActivityModel(model.getBPELProcess(), 
                        model.getStartElement(), model.getOperPattern());
            } catch (Exception exp){
                unRegisterBPsWithEngine(startedBPELIds);
                mDepBindings.removeInComingEventModel(suArtifacts.getUnitName());
                throw exp;
            }
        }
        return startedBPELIds;
    }
    
    /*
     * Utility to check for duplicate entries in the ActivationEntry list
     * relies on the ActivationEntry.equals method.
     */
    private EngineHelper.ActivationEntry checkForDuplicateEntries(List entries) {
    	int size = entries.size();
        for (int i = 0; i < size; i++) {
        	EngineHelper.ActivationEntry iae = (EngineHelper.ActivationEntry) entries.get(i);
            for(int j = size -1 ;j > i; j--) {
            	EngineHelper.ActivationEntry jae = (EngineHelper.ActivationEntry) entries.get(j);
                if (iae.equals(jae)) {
                	return jae;
                }
            }
        }
        return null;
    }
    
    /*
     * Utility to check if there exits an endpoint that has been activated by another 
     * SU corresponding to any one of the entries in the list of ActivationEntry for the SU being deployed.
     * Makes use of the mDeployedSEvsSUMap that contains the list of all the ServiceEndpoints
     * activated by previous SU deployments.
     */
    private EngineHelper.ActivationEntry checkIfEndpointActivatedByAnotherSU(List entries) {
    	int entrySize = entries.size();
    	Iterator iter = mDeployedSEvsSUMap.keySet().iterator();
    	while(iter.hasNext()) {
    		ServiceEndpoint activeEndpoint = (ServiceEndpoint) iter.next();
    		QName activeService = activeEndpoint.getServiceName();
    		String activeEndpointName = activeEndpoint.getEndpointName();
    		for (int i = 0; i < entrySize; i++) {
    			EngineHelper.ActivationEntry actEntry = (EngineHelper.ActivationEntry) entries.get(i);
    			QName entryService = actEntry.getServiceName();
    			String entryEndpointName = actEntry.getEndpointName();
    			if (entryService.equals(activeService) &&
    				entryEndpointName.equals(activeEndpointName)) {
    				return actEntry;
    			}
    				
    		}
    	}
    	return null;
    }
}
