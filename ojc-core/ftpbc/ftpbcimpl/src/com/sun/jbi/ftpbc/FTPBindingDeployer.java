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
 * @(#)FTPBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc;

import com.sun.jbi.ftpbc.persistence.FTPBCPersistStore;
import com.sun.jbi.ftpbc.util.AlertsUtil;
import com.sun.jbi.ftpbc.util.EPUtil;
import com.sun.jbi.ftpbc.util.Utils;

import com.sun.jbi.bindings.synchronization.CompositeLockRegistry;
import com.sun.jbi.bindings.synchronization.CompositeLock;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.bindings.synchronization.DataBaseToken;
import com.sun.jbi.bindings.synchronization.FileToken;
import com.sun.jbi.bindings.synchronization.Sync;
import com.sun.jbi.bindings.synchronization.CompositeLockFactory;
import com.sun.jbi.bindings.synchronization.SyncException;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Collection;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.io.File;
import java.net.URI;
import javax.xml.namespace.QName;

/**
 * This class implements the ServiceUnitManager interface.
 * The ServiceUnitManager interface defines component-supplied
 * methods for managing service unit deployments.
 * generic code took from other BC;
 *
 * @author jfu jim.fu@sun.com
 */
public class FTPBindingDeployer implements ServiceUnitManager {

    private static final Messages mMessages =
            Messages.getMessages(FTPBindingDeployer.class);
    private static Logger mLogger = Messages.getLogger(FTPBindingDeployer.class);
    private HashMap mServiceUnits;
    private ComponentContext mContext;
    private FTPBindingLifeCycle mLifeCycle;
    private Map mTokenPersistParams;
    private CompositeLockFactory mCompLockFactory;

    public FTPBindingDeployer(ComponentContext context, FTPBindingLifeCycle lifeCycle) {
        mContext = context;
        mLifeCycle = lifeCycle;
        mServiceUnits = new HashMap();
    }

    /**
     * Initiate a BC Deployment.
     *
     * @param suId - service unit ID
     * @param suPath - Path of the service assembly file
     */
    public String deploy(String suId, String suPath) throws DeploymentException {
        String taskName = "deploy"; // NO18N

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002001.FBD_Deploy_SU", new Object[]{suId, suPath}));
        }

        ServiceUnit su = null;
        try {
            su = (ServiceUnit) mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                        suPath,
                        mContext,
                        mLifeCycle.getRuntimeConfigurationMBean(),
                        mLifeCycle.getStatusProviderHelper(),
                        mLifeCycle.getInboundReceiver());
            }
            registerEndpointLockAndPersistenceInfo(su);
            su.deploy();
        } catch (Exception ex) {
            if (su != null) {
                try {
                    su.stop();
                    su.shutdown();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }
            String msg = mMessages.getString("FTPBC-E002001.[ALERT].FBD_Failed_deploy_SU", ex.getMessage());
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "FBC_DEPLOY_1",
                    null,
                    msg,
                    ex);
            AlertsUtil.getAlerter().critical(exMsg,
                    FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FTPBC-E002001");
            throw new DeploymentException(exMsg, ex);
        }

        //Put the service unit map, only after the deployment succeeds
        mServiceUnits.put(suId, su);

        return createSuccessMessage(taskName, mContext.getComponentName());
    }

    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init"; // NO18N

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002002.FBD_Init_SU", new Object[]{suId, suPath}));
        }

        createCompositeLockFactory();

        ServiceUnit su = null;

        try {
            su = (ServiceUnit) mServiceUnits.get(suId);
            // Prepare for start if the deployment hasn't been processed yet.
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                        suPath,
                        mContext,
                        mLifeCycle.getRuntimeConfigurationMBean(),
                        mLifeCycle.getStatusProviderHelper(),
                        mLifeCycle.getInboundReceiver());
            }

            su.init();
            registerEndpointLockAndPersistenceInfo(su);
            mServiceUnits.put(suId, su);

            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002003.FBD_Complete_init_SU", suId));
            }
        } catch (Exception ex) {
            String errMsg = mMessages.getString("FTPBC-E002002.[ALERT].FBD_Failed_init_SU",
                    new Object[]{suId, Utils.getStackTrace(ex)});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, errMsg);
            }
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "FBC_INIT_1",
                    null,
                    errMsg,
                    ex);
            AlertsUtil.getAlerter().critical(exMsg,
                    FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FTPBC-E002002");
            throw new DeploymentException(exMsg, ex);
        }
    }

    public void start(String suId) throws DeploymentException {
        String taskName = "start"; // NO18N
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002004.FBD_Starting_SU", suId));
        }

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                registerEndpointLockAndPersistenceInfo(su);
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FTPBC-E002006.[ALERT].SU_preprocess_failed",
                        new Object[]{suId, ex.getMessage()});
                mLogger.log(Level.SEVERE, errMsg, ex);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_PRE_START_1",
                        null,
                        errMsg,
                        ex);
                AlertsUtil.getAlerter().critical(exMsg,
                        FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FTPBC-E002006");
                throw new DeploymentException(exMsg, ex);
            }

            try {
                su.start();
            } catch (Exception ex) {
                try {
                    cleanUpRegisteredLockAndPersistenceInfo(su);
                } catch (Exception ex1) {
                    // ignore on purpose
                }
                String errMsg = mMessages.getString("FTPBC-E002003.[ALERT].FBD_Error_start_SU",
                        new Object[]{suId, ex.getMessage()});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, errMsg);
                }
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_START_1",
                        suId,
                        errMsg,
                        ex);
                AlertsUtil.getAlerter().critical(exMsg,
                        FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FTPBC-E002003");
                throw new DeploymentException(exMsg, ex);
            }
        }

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002005.FBD_Complete_start_BC"));
        }
    }

    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002006.FBD_Stop_SU", suId));
        }

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FTPBC-E002004.[ALERT].FBD_Error_stop_SU", new Object[]{suId, ex.getMessage()});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, errMsg);
                }
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_STOP_1",
                        suId,
                        errMsg,
                        ex);
                AlertsUtil.getAlerter().critical(exMsg,
                        FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FTPBC-E002004");
                throw new DeploymentException(exMsg, ex);
            }
        }
        // further close up the channel on the inbound lock file
        try {
            cleanUpRegisteredLockAndPersistenceInfo(su);
        } catch (Exception ex) {
            String errMsg = mMessages.getString("FTPBC-E002008.[ALERT].SU_postprocess_failed",
                    new Object[]{suId, ex.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "FBC_STOP_POST_PROCESSING",
                    null,
                    errMsg,
                    ex);
            AlertsUtil.getAlerter().critical(exMsg,
                    FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FTPBC-E002008");
            throw new DeploymentException(exMsg, ex);
        }

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002007.FBD_Complete_stop_SU", suId));
        }
    }

    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002008.FBD_Shutdown_SU", suId));
        }
        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FTPBC-E002005.[ALERT].FBD_Error_shutdown_SU",
                        new Object[]{suId, ex.getMessage()});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, errMsg);
                }
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_SHUTDOWN_1",
                        suId,
                        errMsg,
                        ex);
                AlertsUtil.getAlerter().critical(exMsg,
                        FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FTPBC-E002005");
                throw new DeploymentException(exMsg, ex);
            }
            // further close up the channel on the inbound lock file
            try {
                cleanUpRegisteredLockAndPersistenceInfo(su);
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FTPBC-E002009.[ALERT].SU_shutdown_postprocess_failed",
                        new Object[]{suId, ex.getMessage()});
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_SHUTDOWN_POST_PROCESSING",
                        null,
                        errMsg,
                        ex);
                AlertsUtil.getAlerter().critical(exMsg,
                        FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FTPBC-E002009");
                throw new DeploymentException(exMsg, ex);
            }
        }
    }

    /**
     * Cancel a Service Deployment.  If the deployment is in use
     * (has dependencies), then this operation may fail.
     *
     * @param name - name of the service unit
     * @param root - root of the service unit
     */
    public String undeploy(String name, String root) throws DeploymentException {
        String taskName = "undeploy";

        if (mServiceUnits.containsKey(name)) {
            ServiceUnit su = (ServiceUnit) mServiceUnits.remove(name);
            // deregister per endpoint locks etc.
            try {
                cleanUpRegisteredLockAndPersistenceInfo(su);
            } catch (Exception ex) {
                throw new DeploymentException(mMessages.getString("FTPBC-E002007.Exception_deregister_EP_from_inbound_EP_lock_reg", new Object[]{ex}));
            }
        }

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R002009.FBD_Undeploy_SU", new Object[]{name, root}));
        }
        return createSuccessMessage(taskName, mContext.getComponentName());
    }

    public HashMap getServiceUnits() {
        return mServiceUnits;
    }

    /**
     * in case, this is called by multiple SU deploy / init
     * 
     */
    private synchronized void createCompositeLockFactory() throws DeploymentException {
        extractClusterSupportConfig();
        // the present of mTokenPersistenceParams indicates
        // cluster enabled
        // wether cluster enabled or not, there is always a comp lock factory
        // since FTPBC persist store (an impl of PersistStore) is wrapped in
        // the CompositeLock
        if (mCompLockFactory == null) {
            try {
                mCompLockFactory = CompositeLockFactory.newInstance(mTokenPersistParams);
            } catch (SyncException ex) {
                throw new DeploymentException(ex);
            }
        }
    }

    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        return msgBuilder.createSuccessMessage(taskName);
    }

    private String createExceptionMessage(String componentName,
            String taskName,
            String status,
            String locToken,
            String locParam,
            String locMessage,
            Throwable exObj) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        return msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);
    }

    private void extractClusterSupportConfig() throws DeploymentException {
        // for now, all the params for persistence are from system properties
        // they won't change over SU deploy / init
        String p = System.getProperty(FTPBCComponentContext.PROP_IS_CLUSTERED);

        if (p == null) {
            p = System.getProperty(FTPBCComponentContext.PROP_JBI_IS_CLUSTERED);
        }

        if (p != null && p.trim().equalsIgnoreCase("true")) {
            // cluster aware enabled
            String url = System.getProperty(FTPBCComponentContext.PROP_TOKEN_PERSISTENCE_URL);
            if (url == null || url.trim().length() == 0) {
                throw new DeploymentException("Missing Token Persistence URL when Cluster Aware is enabled, system property: " + FTPBCComponentContext.PROP_IS_CLUSTERED + " = true");
            }
            url = url.trim();
            if (mTokenPersistParams == null) {
                mTokenPersistParams = new HashMap();
            } else {
                mTokenPersistParams.clear();
            }
            if (url.startsWith("jdbc:")) {
                String clazz = System.getProperty(FTPBCComponentContext.PROP_TOKEN_DB_DRV_CLAZZ);
                if (clazz == null || clazz.trim().length() == 0) {
                    throw new DeploymentException("Missing DriverClassName when Cluster Aware is enabled (" + FTPBCComponentContext.PROP_IS_CLUSTERED + " = true ) and Token Persistence URL (" + FTPBCComponentContext.PROP_TOKEN_PERSISTENCE_URL + ") is of JDBC scheme.");
                }
                clazz = clazz.trim();
                mTokenPersistParams.put(DataBaseToken.SYNC_TOKEN_TYPE, Sync.SYNC_DB_TOKEN);
                mTokenPersistParams.put(DataBaseToken.TOKEN_PROPERTY_DB_URL, url);
                mTokenPersistParams.put(DataBaseToken.TOKEN_PROPERTY_DB_DRIVER, clazz);
                mTokenPersistParams.put(DataBaseToken.TOKEN_REG_TABLE_INFO, FTPBCComponentContext.TOKEN_DB_REGISTRY_TABLE_INFO);
                mTokenPersistParams.put(DataBaseToken.TOKEN_TKN_TABLE_INFO, FTPBCComponentContext.TOKEN_DB_TOKEN_TABLE_INFO);
            } else if (url.startsWith("file:")) {
                mTokenPersistParams.put(DataBaseToken.SYNC_TOKEN_TYPE, Sync.SYNC_FILE_TOKEN);
                URI fileURI = null;
                try {
                    fileURI = new URI(url);
                } catch (Exception e) {
                    throw new DeploymentException("Exception when parsing file URI : " + url + ", exception : " + e.getLocalizedMessage());
                }
                File tokenBaseDir = null;
                try {
                    tokenBaseDir = new File(fileURI);
                } catch (Exception e) {
                    throw new DeploymentException("Exception when instanitating token base dir as File object : exception : " + e.getLocalizedMessage());
                }
                mTokenPersistParams.put(FileToken.TOKEN_PROPERTY_LOCK_BASE_DIR, tokenBaseDir);
            } else {
                // assume it is jndi name
                mTokenPersistParams.put(DataBaseToken.SYNC_TOKEN_TYPE, Sync.SYNC_DB_TOKEN);
                mTokenPersistParams.put(DataBaseToken.TOKEN_PROPERTY_DATASRC_JNDI_NAME, url);
            }
        } else {
            if (mTokenPersistParams != null) {
                mTokenPersistParams.clear();
                mTokenPersistParams = null;
            }
        }
    }

    /**
     * For each endpoint in the SU, if it is inbound, register 
     * it in LockRegistry for all the persistence location 
     * info - for clustering support.
     * for outbound, also register persistence info so that
     * the sequence numbering etc. has the info about where the
     * persisted sequences are.
     * 
     * for now leave the global sequence.
     * 
     * @param su
     * @throws java.lang.Exception
     */
    private void registerEndpointLockAndPersistenceInfo(ServiceUnit su) throws Exception {
        Collection<Endpoint> theEndpoints = su.getEndpoints();
        if (theEndpoints == null) // only when run in junit
        {
            return;
        }

        String suID = su.getServiceUnitId();
        String suDir = EPUtil.toFilepath(suID);
        for (Endpoint aEndpoint : theEndpoints) {
            if (aEndpoint.getServiceUnitID() == null && suID != null) {
                aEndpoint.setServiceUnitID(suID);
            }
            // register lock etc at the operation level
            String baseLoc = aEndpoint.getAddress().getPersistenceBaseLocation();
            String serviceDir = EPUtil.toFilepath(aEndpoint.getServiceName().getLocalPart());
            String epDir = EPUtil.toFilepath(aEndpoint.getEndpointName());
            File baseDir = null;
            Map operations = aEndpoint.getOperations();

            if (operations.size() > 0) {
                // validate meta info
                //
                // if explicitly specified, baseLocation is the persistence base directory
                // otherwise, a default location will be used:
                // for glassfish, it is ${com.sun.aas.instanceRoot}/jbi/persistence

                // under the persistence base directory, per endpoint unique directory in the form of
                // <operation_name_first_16_char><UUID_generatedfrom_en_unique_name>
                // each end point -> 1-N operations
                //
                // the hierarchy is like the following:
                //
                // <baseLocation>
                //      |
                //      V
                // <SA_name>-<bc-name> [SU-ID]
                //                 |
                //                 |
                //                 +---------> <ServiceName-1><UUID-from-namespace-URI>
                //                                  |
                //                                  |
                //                                  +------> <Endpoint-Name-1>
                //                                  |              |
                //                                  |              +-----------> <operation-name-1><derived-UUID>
                //                                  |              |
                //                                  |              +-----------> <operation-name-2><derived-UUID>
                //                                  |
                //                                  +------> <Endpoint-Name-2>
                //                                  |              |
                //                                  |              +-----------> <operation-name-1><derived-UUID>
                //                                  |              |
                //                                  |              +-----------> <operation-name-2><derived-UUID>
                //                                ...             ... 
                //                                ...             ...

                if (baseLoc != null && baseLoc.trim().length() > 0) {
                    // use the explicitly specified persistence base location
                    baseDir = new File(baseLoc);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "FTPBC-D002001.SU_Init_Persist_Base_Specified",
                                new Object[]{
                                    baseLoc,
                                    aEndpoint.getServiceName(),
                                    aEndpoint.getEndpointName()});
                    }
                } else {
                    // use the default
                    baseDir = Utils.getLocalJBIPersistenceLocation(aEndpoint.getServiceUnit().getServiceUnitPath());
                    if (baseDir == null) {
                        throw new Exception(
                                mMessages.getString("FTPBC-E002011.Failed_Locate_Def_Persist_Base",
                                new Object[]{
                                    su.getServiceUnitId(),
                                    aEndpoint.getServiceName(),
                                    aEndpoint.getEndpointName()
                                }));
                    }
                }

                boolean dirOK = true;
                if (!baseDir.exists()) {
                    dirOK = baseDir.mkdirs();
                }

                if (!baseDir.exists()) {
                    throw new Exception(mMessages.getString("FTPBC-E002012.Persist_Not_Exist",
                            new Object[]{
                                baseDir.getPath(),
                                su.getServiceUnitId(),
                                aEndpoint.getServiceName(),
                                aEndpoint.getEndpointName()
                            }));
                }

                if (!baseDir.isDirectory()) {
                    throw new Exception(mMessages.getString("FTPBC-E002013.Persist_Not_Dir",
                            new Object[]{
                                baseDir.getPath(),
                                su.getServiceUnitId(),
                                aEndpoint.getServiceName(),
                                aEndpoint.getEndpointName()
                            }));
                }

                File endpointDir = new File(new File(new File(baseDir, suDir), serviceDir), epDir);

                dirOK = endpointDir.mkdirs();

                if (!endpointDir.exists()) {
                    throw new Exception(mMessages.getString("FTPBC-E002012.Persist_Not_Exist",
                            new Object[]{
                                endpointDir.getPath(),
                                su.getServiceUnitId(),
                                aEndpoint.getServiceName(),
                                aEndpoint.getEndpointName()
                            }));
                }

                if (!endpointDir.isDirectory()) {
                    throw new Exception(mMessages.getString("FTPBC-E002013.Persist_Not_Dir",
                            new Object[]{
                                endpointDir.getPath(),
                                su.getServiceUnitId(),
                                aEndpoint.getServiceName(),
                                aEndpoint.getEndpointName()
                            }));
                }

                for (Iterator opIter = operations.keySet().iterator(); opIter.hasNext();) {
                    QName opName = (QName) opIter.next();
                    String mep = (String) aEndpoint.getOperationMsgExchangePattern().get(opName);
                    File opDir = new File(endpointDir, EPUtil.fromQName2UniqueString(opName));
                    dirOK = opDir.mkdirs();

                    if (!opDir.exists()) {
                        throw new Exception(mMessages.getString("FTPBC-E002013.Persist_Not_Exist_Per_Op",
                                new Object[]{
                                    opDir.getPath(),
                                    opName.toString(),
                                    su.getServiceUnitId(),
                                    aEndpoint.getServiceName(),
                                    aEndpoint.getEndpointName()
                                }));
                    }

                    if (!opDir.isDirectory()) {
                        throw new Exception(mMessages.getString("FTPBC-E002014.Persist_Not_Dir_Per_Op",
                                new Object[]{
                                    opDir.getPath(),
                                    opName.toString(),
                                    su.getServiceUnitId(),
                                    aEndpoint.getServiceName(),
                                    aEndpoint.getEndpointName()
                                }));
                    }

                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "FTPBC-D002001.FBD_Init_SU_Register_Persistence_Root",
                                new Object[]{
                                    opDir.getCanonicalPath(),
                                    aEndpoint.getServiceName(),
                                    aEndpoint.getEndpointName(),
                                    opName.toString()});
                    }
                    // to this point, the token factory should be created
                    // store is a per operation object
                    String opID = aEndpoint.getOperationUUID(opName);
                    String tokenType = null;
                    HashMap opParams = new HashMap();

                    if (mTokenPersistParams != null) {
                        // token persistence params present
                        // indicates that synchronization
                        // is required - e.g., for clustering awareness
                        tokenType = (String) mTokenPersistParams.get(DataBaseToken.SYNC_TOKEN_TYPE);
                        if (tokenType != null) {
                            // prepare per operation token params for 
                            if (tokenType.equals(Sync.SYNC_DB_TOKEN)) {
                                // DB based token
                                opParams.put(DataBaseToken.TOKEN_ID_VALUE, opID);
                                opParams.put(DataBaseToken.TOKEN_TKN_TABLE_NAME, "T".concat(opID.replace('-', '_')));
                            } else if (tokenType.equals(Sync.SYNC_FILE_TOKEN)) {
                                opParams.put(FileToken.TOKEN_PROPERTY_LOCKFILE, opID.concat(".lock"));
                            }
                        }
                    }

                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "suID:::" + su.getServiceUnitId() + " suPath:::" + su.getServiceUnitPath() + " op:::" + opName.toString() + " opID::" + opID + " MEP:::" + mep);
                    }

                    CompositeLock l = CompositeLockRegistry.register(mCompLockFactory.createCompositeLock(opID, opParams, new FTPBCPersistStore(opDir)));

                    if (l == null) {
                        throw new Exception(
                                mMessages.getString("FTPBC-E002010.Failed_Register_Per_Op_CompositeLock",
                                new Object[]{
                                    opName.toString(),
                                    su.getServiceUnitId(),
                                    aEndpoint.getServiceName(),
                                    aEndpoint.getEndpointName(),
                                    opDir.getPath(),
                                    "NULL"}));
                    }
                }
            }
        }
    }

    /**
     * when a su is undeployed, remove all the endpoints associated with it
     * and also remove inbound endpoints' composite lock entry in CompositeLockRegistry;
     *
     **/
    private void cleanUpRegisteredLockAndPersistenceInfo(ServiceUnit su) throws Exception {
        Collection<Endpoint> theEndpoints = su.getEndpoints();
        if (theEndpoints != null) {
            for (Endpoint aEndpoint : theEndpoints) {
                for (Iterator opIter = aEndpoint.getOperations().keySet().iterator(); opIter.hasNext();) {
                    CompositeLockRegistry.remove(aEndpoint.getOperationUUID((QName) opIter.next()));
                }
            }
        }
    }
}
