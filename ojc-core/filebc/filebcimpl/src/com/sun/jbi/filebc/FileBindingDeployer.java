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
 * @(#)FileBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.filebc.Endpoint.EndpointType;
import com.sun.jbi.filebc.util.AlertsUtil;
import com.sun.jbi.filebc.util.EPUtil;
import com.sun.jbi.filebc.validator.EndpointValidator;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;
import java.util.logging.Level;

/** This class implements the ServiceUnitManager interface.
 *  The ServiceUnitManager interface defines component-supplied
 *  methods for managing service unit deployments.
 *
 * @author Sherry Weng
 */
public class FileBindingDeployer implements ServiceUnitManager {

    private static final Messages mMessages =
            Messages.getMessages(FileBindingDeployer.class);
    private static Logger mLogger = Messages.getLogger(FileBindingDeployer.class);
    private HashMap mServiceUnits;
    private ComponentContext mContext;
    private FileBindingLifeCycle mLifeCycle;

    public FileBindingDeployer(ComponentContext context, FileBindingLifeCycle lifeCycle) {
        mContext = context;
        mLifeCycle = lifeCycle;
        mServiceUnits = new HashMap();
    }

    /**
     * Initiate a BC Deployment.
     *
     * @param suId - service unit ID
     * @param asaFilePath - Path of the service assembly file
     */
    public String deploy(String suId, String suPath) throws DeploymentException {
        String taskName = "deploy";

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "FBD_Deploy_SU", new Object[]{suId, suPath});
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
            //registerEPInLockRegistry(su);
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
            String errMsg = mMessages.getString("FILEBC-E00201.Serviceunit_deploy_failed",
                    new Object[]{suId, ex.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            AlertsUtil.getAlerter().critical(errMsg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00201");
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "FBC_DEPLOY_1",
                    null,
                    errMsg,
                    ex);

            throw new DeploymentException(exMsg, ex);
        }

        //Put the service unit map, only after the deployment succeeds
        mServiceUnits.put(suId, su);

        return createSuccessMessage(taskName, mContext.getComponentName());
    }

    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "FBD_Init_SU", new Object[]{suId, suPath});
        }

        ServiceUnit su = null;
        try {
            // Prepare for start if the deployment hasn't been processed yet.
            su = (ServiceUnit) mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                        suPath,
                        mContext,
                        mLifeCycle.getRuntimeConfigurationMBean(),
                        mLifeCycle.getStatusProviderHelper(),
                        mLifeCycle.getInboundReceiver());

            }
            su.init();
            validateServiceUnitForUniqueness(su);
            registerEPInLockRegistry(su);
            mServiceUnits.put(suId, su);

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "FBD_Complete_Init_SU", suId);
            }
        } catch (Exception ex) {
            // Clean up our state
            if (su != null) {
                try {
                    su.stop();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }
            String errMsg = mMessages.getString("FILEBC-E00202.Serviceunit_init_failed",
                    new Object[]{suId, ex.getMessage()});
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(errMsg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00202");
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "FBC_INIT_1",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg, ex);
        }
    }

    public void start(String suId) throws DeploymentException {
        String taskName = "start";
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "FBD_Starting_SU", suId);
        }

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);

        if (su != null) {
            try {
                registerEPInLockRegistry(su);
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FILEBC-E00203.Serviceunit_preprocess_failed",
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
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00203");
                throw new DeploymentException(exMsg, ex);
            }

            try {
                su.start();
            } catch (Exception ex) {
                try {
                    deregisterEPFrom_Lock_Seq_AppendDest_Registry(su);
                } catch (Exception ex1) {
                    // ignore on purpose
                }
                String errMsg = mMessages.getString("FILEBC-E00204.Serviceunit_start_failed",
                        new Object[]{suId, ex.getMessage()});
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_START_1",
                        null,
                        errMsg,
                        ex);
                AlertsUtil.getAlerter().critical(exMsg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00204");
                throw new DeploymentException(exMsg, ex);
            }
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "FBD_Complete_Start_SU", suId);
        }
    }

    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "FBD_Stop_SU", suId);
        }

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FILEBC-E00205.Serviceunit_stop_failed",
                        new Object[]{suId, ex.getMessage()});
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_STOP_1",
                        null,
                        errMsg,
                        ex);
                AlertsUtil.getAlerter().critical(exMsg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00205");
                throw new DeploymentException(exMsg, ex);
            }
            // further close up the channel on the inbound lock file
            try {
                cleanUpEPLocks(su);
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FILEBC-E00206.Serviceunit_postprocess_failed",
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
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00206");
                throw new DeploymentException(exMsg, ex);
            }
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "FBD_Complete_Stop_SU", suId);
        }
    }

    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "FBD_Shutdown_SU", suId);
        }
        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
                //                if (mServiceUnits.containsKey(suId)) {
                //                    mServiceUnits.remove(suId);
                //                }
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FILEBC-E00207.Serviceunit_shutdown_failed",
                        new Object[]{suId, ex.getMessage()});
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_SHUTDOWN_1",
                        null,
                        errMsg,
                        ex);
                AlertsUtil.getAlerter().critical(exMsg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00207");
                throw new DeploymentException(exMsg, ex);
            }
            // when shutdown, it should be stopped already, but call closeChannels()
            // any way
            // further close up the channel on the inbound lock file
            try {
                cleanUpEPLocks(su);
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FILEBC-E00208.Serviceunit_shutdown_postprocess_failed",
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
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00208");
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

        // make sure the service unit is properly shut down
        // shutDown(name); // no longer needed - since the framework will call
        // shutdown before undeploy if necessary

        if (mServiceUnits.containsKey(name)) {
            ServiceUnit su = (ServiceUnit) mServiceUnits.remove(name);
            // if it is inbound also de-register the lock entry in LockRegistry
            if (su != null) {
                try {
                    deregisterEPFrom_Lock_Seq_AppendDest_Registry(su);
                } catch (Exception ex) {
                    throw new DeploymentException(mMessages.getString("FILEBC-E00209.FBD_Exception_deregister_EP_from_inbound_EP_lock_reg", new Object[]{ex}));
                }
            }
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "FBD_Undeploy_SU", new Object[]{name, root});
        }
        return createSuccessMessage(taskName, mContext.getComponentName());
    }

    public HashMap getServiceUnits() {
        return mServiceUnits;
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

    private boolean validateServiceUnitForUniqueness(ServiceUnit su) throws Exception {
        Collection<Endpoint> theEndpoints = su.getEndpoints();
        for (Endpoint aEndpoint : theEndpoints) {
            Set<Map.Entry<String, ServiceUnit>> suEntries = mServiceUnits.entrySet();
            for (Map.Entry<String, ServiceUnit> suEntry : suEntries) {
                ServiceUnit aServiceUnit = suEntry.getValue();
                if (aServiceUnit.getServiceUnitId().equals(su.getServiceUnitId())) {
                    // validation within the service unit is done at SU deployment time
                    continue;
                }
                Collection<Endpoint> activatedEndpoints = aServiceUnit.getEndpoints();

                EndpointValidator.validateEndpointForUniqueness(activatedEndpoints, aEndpoint, true);
            }
        }

        return true;
    }

    /**
     * register ep (both inbound and outbound) in the lock registry
     **/
    private void registerEPInLockRegistry(ServiceUnit su) throws Exception {
        Collection<Endpoint> theEndpoints = su.getEndpoints();
        String key = null;
        FileOutputStream fos = null;
        File lockFile = null;
        File targetDir = null;
        String fileDir = null;
        String persistBaseDir = null;

        if (theEndpoints == null) // only when run in junit
        {
            return;
        }

        for (Endpoint aEndpoint : theEndpoints) {
            if (aEndpoint.getServiceUnitID() == null && su.getServiceUnitId() != null) {
                aEndpoint.setServiceUnitID(su.getServiceUnitId());
            }
            // there is endpoint type info in the EP UUID
            // so with the same unique name, a endpoint can be registered for
            // IB and OB
            key = aEndpoint.getEPUUID();
            String lockName = null;
            if (LockRegistry.get(key) == null) {
                fos = null;
                lockFile = null;
                fileDir = null;
                try {
                    // if fileDirectory value equals at least one existing registered EP's
                    // then need to further check lockName, and workArea
                    // validation - lock name conflict

                    // open esb issue #: 1532
                    // introduced file:address->persistenceBaseLoc
                    // if specified, it will be the root dir
                    // for the work area base dir, under which
                    // the operational files such as lock file, seq files,
                    // etc. will be created

                    // fileDir must not be empty
                    fileDir = EPUtil.getFileDirectory(aEndpoint);
                    persistBaseDir = EPUtil.getPersistenceBaseDirectory(aEndpoint);
                    File root = null;
                    if (persistBaseDir != null && persistBaseDir.trim().length() > 0) {
                        // goes to the wsdl specified persist base location
                        root = new File(persistBaseDir.trim());
                        if (root.exists()) {
                            if (!root.isDirectory()) {
                                // report error
                                throw new Exception(
                                        mMessages.getString("FILEBC-E00211.Persist_Base_Exists_Not_Dir",
                                        new Object[]{
                                            persistBaseDir,
                                            aEndpoint.getServiceName().toString(),
                                            aEndpoint.getEndpointName()
                                        }));
                            }
                        } else {
                            boolean ok = root.mkdirs();
                            if (!ok) {
                                // report error
                                throw new Exception(
                                        mMessages.getString("FILEBC-E00212.Mkdir_Failed_Create_Lock_File",
                                        new Object[]{
                                            root.getPath(),
                                            aEndpoint.getServiceName().toString(),
                                            aEndpoint.getEndpointName()
                                        }));
                            }
                        }
                    } else {
                        root = new File(fileDir);
                    }

                    targetDir = new File(root, EPUtil.getWorkAreaBaseDir(aEndpoint));
                    targetDir.mkdirs();

                    // now add one more layer to distinguish IB and OB
                    targetDir = new File(targetDir, key);
                    targetDir.mkdirs();

                    //write out info about this directory
                    writeWorkDirReadme(new File(targetDir, "readme.txt"), aEndpoint);
                    aEndpoint.setWorkAreaDir(targetDir.getAbsolutePath());
                    lockName = aEndpoint.getFileAddress().getLockName();
                    lockName = lockName != null && lockName.trim().length() > 0 ? lockName : Lock.DEFAULT_INBOUND_LOCKFILE_NAME;
                    lockFile = new File(targetDir, lockName);
                    lockFile.createNewFile();
                    fos = new FileOutputStream(lockFile);
                } catch (FileNotFoundException ex) {
                    // delay channel creation util thread polling time
                    if (mLogger.isLoggable(Level.FINE)) {
                        String msg = mMessages.getString("FBD_Init_SU_FileNotFound",
                                new Object[]{fileDir != null ? fileDir : "", lockName, key});
                        mLogger.log(Level.FINE, msg, ex);
                    }
                }
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "FBD_Init_SU_Register_Lock",
                            new Object[]{lockFile.getCanonicalPath(), key});
                }
                Lock l = LockRegistry.register(key, new Lock((fos != null ? fos.getChannel() : null), new ReentrantLock(), lockFile.getCanonicalPath()));
                if (l == null) {
                    throw new Exception(
                            mMessages.getString("FILEBC-E00210.Failed_register_lock_4_inbound_endpoint",
                            new Object[]{aEndpoint.getServiceName().toString(),
                                aEndpoint.getEndpointName(),
                                lockFile.getCanonicalPath()
                            }));
                }
            } else {
                //do nothing - already registered
                }
        }

    }

    private void writeWorkDirReadme(File file, Endpoint endpoint) {
        if (!file.exists()) {
            try {
                String comAppName = endpoint.getServiceUnitID();
                int pos = comAppName.lastIndexOf(EPUtil.FILEBINDINGNAME);
                if (pos > -1) {
                    comAppName = comAppName.substring(0, pos);
                }
                OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(file), "UTF-8");
                writer.write("This is the work area and archive folder for endpoint identified with the following information: \n");
                writer.write("Composite Application Name: \n");
                writer.write("    " + comAppName + "\n");
                writer.write("Service QName: \n");
                writer.write("    " + endpoint.getServiceName().toString() + "\n");
                writer.write("Port Name: \n");
                writer.write("    " + endpoint.getEndpointName());
                writer.write("Endpoint Type : " + (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND ? "INBOUND" : "OUTBOUND"));
                writer.flush();
                writer.close();
            } catch (Exception ex) {
                if (mLogger.isLoggable(Level.FINE)) {
                    String msg = mMessages.getString("FBD_Init_SU_README_FAIL",
                            new Object[]{file.getParent() != null ? file.getParent() : "", endpoint.getServiceName().toString() + endpoint.getEndpointName()});

                    mLogger.log(Level.FINE, "failed to create work dir readme file", ex);
                }
            }
        }
    }

    /**
     * when a su is undeployed, remove all the EPs associated with it
     * and also remove inbound EP's lock entry in LockRegistry and outbound
     * EP's sequence file from sequence registry;
     **/
    private void deregisterEPFrom_Lock_Seq_AppendDest_Registry(ServiceUnit su) throws Exception {
        Collection<Endpoint> theEndpoints = su.getEndpoints();
        if (theEndpoints != null) {
            for (Endpoint aEndpoint : theEndpoints) {
                //if (aEndpoint.getEndpointType() == EndpointType.INBOUND) {
                LockRegistry.remove(aEndpoint.getEPUUID());
                //}
                if (aEndpoint.getEndpointType() == EndpointType.OUTBOUND) {
                    SequenceRegistry.deregister(aEndpoint.getEPUUID());
//                    AppendDestinationRegistry.deregister(aEndpoint.getEPUUID());
                }
            }
        }
    }

    /**
     * close the file channels associated with endpoints
     * related to the SU
     **/
    private void cleanUpEPLocks(ServiceUnit su) throws Exception {
        Collection<Endpoint> theEndpoints = su.getEndpoints();
        if (theEndpoints != null) {
            for (Endpoint aEndpoint : theEndpoints) {
                LockRegistry.remove(aEndpoint.getEPUUID());
            }
        }
    }
}
