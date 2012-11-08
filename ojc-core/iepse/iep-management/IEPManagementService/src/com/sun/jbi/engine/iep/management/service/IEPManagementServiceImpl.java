/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.iep.management.service;

import com.sun.esb.management.api.administration.AdministrationService;
import com.sun.esb.management.api.configuration.ConfigurationService;
import com.sun.esb.management.base.services.AbstractServiceImpl;
import com.sun.esb.management.client.ManagementClient;
import com.sun.esb.management.client.ManagementClientFactory;
import com.sun.esb.management.common.ManagementRemoteException;
import com.sun.jbi.engine.iep.management.api.IEPManagementService;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

/**
 *
 * @author rdwivedi
 */
public class IEPManagementServiceImpl extends AbstractServiceImpl
        implements IEPManagementService {

    private static final String ADMINISTRATION_KEY = "Administration";
    private String mComponentName = "sun-iep-engine";

    

    public IEPManagementServiceImpl(MBeanServerConnection serverConnection) {
        super(serverConnection);
    
    }

    public IEPManagementServiceImpl(MBeanServerConnection serverConnection,
            boolean isRemoteConnection) {
        super(serverConnection, isRemoteConnection);
        
    }


    public String callManagementFunction(String[] cmdParams,String targetName)
            throws ManagementRemoteException  {
        int pSize = cmdParams.length - 2;
        Object[] params = new Object [pSize] ;
        String[] signatures = new String [pSize];
        for(int i = 0 ; i < pSize; i++){
            params[i] = cmdParams[i+2];
            signatures[i] = "java.lang.String";
        }
        String result = null;
        result = callIEPManagementFunction(cmdParams[1], params, signatures,
                targetName).toString();
        return result;
    }


    public String setManagedAttributeForOperator(String planName, String oprName,
            String propName,Object value,String targetName)
            throws ManagementRemoteException {
        Object[] params = new Object [] {planName,oprName,propName,value} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String",
                                            "java.lang.String","java.lang.Object"};
        String result = null;
        result = (String)callIEPManagementFunction("setManageablePropertyValue",
                params, signatures, targetName);
        return result;
        
    }
    public String[] listDeployedIEP(String suName,String targetName)
            throws ManagementRemoteException {
        Object[] params = new Object [] {suName} ;
        String[] signatures = new String [] { "java.lang.String"};
        String[] result = null;
        result = (String[])callIEPManagementFunction("listProcessAttributes",
                params, signatures, targetName);
        return result;

    }
    public String[] listOperatorsForIEP(String iepName,String targetName)
            throws ManagementRemoteException {
        
        Object[] params = new Object [] {iepName} ;
        String[] signatures = new String [] { "java.lang.String"};
        String[] result = null;
        result = (String[])callIEPManagementFunction(
                    "listOperators", params, signatures,
                    targetName);
        return result;

    }

    public Map getManagedAttributeForOperator(String planName,String oprName,
            String targetName) throws ManagementRemoteException {
        Object[] params = new Object [] {planName,oprName} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String"};
        Map result = null;
        result =(Map) callIEPManagementFunction(
                    "getManagedAttributesForOperator", params, signatures,
                    targetName);
        return result;

    }

    public String setIEPForDebugging(String planName,String targetName) throws ManagementRemoteException {
        Object[] params = new Object [] {planName} ;
        String[] signatures = new String [] { "java.lang.String"};
        String result = null;
        result = (String)callIEPManagementFunction("setIEPForDebugging",
                params, signatures, targetName);
        return result;

    }
    /**
     * Unlink an IEP process from step by step process. It will make this process
     * avialbale for Round Robin shceduler and let the procecss run as usual in IEP engine.
     * @param planName
     */
    public String unsetIEPForDebugging(String planName,String targetName) throws ManagementRemoteException {
        Object[] params = new Object [] {planName} ;
        String[] signatures = new String [] { "java.lang.String"};
        String result = null;
        result = (String)callIEPManagementFunction("unsetIEPForDebugging",
                params, signatures, targetName);
        return result;
    }
    
    /**
     * Get the Debug Information in formatted string.
     * @param planName
     * @param oprName
     * @return
     * @throws java.lang.Exception
     */
    public String getCurrentDebugInfo(String planName, String oprName,String targetName) throws Exception {
        Object[] params = new Object [] {planName,oprName} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String"};
        String result = null;
        result = (String)callIEPManagementFunction("getCurrentDebugInfo",
                params, signatures, targetName);
        return result;


    }
    /**
     * Execute a debug command in step by step processor. Allowed command are
     * next : execute next available operator.
     * completeCycle : completes one cycle of IEP process .

     * nextCompleteCycles noOfTime(int) : complete cycles of IEP process noOfTime times.

     * nextCompleteCyclesForTimePeriod milliseconds (long) : complete the cycles of IEP process for milliseconds time.

     * addPauseBefore operatorName : add a pause (breakpoint) before an IEP process.

     * nextTillPause : execute iep process till a pause is found or cycle is complete.

     * nextOperator : displays the name of the operator that is to be executed next.

     * removePauseBefore operatorName : removes the pause before the operator if there is one available.

     * info : shows the relevent information about the last executed operator.

     * info oprName : shows the debug information about the oprName operator.

     * @param cmd see above for avilable commands.
     * @param planName
     * @param args
     * @return String message as a result of command results.
     * @throws java.lang.Exception
     */
    public String executeDebugCommand(String cmd, String planName, Object args,String targetName) throws Exception {
        Object[] params = new Object [] {planName,cmd,(Object)args} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String","java.lang.Object"};
        String result = null;
        result = (String)callIEPManagementFunction("executeDebugCommand",
                params, signatures, targetName);
        return result;
    }

    /**
     *
     * @param planName
     * @param operatorName
     * @return
     * @throws java.lang.Exception
     */
    public String setEnableDataAccessObject(String planName, String operatorName, String tabName,String targetName) throws Exception {
        Object[] params = new Object [] {planName,operatorName,tabName} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String","java.lang.String"};
        String result = null;
        result =(String) callIEPManagementFunction(
                    "setEnableDataAccessObject", params, signatures,
                    targetName);
        return result;
    }

    public String disableDataAccess(String planName, String oprName,String targetName) throws Exception {
        Object[] params = new Object [] {planName,oprName} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String"};
        String result = null;
        result =(String) callIEPManagementFunction(
                    "disableDataAccess", params, signatures,
                    targetName);
        return result;
    }
    /**
     * Set a property for the iep java object that externalize the data.
     * @param planName name of the iep process plan
     * @param operatorName name of the operator
     * @param propertyName property name
     * @param value value of the property must be simple serilaizable java object.
     * @throws java.lang.Exception
     */
    public String setIEPDataAccessProperty(String planName, String operatorName, String propertyName , String value,String targetName ) throws Exception {
        String result = "Not Implemented";
        return result;
    }
    /**
     *
     * @param planName planName
     * @param operatorName Operator name
     * @param jndi Jndi url
     * @param tableName tableName to be created and used by save stream
     * @return result of the operation
     */
    public String enableSaveStream(String planName, String operatorName , String jndi, String tableName,String isGlobal,String targetName)  throws ManagementRemoteException {
        Object[] params = new Object [] {planName,operatorName,jndi,tableName,isGlobal} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String","java.lang.String","java.lang.String","java.lang.String"};
        String result = null;
        result =(String) callIEPManagementFunction(
                    "enableSaveStream", params, signatures,
                    targetName);
        return result;
    }
    /**
     *
     * @param planName plan Name
     * @param saveStreamName name of the save stream operator that gets added and returned
     * as a call to enableSaveStream
     * @return result of the operation.
     */
    public String removeSaveStream(String planName, String saveStreamName,String targetName)  throws ManagementRemoteException {
        Object[] params = new Object [] {planName,saveStreamName} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String"};
        String result = null;
        result =(String) callIEPManagementFunction(
                    "removeSaveStream", params, signatures,
                    targetName);
        return result;
    }

    private ConfigurationService getConfig(ManagementClient client) throws
            ManagementRemoteException{

        ConfigurationService configurationService = null;
        try {
            configurationService = client.getConfigurationService();
        } catch (ManagementRemoteException e) {
            throw e;
        }
        return configurationService;
    }

    private Object callIEPManagementFunction(String methodName,Object[] params,
            String[] signature,String targetName)
            throws ManagementRemoteException {

        if (targetName == null) {
            return  invokeMBeanOperation(getIEPManagementServiceObjectName()
                    , methodName,
                    params,
                    signature);
        } else {
            ManagementClient client = null;
            try{
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                //client = ManagementClientFactory.getInstance(mHost, mPort, mUser, mPwd);
                ConfigurationService configurationService = getConfig(client);
                List<String> instances =  getAllIEPManagementServiceInstances(client, targetName);

                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    return  configurationService.invokeExtensionMBeanOperation(mComponentName, ADMINISTRATION_KEY,
                            methodName, params,
                            signature, targetName, instName);
                }
            }catch(ManagementRemoteException e) {
                e.printStackTrace();
                    throw e;
                }
            
        }
        return null;
    }

    private static ObjectName getIEPManagementServiceObjectName()
            throws ManagementRemoteException {
        String mbeanName = "com.sun.jbi:JbiName=server,ComponentName=sun-iep-engine,ControlType=Custom,CustomControlName=Administration,ComponentType=Installed,InstalledType=Engine";
        // com.sun.jbi:JbiName=server,CustomControlName=Administration,ComponentName=sun-bpel-engine,ControlType=Custom,ComponentType=Installed,InstalledType=Engine
        // String mbeanName =
        // "com.sun.ebi:ServiceType=ManagementAPI,InstallationType=serviceEngines,IdentificationName=sun-bpel-engine";
        ObjectName objectName = null;
        try {
            objectName = new ObjectName(mbeanName);
        } catch (MalformedObjectNameException e) {
            throw new ManagementRemoteException(e);
        } catch (NullPointerException e) {
            throw new ManagementRemoteException(e);
        }
        return objectName;
    }
    private  static List <String> getAllIEPManagementServiceInstances(
            ManagementClient client, String target)
            throws ManagementRemoteException {
        AdministrationService administrationService = null;
        Map<String /*targetName*/, String[] /*instanceNames*/> targetNameToInstancesMap = null;
        List <String> instances = new ArrayList<String> ();
        try {
            administrationService = client.getAdministrationService();
            targetNameToInstancesMap = administrationService.listTargetNames();
            String [] instanceNames = targetNameToInstancesMap.get(target);
            if (instanceNames != null) {
                for (int i = 0; i < instanceNames.length; i ++) {
                    instances.add(instanceNames[i]);
                }
            }
        } catch (Exception e) {
            throw new ManagementRemoteException(e);
        }
        return instances;
    }

    public String startNewChangeSet(String targetName) throws ManagementRemoteException {
        String result = null;
        result = (String)callIEPManagementFunction("startNewChangeSet",
                null, null, targetName);
        return result;
    }

    public String applyChangeSet(String targetName) throws ManagementRemoteException  {
        String result = null;
        result = (String)callIEPManagementFunction("applyChangeSet",
                null, null, targetName);
        return result;
    }

    public String ignoreChangeSet(String targetName) throws ManagementRemoteException  {
        String result = null;
        result = (String)callIEPManagementFunction("ignoreChangeSet",
                null, null, targetName);
        return result;
    }
    
    public String exportPlan(String planName, String planVersion, String targetName) throws ManagementRemoteException {
        Object[] params = new Object [] {planName,planVersion} ;
        String[] signatures = new String [] { "java.lang.String","java.lang.String"};
        String result = null;
        result =(String) callIEPManagementFunction(
                    "exportPlan", params, signatures,
                    targetName);
        return result;
    }
    
    public String purgePlanVersions(String planName, String targetName) throws ManagementRemoteException {
        Object[] params = new Object [] {planName} ;
        String[] signatures = new String [] { "java.lang.String"};
        String result = null;
        result =(String) callIEPManagementFunction(
                    "purgePlanVersions", params, signatures,
                    targetName);
        return result;
    }
    
    public String purgeAllPlanVersions(String targetName) throws ManagementRemoteException {
        String result = null;
        result =(String) callIEPManagementFunction(
                    "purgeAllPlanVersions", null, null,
                    targetName);
        return result;
    }
}
