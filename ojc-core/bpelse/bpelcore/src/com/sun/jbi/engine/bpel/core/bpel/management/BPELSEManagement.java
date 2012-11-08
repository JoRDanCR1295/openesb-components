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
 * @(#)$Id: BPELSEManagement.java,v 1.40 2010/02/04 02:51:14 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.management;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sql.rowset.serial.SerialClob;
import javax.wsdl.Message;
import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathContextFactory;
import org.apache.commons.jxpath.Pointer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.EntityResolver;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.DefaultWSDLResolverFactory;
import com.sun.bpel.model.DefaultXSDResolverFactory;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.parser.impl.ParseContextImpl;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.model.visitor.IXSDResolver;
import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.EventsConfigurationHelper;
import com.sun.jbi.engine.bpel.core.bpel.event.ProcessEventsConfig;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.BPELEventFactory;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.VariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.management.util.SchemaConstants;
import com.sun.jbi.engine.bpel.core.bpel.management.util.SearchStringParser;
import com.sun.jbi.engine.bpel.core.bpel.management.util.SearchUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.MonitorManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateFactory;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMFactory;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;



public class BPELSEManagement {

    /** Log handle */
    private static final Logger LOGGER = Logger
            .getLogger(BPELSEManagement.class.getName());

    public enum SimpleVarType {
    	String("S"),
    	Numeric("N"),
    	Boolean("B"),
    	Date("D");
    	
    	String mCode;
    	
    	SimpleVarType(String code) {
    		mCode = code;
    	}
    	
    	public String getCode() {
    		return mCode;
    	}
    }

    /** Sort order ASC * */
    private static final String ASC = "ASC";

    /** Sort order ASC * */
    private static final String DESC = "DESC";

    /** Used for alerting * */
    public static enum ActionType {
        Suspended, Resumed, Terminated, ChangeVariable
    }
    
    private static final String SIMPLE_TYPE_NOTE_PREFIX = "Searchable. Type: ";
    
    private static final String SEARCH_SQL_PREFIX = " AND " + SchemaConstants.INSTANCE_ID + " IN ";

    private static final String GET_BPEL_PATH_QUERY = "SELECT " + SchemaConstants.SU_ZIP_ARCHIVE + " FROM " 
    	+ MonitorDBSchemaCreation.SERVICE_UNIT + " WHERE " + SchemaConstants.SU_NAME + " = ?";
    
    private static final String GET_BPEL_VARIABLE_QUERY = "SELECT " + SchemaConstants.VAR_VALUE + " FROM " 
    	+ MonitorDBSchemaCreation.MONITOR_BPEL_VARIABLE + " WHERE " + SchemaConstants.INSTANCE_ID + " = ? AND " 
    	+ SchemaConstants.VAR_ID + " = ?";
  
    private static final String GET_SIMPLE_VARIABLE_QUERY = "SELECT " + SchemaConstants.STR_VALUE + " FROM "
    	+ MonitorDBSchemaCreation.MONITOR_SIMPLE_VARIABLE + " WHERE " + SchemaConstants.INSTANCE_ID + " = ? AND "
    	+ SchemaConstants.VAR_ID + " = ?";

    private static final String GET_BPEL_INSTANCES_NO_ALL_QUERY = "SELECT COUNT(" + SchemaConstants.INSTANCE_ID
		+ ") FROM " + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE;
    
    private static final String GET_BPEL_INSTANCES_NO_BY_STATUS_QUERY = GET_BPEL_INSTANCES_NO_ALL_QUERY + " WHERE "
    	+ SchemaConstants.STATUS + " = ?";
    
    private static final String GET_BPEL_INSTANCES_NO_BY_PROCESS_QUERY = GET_BPEL_INSTANCES_NO_ALL_QUERY + " WHERE "
    	+ SchemaConstants.BPEL_ID + " = ?";
    
    private static final String GET_BPEL_INSTANCES_NO_BY_PROCESS_STATUS_QUERY = GET_BPEL_INSTANCES_NO_ALL_QUERY 
    	+ " WHERE " + SchemaConstants.BPEL_ID + " = ? AND " + SchemaConstants.STATUS + " = ?";
    
    private static final String GET_BPEL_INSTANCES_NO_BY_INSTANCE_ID_QUERY = GET_BPEL_INSTANCES_NO_ALL_QUERY + " WHERE "
    	+ SchemaConstants.INSTANCE_ID + " = ?";
    
    private static final String GET_BPEL_INSTANCES_ALL_QUERY = "SELECT " + SchemaConstants.INSTANCE_ID + ", " 
		+ SchemaConstants.BPEL_ID + ", " + SchemaConstants.STATUS + ", " + SchemaConstants.START_TIME + ", " 
		+ SchemaConstants.END_TIME + ", " + SchemaConstants.UPDATED_TIME + " FROM " 
		+ MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE;
    	
    private static final String GET_BPEL_INSTANCES_BY_STATUS_QUERY =  GET_BPEL_INSTANCES_ALL_QUERY + " WHERE "
    	+ SchemaConstants.STATUS + " = ?";
    
    private static final String GET_BPEL_INSTANCES_BY_PROCESS_QUERY = GET_BPEL_INSTANCES_ALL_QUERY + " WHERE "
    	+ SchemaConstants.BPEL_ID + " = ?";

    private static final String GET_BPEL_INSTANCES_BY_PROCESS_STATUS_QUERY = GET_BPEL_INSTANCES_ALL_QUERY + " WHERE "
    	+ SchemaConstants.BPEL_ID + " = ? AND " + SchemaConstants.STATUS + " = ?";
    
    private static final String GET_BPEL_INSTANCES_BY_INSTANCE_ID_QUERY =  GET_BPEL_INSTANCES_ALL_QUERY + " WHERE "
    	+ SchemaConstants.INSTANCE_ID + " = ?";

    private static final String GET_INNER_VAR_COMMON = "SELECT v." + SchemaConstants.VAR_NAME + ", v." 
    	+ SchemaConstants.VAR_ID + ", a." + SchemaConstants.ACTIVITY_XPATH + " FROM " 
    	+ MonitorDBSchemaCreation.MONITOR_BPEL_VARIABLE + " v, " + MonitorDBSchemaCreation.MONITOR_BPEL_ACTIVITY 
    	+ " a WHERE v." + SchemaConstants.SCOPE_ID + " = a." + SchemaConstants.ACTIVITY_ID + " AND v." 
    	+ SchemaConstants.INSTANCE_ID + " = a." + SchemaConstants.INSTANCE_ID + " AND v." 
    	+ SchemaConstants.IS_FAULT + " = 'N' AND v." + SchemaConstants.INSTANCE_ID + " = ?" ;
    
    private static final String GET_INNER_VAR_QUERY = GET_INNER_VAR_COMMON + " AND v." + SchemaConstants.IS_FAULT 
    	+ " = 'N' AND v." + SchemaConstants.SCOPE_ID + " <> " + RBPELProcess.DEFAULT_PROCESS_SCOPE_ID; 
    
    private static final String GET_INNER_SINGLE_VAR_QUERY = GET_INNER_VAR_COMMON + " AND v." 
    	+ SchemaConstants.VAR_NAME + " = ? AND v." + SchemaConstants.SCOPE_ID + " <> " 
    	+ RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
    
    private static final String GET_INNER_SIMPLE_VAR_COMMON = "SELECT v." + SchemaConstants.VAR_NAME + ", v." 
		+ SchemaConstants.VAR_ID + ", a." + SchemaConstants.ACTIVITY_XPATH + ", v." + SchemaConstants.VAR_TYPE 
		+ " FROM " + MonitorDBSchemaCreation.MONITOR_SIMPLE_VARIABLE + " v, "
		+ MonitorDBSchemaCreation.MONITOR_BPEL_ACTIVITY + " a WHERE v." + SchemaConstants.SCOPE_ID + " = a." 
		+ SchemaConstants.ACTIVITY_ID + " AND v." + SchemaConstants.INSTANCE_ID + " = a." + SchemaConstants.INSTANCE_ID 
		+ " AND v." + SchemaConstants.INSTANCE_ID + " = ?";
    
    private static final String GET_INNER_SIMPLE_VAR_QUERY = GET_INNER_SIMPLE_VAR_COMMON + " AND v." 
    	+ SchemaConstants.SCOPE_ID + " <> " + RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;

    private static final String GET_INNER_SINGLE_SIMPLE_VAR_QUERY = GET_INNER_SIMPLE_VAR_COMMON + " AND v." 
		+ SchemaConstants.VAR_NAME + " = ? AND v." + SchemaConstants.SCOPE_ID + " <> " 
		+ RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
    
    private static final String GET_PROCESS_SINGLE_VAR_QUERY = "SELECT " + SchemaConstants.VAR_NAME + ", "
    	+ SchemaConstants.VAR_ID + " FROM " + MonitorDBSchemaCreation.MONITOR_BPEL_VARIABLE + " WHERE "
    	+ SchemaConstants.IS_FAULT + " = 'N' AND " + SchemaConstants.INSTANCE_ID + " = ? AND "
    	+ SchemaConstants.VAR_NAME + " = ? AND " + SchemaConstants.SCOPE_ID + " = " 
    	+ RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
    
    private static final String GET_PROCESS_SINGLE_SIMPLE_VAR_QUERY = "SELECT " + SchemaConstants.VAR_NAME + ", "
		+ SchemaConstants.VAR_ID + ", " + SchemaConstants.VAR_TYPE + " FROM " 
		+ MonitorDBSchemaCreation.MONITOR_SIMPLE_VARIABLE + " WHERE " + SchemaConstants.INSTANCE_ID + " = ? AND "
		+ SchemaConstants.VAR_NAME + " = ? AND " + SchemaConstants.SCOPE_ID + " = " 
		+ RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
    
    private static final String GET_PROCESS_VAR_QUERY = "SELECT " + SchemaConstants.VAR_NAME + ", "
    	+ SchemaConstants.VAR_ID + " FROM " + MonitorDBSchemaCreation.MONITOR_BPEL_VARIABLE + " WHERE " 
    	+ SchemaConstants.IS_FAULT + " = 'N' AND " + SchemaConstants.INSTANCE_ID + " = ? AND "
    	+ SchemaConstants.SCOPE_ID + " = " + RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
    
    private static final String GET_PROCESS_SIMPLE_VAR_QUERY = "SELECT " + SchemaConstants.VAR_NAME + ", " 
    	+ SchemaConstants.VAR_ID + ", " + SchemaConstants.VAR_TYPE + " FROM " 
    	+ MonitorDBSchemaCreation.MONITOR_SIMPLE_VARIABLE + " WHERE " + SchemaConstants.INSTANCE_ID + " = ? AND "
    	+ SchemaConstants.SCOPE_ID + " = " + RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;

    private static final String GET_PROCESS_FAULT_QUERY = "SELECT " + SchemaConstants.VAR_VALUE + " FROM "
    	+ MonitorDBSchemaCreation.MONITOR_BPEL_VARIABLE + " WHERE " + SchemaConstants.IS_FAULT + " = 'Y' AND " 
    	+ SchemaConstants.INSTANCE_ID + " = ? AND (" + SchemaConstants.SCOPE_ID + " = " 
    	+ RBPELProcess.DEFAULT_PROCESS_SCOPE_ID + " OR " + SchemaConstants.SCOPE_ID + " = " 
    	+ BPELDocument.ACTIVITY_START_ID + ")";
    
    private static final String GET_ACTIVITY_STATUS_BY_INSTANCE = "SELECT " + SchemaConstants.ACTIVITY_ID + ", "
    	+ SchemaConstants.ACTIVITY_XPATH + ", " + SchemaConstants.ITERATION + ", " + SchemaConstants.STATUS + ", "
    	+ SchemaConstants.START_TIME + ", " + SchemaConstants.END_TIME + " FROM " 
    	+ MonitorDBSchemaCreation.MONITOR_BPEL_ACTIVITY + " WHERE " + SchemaConstants.INSTANCE_ID + " = ? ORDER BY "
    	+ SchemaConstants.START_TIME + " ASC";

    private static final String GET_INVOKE_COMMON = "SELECT a." + SchemaConstants.INSTANCE_ID + ", a." 
		+ SchemaConstants.BPEL_ID + ", a." + SchemaConstants.STATUS + ", a." + SchemaConstants.START_TIME + ", a."
		+ SchemaConstants.END_TIME + ", a." + SchemaConstants.UPDATED_TIME + " FROM " 
		+ MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + " a, " + MonitorDBSchemaCreation.MONITOR_BPEL_ACTIVITY 
		+ " b WHERE a." + SchemaConstants.INSTANCE_ID + " = b." + SchemaConstants.INSTANCE_ID; ;
    
    private static final String GET_INVOKEE = GET_INVOKE_COMMON + " AND b." + SchemaConstants.CRMP_RECEIVE_ID 
    	+ " = ?";
    
    private static final String GET_INVOKER = GET_INVOKE_COMMON + " AND b." + SchemaConstants.CRMP_INVOKE_ID + " = ?";

    private static final String GET_INVOKEE_INVOKEEID = "SELECT " + SchemaConstants.CRMP_INVOKE_ID + " FROM "
    	+ MonitorDBSchemaCreation.MONITOR_BPEL_ACTIVITY + " WHERE " + SchemaConstants.ACTIVITY_ID + " = ? AND " 
    	+ SchemaConstants.INSTANCE_ID + " = ? AND " + SchemaConstants.CRMP_INVOKE_ID + " IS NOT NULL";
    
    private static final String GET_INVOKEE_INVOKEEIDS = "SELECT " + SchemaConstants.CRMP_INVOKE_ID + " FROM " 
    	+ MonitorDBSchemaCreation.MONITOR_BPEL_ACTIVITY + " WHERE " + SchemaConstants.INSTANCE_ID + " = ? AND "
    	+ SchemaConstants.CRMP_INVOKE_ID + " IS NOT NULL";

    private static final String GET_INVOKER_RECEIVEID = "SELECT " + SchemaConstants.CRMP_RECEIVE_ID + " FROM " 
    	+ MonitorDBSchemaCreation.MONITOR_BPEL_ACTIVITY + " WHERE " + SchemaConstants.ACTIVITY_ID + " = ? AND " 
    	+ SchemaConstants.INSTANCE_ID + " = ? AND " + SchemaConstants.CRMP_RECEIVE_ID + " IS NOT NULL";
    
    private static final String GET_INVOKER_RECEIVEIDS = "SELECT " + SchemaConstants.CRMP_RECEIVE_ID + " FROM " 
		+ MonitorDBSchemaCreation.MONITOR_BPEL_ACTIVITY + " WHERE " + SchemaConstants.INSTANCE_ID + " = ? AND " 
		+ SchemaConstants.CRMP_RECEIVE_ID + " IS NOT NULL";

    private static final String BP_PROCESS = "/bpws:process";

    private static final int BUFFER_SIZE = 4096;

    private static final int MAX_INSTANCE = 1000;

//    private static final String MBEAN_OBJECT_STR = 
//    	"com.sun.ebi:ServiceType=ManagementAPI,InstallationType=serviceEngines,IdentificationName=sun-bpel-engine";
//
//    private static final String SUSPEND_INSTANCE = "suspendInstance";
//
//    private static final String RESUME_INSTANCE = "resumeInstance";
//
//    private static final String TERMINATE_INSTANCE = "terminateInstance";
//
//    private static final String CHANGE_VARIABLE_VALUE = "changeVariableValue";

    private Engine mEngine;

    private DBConnectionFactory mConnFactory;

    private boolean isInitialized;

    private static final JXPathContextFactory JXPATH_FACTORY = JXPathContextFactory
            .newInstance();

    public BPELSEManagement(Engine engine) throws Exception {
        mEngine = engine;
    }

    private void checkMonitorEnabled() throws Exception {
        if (!mEngine.isMonitorEnabled()) {
            throw new Exception(I18n.loc("BPCOR-6116: Monitoring is not enabled for the engine"));
        }
    }

    private void checkVariableMonitorEnabled() throws Exception {
        if (!mEngine.isMonitorEnabled()) {
            throw new Exception(I18n.loc("BPCOR-6116: Monitoring is not enabled for the engine"));
        }
        if (!mEngine.isVariableMonitorEnabled()) {
            throw new Exception(I18n.loc("BPCOR-6162: Variable Monitoring is not enabled for the engine"));
        }
    }

    /**
     * Change BPEL variable value, only the leaf node can be changed
     * 
     * @param instanceId
     *            The instance id of the instance on which the variable will be
     *            changed
     * @param varId
     *            The variable Id, can not be null
     * @param partName
     *            The part name of the variable, if null, the variable is not
     *            message type
     * @param xpath
     *            The xpath leading to the leaf node to be changed, if null, the
     *            part itself is
     * @param value
     *            The new value to change
     * @param mbeanConn
     *            Mbean Server connection
     * @throws Exception
     */
    public Boolean changeVariableValue(String instanceId, Long varId, String partName, String xpath, String value) 
    throws Exception {
    	
        initDataSource();
        checkVariableMonitorEnabled();
        // checkMonitorEnabled();
        String errorMsg = null;
        String varName = null;
        QName bpName = null;
        if (Utility.isEmpty(instanceId)) {
            errorMsg = I18n.loc("BPCOR-6117: bpel Instance id is not passed in");
        } else if (varId == 0) {
            errorMsg = I18n.loc("BPCOR-6118: bpel variable id is not passed in");
        } else {
            MonitorManager monitorMgr = getMonitorManager(instanceId);
            if (monitorMgr == null) {
                return Boolean.FALSE; // Not found
            } else {
                VariableScope context = monitorMgr.getVariableScope(varId);
                if (context == null) {
                    errorMsg = I18n.loc("BPCOR-6044: Can not find variable, instance: {0}, variable {1}", 
                    		instanceId, new Long(varId));
                } else {
                    RVariable rvar = null;
                    Set<RVariable> rvars = context.getRuntimeVariables().keySet();
                    for (RVariable rv : rvars) {
                        if (rv.getUniqueId() == varId) {
                            rvar = rv;
                            break;
                        }
                    }
                    if (rvar == null) {
                        errorMsg = I18n.loc("BPCOR-6044: Can not find variable, instance: {0}, variable {1}",
                        		instanceId, new Long(varId));
                    } else {
                        RuntimeVariable runtimeVar = context.getRuntimeVariable(rvar);
                        varName = rvar.getName();
                        if (runtimeVar == null) {
                            errorMsg = I18n.loc("BPCOR-6044: Can not find variable, instance: {0}, variable {1}", 
                            		instanceId, new Long(varId));
                        } else {
                            bpName = monitorMgr.getInstance().getBPELProcessManager().getBPELProcess().getBPELId();
                            String oldValue = runtimeVar.getSerializedValue().toString();
                            try {
                                if (Utility.isEmpty(partName)) {
                                    if (Utility.isEmpty(xpath)) {
                                        changeVariableSchemaTypeValue(runtimeVar, value, context, bpName, instanceId, 
                                        		oldValue);
                                    } else {
                                        changeVariableSchemaTypeValue(runtimeVar, xpath, value, context, bpName, 
                                        		instanceId,oldValue);
                                    }
                                } else {
                                    if (Utility.isEmpty(xpath)) {
                                        changeVariableMessageTypeValue(runtimeVar, partName, value, context, bpName, 
                                        		instanceId, oldValue);
                                    } else {
                                        changeVariableMessageTypeValue(runtimeVar, partName, xpath, value, context, 
                                        		bpName, instanceId, oldValue);
                                    }
                                }
                            } catch (Exception e) {
                                throw new Exception(e.getMessage());
                            }
                        }
                    }
                }
            }
        }

        if (errorMsg != null) {
            throw new Exception(errorMsg);
        }
        BPELTraceManager.getInstance().alertBPInstanceChangeByAPI(mEngine.getId(), bpName.toString(), instanceId, 
        		varName, ActionType.ChangeVariable);
        return Boolean.TRUE;
    }
    
    /**
     * Sets the event generation flag for a particular process. 
     * 
     * @param processName the name of the process to change the event generation flag for.
     * @param turnOn true turns event generation on and false turns it off.
     * @return the previous state of the event generation flag.
     */
    public void setProcessEventsGenerationFlag(Map<String, Boolean> configValues) throws Exception {
    	
    	// This is done is two stages so that as much as possible change values for all processes or for none. That is 
    	// why we first make sure that all the process ids are valid in the first step and if yes then change the 
    	// values in the second step.
    	Map<ProcessEventsConfig, BPELProcessManager> bpelEventsMap = 
    		new HashMap<ProcessEventsConfig, BPELProcessManager>();
    	for (Map.Entry<String, Boolean> entry : configValues.entrySet()) {
    		QName processQName = null;
    		try {
    			processQName = QName.valueOf(entry.getKey());
    		} catch (Exception e) {
    			String errorMsg = I18n.loc("BPCOR-6112: bpel process name: {0} is not a well formed QName", 
    					entry.getKey());
    			throw new Exception(errorMsg);
    		}
    		BPELProcessManager procManager = mEngine.getBPELProcessManager(processQName);
    		if (procManager == null) {
    			String errorMsg = I18n.loc("BPCOR-6195: bpel process {0} is not registered with the BPEL SE. " +
    					"No values were updated.", entry.getKey());
    			throw new Exception(errorMsg);
    		}
    		assert(entry.getValue() != null);
    		if (procManager.getGenerateEventsFlag() != entry.getValue().booleanValue()) {
    			ProcessEventsConfig config = new ProcessEventsConfig(processQName, procManager.getServiceUnitName());
    			config.setProcessEventsFlag(entry.getValue());
    			bpelEventsMap.put(config, procManager);
    		}
    	}
        
    	if (!bpelEventsMap.isEmpty()) {
    		if (mEngine.isMonitorEnabled()) {
    			initDataSource();
    			EventsConfigurationHelper.persistEventsGenerationFlag(mConnFactory, bpelEventsMap.keySet());
    		}
    		// Now that persistence to the monitoring tables is complete (if monitoring was on) change the values on 
    		// the process managers.
    		for (ProcessEventsConfig config : bpelEventsMap.keySet()) {
    			BPELProcessManager procManager = bpelEventsMap.get(config);
    			procManager.setGenerateEventsFlag(config.getProcessEventsFlag());
    		}
    	}
    }
    
    /**
     * 
     * @return
     * @throws Exception
     */
    public Map<String, Map<String, Boolean>> getAllProcessEventsGenerationFlag() throws Exception {
    	Collection<BPELProcessManager> procManagerCol = mEngine.getBPELProcessManagers();
    	
    	Map<String, Map<String, Boolean>> allProcEventGenValMap = new HashMap<String, Map<String, Boolean>>();
    	for (BPELProcessManager procManager : procManagerCol) {
    		String suName = procManager.getServiceAssemblyName();
    		String bpelId = procManager.getBPELProcess().getBPELId().toString();
    		Boolean eventFlag = new Boolean(procManager.getGenerateEventsFlag());
    		Map<String, Boolean> procEventGenValMap = allProcEventGenValMap.get(suName);
    		if (procEventGenValMap == null) {
    			procEventGenValMap = new HashMap<String, Boolean>();
    			allProcEventGenValMap.put(suName, procEventGenValMap);
    		}
    		procEventGenValMap.put(bpelId, eventFlag);
    	}
    	return allProcEventGenValMap;
    }

    /**
     * Resume a suspended BPEL instance
     * 
     * @param bpId
     *            The bpel instance to be resumed
     * @param mbeanConn
     *            Mbean Server connection
     * @throws Exception
     */
    public Boolean resumeInstance(String instanceId) throws Exception {
        initDataSource();
        // checkMonitorEnabled();
        String errorMsg = null;
        if (Utility.isEmpty(instanceId)) {
            errorMsg = I18n.loc("BPCOR-6117: bpel Instance id is not passed in");
        } else {
            MonitorManager monitorMgr = getMonitorManager(instanceId);
            if (monitorMgr == null || !monitorMgr.getInstance().isSuspended()) {
                // errorMsg = MESSAGES.getString(
                // "BPELSEManagement.Can_Not_Find_Instance",
                // instanceId);
                return Boolean.FALSE;
            } else {
                monitorMgr.resumeInstance();
                QName bpName = monitorMgr.getInstance().getBPELProcessManager().getBPELProcess().getBPELId();
                BPELTraceManager.getInstance().alertBPInstanceChangeByAPI(mEngine.getId(), bpName.toString(), 
                		instanceId, null, ActionType.Resumed);
                return Boolean.TRUE;
            }
        }
        if (errorMsg != null) {
            throw new Exception(errorMsg);
        }
        return Boolean.FALSE;
    }

    /**
     * Resume all instances of a bpel process
     * 
     * @param processName
     *            The process name (QName)
     * @return The list of resumed instance ids
     * @throws Exception
     */
    public List<String> resumeAllInstance(String processName) throws Exception {
        initDataSource();
        String errorMsg = null;
        if (Utility.isEmpty(processName)) {
            errorMsg = I18n.loc("BPCOR-6120: bpel process name is not passed in");
        }
        QName processQName = null;
        try {
            processQName = QName.valueOf(processName);
        } catch (Exception e) {
            errorMsg = I18n.loc("BPCOR-6112: bpel process name: {0} is not a well formed QName", processName);
        }
        if (processQName != null) {
            List<String> suspendedIds = new ArrayList<String>();
            List<MonitorManager> mmanagers = getMonitorManagers(processQName, false, true, false);
            for (MonitorManager mmanager : mmanagers) {
                try {
                    mmanager.resumeInstance();
                    BPELTraceManager.getInstance().alertBPInstanceChangeByAPI(mEngine.getId(), processName,
                            mmanager.getInstance().getId(), null, ActionType.Resumed);
                    suspendedIds.add(mmanager.getInstance().getId());
                } catch (Exception e) {
                    // Ignore the exception, just log here
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6121: Error in resuming instance : {0}", mmanager), e);
                }
            }
            return suspendedIds;
        }
        if (errorMsg != null) {
            throw new Exception(errorMsg);
        }
        return null;
    }

    /**
     * Suspend BPEL instance
     * 
     * @param bpId
     *            The bpel instance to be suspended
     * @param mbeanConn
     *            Mbean Server connection
     * @throws Exception
     */
    public Boolean suspendInstance(String instanceId) throws Exception {
        initDataSource();
        // checkMonitorEnabled();
        String errorMsg = null;
        if (Utility.isEmpty(instanceId)) {
            errorMsg = I18n.loc("BPCOR-6117: bpel Instance id is not passed in");
        } else {
            MonitorManager monitorMgr = getMonitorManager(instanceId);
            if (monitorMgr == null) {
                // errorMsg = MESSAGES.getString(
                // "BPELSEManagement.Can_Not_Find_Instance",
                // instanceId);
                return Boolean.FALSE;
            } else {
                monitorMgr.suspendInstance();

                return Boolean.TRUE;
            }
        }
        if (errorMsg != null) {
            throw new Exception(errorMsg);
        }
        return Boolean.FALSE;
    }

    /**
     * Suspend all instances of a bpel process
     * 
     * @param processName
     *            The process name (QName)
     * @return The list of suspended instance ids
     * @throws Exception
     */
    public List<String> suspendAllInstance(String processName) throws Exception {
        initDataSource();
        String errorMsg = null;
        if (Utility.isEmpty(processName)) {
            errorMsg = I18n.loc("BPCOR-6120: bpel process name is not passed in");
        }
        QName processQName = null;
        try {
            processQName = QName.valueOf(processName);
        } catch (Exception e) {
            errorMsg = I18n.loc("BPCOR-6112: bpel process name: {0} is not a well formed QName", processName);
        }
        if (processQName != null) {
            List<String> suspendedIds = new ArrayList<String>();
            List<MonitorManager> mmanagers = getMonitorManagers(processQName,
                    true, false, false);
            for (MonitorManager mmanager : mmanagers) {
                try {
                    mmanager.suspendInstance();
                    BPELTraceManager.getInstance().alertBPInstanceChangeByAPI(mEngine.getId(), processName, 
                    		mmanager.getInstance().getId(), null, ActionType.Suspended);
                    suspendedIds.add(mmanager.getInstance().getId());
                } catch (Exception e) {
                    // Ignore the exception, just log here
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6123: Error in suspending instance : {0}", mmanager), e);
                }
            }
            return suspendedIds;
        }
        if (errorMsg != null) {
            throw new Exception(errorMsg);
        }
        return null;
    }

    /**
     * Terminate a BPEL instance
     * 
     * @param bpId
     *            The bpel instance to be terminated
     * @param mbeanConn
     *            Mbean Server connection
     * @throws Exception
     */
    public Boolean terminateInstance(String instanceId) throws Exception {
        initDataSource();
        // checkMonitorEnabled();
        String errorMsg = null;
        if (Utility.isEmpty(instanceId)) {
            errorMsg = I18n.loc("BPCOR-6117: bpel Instance id is not passed in");
        } else {
            MonitorManager monitorMgr = getMonitorManager(instanceId);
            if (monitorMgr == null) {
                // errorMsg = MESSAGES.getString(
                // "BPELSEManagement.Can_Not_Find_Instance",
                // instanceId);
                return Boolean.FALSE;
            } else {
                monitorMgr.terminateInstance();
                QName bpName = monitorMgr.getInstance().getBPELProcessManager().getBPELProcess().getBPELId();
                BPELTraceManager.getInstance().alertBPInstanceChangeByAPI(mEngine.getId(), bpName.toString(), 
                		instanceId, null, ActionType.Terminated);

                return Boolean.TRUE;
            }
        }
        if (errorMsg != null) {
            throw new Exception(errorMsg);
        }
        return Boolean.FALSE;
    }

    /**
     * Terminate all instances of a bpel process
     * 
     * @param processName
     *            The process name (QName)
     * @return The list of resumed instance ids
     * @throws Exception
     */
    public List<String> terminateAllInstance(String processName)
            throws Exception {
        initDataSource();
        String errorMsg = null;
        if (Utility.isEmpty(processName)) {
            errorMsg = I18n.loc("BPCOR-6120: bpel process name is not passed in");
        }
        QName processQName = null;
        try {
            processQName = QName.valueOf(processName);
        } catch (Exception e) {
            errorMsg = I18n.loc("BPCOR-6112: bpel process name: {0} is not a well formed QName", processName);
        }
        if (processQName != null) {
            List<String> suspendedIds = new ArrayList<String>();
            List<MonitorManager> mmanagers = getMonitorManagers(processQName, false, false, true);
            for (MonitorManager mmanager : mmanagers) {
                try {
                    mmanager.terminateInstance();
                    BPELTraceManager.getInstance().alertBPInstanceChangeByAPI(mEngine.getId(), processName,
                            mmanager.getInstance().getId(), null, ActionType.Terminated);
                    suspendedIds.add(mmanager.getInstance().getId());
                } catch (Exception e) {
                    // Ignore the exception, just log here
                    LOGGER.log(Level.FINE, I18n.loc("BPCOR-6122: Error in terminating instance : {0}", mmanager), e);
                }
            }
            return suspendedIds;
        }
        if (errorMsg != null) {
            throw new Exception(errorMsg);
        }
        return null;
    }

    /**
     * Get the list of BPEL processes files.
     * 
     * @param suName
     *            The name of the SU.
     * @return The list of BPELProcess File names, empty list will be returned
     * @throws Exception
     */
    public List<String> getBPELProcesses(String suName) throws Exception {
        initDataSource();
        List<String> bpelProcessList = new ArrayList<String>();
        List<String> bpelProcessFileList = new ArrayList<String>();
        Map<String, String> bpelsMap = new HashMap<String, String>();
        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);
        	InputStream bpelJarIS = getBPELsArchive(suName, conn);
        	if (bpelJarIS == null)
        		return bpelProcessList;
        	JarInputStream bpelsJarFile = new JarInputStream(bpelJarIS);
        	// walk its entries
        	JarEntry bpelWsdlEntry = bpelsJarFile.getNextJarEntry();
        	// get the bpels jar entry
        	while (bpelWsdlEntry != null) {
        		// extract all files from the bpel service unit
        		extractBpelOrWsdlFile(bpelsMap, bpelsJarFile, bpelWsdlEntry);
        		bpelWsdlEntry = bpelsJarFile.getNextJarEntry();
        	}
        	// use the map to get the bpelProcess
        	for (String bpelRelativepath : bpelsMap.keySet()) {
        		if (!bpelRelativepath.endsWith(".bpel")) {
        			// skip wsdl entries
        			continue;
        		}
        		String bpelfilepath = bpelsMap.get(bpelRelativepath);
        		bpelProcessFileList.add(bpelfilepath);
        	}
        	ParsingCaches caches = new ParsingCaches();
        	DeferredActionRegistry registry = new DeferredActionRegistry();
        	EntityResolver entityResolver = null;
        	for (String fileName : bpelProcessFileList) {
        		File bpelFile = new File(fileName);
        		String bpelFileURI = bpelFile.toURI().toString();
        		// try {
        			URL url = new URL("file", null, fileName); // NO I18N at
        			// present
        			InputStream is = url.openStream();
        			InputStreamReader reader = new InputStreamReader(is,
        			"UTF-8");

        			BPELParseContext parseContext = new ParseContextImpl();

                    parseContext.setCatalog(bpelFile.getParentFile(),bpelFile);
        			if (parseContext.hasCatalog()) {
        				entityResolver = parseContext.getBaseURIResolver();
        			}
        			IWSDLResolver wsdlResolver = DefaultWSDLResolverFactory
        			.getInstance().newWSDLResolver(bpelFileURI,
        					parseContext);
        			parseContext.setWSDLResolver(wsdlResolver);

        			// set the xsd resolver
        			IXSDResolver xsdLoader = DefaultXSDResolverFactory
        			.getInstance().newXSDResolver(bpelFileURI,
        					parseContext);
        			parseContext.setXSDResolver(xsdLoader);

        			parseContext.setCaches(caches);
        			parseContext.setDeferredActionRegistry(registry);
        			BPELDocument bpelDoc = BPELDocumentParseFactory
        			.getInstance().load(reader, parseContext);
        			bpelDoc.setBaseURI(fileName);

        			RBPELProcess bProc = (RBPELProcess) bpelDoc
        			.getDocumentProcess();
        			bpelProcessList.add(bProc.getBPELId().toString());
        	}
        	WSDL4JExt.applySingleSchemaTypeLoader(registry, entityResolver);
        } catch (Exception e) {                
        	LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6124: Error occurred in getting processes from SU"), e);
        	throw e;
        } finally {
        	closeConnection(abstractConn);
        }
            
        for (String fileName : bpelProcessFileList) {
            File tempFile = new File(fileName);
            tempFile.delete();
        }
        return bpelProcessList;
    }

    /**
     * Get the list of ActivityStatus of a BPEL instance
     * 
     * @param instanceId
     * @return List of ActivityStatus wrapped in a TabularData
     * @throws Exception
     */
    public/* List<ActivityStatus> */List<Map<String, Object>> getBPELInstanceActivityStatus(
            String instanceId) throws Exception {
        initDataSource();
        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        PreparedStatement query = null;
        ResultSet resultSet = null;
        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);

        	query = conn.prepareStatement(GET_ACTIVITY_STATUS_BY_INSTANCE);
        	query.setString(1, instanceId);
        	resultSet = query.executeQuery();
        	while (resultSet.next()) {
        		// BPInstanceInfo inst = new BPInstanceInfo ();
        		Map<String, Object> bpinfo = new HashMap<String, Object>();
        		bpinfo.put("activityId", Long.valueOf(resultSet.getLong(1)));
        		bpinfo.put("activityXpath", resultSet.getString(2));
        		bpinfo.put("iteration", resultSet.getInt(3));
        		bpinfo.put("status", resultSet.getString(4));
        		bpinfo.put("startTime", resultSet.getTimestamp(5));
        		bpinfo.put("endTime", resultSet.getTimestamp(6));
        		Timestamp lastTime = ((Timestamp) bpinfo.get("endTime") == null ? new Timestamp(
        				Calendar.getInstance().getTimeInMillis())
        		: (Timestamp) bpinfo.get("endTime"));
        		float lasted = (float) ((lastTime.getTime() - ((Timestamp) bpinfo
        				.get("startTime")).getTime()) / 1000.0);
        		bpinfo.put("lasted", new Float(lasted));
        		result.add(bpinfo);
        	}
        } finally {
        	closeResultSet(resultSet);
        	closeStatement(query);
        	closeConnection(abstractConn);
        }
            
        return result;
    }
    
    /**
     * @see BPELSEManagementMBean#getBPELInstances(String, String, String,
     *      Integer, String, String)
     */
    public List<Map<String, Object>> getBPELInstances(
            String bpelQName, String status, String instanceId,
            Integer maxRecords, String sortColumn, String order)
            throws Exception {
    	return queryBPELInstances(bpelQName, status, null, instanceId, maxRecords, sortColumn, order);
    }
    
    /**
     * @see BPELSEManagementMBean#searchBPELInstances(String, String, String,
     *      Integer, String, String)
     */
    public List<Map<String, Object>> searchBPELInstances(
            String bpelQName, String status, String searchString,
            Integer maxRecords, String sortColumn, String order)
            throws Exception {
    	// DEVNOTE: The bpelQName and the searchString should not be null since a search on 
    	// bpel variables requires that both be not null
    	if ((bpelQName == null) || (searchString == null)) {
    		throw new Exception(I18n.loc("BPCOR-6104: The bpelQName parameter as well as the searchString " +
    				"parameter are required to search for bpel instances by variable value."));
    	}
    	return queryBPELInstances(bpelQName, status, searchString, null, maxRecords, sortColumn, order);
    }

    /**
     * @see BPELSEManagementMBean#getBPELInstances(String, String, String,
     *      Integer, String, String)
     */
    private List<Map<String, Object>> queryBPELInstances(
            String bpelQName, String status, String searchString, String instanceId,
            Integer maxRecords, String sortColumn, String order)
            throws Exception {
    	
    	// Pre-execution checks.
    	// If the searchString is specified then the bpelQName must be specified also
    	if ((searchString != null) && (bpelQName == null)) {
    		throw new Exception(I18n.loc("BPCOR-6104: The bpelQName parameter as well as the searchString " +
			"parameter are required to search for bpel instances by variable value."));
    	}
    	
    	if (maxRecords != null && maxRecords.intValue() > MAX_INSTANCE) {
    		throw new Exception (I18n.loc("BPCOR-6189: maxRecords can not be larger than 1000"));
    	}
        if (sortColumn != null && 
        		!(sortColumn.equalsIgnoreCase(SchemaConstants.START_TIME) 
        				|| sortColumn.equalsIgnoreCase(SchemaConstants.END_TIME) 
        				|| sortColumn.equalsIgnoreCase(SchemaConstants.UPDATED_TIME))) 
        {
            throw new Exception(I18n.loc("BPCOR-6126: sortColumn must be one of \"startTime\","
                            + "\"endTime\" or \"updatedTime\""));
        }
        
        if (order != null && !(order.equalsIgnoreCase(ASC) || order.equalsIgnoreCase(DESC))) {
            throw new Exception(I18n.loc("BPCOR-6127: order must be either \"ASC\" or \"DESC\""));
        } else if (order != null) {
            order = order.toUpperCase();
        }
    	
    	// DEVNOTE: If the instanceid is null, the searchstring will be discarded. However since this is a 
    	// private method and we control the parameters to this method. This check is not required.
        String searchSQLPart = "";
    	if (searchString != null) {
    		try {
    			SearchStringParser parser = new SearchStringParser(searchString);
    			SearchUnit part =  parser.parseSearchString();
    			searchSQLPart = SEARCH_SQL_PREFIX + part.generateSQL() + " ";
    		} catch (Exception e) {
    			throw new Exception(I18n.loc("BPCOR-6114: Error parsing the seach string and generating the SQL " +
    					"required. Error Message:{0}", e.getLocalizedMessage()), e);
    		}
    	}

        initDataSource();
        ResultSet resultSet = null;
        PreparedStatement query = null;
        PreparedStatement queryCount = null;
        ResultSet countResult = null;

        boolean doQuery = false;
        int total = 0;

        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();

        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);
        	int max = MAX_INSTANCE;
        	if (maxRecords != null) {
        		max = maxRecords.intValue();
        	}

        	if (instanceId != null) {
        		queryCount = conn.prepareStatement(GET_BPEL_INSTANCES_NO_BY_INSTANCE_ID_QUERY);
        		queryCount.setString(1, instanceId);
        		countResult = queryCount.executeQuery();
        		if (countResult.next()) {
        			total = countResult.getInt(1);
        		}
        		if (maxRecords != null || total <= MAX_INSTANCE) {
        			doQuery = true;
        			query = conn.prepareStatement(GET_BPEL_INSTANCES_BY_INSTANCE_ID_QUERY);
        			query.setString(1, instanceId);
        		}
        	} else if (bpelQName != null && status != null) {
        		queryCount = conn.prepareStatement(GET_BPEL_INSTANCES_NO_BY_PROCESS_STATUS_QUERY + searchSQLPart);
        		queryCount.setString(1, bpelQName);
        		queryCount.setString(2, status);
        		countResult = queryCount.executeQuery();
        		if (countResult.next()) {
        			total = countResult.getInt(1);
        		}
        		if (maxRecords != null || total <= MAX_INSTANCE) {
        			doQuery = true;
        			query = conn.prepareStatement(getQuery(GET_BPEL_INSTANCES_BY_PROCESS_STATUS_QUERY  
        					+ searchSQLPart, sortColumn, order, maxRecords, conn));
        			query.setString(1, bpelQName);
        			query.setString(2, status);
        		}
        	} else if (bpelQName == null && status != null) {
        		queryCount = conn.prepareStatement(GET_BPEL_INSTANCES_NO_BY_STATUS_QUERY);
        		queryCount.setString(1, status);
        		countResult = queryCount.executeQuery();
        		if (countResult.next()) {
        			total = countResult.getInt(1);
        		}
        		if (maxRecords != null || total <= MAX_INSTANCE) {
        			doQuery = true;
        			query = conn.prepareStatement(getQuery
        					(GET_BPEL_INSTANCES_BY_STATUS_QUERY, sortColumn, order, maxRecords, conn));
        			query.setString(1, status);
        		}
        	} else if (bpelQName != null && status == null) {
        		queryCount = conn.prepareStatement(GET_BPEL_INSTANCES_NO_BY_PROCESS_QUERY + searchSQLPart);
        		queryCount.setString(1, bpelQName.toString());
        		countResult = queryCount.executeQuery();
        		if (countResult.next()) {
        			total = countResult.getInt(1);
        		}
        		if (maxRecords != null || total <= MAX_INSTANCE) {
        			doQuery = true;
        			query = conn.prepareStatement(getQuery
        					(GET_BPEL_INSTANCES_BY_PROCESS_QUERY + searchSQLPart, sortColumn, order, maxRecords, conn));
        			query.setString(1, bpelQName.toString());
        		}
        	} else {
        		queryCount = conn.prepareStatement(GET_BPEL_INSTANCES_NO_ALL_QUERY);
        		countResult = queryCount.executeQuery();
        		if (countResult.next()) {
        			total = countResult.getInt(1);
        		}
        		if (maxRecords != null || total <= MAX_INSTANCE) {
        			doQuery = true;
        			query = conn.prepareStatement(
        					getQuery(GET_BPEL_INSTANCES_ALL_QUERY, sortColumn, order, maxRecords, conn));
        		}
        	}

        	if (!doQuery) {
        		// header
        		Map<String, Object> bpinfo = new HashMap<String, Object>();
        		bpinfo.put("total", new Integer(total));
        		bpinfo.put("overflow", new Boolean(true));
        		bpinfo.put("returned", new Integer(0));
        		result.add(bpinfo);
        	} else {
        		resultSet = query.executeQuery();
        		int i = 0;
        		Map<String, Object> bpinfo = new HashMap<String, Object>();
        		while (i < max && resultSet.next()) {
        			// BPInstanceInfo inst = new BPInstanceInfo ();
        			bpinfo = new HashMap<String, Object>();
        			bpinfo.put("id", resultSet.getString(1));
        			bpinfo.put("bpelId", resultSet.getString(2));
        			bpinfo.put("status", resultSet.getString(3));
        			bpinfo.put("startTime", resultSet.getTimestamp(4));
        			bpinfo.put("endTime", resultSet.getTimestamp(5));
        			bpinfo.put("lastUpdateTime", resultSet.getTimestamp(6));
        			Timestamp lastTime = (((Timestamp) bpinfo.get("endTime") == null) ? 
        					new Timestamp(Calendar.getInstance().getTimeInMillis()) : 
        						(Timestamp) bpinfo.get("endTime"));
        			float lasted = (float) ((lastTime.getTime() - 
        					((Timestamp) bpinfo.get("startTime")).getTime()) / 1000.0);
        			bpinfo.put("lasted", new Float(lasted));
        			result.add(bpinfo);
        			i++;
        		}
        		bpinfo = new HashMap<String, Object>();
        		if (total < max) {
        			total = i;
        		}
        		bpinfo.put("total", new Integer(total));
        		bpinfo.put("overflow", new Boolean(false));
        		bpinfo.put("returned", new Integer(i));
        		result.add(0, bpinfo);
        	}
        } finally {
        	closeResultSet(resultSet);
        	closeResultSet(countResult);
        	closeStatement(query);
        	closeStatement(queryCount);
        	closeConnection(abstractConn);
        }

        return result;
    }

    public List<Map<String, Object>> listBPELVaraibles(String instanceId,
            String varName) throws Exception {
        initDataSource();
        checkVariableMonitorEnabled();
        if (varName == null) {
            return listBPELVaraibles(instanceId);
        }
        PreparedStatement scopeStmt = null;
        PreparedStatement scopeSimpleStmt = null;
        PreparedStatement processStmt = null;
        PreparedStatement processSimpleStmt = null;
        ResultSet rs = null;

        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);

        	// Simple variables NOT in default scope
        	scopeSimpleStmt = conn.prepareStatement(GET_INNER_SINGLE_SIMPLE_VAR_QUERY);
        	scopeSimpleStmt.setString(1, instanceId);
        	scopeSimpleStmt.setString(2, varName);
        	rs = scopeSimpleStmt.executeQuery();
        	while (rs.next()) {
        		Map<String, Object> varinfo = new HashMap<String, Object>();
        		varinfo.put("varName", rs.getString(1));
        		varinfo.put("varId", Long.valueOf(rs.getLong(2)));
        		varinfo.put("xpath", rs.getString(3));
        		String notes = SIMPLE_TYPE_NOTE_PREFIX + getVariableTypeFromCode(rs.getString(4));
        		varinfo.put("notes", notes);
        		result.add(varinfo);
        	}
        	// Complex variables NOT in default scope
        	scopeStmt = conn.prepareStatement(GET_INNER_SINGLE_VAR_QUERY);
        	scopeStmt.setString(1, instanceId);
        	scopeStmt.setString(2, varName);
        	rs = scopeStmt.executeQuery();
        	while (rs.next()) {
        		Map<String, Object> varinfo = new HashMap<String, Object>();
        		varinfo.put("varName", rs.getString(1));
        		varinfo.put("varId", Long.valueOf(rs.getLong(2)));
        		varinfo.put("xpath", rs.getString(3));
        		varinfo.put("notes", "");
        		result.add(varinfo);
        	}
        	// Simple variables in default scope
        	processSimpleStmt = conn.prepareStatement(GET_PROCESS_SINGLE_SIMPLE_VAR_QUERY);
        	processSimpleStmt.setString(1, instanceId);
        	processSimpleStmt.setString(2, varName);
        	rs = processSimpleStmt.executeQuery();
        	while (rs.next()) {
        		Map<String, Object> varinfo = new HashMap<String, Object>();
        		varinfo.put("varName", rs.getString(1));
        		varinfo.put("varId", Long.valueOf(rs.getLong(2)));
        		varinfo.put("xpath", BP_PROCESS);
        		String notes = SIMPLE_TYPE_NOTE_PREFIX + getVariableTypeFromCode(rs.getString(3));
        		varinfo.put("notes", notes);
        		result.add(varinfo);
        	}
        	// Complex variables in default scope
        	processStmt = conn.prepareStatement(GET_PROCESS_SINGLE_VAR_QUERY);
        	processStmt.setString(1, instanceId);
        	processStmt.setString(2, varName);
        	rs = processStmt.executeQuery();
        	while (rs.next()) {
        		Map<String, Object> varinfo = new HashMap<String, Object>();
        		varinfo.put("varName", rs.getString(1));
        		varinfo.put("varId", Long.valueOf(rs.getLong(2)));
        		varinfo.put("xpath", BP_PROCESS);
        		varinfo.put("notes", "");
        		result.add(varinfo);
        	}

        } finally {
        	closeResultSet(rs);
        	closeStatement(scopeSimpleStmt);
        	closeStatement(processSimpleStmt);
        	closeStatement(scopeStmt);
        	closeStatement(processStmt);
        	closeConnection(abstractConn);
        }
            
        return result;
    }

    public List<Map<String, Object>> listBPELVaraibles(String instanceId)
            throws Exception {
        initDataSource();
        checkVariableMonitorEnabled();
        PreparedStatement scopeStmt = null;
        PreparedStatement scopeSimpleStmt = null;
        PreparedStatement processStmt = null;
        PreparedStatement processSimpleStmt = null;
        ResultSet rs = null;

        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);

        	// Simple variables NOT in default scope.
        	scopeSimpleStmt = conn.prepareStatement(GET_INNER_SIMPLE_VAR_QUERY);
        	scopeSimpleStmt.setString(1, instanceId);
        	rs = scopeSimpleStmt.executeQuery();
        	while (rs.next()) {
        		Map<String, Object> varinfo = new HashMap<String, Object>();
        		varinfo.put("varName", rs.getString(1));
        		varinfo.put("varId", Long.valueOf(rs.getLong(2)));
        		varinfo.put("xpath", rs.getString(3));
        		String notes = SIMPLE_TYPE_NOTE_PREFIX + getVariableTypeFromCode(rs.getString(4));
        		varinfo.put("notes", notes);
        		result.add(varinfo);
        	}
        	// Complex variables NOT in default scope
        	scopeStmt = conn.prepareStatement(GET_INNER_VAR_QUERY);
        	scopeStmt.setString(1, instanceId);
        	rs = scopeStmt.executeQuery();
        	while (rs.next()) {
        		Map<String, Object> varinfo = new HashMap<String, Object>();
        		varinfo.put("varName", rs.getString(1));
        		varinfo.put("varId", Long.valueOf(rs.getLong(2)));
        		varinfo.put("xpath", rs.getString(3));
        		varinfo.put("notes", "");
        		result.add(varinfo);
        	}
        	// Simple variables in default scope
        	processSimpleStmt = conn.prepareStatement(GET_PROCESS_SIMPLE_VAR_QUERY);
        	processSimpleStmt.setString(1, instanceId);
        	rs = processSimpleStmt.executeQuery();
        	while (rs.next()) {
        		Map<String, Object> varinfo = new HashMap<String, Object>();
        		varinfo.put("varName", rs.getString(1));
        		varinfo.put("varId", Long.valueOf(rs.getLong(2)));
        		varinfo.put("xpath", BP_PROCESS);
        		String notes = SIMPLE_TYPE_NOTE_PREFIX + getVariableTypeFromCode(rs.getString(3));
        		varinfo.put("notes", notes);
        		result.add(varinfo);
        	}
        	// Complex variables in default scope
        	processStmt = conn.prepareStatement(GET_PROCESS_VAR_QUERY);
        	processStmt.setString(1, instanceId);
        	rs = processStmt.executeQuery();
        	while (rs.next()) {
        		Map<String, Object> varinfo = new HashMap<String, Object>();
        		varinfo.put("varName", rs.getString(1));
        		varinfo.put("varId", Long.valueOf(rs.getLong(2)));
        		varinfo.put("xpath", BP_PROCESS);
        		varinfo.put("notes", "");
        		result.add(varinfo);
        	}

        } finally {
        	closeResultSet(rs);
        	closeStatement(scopeSimpleStmt);
        	closeStatement(processSimpleStmt);
        	closeStatement(scopeStmt);
        	closeStatement(processStmt);
        	closeConnection(abstractConn);
        }

        return result;
    }

    /**
     * Get the fault detail of a faulted bpel instance;
     * 
     * @param bpid
     *            the faulted bpel instance id
     * 
     */
    public String getBPELInstanceFault(String bpid) throws Exception {
        initDataSource();
        PreparedStatement query = null;
        ResultSet resultSet = null;
        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);

        	query = conn.prepareStatement(GET_PROCESS_FAULT_QUERY);
        	query.setString(1, bpid);
        	resultSet = query.executeQuery();
        	if (resultSet.next()) {
        		String tmpRes = null;
                if( mConnFactory.getType() == ConnectionProperties.POSTGRES_DB.intValue() ) {
                    tmpRes = resultSet.getString(1);
                } else {
                    Clob obtained = resultSet.getClob(1);
                    if (obtained != null) {
                        SerialClob clob = new SerialClob(obtained);
                        tmpRes = getValue(clob);
                    }
                }
                return tmpRes;
        	}

        } finally {
        	closeResultSet(resultSet);
        	closeStatement(query);
        	closeConnection(abstractConn);
        }

        return null;
    }

    /**
     * Get the list of invoker bpel instance ids for a specific bpel instance
     * 
     * @param bpid
     *            The invoked bpel instance id
     * @param receiveActivityId
     *            Optional receive activity id of the invoked bpel instance, if
     *            present, returns only one invoker bpel instance that sends msg
     *            to the specific receive
     * 
     * @return The list of invoker instances
     * @throws Exception
     */
    public/* List<BPInstanceInfo> */List<Map<String, Object>> getInvokerInstance(
            String bpid, Long receiveActivityId) throws Exception {
        initDataSource();
        checkVariableMonitorEnabled();
        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        List<String /* invokerid */> invokerIds = new ArrayList<String>();
        PreparedStatement invokeridQuery = null;
        PreparedStatement invokerQuery = null;
        ResultSet invokerIdResultSet = null;
        ResultSet invokerResultSet = null;
        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);
        	if (receiveActivityId != null) {
        		invokeridQuery = conn
        		.prepareStatement(GET_INVOKER_RECEIVEID);
        		invokeridQuery.setLong(1, receiveActivityId);
        		invokeridQuery.setString(2, bpid);
        	} else {
        		invokeridQuery = conn
        		.prepareStatement(GET_INVOKER_RECEIVEIDS);
        		invokeridQuery.setString(1, bpid);
        	}
        	invokerIdResultSet = invokeridQuery.executeQuery();
        	while (invokerIdResultSet.next()) {
        		invokerIds.add(invokerIdResultSet.getString(1));
        	}
        	if (invokerIds.size() > 0) {
        		invokerQuery = conn.prepareStatement(GET_INVOKER);
        		for (String invokerId : invokerIds) {
        			invokerQuery.setString(1, invokerId);
        			invokerResultSet = invokerQuery.executeQuery();
        			populateBPELInfoListMap(invokerResultSet, result);
        			// if (invokerResultSet.next()) {
        				// Map<String, Object> bpinfo = new HashMap<String,
        				// Object> ();
        				// bpinfo.put("id", invokerResultSet.getString(1));
        				// bpinfo.put("bpelId", invokerResultSet.getString(2));
        			// bpinfo.put("status", invokerResultSet.getString(3));
        			// bpinfo.put("startTime",
        			// invokerResultSet.getTimestamp(4));
        			// bpinfo.put("endTime",
        			// invokerResultSet.getTimestamp(5));
        			// bpinfo.put("lastUpdateTime",
        			// invokerResultSet.getTimestamp(6));
        			// Timestamp lastTime = ((Timestamp)
        			// bpinfo.get("endTime") == null ? new Timestamp
        			// (Calendar.getInstance().getTimeInMillis()) :
        			// (Timestamp) bpinfo.get("endTime"));
        			// float lasted = (float)((lastTime.getTime() -
        			// ((Timestamp)
        			// bpinfo.get("startTime")).getTime())/1000.0);
        			// bpinfo.put("lasted", new Float(lasted));
        			// result.add(bpinfo);
        			// }
        		}
        	}
        } finally {
        	closeResultSet(invokerResultSet);
        	closeResultSet(invokerIdResultSet);
        	closeStatement(invokerQuery);
        	closeStatement(invokeridQuery);
        	closeConnection(abstractConn);
        }

        return result;
    }

    /**
     * Get the list of invokee bpel instance ids for a specific bpel instance
     * 
     * @param bpid
     *            The invoked bpel instance id
     * @param invokeActivityId
     *            Optional invoke activity id of the invoker bpel instance, if
     *            present, returns the only one invokee bpel instance that
     *            receives msg from the specific invoke
     * @return The list of invokee instances
     * @throws Exception
     */
    public/* List<BPInstanceInfo> */List<Map<String, Object>> getInvokeeInstance(
            String bpid, Long invokeActivityId) throws Exception {
        initDataSource();
        checkVariableMonitorEnabled();
        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        List<String /* invokerid */> receiveIds = new ArrayList<String>();
        PreparedStatement invokeeidQuery = null;
        PreparedStatement invokeeQuery = null;
        ResultSet invokeeIdResultSet = null;
        ResultSet invokeeResultSet = null;
        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);

        	if (invokeActivityId != null) {
        		invokeeidQuery = conn
        		.prepareStatement(GET_INVOKEE_INVOKEEID);
        		invokeeidQuery.setLong(1, invokeActivityId);
        		invokeeidQuery.setString(2, bpid);
        	} else {
        		invokeeidQuery = conn
        		.prepareStatement(GET_INVOKEE_INVOKEEIDS);
        		invokeeidQuery.setString(1, bpid);
        	}
        	invokeeIdResultSet = invokeeidQuery.executeQuery();
        	while (invokeeIdResultSet.next()) {
        		receiveIds.add(invokeeIdResultSet.getString(1));
        	}
        	if (receiveIds.size() > 0) {
        		invokeeQuery = conn.prepareStatement(GET_INVOKEE);
        		for (String invokerId : receiveIds) {
        			invokeeQuery.setString(1, invokerId);
        			invokeeResultSet = invokeeQuery.executeQuery();
        			populateBPELInfoListMap(invokeeResultSet, result);
        			// if (invokeeResultSet.next()) {
        				// Map<String, Object> bpinfo = new HashMap<String,
        				// Object> ();
        				// bpinfo.put("id", invokeeResultSet.getString(1));
        				// bpinfo.put("bpelId", invokeeResultSet.getString(2));
        			// bpinfo.put("status", invokeeResultSet.getString(3));
        			// bpinfo.put("startTime",
        			// invokeeResultSet.getTimestamp(4));
        			// bpinfo.put("endTime",
        			// invokeeResultSet.getTimestamp(5));
        			// bpinfo.put("lastUpdateTime",
        			// invokeeResultSet.getTimestamp(6));
        			// Timestamp lastTime = ((Timestamp)
        			// bpinfo.get("endTime") == null ? new Timestamp
        			// (Calendar.getInstance().getTimeInMillis()) :
        			// (Timestamp) bpinfo.get("endTime"));
        			// float lasted = (float)((lastTime.getTime() -
        			// ((Timestamp)
        			// bpinfo.get("startTime")).getTime())/1000.0);
        			// bpinfo.put("lasted", new Float(lasted));
        			// result.add(bpinfo);
        			// }
        		}
        	}
        } finally {
        	closeResultSet(invokeeResultSet);
        	closeResultSet(invokeeIdResultSet);
        	closeStatement(invokeeQuery);
        	closeStatement(invokeeidQuery);
        	closeConnection(abstractConn);
        }

        return result;
    }

    /**
     * Get the BPEL variable value, the content of BPEL variable will be
     * returned
     * 
     * @param instanceId
     *            The instance id of the instance
     * @param varId
     *            The variable Id, can not be null
     * @return The content of the BPEL variable
     * @throws Exception
     */
    public String getVariableValue(String instanceId, Long varId)
            throws Exception {
 
        initDataSource();
        checkVariableMonitorEnabled();
        PreparedStatement query = null;
        PreparedStatement querySimple = null;
        ResultSet resultSet = null;
        Connection conn = null;
        AbstractDBConnection abstractConn = null;

        try {
        	abstractConn = mConnFactory.createNonXAConnection();
        	conn = abstractConn.getUnderlyingConnection();
        	conn.setAutoCommit(false);

        	querySimple = conn.prepareStatement(GET_SIMPLE_VARIABLE_QUERY);
        	querySimple.setString(1, instanceId);
        	querySimple.setLong(2, varId);
        	resultSet = querySimple.executeQuery();
        	if (resultSet.next()) {
        		String obtained = resultSet.getString(1);
        		return obtained;
        	}
        	// The variable was not found in the simple variable table. Do the query against the complex
        	// variable table.
        	query = conn.prepareStatement(GET_BPEL_VARIABLE_QUERY);
        	query.setString(1, instanceId);
        	query.setLong(2, varId);
        	resultSet = query.executeQuery();
        	if (resultSet.next()) {
                String tmpRes = null;
                if( mConnFactory.getType() == ConnectionProperties.POSTGRES_DB.intValue() ) {
                    tmpRes = resultSet.getString(1);
                } else {
                    Clob obtained = resultSet.getClob(1);
                    if (obtained != null) {
                        SerialClob clob = new SerialClob(obtained);
                        tmpRes = getValue(clob);
                    }
                }
                return tmpRes;
        	}

        } finally {
        	closeResultSet(resultSet);
        	closeStatement(querySimple);
        	if (query != null) {
        		closeStatement(query);
        	}
        	closeConnection(abstractConn);
        }

        return null;
    }

    /**
     * Is Monitoring enabled
     * 
     * @return true if monitoring is enabled
     * @throws ManagementRemoteException
     */
    public Boolean isMonitoringEnabled() {
        return Boolean.valueOf(mEngine.isMonitorEnabled());
    }

    
    /**
     * Is Monitoring Variable Enabled. 
     * @return true if monitoring is enabled
     */
    public Boolean  isMonitoringVariableEnabled () {
        return Boolean.valueOf(mEngine.isVariableMonitorEnabled());
    }
    /**
     * Is persistence enabled
     * 
     * @return true if persistence is enabled
     * @throws ManagementRemoteException
     */
    public Boolean isPersistenceEnabled() {
        return Boolean.valueOf(mEngine.isPersistenceEnabled());
    }

	public Timestamp getLastInstanceProcessedTime(String bpelQName) {
		BPELProcessManager procManager = getBPELProcessManager(bpelQName);
		if (procManager != null) {
			return procManager.getPerformanceManger().getLastProcessedInstanceTimestamp();
		} else {
			return null;
		}
	}
	
    public int getLiveInstancesCount(String bpelQName) {
		BPELProcessManager procManager = getBPELProcessManager(bpelQName);
		if (procManager != null) {
			return procManager.getPerformanceManger().getRunningInstancesCount();
		} else {
			return -1; 
		}
    }
    
    public int getCompletedInstancesCount(String bpelQName) {
		BPELProcessManager procManager = getBPELProcessManager(bpelQName);
		if (procManager != null) {
			return procManager.getPerformanceManger().getCompletedInstancesCount();
		} else {
			return -1;
		}
    }
    
    public int getSuspendedInstancesCount(String bpelQName) {
		BPELProcessManager procManager = getBPELProcessManager(bpelQName);
		if (procManager != null) {
			return procManager.getPerformanceManger().getSuspendedInstancesCount();
		} else {
			return -1;
		}
    }
    
    public int getFaultedInstancesCount(String bpelQName) {
		BPELProcessManager procManager = getBPELProcessManager(bpelQName);
		if (procManager != null) {
			return procManager.getPerformanceManger().getFaultedInstancesCount();
		} else {
			return -1;
		}
    }
    
    public int getTerminatedInstancesCount(String bpelQName) {
		BPELProcessManager procManager = getBPELProcessManager(bpelQName);
		if (procManager != null) {
			return procManager.getPerformanceManger().getTerminatedInstancesCount();
		} else {
			return -1;
		}
    }
    
	public boolean setInstanceProcessingRatePeriod(String bpelQName, Long period) {
		BPELProcessManager bpelProcessManager = null;
		boolean result = false;
		if (bpelQName != null) {
			bpelProcessManager = getBPELProcessManager(bpelQName);
			if (bpelProcessManager != null) {
				result = bpelProcessManager.getPerformanceManger().setInstanceProcessingRatePeriod(period);
			}
		} else {
			RBPELProcess model = null;
			Collection bpelProcesses = mEngine.getBPELProcesses();
			for (Iterator iter = bpelProcesses.iterator(); iter.hasNext();) {
				model = (RBPELProcess) iter.next();
				bpelProcessManager = getBPELProcessManager(model.getBPELId().toString());
				result = bpelProcessManager.getPerformanceManger().setInstanceProcessingRatePeriod(period);
			}
		}
		// the returned result is not the perfect mechanism (since it is boolean flag to indicate 
		// the outcome of series of operation, but for now it should be ok..
		return result;
	}

	public double getInstanceProcessingRate(String bpelQName) {
		BPELProcessManager procManager = getBPELProcessManager(bpelQName);
		if (procManager != null) {
			return procManager.getPerformanceManger().getInstanceProcessingRate();
		} else {
			return -1;
		}
	}
    
    public String getBusinessProcessDefinition(String bpelQNameAsString){
		BPELProcessManager procManager = getBPELProcessManager(bpelQNameAsString);
		if (procManager != null) {
			return procManager.getBPELProcess().getSerializedBPELDocument();
		} else {
			return null;
		}
    }
    
    public String[] getMemInstanceInfo(String bpelQName, String instanceId) {
        return getBPELProcessManager(bpelQName).getPerformanceManger().getMemInstanceInfo(instanceId);
    }

    public Map<String, String[]> getMemInstances(String bpelQName, String status) {
        return getBPELProcessManager(bpelQName).getPerformanceManger().getMemInstantances(status);
    }
    
    private BPELProcessManager getBPELProcessManager(String bpelQNameAsString) {
        QName processQName = QName.valueOf(bpelQNameAsString);
        return mEngine.getBPELProcessManager(processQName);
    }
    
    /*
     * private MonitorManager getMonitorManager(String instanceId) { Collection<RBPELProcess>
     * bpelModels = mEngine.getBPELProcesses(); for (RBPELProcess model :
     * bpelModels) { BPELProcessManager manager =
     * mEngine.getBPELProcessManager(model .getBPELId()); if (manager != null) {
     * Collection<BPELProcessInstance> procInstColl = manager
     * .getProcessInstances(); for (BPELProcessInstance processInstance :
     * procInstColl) { if (processInstance.getId().equals(instanceId)) { return
     * processInstance.getMonitorMgr(); } } } } return null; }
     */

    private MonitorManager getMonitorManager(String instanceId) {
        Collection<RBPELProcess> bpelModels = mEngine.getBPELProcesses();
        MonitorManager mm = null;
        for (RBPELProcess model : bpelModels) {
            BPELProcessManager manager = mEngine.getBPELProcessManager(model
                    .getBPELId());
            if (manager != null) {
                mm = manager.getMonitorManager(instanceId);
                if (mm != null) {
                    return mm;
                }
            }
        }
        for (RBPELProcess model : bpelModels) {
            BPELProcessManager manager = mEngine.getBPELProcessManager(model
                    .getBPELId());
            try {
                BPELProcessManager bpMgr = manager.recreateInstance(instanceId);
                if (bpMgr != null) {
                    return bpMgr.getMonitorManager(instanceId);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

        }
        return null;
    }

    /*
     * private List<MonitorManager> getMonitorManagers (QName processId,
     * boolean toSuspend, boolean toResume, boolean toTerminate) {
     * BPELProcessManager manager = mEngine.getBPELProcessManager (processId);
     * List<MonitorManager> mmanager = new ArrayList<MonitorManager> (); if
     * (manager != null) { Collection<BPELProcessInstance> procInstColl =
     * manager.getProcessInstances(); for (BPELProcessInstance processInstance :
     * procInstColl) { if (toSuspend) { if (! processInstance.isSuspended() || !
     * processInstance.isTerminated()) {
     * mmanager.add(processInstance.getMonitorMgr()); } }else if (toResume) { if
     * (processInstance.isSuspended() || ! processInstance.isTerminated()) {
     * mmanager.add(processInstance.getMonitorMgr()); } }else if (toTerminate) {
     * if (! processInstance.isTerminated()) {
     * mmanager.add(processInstance.getMonitorMgr()); } } } } return mmanager; }
     */

    private List<MonitorManager> getMonitorManagers(QName processId,
            boolean toSuspend, boolean toResume, boolean toTerminate) {
        BPELProcessManager manager = mEngine.getBPELProcessManager(processId);
        List<MonitorManager> mmanager = new ArrayList<MonitorManager>();
        if (manager != null) {
            Collection<BPELProcessInstance> procInstColl = manager
                    .getProcessInstances(processId.toString());
            for (BPELProcessInstance processInstance : procInstColl) {
                if (toSuspend) {
                    if (!processInstance.isSuspended()
                            && !processInstance.isExiting()) {
                        mmanager.add(processInstance.getMonitorMgr());
                    }
                } else if (toResume) {
                    if (processInstance.isSuspended()
                            && !processInstance.isExiting()) {
                        mmanager.add(processInstance.getMonitorMgr());
                    }
                } else if (toTerminate) {
                    if (!processInstance.isExiting()) {
                        mmanager.add(processInstance.getMonitorMgr());
                    }
                }
            }
        }
        return mmanager;
    }

    private void changeVariableMessageTypeValue(
            RuntimeVariable runtimeVariable, String partName, String xpath,
            String value, VariableScope context, QName bpName,
            String instanceId, String oldVal) throws XpathExpressionException,
            SchemaViolationException {
        if (value == null) {
            return;
        }
        RVariable variableModel = runtimeVariable.getVariableDef();
        if (variableModel == null) {
            return;
        }

        Message vMeg = variableModel.getWSDLMessageType();
        if (vMeg != null) {
            // WSDL Message
//            Part partModel = vMeg.getPart(partName);
            WSMessage variableData = runtimeVariable.getWSMessage();
            if (variableData != null) {
                Element part = variableData.getPart(partName);
                if (part == null) {
                    part = variableData.createPart(partName);
                }
                Map<String, String> prefixMap = new HashMap<String, String>();
                getPrefixMap(part, prefixMap);
                setValue(part, value, xpath, prefixMap);
                persistState(bpName, instanceId, runtimeVariable, oldVal);
            }
        } else {
            throw new SchemaViolationException(variableModel.getName()
                    + " is not a WSDLMessage variable");
        }
    }

    private void persistState(QName bpName, String instanceId, RuntimeVariable runtimeVariable, String oldVal) {
        try {
//            BPELProcessManager manager = mEngine.getBPELProcessManager(bpName);
            if (mEngine.isPersistenceEnabled()) {
                State varChangeState = StateFactory.getStateFactory()
                        .createVariableChangeState(mEngine.getId(), bpName,
                                instanceId, runtimeVariable);
                mEngine.getStateManager().persistState(varChangeState,
                        TransactionInfo.getLocalTxInfo(), null);
            }
            // Change the variable in monitor table
            Variable varold = new VariableImpl(
                    runtimeVariable.getVariableDef(), oldVal, null);
            Variable varnew = new VariableImpl(
                    runtimeVariable.getVariableDef(), runtimeVariable
                            .getSerializedValue(), null);

            Map<VariableEvent.VariableType, List<Variable>> varMap = 
            	new HashMap<VariableEvent.VariableType, List<Variable>>();
            List<Variable> oldList = new ArrayList<Variable>();
            oldList.add(varold);
            List<Variable> newList = new ArrayList<Variable>();
            newList.add(varnew);
            // varMap.put(VariableEvent.VariableType.OLD, oldList);
            varMap.put(VariableEvent.VariableType.NEW, newList);
            VariableEvent event = BPELEventFactory.createVariableEvent(mEngine
                    .getId(), bpName, instanceId,
                    BPELEvent.EventType.VARIABLE_CHANGED,
                    MonitorManager.USER_INITIATED_CHANGE_ACTIVITY_ID, varMap,
                    null);
            mEngine.postEvent(event);
        } catch (Exception e) {
            // Just log
        }

    }

    private void changeVariableMessageTypeValue(
            RuntimeVariable runtimeVariable, String partName, String value,
            VariableScope context, QName bpName, String instanceId,
            String oldVal) throws SchemaViolationException {
        if (value == null) {
            return;
        }
        RVariable variableModel = runtimeVariable.getVariableDef();
        if (variableModel == null) {
            return;
        }
        Message vMeg = variableModel.getWSDLMessageType();
        if (vMeg != null) {
            // WSDL Message
//            Part partModel = vMeg.getPart(partName);
            WSMessage variableData = runtimeVariable.getWSMessage();
            if (variableData != null) {
                Element part = variableData.getPart(partName);
                if (part == null) {
                    part = variableData.createPart(partName);
                }
                Map<String, String> prefixMap = new HashMap<String, String>();
                try {
                    setValue(part, value, "/", prefixMap);
                } catch (XpathExpressionException e) {
                    throw new SchemaViolationException(variableModel.getName()
                            + " part:" + partName + " is not a simple type");
                }
                persistState(bpName, instanceId, runtimeVariable, oldVal);

            }
        } else {
            throw new SchemaViolationException(variableModel.getName()
                    + " is not a WSDLMessage variable");
        }
    }

    private void getPrefixMap(Element part, Map<String, String> prefixMap) {
        prefixMap.put(part.getPrefix(), part.getNamespaceURI());
        NodeList children = part.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child instanceof Element) {
                getPrefixMap((Element) child, prefixMap);
            }
        }
    }

    private void changeVariableSchemaTypeValue(RuntimeVariable runtimeVariable,
            String value, VariableScope context, QName bpName,
            String instanceId, String oldVal) throws SchemaViolationException {
        if (value == null) {
            return;
        }
        RVariable variableModel = runtimeVariable.getVariableDef();
        if (variableModel == null) {
            return;
        }

        if (variableModel.getWSDLMessageType() != null) {
            throw new SchemaViolationException(variableModel.getName()
                    + "is not a XSD type variable");
        }
        Object variableData = runtimeVariable.getXSDVariableData();
        if (variableData != null) {
            if (variableModel.isBoolean()) {
                runtimeVariable.setXSDVariableData(new Boolean(value));
            } else if (variableModel.isString()) {
                runtimeVariable.setXSDVariableData(value);
            } else if (variableModel.isNumber()) {
                if (variableData instanceof BigDecimal) {
                    runtimeVariable.setXSDVariableData(new BigDecimal(value));
                } else if (variableData instanceof Byte) {
                    runtimeVariable.setXSDVariableData(new Byte(value));
                } else if (variableData instanceof Double) {
                    runtimeVariable.setXSDVariableData(new Double(value));
                } else if (variableData instanceof Float) {
                    runtimeVariable.setXSDVariableData(new Float(value));
                } else if (variableData instanceof Integer) {
                    runtimeVariable.setXSDVariableData(new Integer(value));
                } else if (variableData instanceof Long) {
                    runtimeVariable.setXSDVariableData(new Long(value));
                } else if (variableData instanceof Short) {
                    runtimeVariable.setXSDVariableData(new Short(value));
                } else {
                    throw new SchemaViolationException(variableModel.getName()
                            + "is not a Simple XSD type variable");
                }

            }
            persistState(bpName, instanceId, runtimeVariable, oldVal);
        }

    }

    private void changeVariableSchemaTypeValue(RuntimeVariable runtimeVariable,
            String xpath, String value, VariableScope context, QName bpName,
            String instanceId, String oldVal) throws XpathExpressionException,
            SchemaViolationException {
        if (value == null) {
            return;
        }
        RVariable variableModel = runtimeVariable.getVariableDef();
        if (variableModel == null) {
            return;
        }

        if (variableModel.getWSDLMessageType() != null) {
            throw new SchemaViolationException(variableModel.getName()
                    + "is not a XSD type variable");
        }
        Object variableData = runtimeVariable.getXSDVariableData();
        if (variableData != null) {
            Element bean = null;
            if (variableData instanceof Document) {
                bean = ((Document) variableData).getDocumentElement();
            } else if (variableData instanceof Element) {
                bean = (Element) variableData;
            } else {
                throw new SchemaViolationException(variableModel.getName()
                        + "is not a complex XSD type variable");
            }
            Map<String, String> prefixMap = new HashMap<String, String>();
            getPrefixMap(bean, prefixMap);
            setValue(bean, value, xpath, prefixMap);
            persistState(bpName, instanceId, runtimeVariable, oldVal);
        }
    }

    private void setValue(Object bean, String val, String xpath,
            Map<String, String> prefixMap) throws XpathExpressionException,
            SchemaViolationException {
        String simXpath = xpath;
        DOMFactory domFactory = new DOMFactory();
        JXPathContext ctx = newJXPathContext(bean);
        ctx.setFactory(domFactory);
        if (prefixMap.size() > 0) {
            Set<Map.Entry<String, String>> mappings = prefixMap.entrySet();
            for (Map.Entry<String, String> mapping : mappings) {
                ctx.registerNamespace(mapping.getKey(), mapping.getValue());
            }
        }
        Pointer pointer = ctx.getPointer(simXpath);
        Object node = pointer.getNode();
        if (node instanceof Element) {
            if (!isLeaf((Element) pointer.getNode())) {
                throw new SchemaViolationException(
                        "failed to set value--xpath  "
                                + xpath
                                + " doesn't locate a text node or attribute node");
            }
        }
        try {
            ctx.setValue(simXpath, val);
        } catch (Exception e) {
            throw new XpathExpressionException(e.getMessage());
        }
    }

//    private String formatXpath(String toQuery) {
//        String targetQuery = toQuery;
//        if (toQuery.endsWith("text()")) {
//            int position = toQuery.indexOf("text()");
//            targetQuery = toQuery.substring(0, position - 1);
//        } else {
//            char[] charArray = toQuery.toCharArray();
//            if (charArray[charArray.length - 1] == ']') {
//                int position = charArray.length - 2;
//                while (charArray[position] != '[') {
//                    position--;
//                }
//                targetQuery = toQuery.substring(0, position);
//            }
//        }
//        return targetQuery;
//    }

    private boolean isLeaf(Element node) {
        if (!node.hasChildNodes() && (!node.hasAttributes())) {
            return true;
        }
        if (node.hasChildNodes() && (node.getFirstChild() instanceof Text)) {
            return true;
        }
        return false;
    }

    /**
     * Create a new JXPathContext
     *
     * @param bean
     *            Context value
     *
     * @return The new context
     */
    private JXPathContext newJXPathContext(Object bean) {
        JXPathContext context = JXPATH_FACTORY.newContext(null, bean);
        context.setLenient(true);
        return context;
    }

    private synchronized void initDataSource() throws Exception {
        checkMonitorEnabled();
        if (!isInitialized) {
            mConnFactory = mEngine.getDBConnectionFactory();
        }
        isInitialized = true;
    }

    private static InputStream getBPELsArchive(String suName, Connection conn)
            throws Exception {
        InputStream archiveBytes = null;
        PreparedStatement query = null;
        ResultSet resultSet = null;
        try {
            query = conn.prepareStatement(GET_BPEL_PATH_QUERY);
            query.setString(1, suName);
            resultSet = query.executeQuery();
            if (resultSet == null) {
                return archiveBytes;
            }
            while (resultSet.next()) {

                Blob suArchive = resultSet.getBlob(1);
                return suArchive.getBinaryStream();
            }
        } catch (SQLException ex) {
            throw ex;
        } finally {
            closeResultSet(resultSet);
            closeStatement(query);
            closeConnection(conn);
        }
        return archiveBytes;

    }

    private static void extractBpelOrWsdlFile(Map<String, String> bpelWsdlMap,
            JarInputStream bpelsJarFile, JarEntry entry) throws Exception {
        String entryName = entry.getName();
        String firstChar = entry.getName().substring(0, 1);

        if (firstChar.equals("/") || firstChar.equals("\\")) {
            entryName = entry.getName().substring(1);
        }
        // If the entry has a seperator which isn't the platform's seperator
        // replace the seperator with the platform's
        if ((entryName.indexOf("\\") != -1) && !File.separator.equals("\\")) {

            entryName.replaceAll("\\\\", "/");
        } else if ((entryName.indexOf("/") != -1)
                && !File.separator.equals("/")) {
            entryName.replaceAll("/", "\\\\");
        }

        String tempdir = System.getProperty("java.io.tmpdir");
        if (!(tempdir.endsWith("/") || tempdir.endsWith("\\")))
            tempdir = tempdir + File.separator;

        String tempFilePath = tempdir + entryName;
        createTempDirIfNeeded(tempFilePath);
        byte[] entryBytes = null;
        File tempFile = new File(tempFilePath);
        FileOutputStream tempFileOS = new FileOutputStream(tempFile);
        boolean sizeKnown = entry.getSize() != -1;
        if (sizeKnown) {
            entryBytes = new byte[(int) entry.getSize()];
            bpelsJarFile.read(entryBytes);
            tempFileOS.write(entryBytes);
        } else {
            entryBytes = new byte[BUFFER_SIZE];
            int bytesRead = bpelsJarFile.read(entryBytes);
            while (true) {
                tempFileOS.write(entryBytes, 0, bytesRead);
                tempFileOS.flush();
                bytesRead = bpelsJarFile.read(entryBytes);
                if (bytesRead == -1) {
                    break;
                }
            }
        }
        tempFileOS.close();
        bpelWsdlMap.put(entryName, tempFilePath);

    }

    private static void createTempDirIfNeeded(String filePath) throws Exception {
        File tempFile = new File(filePath);
        File tempDir = tempFile.getParentFile();
        if (tempDir == null || tempDir.exists()) {
            return;
        }
        tempDir.mkdirs();
    }

    private static void closeStatement(Statement stm) {
        if (stm != null) {
            try {
                stm.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    private static void closeResultSet(ResultSet resultSet) {
        if (resultSet != null) {
            try {
                resultSet.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    private static void closeConnection(Connection conn) {
        if (conn != null) {
            try {
                conn.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    private static void closeConnection(AbstractDBConnection conn) {
        if (conn != null) {
            try {
                conn.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    private static String getValue(SerialClob clob) throws Exception {
        Reader retVal = null;
        try {
            retVal = clob.getCharacterStream();
            BufferedReader br = new BufferedReader(retVal);
//            boolean read = true;
            StringBuffer sb = new StringBuffer();
            String s = null;
            while ((s = br.readLine()) != null) {
                sb.append(s);
                sb.append("\n");
            }
            return sb.toString();
        } catch (Exception e) {
            throw e;
        }
    }

    private static String getQuery(String generic, String sortColumn,
            String order, Integer maxRow, Connection conn) throws Exception {
        String query = generic;
        if (conn.getMetaData().getDriverName().startsWith("Apache Derby")) {
            if (sortColumn != null) {
                query = generic + " ORDER BY " + sortColumn.toUpperCase();
                if (order != null) {
                    query = query + " " + order;
                }
            }
        } else if (conn.getMetaData().getDriverName().startsWith("Oracle")) {
            if (maxRow != null) {
                if (generic.indexOf("WHERE") != -1) {
                    query = generic + " and ROWNUM <= " + maxRow;
                } else {
                    query = generic + " where ROWNUM <= " + maxRow;
                }
            }
            if (sortColumn != null) {
                query = generic + " ORDER BY " + sortColumn.toUpperCase();
                if (order != null) {
                    query = query + " " + order;
                }
            }
        } else if (conn.getMetaData().getDriverName().startsWith("MySQL")) {
            if (sortColumn != null) {
                query = generic + " ORDER BY " + sortColumn.toUpperCase();
                if (order != null) {
                    query = query + " " + order;
                }
            }
            if (maxRow != null) {
            	query = query + " LIMIT " + maxRow;
            }
        }
        return query;
    }

    private static void populateBPELInfoListMap(ResultSet rs,
            List<Map<String, Object>> result) throws Exception {
        while (rs.next()) {
            //			BPInstanceInfo inst = new BPInstanceInfo ();
            Map<String, Object> bpinfo = new HashMap<String, Object>();
            bpinfo = new HashMap<String, Object>();
            bpinfo.put("id", rs.getString(1));
            bpinfo.put("bpelId", rs.getString(2));
            bpinfo.put("status", rs.getString(3));
            bpinfo.put("startTime", rs.getTimestamp(4));
            bpinfo.put("endTime", rs.getTimestamp(5));
            bpinfo.put("lastUpdateTime", rs.getTimestamp(6));
            Timestamp lastTime = ((Timestamp) bpinfo.get("endTime") == null ? new Timestamp(
                    Calendar.getInstance().getTimeInMillis())
                    : (Timestamp) bpinfo.get("endTime"));
            float lasted = (float) ((lastTime.getTime() - ((Timestamp) bpinfo
                    .get("startTime")).getTime()) / 1000.0);
            bpinfo.put("lasted", new Float(lasted));
            result.add(bpinfo);
        }
    }
    
    private String getVariableTypeFromCode(String code) {
    	if (code == null) {
    		return "";
    	} else if (code.equals(SimpleVarType.String.getCode())) {
    		return SimpleVarType.String.toString();
    	} else if (code.equals(SimpleVarType.Numeric.getCode())) {
    		return SimpleVarType.Numeric.toString();
    	} else if (code.equals(SimpleVarType.Boolean.getCode())) {
    		return SimpleVarType.Boolean.toString();
    	} else if (code.equals(SimpleVarType.Date.getCode())) {
    		return SimpleVarType.Date.toString();
    	}
    	return "";
    }

}
