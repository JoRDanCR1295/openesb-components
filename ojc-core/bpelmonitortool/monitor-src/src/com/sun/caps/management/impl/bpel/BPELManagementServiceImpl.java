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
 * @(#)$Id: BPELManagementServiceImpl.java,v 1.10 2008/09/26 23:28:41 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.caps.management.impl.bpel;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.management.MBeanServerConnection;

import com.sun.caps.management.api.bpel.BPELManagementService;
import com.sun.caps.management.common.AbstractServiceImpl;
import com.sun.caps.management.common.CAPSJMXObjectNames;
import com.sun.caps.management.common.ManagementRemoteException;
import com.sun.esb.management.api.administration.AdministrationService;
import com.sun.esb.management.api.configuration.ConfigurationService;
import com.sun.esb.management.client.ManagementClient;
import com.sun.esb.management.client.ManagementClientFactory;

public class BPELManagementServiceImpl extends AbstractServiceImpl implements
        BPELManagementService {

    private static final String SUSPEND_INSTANCE = "suspendInstance";

    private static final String RESUME_INSTANCE = "resumeInstance";

    private static final String TERMINATE_INSTANCE = "terminateInstance";

    private static final String CHANGE_VARIABLE_VALUE = "changeVariableValue";

    private static final String IS_MONITORED_ENABLED = "isMonitoringEnabled";

    private static final String IS_MONITORED_VARIABLE_ENABLED = "isMonitoringVariableEnabled";

    private static final String IS_PERSISTENCE_ENABLED = "isPersistenceEnabled";

    private static final String GET_INSTANCE_ACTIVITY_STATUS = "getBPELInstanceActivityStatus";

    private static final String GET_INSTANCE_FAULT = "getBPELInstanceFault";

    private static final String GET_INSTANCES = "getBPELInstances";
    
    private static final String SEARCH_INSTANCES = "searchBPELInstances";

    private static final String GET_BPEL_PROCESSES = "getBPELProcesses";

    private static final String GET_INVOKER_INSTANCE = "getInvokerInstance";

    private static final String GET_INVOKEE_INSTANCE = "getInvokeeInstance";

    private static final String GET_VARIABLE_VALUE = "getVariableValue";

    private static final String LIST_VAR_INFO = "listBPELVaraibles";

    private static final String SUSPEND_ALL_INSTANCE = "suspendAllInstance";

    private static final String RESUME_ALL_INSTANCE = "resumeAllInstance";

    private static final String TERMINATE_ALL_INSTANCE = "terminateAllInstance";

    private static final String COMPONENT_NAME = "sun-bpel-engine";

    // String targetName = "server";
    private static final String ADMINISTRATION_KEY = "Administration";

    public BPELManagementServiceImpl(MBeanServerConnection serverConnection) {
        super(serverConnection);
        // TODO Auto-generated constructor stub
    }

    public BPELManagementServiceImpl(MBeanServerConnection serverConnection,
            boolean isRemoteConnection) {
        super(serverConnection, isRemoteConnection);
        // TODO Auto-generated constructor stub
    }

    public boolean changeVariableValue(String instanceId, long varId,
            String partName, String xpath, String value, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return (Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(),
                    CHANGE_VARIABLE_VALUE, new Object[] { instanceId,
                            new Long(varId), partName, xpath, value },
                    new String[] { "java.lang.String", "java.lang.Long",
                            "java.lang.String", "java.lang.String",
                            "java.lang.String" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                for (String instName : instances) {
                    Boolean result = (Boolean) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, CHANGE_VARIABLE_VALUE,
                                    new Object[] { instanceId, new Long(varId),
                                            partName, xpath, value },
                                    new String[] { "java.lang.String",
                                            "java.lang.Long",
                                            "java.lang.String",
                                            "java.lang.String",
                                            "java.lang.String" }, target,
                                    instName);
                    if (result.booleanValue()) {
                        return true;
                    }
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        return false;
    }

    public List<ActivityStatus> getBPELInstanceActivityStatus(String bpId,
            String target) throws ManagementRemoteException {
        // TODO Auto-generated method stub
        // TODO Auto-generated method stub
        List<ActivityStatus> bpInfos = new ArrayList<ActivityStatus>();
        List<Map<String, Object>> actStatusInfos = null;
        if (target == null) {
            actStatusInfos = (List<Map<String, Object>>) invokeMBeanOperation(
                    CAPSJMXObjectNames.getBPELManagementServiceObjectName(),
                    GET_INSTANCE_ACTIVITY_STATUS, new Object[] { bpId },
                    new String[] { "java.lang.String" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    actStatusInfos = (List<Map<String, Object>>) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY,
                                    GET_INSTANCE_ACTIVITY_STATUS,
                                    new Object[] { bpId },
                                    new String[] { "java.lang.String" },
                                    target, instName);
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        for (Map<String, Object> actStatusInfo : actStatusInfos) {
            ActivityStatus info = new ActivityStatus();
            info.activityId = ((Long) actStatusInfo.get("activityId"))
                    .toString();
            info.activityXpath = (String) actStatusInfo.get("activityXpath");
            info.endTime = (Timestamp) actStatusInfo.get("endTime");
            info.lasted = ((Float) actStatusInfo.get("lasted")).floatValue();
            info.startTime = (Timestamp) actStatusInfo.get("startTime");
            info.status = ActivityStatus.Status.valueOf((String) actStatusInfo
                    .get("status"));
            info.iteration = ((Integer) actStatusInfo.get("iteration"))
                    .intValue();
            bpInfos.add(info);
        }
        return bpInfos;
    }

    public String getBPELInstanceFault(String bpid, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return (String) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(), GET_INSTANCE_FAULT,
                    new Object[] { bpid }, new String[] { "java.lang.String" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    return (String) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, GET_INSTANCE_FAULT,
                                    new Object[] { bpid },
                                    new String[] { "java.lang.String" },
                                    target, instName);
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        return null;

    }

    private BPInstanceQueryResult queryBPELInstances(String bpelQName,
            BPStatus status, String searchString, String instanceId, Integer maxRecords,
            SortColumn sortColumn, SortOrder order, String target)
            throws ManagementRemoteException {
        
    	// If the search string is not empty then call the search bpel instances method.
    	String methodToCall = SEARCH_INSTANCES;
    	String varValue = searchString;
    	if (searchString == null) {
    		varValue = instanceId;
    		methodToCall = GET_INSTANCES;
    	}
    	
        List<Map<String, Object>> instancesInfos = null;
        if (target == null) {
            instancesInfos = (List<Map<String, Object>>) invokeMBeanOperation(
                    CAPSJMXObjectNames.getBPELManagementServiceObjectName(),
                    methodToCall, new Object[] { bpelQName,
                            status == null ? null : status.name(), varValue,
                            maxRecords,
                            sortColumn == null ? null : sortColumn.toString(),
                            order == null ? null : order.toString() },
                    new String[] { "java.lang.String", "java.lang.String",
                            "java.lang.String", "java.lang.Integer",
                            "java.lang.String", "java.lang.String" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    instancesInfos = (List<Map<String, Object>>) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, methodToCall,
                                    new Object[] {
                                            bpelQName,
                                            status == null ? null : status
                                                    .name(),
                                            varValue,
                                            maxRecords,
                                            sortColumn == null ? null
                                                    : sortColumn.toString(),
                                            order == null ? null : order
                                                    .toString() },
                                    new String[] { "java.lang.String",
                                            "java.lang.String",
                                            "java.lang.String",
                                            "java.lang.Integer",
                                            "java.lang.String",
                                            "java.lang.String" }, target,
                                    instName);
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        BPInstanceQueryResult queryResult = new BPInstanceQueryResult();
        if (instancesInfos == null) {
            List<BPInstanceInfo> bpInfos = new ArrayList<BPInstanceInfo>();
            queryResult.bpInstnaceList = bpInfos;
            queryResult.overflow = false;
            queryResult.returned = 0;
            queryResult.total = 0;
            return queryResult;
        }
        List<BPInstanceInfo> bpInfos = new ArrayList<BPInstanceInfo>();
        queryResult.bpInstnaceList = bpInfos;
        boolean start = true;

        for (Map<String, Object> instanceinfo : instancesInfos) {
            if (start) {
                queryResult.overflow = ((Boolean) instanceinfo.get("overflow"))
                        .booleanValue();
                queryResult.returned = ((Integer) instanceinfo.get("returned"))
                        .intValue();
                queryResult.total = ((Integer) instanceinfo.get("total"))
                        .intValue();
                start = false;
            } else {
                BPInstanceInfo info = new BPInstanceInfo();
                info.bpelId = (String) instanceinfo.get("bpelId");
                info.endTime = (Timestamp) instanceinfo.get("endTime");
                info.id = (String) instanceinfo.get("id");
                info.lasted = ((Float) instanceinfo.get("lasted")).floatValue();
                info.lastUpdateTime = (Timestamp) instanceinfo
                        .get("lastUpdateTime");
                info.startTime = (Timestamp) instanceinfo.get("startTime");
                info.status = BPStatus.valueOf((String) instanceinfo
                        .get("status"));
                bpInfos.add(info);
            }
        }
        return queryResult;
    }
    
    public BPInstanceQueryResult getBPELInstances(String bpelQName,
            BPStatus status, String instanceId, Integer maxRecords,
            SortColumn sortColumn, SortOrder order, String target)
            throws ManagementRemoteException {
    	
        return queryBPELInstances(bpelQName, status, null, instanceId, maxRecords, sortColumn, order, target);
    }
    
    public BPInstanceQueryResult searchBPELInstances(String bpelQName,
            BPStatus status, String searchString, Integer maxRecords,
            SortColumn sortColumn, SortOrder order, String target)
            throws ManagementRemoteException {
    	
        return queryBPELInstances(bpelQName, status, searchString, null, maxRecords, sortColumn, order, target);
    }

    public List<String> getBPELProcessIds(String suName, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method
        // stubCAPSJMXObjectNames.getBPELManagementServiceObjectName()
        if (target == null) {
            return (List<String>) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(), GET_BPEL_PROCESSES,
                    new Object[] { suName },
                    new String[] { "java.lang.String" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    return (List<String>) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, GET_BPEL_PROCESSES,
                                    new Object[] { suName },
                                    new String[] { "java.lang.String" },
                                    target, instName);
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        return null;
    }

    public List<BPInstanceInfo> getInvokeeInstance(String bpid,
            Long invokeActivityId, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        List<BPInstanceInfo> bpInfos = new ArrayList<BPInstanceInfo>();
        List<Map<String, Object>> instancesInfos = null;
        if (target == null) {
            instancesInfos = (List<Map<String, Object>>) invokeMBeanOperation(
                    CAPSJMXObjectNames.getBPELManagementServiceObjectName(),
                    GET_INVOKEE_INSTANCE,
                    new Object[] { bpid, invokeActivityId }, new String[] {
                            "java.lang.String", "java.lang.Long" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    instancesInfos = (List<Map<String, Object>>) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, GET_INVOKEE_INSTANCE,
                                    new Object[] { bpid, invokeActivityId },
                                    new String[] { "java.lang.String",
                                            "java.lang.Long" }, target,
                                    instName);
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }

        }
        populateBPELInfo(instancesInfos, bpInfos);
        return bpInfos;
    }

    public List<BPInstanceInfo> getInvokerInstance(String bpid,
            Long receiveActivityId, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        List<BPInstanceInfo> bpInfos = new ArrayList<BPInstanceInfo>();
        List<Map<String, Object>> instancesInfos = null;
        if (target == null) {
            instancesInfos = (List<Map<String, Object>>) invokeMBeanOperation(
                    CAPSJMXObjectNames.getBPELManagementServiceObjectName(),
                    GET_INVOKER_INSTANCE, new Object[] { bpid,
                            receiveActivityId }, new String[] {
                            "java.lang.String", "java.lang.Long" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    instancesInfos = (List<Map<String, Object>>) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, GET_INVOKER_INSTANCE,
                                    new Object[] { bpid, receiveActivityId },
                                    new String[] { "java.lang.String",
                                            "java.lang.Long" }, target,
                                    instName);
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        populateBPELInfo(instancesInfos, bpInfos);
        return bpInfos;
    }

    public String getVariableValue(String instanceId, long varId, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return (String) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(), GET_VARIABLE_VALUE,
                    new Object[] { instanceId, varId }, new String[] {
                            "java.lang.String", "java.lang.Long" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    return (String) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, GET_VARIABLE_VALUE,
                                    new Object[] { instanceId, varId },
                                    new String[] { "java.lang.String",
                                            "java.lang.Long" }, target,
                                    instName);
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        return null;
    }

    public boolean isMonitoringEnabled(String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return ((Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(),
                    IS_MONITORED_ENABLED, new Object[] {}, new String[] {}))
                    .booleanValue();
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    return ((Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                            .getBPELManagementServiceObjectName(),
                            IS_MONITORED_ENABLED, new Object[] {},
                            new String[] {})).booleanValue();
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        return false;
    }

    public boolean isMonitoringVariableEnabled(String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return ((Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(),
                    IS_MONITORED_VARIABLE_ENABLED, new Object[] {},
                    new String[] {})).booleanValue();
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    return ((Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                            .getBPELManagementServiceObjectName(),
                            IS_MONITORED_VARIABLE_ENABLED, new Object[] {},
                            new String[] {})).booleanValue();
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        return false;
    }

    public boolean isPersistenceEnabled(String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return ((Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(),
                    IS_PERSISTENCE_ENABLED, new Object[] {}, new String[] {}))
                    .booleanValue();
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    return ((Boolean) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, IS_PERSISTENCE_ENABLED,
                                    new Object[] {}, new String[] {}, target,
                                    instName)).booleanValue();
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        return false;
    }

    public boolean resumeInstance(String bpId, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return ((Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(), RESUME_INSTANCE,
                    new Object[] { bpId }, new String[] { "java.lang.String" }))
                    .booleanValue();
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    try {
                        for (String instName : instances) {
                            boolean resumed = ((Boolean) configurationService
                                    .invokeExtensionMBeanOperation(
                                            COMPONENT_NAME,
                                            ADMINISTRATION_KEY,
                                            RESUME_INSTANCE,
                                            new Object[] { bpId },
                                            new String[] { "java.lang.String" },
                                            target, instName)).booleanValue();
                            if (resumed) {
                                return true;
                            }
                        }
                    } catch (Exception e) {
                        throw new ManagementRemoteException(e);
                    }
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
            return false;
        }
    }

    /**
     * Suspend all instances of a bpel process
     * 
     * @param processName
     *            The process name (QName)
     * @return The list of suspended instance ids
     * @throws Exception
     */
    public List<String> resumeAllInstance(String processName, String target)
            throws ManagementRemoteException {
        if (target == null) {
            return (List<String>) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(), RESUME_ALL_INSTANCE,
                    new Object[] { processName },
                    new String[] { "java.lang.String" });
        } else {
            ManagementClient client = null;
            List<String> resumed = new ArrayList<String>();
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    try {
                        for (String instName : instances) {
                            List<String> haveResumed = (List<String>) configurationService
                                    .invokeExtensionMBeanOperation(
                                            COMPONENT_NAME,
                                            ADMINISTRATION_KEY,
                                            RESUME_ALL_INSTANCE,
                                            new Object[] { processName },
                                            new String[] { "java.lang.String" },
                                            target, instName);
                            if (haveResumed != null && haveResumed.size() > 0) {
                                resumed.addAll(haveResumed);
                            }
                        }
                    } catch (Exception e) {
                        throw new ManagementRemoteException(e);
                    }
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
            return resumed;
        }
    }

    public boolean suspendInstance(String bpId, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return ((Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(), SUSPEND_INSTANCE,
                    new Object[] { bpId }, new String[] { "java.lang.String" }))
                    .booleanValue();
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    try {
                        for (String instName : instances) {
                            boolean suspend = ((Boolean) configurationService
                                    .invokeExtensionMBeanOperation(
                                            COMPONENT_NAME,
                                            ADMINISTRATION_KEY,
                                            SUSPEND_INSTANCE,
                                            new Object[] { bpId },
                                            new String[] { "java.lang.String" },
                                            target, instName)).booleanValue();
                            if (suspend) {
                                return true;
                            }
                        }
                    } catch (Exception e) {
                        throw new ManagementRemoteException(e);
                    }
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
            return false;
        }
    }

    /**
     * Suspend all instances of a bpel process
     * 
     * @param processName
     *            The process name (QName)
     * @return The list of suspended instance ids
     * @throws Exception
     */
    public List<String> suspendAllInstance(String processName, String target)
            throws ManagementRemoteException {
        if (target == null) {
            return (List<String>) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(),
                    SUSPEND_ALL_INSTANCE, new Object[] { processName },
                    new String[] { "java.lang.String" });
        } else {
            ManagementClient client = null;
            List<String> resumed = new ArrayList<String>();
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    try {
                        for (String instName : instances) {
                            List<String> haveResumed = (List<String>) configurationService
                                    .invokeExtensionMBeanOperation(
                                            COMPONENT_NAME,
                                            ADMINISTRATION_KEY,
                                            SUSPEND_ALL_INSTANCE,
                                            new Object[] { processName },
                                            new String[] { "java.lang.String" },
                                            target, instName);
                            if (haveResumed != null && haveResumed.size() > 0) {
                                resumed.addAll(haveResumed);
                            }
                        }
                    } catch (Exception e) {
                        throw new ManagementRemoteException(e);
                    }
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
            return resumed;
        }
    }

    public boolean terminateInstance(String bpId, String target)
            throws ManagementRemoteException {
        // TODO Auto-generated method stub
        if (target == null) {
            return ((Boolean) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(), TERMINATE_INSTANCE,
                    new Object[] { bpId }, new String[] { "java.lang.String" }))
                    .booleanValue();
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    try {
                        for (String instName : instances) {
                            boolean suspend = ((Boolean) configurationService
                                    .invokeExtensionMBeanOperation(
                                            COMPONENT_NAME,
                                            ADMINISTRATION_KEY,
                                            TERMINATE_INSTANCE,
                                            new Object[] { bpId },
                                            new String[] { "java.lang.String" },
                                            target, instName)).booleanValue();
                            if (suspend) {
                                return true;
                            }
                        }
                    } catch (Exception e) {
                        throw new ManagementRemoteException(e);
                    }
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
            return false;
        }
    }

    /**
     * Suspend all instances of a bpel process
     * 
     * @param processName
     *            The process name (QName)
     * @return The list of suspended instance ids
     * @throws Exception
     */
    public List<String> terminateAllInstance(String processName, String target)
            throws ManagementRemoteException {
        if (target == null) {
            return (List<String>) invokeMBeanOperation(CAPSJMXObjectNames
                    .getBPELManagementServiceObjectName(),
                    TERMINATE_ALL_INSTANCE, new Object[] { processName },
                    new String[] { "java.lang.String" });
        } else {
            ManagementClient client = null;
            List<String> resumed = new ArrayList<String>();
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    try {
                        for (String instName : instances) {
                            List<String> haveResumed = (List<String>) configurationService
                                    .invokeExtensionMBeanOperation(
                                            COMPONENT_NAME,
                                            ADMINISTRATION_KEY,
                                            TERMINATE_ALL_INSTANCE,
                                            new Object[] { processName },
                                            new String[] { "java.lang.String" },
                                            target, instName);
                            if (haveResumed != null && haveResumed.size() > 0) {
                                resumed.addAll(haveResumed);
                            }
                        }
                    } catch (Exception e) {
                        throw new ManagementRemoteException(e);
                    }
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
            return resumed;
        }
    }

    public List<VarInfo> listBPELVaraibles(String instanceId, String varName,
            String target) throws ManagementRemoteException {
        // TODO Auto-generated method stub
        List<Map<String, Object>> varInfos = null;
        if (target == null) {
            varInfos = (List<Map<String, Object>>) invokeMBeanOperation(
                    CAPSJMXObjectNames.getBPELManagementServiceObjectName(),
                    LIST_VAR_INFO, new Object[] { instanceId, varName },
                    new String[] { "java.lang.String", "java.lang.String" });
        } else {
            ManagementClient client = null;
            try {
                client = ManagementClientFactory
                        .getInstance(getMBeanServerConnection());
                ConfigurationService configurationService = getConfigService(client);
                List<String> instances = getAllBPELManagementServiceInstances(
                        client, target);
                if (instances.size() > 0) {
                    String instName = instances.get(0);
                    varInfos = (List<Map<String, Object>>) configurationService
                            .invokeExtensionMBeanOperation(COMPONENT_NAME,
                                    ADMINISTRATION_KEY, LIST_VAR_INFO,
                                    new Object[] { instanceId, varName },
                                    new String[] { "java.lang.String",
                                            "java.lang.String" }, target,
                                    instName);
                }
            } catch (Exception e) {
                throw new ManagementRemoteException(e);
            }
        }
        List<VarInfo> varInfoList = new ArrayList<VarInfo>();
        for (Map<String, Object> var : varInfos) {
            VarInfo varInfo = new VarInfo();
            varInfo.varId = (Long) var.get("varId");
            varInfo.varName = (String) var.get("varName");
            varInfo.xpath = (String) var.get("xpath");
            varInfo.notes = (String) var.get("notes");
            varInfoList.add(varInfo);
        }
        return varInfoList;
    }

    private static void populateBPELInfo(
            List<Map<String, Object>> instancesInfos,
            List<BPInstanceInfo> bpInfos) {
        for (Map<String, Object> instanceinfo : instancesInfos) {
            BPInstanceInfo info = new BPInstanceInfo();
            info.bpelId = (String) instanceinfo.get("bpelId");
            info.endTime = (Timestamp) instanceinfo.get("endTime");
            info.id = (String) instanceinfo.get("id");
            info.lasted = ((Float) instanceinfo.get("lasted")).floatValue();
            info.lastUpdateTime = (Timestamp) instanceinfo
                    .get("lastUpdateTime");
            info.startTime = (Timestamp) instanceinfo.get("startTime");
            info.status = BPStatus.valueOf((String) instanceinfo.get("status"));
            bpInfos.add(info);
        }
    }

    private static List<String> getAllBPELManagementServiceInstances(
            ManagementClient client, String target)
            throws ManagementRemoteException {
        AdministrationService administrationService = null;
        Map<String /*targetName*/, String[] /*instanceNames*/> targetNameToInstancesMap = null;
        List<String> instances = new ArrayList<String>();
        try {
            administrationService = client.getAdministrationService();
            targetNameToInstancesMap = administrationService.listTargetNames();
            String[] instanceNames = targetNameToInstancesMap.get(target);
            if (instanceNames != null) {
                for (int i = 0; i < instanceNames.length; i++) {
                    instances.add(instanceNames[i]);
                }
            }
        } catch (Exception e) {
            throw new ManagementRemoteException(e);
        }
        return instances;
    }

    private static ConfigurationService getConfigService(ManagementClient client)
            throws ManagementRemoteException {
        ConfigurationService configurationService = null;
        try {
            configurationService = client.getConfigurationService();
        } catch (Exception e) {
            throw new ManagementRemoteException(e);
        }
        return configurationService;
    }

}
