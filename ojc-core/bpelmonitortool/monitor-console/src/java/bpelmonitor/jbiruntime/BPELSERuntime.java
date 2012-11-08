/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bpelmonitor.jbiruntime;

import bpelmonitor.model.DashboardEntry;
import bpelmonitor.model.InstanceFilter;
import bpelmonitor.model.InstanceFilter.InstanceStatus2;
import com.sun.esb.management.api.administration.AdministrationService;
import com.sun.esb.management.api.configuration.ConfigurationService;
import com.sun.esb.management.client.ManagementClient;
import com.sun.esb.management.common.ManagementRemoteException;
import java.lang.management.ManagementFactory;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.xml.namespace.QName;

/**
 *
 * @author mbhasin
 */
public class BPELSERuntime extends JBIRuntime {

    private static final String ADMINISTRATION_KEY = "Administration";
    private static final String GET_BPEL_PROCESSES = "getBPELProcesses";
    private static final String GET_LAST_INSTANCE_PROCESSED_TIME = "getLastInstanceProcessedTime";
    private static final String GET_LIVE_INSTANCES_COUNT = "getLiveInstancesCount";
    private static final String GET_SUSPENDED_INSTANCES_COUNT = "getSuspendedInstancesCount";
    private static final String GET_FAULTED_INSTANCES_COUNT = "getFaultedInstancesCount";
    private static final String GET_TERMINATED_INSTANCES_COUNT = "getTerminatedInstancesCount";
    private static final String GET_PROCESS_KPI = "getProcessKPI";
    private static final String GET_PROCESS_INSTANCES = "getBPELInstances";
    private static final String SUSPEND_INSTANCE = "suspendInstance";
    private static final String SUSPEND_ALL_INSTANCE = "suspendAllInstance";
    private static final String COMPONENT_NAME = "sun-bpel-engine";
    private static final String RESUME_INSTANCE = "resumeInstance";
    private static final String RESUME_ALL_INSTANCE = "resumeAllInstance";
    private static final String TERMINATE_INSTANCE = "terminateInstance";
    private static final String TERMINATE_ALL_INSTANCE = "terminateAllInstance";
    
    static String MBEAN_NAME = "com.sun.jbi:JbiName=server,ComponentName=sun-bpel-engine,ControlType=Custom,CustomControlName=Administration,ComponentType=Installed,InstalledType=Engine";

    /**
     * Following is alternate mechanism of obtaining direct connection BPEL Mbean.
     */
    public List<String> getBusinessProcesses(String suName) {
        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        List<String> bps = null;
        try {
            if (serverConnection != null) {
                bps = (List<String>) serverConnection.invoke(new ObjectName(MBEAN_NAME), GET_BPEL_PROCESSES, new Object[]{suName},
                        new String[]{"java.lang.String"});
            }
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }
        return bps;
    }

    public ArrayList<DashboardEntry> getDashboardData(ArrayList<DashboardEntry> dashboardEntries) {
        Map map = new HashMap();
        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        try {
            if (serverConnection != null) {
                for (Iterator<DashboardEntry> iter = dashboardEntries.iterator(); iter.hasNext();) {
                    DashboardEntry entry = iter.next();
                    String bpName = entry.getBusinessProcessName();

                    Timestamp lastInstanceProcessedTime = (Timestamp) serverConnection.invoke(new ObjectName(MBEAN_NAME), GET_LAST_INSTANCE_PROCESSED_TIME, new Object[]{bpName},
                            new String[]{"java.lang.String"});
                    entry.setLastInstanceProcessedTime(String.valueOf(lastInstanceProcessedTime));

                    Integer liveCount = (Integer) serverConnection.invoke(new ObjectName(MBEAN_NAME), GET_LIVE_INSTANCES_COUNT, new Object[]{bpName},
                            new String[]{"java.lang.String"});
                    entry.setLiveInstnces(String.valueOf(liveCount));

                    Integer suspendedCount = (Integer) serverConnection.invoke(new ObjectName(MBEAN_NAME), GET_SUSPENDED_INSTANCES_COUNT, new Object[]{bpName},
                            new String[]{"java.lang.String"});
                    entry.setSuspendedInstances(String.valueOf(suspendedCount));

                    Integer faultedCount = (Integer) serverConnection.invoke(new ObjectName(MBEAN_NAME), GET_FAULTED_INSTANCES_COUNT, new Object[]{bpName},
                            new String[]{"java.lang.String"});
                    entry.setFaultedInstances(String.valueOf(faultedCount));

                    Integer terminatedCount = (Integer) serverConnection.invoke(new ObjectName(MBEAN_NAME), GET_TERMINATED_INSTANCES_COUNT, new Object[]{bpName},
                            new String[]{"java.lang.String"});
                    entry.setTerminatedInstances(String.valueOf(terminatedCount));

                    Float processKPI = (Float) serverConnection.invoke(new ObjectName(MBEAN_NAME), GET_PROCESS_KPI, new Object[]{bpName},
                            new String[]{"java.lang.String"});
                    entry.setKpi(String.valueOf(processKPI));
                }
            }
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }
        return dashboardEntries;
    }

    public List<Map<String, Object>> getInstances(InstanceFilter filter) {

        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        QName bpQName = filter.getBpName();
        //InstanceStatus status = filter.getStatus();
        InstanceStatus2 status = filter.getStatus();
        List<Map<String, Object>> instancesMap = null;
        try {
            instancesMap = (List<Map<String, Object>>) serverConnection.invoke(new ObjectName(MBEAN_NAME), GET_PROCESS_INSTANCES,
                    new Object[]{bpQName.toString(), status.toString(), null, 10, "startTime", "ASC"},
                    new String[]{"java.lang.String", "java.lang.String",
                        "java.lang.String", "java.lang.Integer",
                        "java.lang.String", "java.lang.String"});
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }

        return instancesMap;
    }

    public boolean suspendInstance(String instanceId) {
        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        boolean result = false;
        try {
            result = ((Boolean) serverConnection.invoke(new ObjectName(MBEAN_NAME), SUSPEND_INSTANCE,
                    new Object[]{instanceId}, new String[]{"java.lang.String"})).booleanValue();
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }

        return result;
    }

    public List<String> suspendAllInstance(String processName) {
        List<String> list = null;
        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        try {
            list = (List<String>) serverConnection.invoke(new ObjectName(MBEAN_NAME), SUSPEND_ALL_INSTANCE,
                    new Object[]{processName}, new String[]{"java.lang.String"});
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }

        return list;
    }

    public boolean resumeInstance(String instanceId) {
        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        boolean result = false;
        try {
            result = ((Boolean) serverConnection.invoke(new ObjectName(MBEAN_NAME), RESUME_INSTANCE,
                    new Object[]{instanceId}, new String[]{"java.lang.String"})).booleanValue();
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }

        return result;
    }

    public List<String> resumeAllInstance(String processName) {
        List<String> list = null;
        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        try {
            list = (List<String>) serverConnection.invoke(new ObjectName(MBEAN_NAME), RESUME_ALL_INSTANCE,
                    new Object[]{processName}, new String[]{"java.lang.String"});
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }

        return list;
    }

    public boolean terminateInstance(String instanceId) {
        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        boolean result = false;
        try {
            result = ((Boolean) serverConnection.invoke(new ObjectName(MBEAN_NAME), TERMINATE_INSTANCE,
                    new Object[]{instanceId}, new String[]{"java.lang.String"})).booleanValue();
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }

        return result;
    }

    public List<String> terminateAllInstance(String processName) {
        List<String> list = null;
        MBeanServerConnection serverConnection = ManagementFactory.getPlatformMBeanServer();
        try {
            list = (List<String>) serverConnection.invoke(new ObjectName(MBEAN_NAME), TERMINATE_ALL_INSTANCE,
                    new Object[]{processName}, new String[]{"java.lang.String"});
        } catch (Exception ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }

        return list;
    }

    public List<String> getBusinessProcesses2(String suName) {

        List<String> bps = null;

        ConfigurationService configurationService = null;

        try {
            configurationService = client.getConfigurationService();
            List<String> instances = getAllBPELManagementServiceInstances(client, targetName);

            String instName = null;
            if (instances.size() > 0) {
                instName = instances.get(0);
            }

            bps = (List<String>) configurationService.invokeExtensionMBeanOperation(COMPONENT_NAME,
                    ADMINISTRATION_KEY, GET_BPEL_PROCESSES,
                    new Object[]{suName},
                    new String[]{"java.lang.String"},
                    targetName, instName);


        } catch (ManagementRemoteException ex) {
            Logger.getLogger(BPELSERuntime.class.getName()).log(Level.SEVERE, null, ex);
        }
        return bps;
    }

    static List<String> getAllBPELManagementServiceInstances(
            ManagementClient client, String target)
            throws ManagementRemoteException {
        AdministrationService administrationService = null;
        Map<String /*targetName*/, String[] /*instanceNames*/> targetNameToInstancesMap = null;
        List<String> instances = new ArrayList<String>();
        try {
            administrationService = client.getAdministrationService();
            targetNameToInstancesMap =
                    administrationService.listTargetNames();
            String[] instanceNames = targetNameToInstancesMap.get(target);
            if (instanceNames != null) {
                for (int i = 0; i <
                        instanceNames.length; i++) {
                    instances.add(instanceNames[i]);
                }

            }
        } catch (Exception e) {
            throw new ManagementRemoteException(e);
        }

        return instances;
    }
}
