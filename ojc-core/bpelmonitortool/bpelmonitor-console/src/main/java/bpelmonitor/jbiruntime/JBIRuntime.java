/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bpelmonitor.jbiruntime;

import bpelmonitor.model.ServiceAssembly;
import bpelmonitor.model.ServiceUnit;
import com.sun.esb.management.api.administration.AdministrationService;
import com.sun.esb.management.api.runtime.RuntimeManagementService;
import com.sun.esb.management.client.ManagementClient;
import com.sun.esb.management.client.ManagementClientFactory;
import com.sun.esb.management.common.ManagementRemoteException;
import com.sun.jbi.ui.common.ServiceAssemblyInfo;
import com.sun.jbi.ui.common.ServiceUnitInfo;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author mbhasin
 */
public class JBIRuntime {

    String hostName = "localhost", userName = "admin", password = "adminadmin";
    /** default HTTP Admin Port */
    int portNumber = 8686;
    String targetName = "server";
    RuntimeManagementService runtimeManagementService = null;
    ManagementClient client = null;
    public static String SUN_BPEL_ENGINE = "sun-bpel-engine";

    public JBIRuntime() {
        try {
            client = ManagementClientFactory.getInstance(hostName, portNumber, userName, password);
        } catch (ManagementRemoteException ex) {
            System.out.println("Exception in connecting to JBI Runtime *******" + ex.getMessage());
            Logger.getLogger(JBIRuntime.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public List<ServiceAssembly> getServiceAssemblies() {

        List<ServiceAssembly> serviceAssemblies = new ArrayList();
        ServiceAssembly sa = null;
        List<ServiceAssemblyInfo> assemblyList = getSAs();
        for (ServiceAssemblyInfo element : assemblyList) {
            sa = new ServiceAssembly(element.getName());
            serviceAssemblies.add(sa);
        }
        return serviceAssemblies;
    }

    public List getServiceUnitsForServiceAssembly(String saName) {
        List serviceUnits = new ArrayList();
        ServiceUnit su = null;
        List<ServiceAssemblyInfo> assemblyList = getSAs();
        for (ServiceAssemblyInfo element : assemblyList) {
            String serviceAssemblyName = element.getName();
            if (saName.equals(serviceAssemblyName)) {
                List<ServiceUnitInfo> unitList = element.getServiceUnitInfoList();
                for (ServiceUnitInfo unitElement : unitList) {
                    String suName = unitElement.getName();
                    su = new ServiceUnit(suName);
                    serviceUnits.add(su);
                }
            }
        }
        return serviceUnits;
    }

    private List<ServiceAssemblyInfo> getSAs() {
        boolean isJBIRuntimeEnabled = false;
        List<ServiceAssemblyInfo> assemblyList = new ArrayList();

        if (client != null) {
            try {
                AdministrationService administrationService = client.getAdministrationService();
                isJBIRuntimeEnabled = administrationService.isJBIRuntimeEnabled();
                if (isJBIRuntimeEnabled) {
                    runtimeManagementService = client.getRuntimeManagementService();
                    String listXML = runtimeManagementService.listServiceAssemblies(SUN_BPEL_ENGINE, targetName);
                    assemblyList = ServiceAssemblyInfo.readFromXmlTextWithProlog(listXML);
                }
            } catch (ManagementRemoteException ex) {
                Logger.getLogger(JBIRuntime.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        return assemblyList;
    }
}
