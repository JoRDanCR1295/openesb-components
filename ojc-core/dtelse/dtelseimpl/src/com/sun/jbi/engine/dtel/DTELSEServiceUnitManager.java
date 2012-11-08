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
 * @(#)DTELSEServiceUnitManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.dtel;

import java.io.File;

import javax.jbi.component.ServiceUnitManager;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jbi.messaging.DeliveryChannel;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.descriptor.Provides;
import com.ibm.wsdl.factory.WSDLFactoryImpl;

import javax.jbi.management.DeploymentException;
import java.util.*;
import java.util.logging.Logger;
import javax.jbi.messaging.DeliveryChannel;
import javax.xml.namespace.QName;
import javax.wsdl.Definition;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.factory.WSDLFactory;

import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;

public class DTELSEServiceUnitManager implements ServiceUnitManager {
    private static final Logger mLogger = Logger.getLogger(DTELSEServiceUnitManager.class.getName());

    private static final String PARTNER_MYROLE = "myRole";
    private static final String PARTNER_PARTNERROLE = "partnerRole";

    private DTELSEComponent mComponent;
    private HashSet mDeployedId;
    private ComponentContext mContext;
    private DeliveryChannel mChannel;
    private StatusProviderHelper mStatusProviderHelper;
    private DtelMapEntryTable mDtelMapEntryTable;
    private Hashtable mWsdlMap = null;

    /** Creates a new instance of DTELSEDeployer */
    public DTELSEServiceUnitManager(DTELSEComponent component) {
        mComponent = component;
    }

    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createSuccessMessage(taskName);

        return retMsg;
    }

    private String createExceptionMessage(String componentName, String taskName, String status, String locToken, String locParam, String locMessage, Throwable exObj) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);

        return retMsg;
    }

    void initialize(DtelMapEntryTable dtelMapEntryTable,
                    ComponentContext context,
                    DeliveryChannel channel,
                    StatusProviderHelper statusProviderHelper,
                    Hashtable wsdlMap) {
        mDtelMapEntryTable = dtelMapEntryTable;
        mDeployedId = new HashSet();
        mContext = context;
        mChannel = channel;
        mStatusProviderHelper = statusProviderHelper;
        mWsdlMap = wsdlMap;
    }

    /**
     * Deploy a Service Unit to the component. This is called by the JBI 
     * implementation in order to deploy the given artifact to the implementing 
     * component. 
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     * @param serviceUnitRootPath - the full path to the Service Unit artifact root directory
     */
     public String deploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        mLogger.info("Deploying service unit : " + serviceUnitName + " from " + serviceUnitRootPath);
        String retMsg = createSuccessMessage("deploy", mContext.getComponentName());
        mLogger.info("Deployed service unit : " + serviceUnitName + " from " + serviceUnitRootPath + " successfully");
        return retMsg;
    }

    /**
     * Undeploy a Service Unit from the component. This is called by the JBI 
     * implementation in order to undeploy the given Service Unit from the 
     * implementing component. The deployment must be shut down before it 
     * can be undeployed
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     * @param serviceUnitRootPath - the full path to the Service Unit artifact root directory
     */
    public String undeploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        mLogger.info("Undeploying service unit : " + serviceUnitName + " from " + serviceUnitRootPath);
        String retMsg = createSuccessMessage("undeploy", mContext.getComponentName());
        mLogger.info("Undeployed service unit : " + serviceUnitName + " from " + serviceUnitRootPath + " successfully");
        return retMsg;
    }

    /**
     * Initialize the deployment. This is the first phase of a two-phase start, 
     * where the component must prepare to receive service requests related to 
     * the deployment (if any). 
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     * @param serviceUnitRootPath - the full path to the Service Unit artifact root directory
     */
    public void init(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        String taskName = "init";
        mLogger.info("Initializing service unit " + serviceUnitName + " serviceUnitRootPath: " + serviceUnitRootPath);
        if (mDeployedId.contains(serviceUnitName)) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "IepSeSum_Init_1", serviceUnitName, "Service Unit has already been deployed" , null);
            throw new DeploymentException(extMsg);
        }
        try {
            // load portmap.xml and dtelmap.xml
            File deploydir = new File(serviceUnitRootPath);
            String[] files = deploydir.list();
            String portmapfile = null;
            String dtelmapfile = null;
            for (int i = 0; i < files.length; i++) {
                String filename = files[i];
                if (filename.endsWith("dtelmap.xml")) {
                    dtelmapfile = deploydir.getAbsolutePath() + File.separator + filename;
                } else if (filename.endsWith("portmap.xml")) {
                    portmapfile = deploydir.getAbsolutePath() + File.separator + filename;
                }
            }

            // process wsdls and place them into wsdl map
            readAllDefinitions(deploydir, serviceUnitName, taskName, "DTELSE_Init_2");

            // load SU jbi.xml
            SUDescriptorSupport sud = new SUDescriptorSupport(serviceUnitRootPath);
            /*
            Provides[] pds = sud.getProvides();
            for (int i=0; i<pds.length; i++) {
                mLogger.info("DTEL Provide[" + i + " ]: " + pds[i].getServiceName()+", "+pds[i].getEndpointName());
            }
            */
            
            // Iterator portmaps = PortMapReader.parse(new File(portmapfile));
            Iterator portmaps = PortMapReader.parse(sud);
            DtelMapReader.parse(new File(dtelmapfile), mDtelMapEntryTable, serviceUnitName, deploydir, mWsdlMap);

            // bind portmap entry and dtel entry using partnerLink
            while (portmaps.hasNext()) {
                PortMapEntry entry = (PortMapEntry) portmaps.next();
                QName serviceName = entry.getServiceName();
                QName partnerLink = entry.getPartnerLink();
                QName endpoint = entry.getEndPoint();
                if (entry.getRole().equalsIgnoreCase(PARTNER_MYROLE)) {
                    List list = mDtelMapEntryTable.getEntryList();
                    for (int i = 0, I = list.size(); i < I; i++) {
                        DtelMapEntry dtelMapEntry = (DtelMapEntry) list.get(i);
                        QName dtelPartnerLink = dtelMapEntry.getPartnerLink();
                        if (partnerLink.equals(dtelPartnerLink)) {
                            ServiceEndpoint serviceEndpoint = mContext.activateEndpoint(serviceName, endpoint.getLocalPart());

                            dtelMapEntry.setService(serviceName);
                            dtelMapEntry.setServiceEndpoint(serviceEndpoint);

                            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                            String statusId = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, endpoint.getLocalPart());
                            reporting.addProvisioningEndpoint(statusId);
                            EndpointStatus endpointStatus = reporting.getEndpointStatus(statusId);
                            dtelMapEntry.setEndpointStatus(statusId, endpointStatus);

                            mLogger.info("Activated dtel (" + dtelMapEntry.getFile()
                                + ", " + dtelMapEntry.getPortType() + ", "
                                + dtelMapEntry.getOperation() + ") inbound service: " + serviceName);
                        }
                    }
                    continue;
                }
                if (entry.getRole().equalsIgnoreCase(PARTNER_PARTNERROLE)) {
                    List list = mDtelMapEntryTable.getEntryList();
                    for (int i = 0, I = list.size(); i < I; i++) {
                        DtelMapEntry dtelMapEntry = (DtelMapEntry) list.get(i);
                        QName dtelOutPartnerLink = dtelMapEntry.getOutPartnerLink(); // dtelOutPartnerLink maybe null
                        if (partnerLink.equals(dtelOutPartnerLink)) {
                            dtelMapEntry.setOutService(serviceName);

                            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                            String statusId = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, endpoint.getLocalPart());
                            reporting.addConsumingEndpoint(statusId);
                            EndpointStatus outEndpointStatus = reporting.getEndpointStatus(statusId);
                            dtelMapEntry.setOutEndpointStatus(statusId, outEndpointStatus);

                            mLogger.info("Set outbound service (" + serviceName + ","
                                + dtelMapEntry.getFile()
                                + ", " + dtelMapEntry.getOutPortType() + ", "
                                + dtelMapEntry.getOutOperation() + ")");
                        }
                    }
                    continue;
                }
            }

            // Add serviceUnitName to lookup table
            mDeployedId.add(serviceUnitName);
        } catch (Exception e) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "DtelSeSum_Init_2", serviceUnitName, "Service Unit init error" , e);
            throw new DeploymentException(extMsg, e);
        }
        mLogger.info("Initialized service unit " + serviceUnitName + " serviceUnitRootPath: " + serviceUnitRootPath + " successfully.");
    }

    private void readAllDefinitions(File asaDir, String id, String taskName, String locToken) throws DeploymentException {
        /*
        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(asaDir.getAbsolutePath() + File.separator + "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);
        */
        EntityResolver resolver = null;
        List wsdls = listWSDLFiles(asaDir);
        File[] wsdlFiles = (File[]) wsdls.toArray(new File[0]);

        // read all wsdl files to see if
        if (wsdlFiles != null ) {
            for (int i = 0; i < wsdlFiles.length; i ++) {
                try {
                    Definition def = readWsdl(wsdlFiles[i], resolver);
                    mWsdlMap.put(def.getTargetNamespace(), def);
                    mLogger.info("Added WSDL " + def.getTargetNamespace() + ", file: " + wsdlFiles[i].getName());
                } catch (Exception e) {
                    String msg = "Unable to read WSDL file " + wsdlFiles[i] + " : " + e.getMessage();
                    String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", locToken, id, msg , e);
                    throw new DeploymentException(exMsg, e);
                }
            }
        }
    }

    private List listWSDLFiles(File currentDir) {
        List cumulativeResults = new ArrayList();
        File[] filesInCurrentDir = currentDir.listFiles();
        for (int fileCount = 0; fileCount < filesInCurrentDir.length; fileCount++) {
            if (filesInCurrentDir[fileCount].isFile()) {
                if (filesInCurrentDir[fileCount].getName().toLowerCase().endsWith(".wsdl")) {
                    cumulativeResults.add(filesInCurrentDir[fileCount]);
                }
            } else if (filesInCurrentDir[fileCount].isDirectory()) {
                List wsdlsInSubDirectories = listWSDLFiles(filesInCurrentDir[fileCount]);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }

    private Definition readWsdl(File f, EntityResolver resolver)  throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

    /**
     * Shut down the deployment. This causes the deployment to return to the 
     * state it was in after deploy() and before init(). 
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        String taskName = "init";
        mLogger.info("Shutting down service unit " + serviceUnitName);
        if (!mDeployedId.contains(serviceUnitName)) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "DtelSeSum_ShutDown_1", serviceUnitName, "Service Unit has not been deployed" , null);
            throw new DeploymentException(extMsg);
        }
        try {
            // Remove serviceUnitName from lookup table
            mDeployedId.remove(serviceUnitName);
            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
            List list = mDtelMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
            for (int i = 0, I = list.size(); i < I; i++) {
                DtelMapEntry dtelMapEntry = (DtelMapEntry)list.get(i);
                ServiceEndpoint serviceEndpoint = dtelMapEntry.getServiceEndpoint();
                if (serviceEndpoint != null) {
                    mContext.deactivateEndpoint(serviceEndpoint);
                }
                String endpointStatusId = dtelMapEntry.getEndpointStatusId();
                if (endpointStatusId != null) {
                    reporting.removeProvisioningEndpoints(new String[]{endpointStatusId});
                }
                String outEndpointStatusId = dtelMapEntry.getOutEndpointStatusId();
                if (outEndpointStatusId != null) {
                    reporting.removeConsumingEndpoints(new String[]{outEndpointStatusId});
                }
                mDtelMapEntryTable.removeEntry(dtelMapEntry);
            }
        } catch (Exception e) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "DtelSeSum_ShutDown_2", serviceUnitName, "Service Unit shutDown error" , e);
            throw new DeploymentException(extMsg, e);
        }
        mLogger.info("Shut down service unit " + serviceUnitName + " successfully.");
    }

    /**
     * Start the deployment. This is the second phase of a two-phase start, 
     * where the component can now initiate service requests related to the 
     * deployment.     
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void start(String serviceUnitName) throws DeploymentException {
        mLogger.info("Starting service unit " + serviceUnitName);
        List list = mDtelMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
        for (int i = 0, I = list.size(); i < I; i++) {
            DtelMapEntry dtelMapEntry = (DtelMapEntry)list.get(i);
            dtelMapEntry.setStarted(true);
        }
        mLogger.info("Started service unit " + serviceUnitName + " successfully.");
    }

    /**
     * Stop the deployment. This causes the component to cease generating 
     * service requests related to the deployment. This returns the deployment 
     * to a state equivalent to after init() was called    
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void stop(java.lang.String serviceUnitName) throws DeploymentException {
        mLogger.info("Stopping down service unit " + serviceUnitName);
        List list = mDtelMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
        for (int i = 0, I = list.size(); i < I; i++) {
            DtelMapEntry dtelMapEntry = (DtelMapEntry)list.get(i);
            dtelMapEntry.setStarted(false);
        }
        mLogger.info("Stopped service unit " + serviceUnitName + " successfully.");
    }

}
