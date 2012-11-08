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
package com.sun.jbi.engine.mashup;

import com.sun.jbi.management.descriptor.ConfigurationException;
import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.PortType;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.xml.sax.EntityResolver;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.engine.mashup.mbean.MashupSERuntimeConfiguration;
import com.sun.jbi.engine.mashup.mbean.MashupSERuntimeConfigurationMBean;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.internationalization.Messages;

public class MashupSEServiceUnitManager implements ServiceUnitManager {

    private static final Logger mLogger = Logger.getLogger(MashupSEServiceUnitManager.class.getName());
    private static final Messages mMessages = Messages.getMessages(MashupSEServiceUnitManager.class);
    private static final String PARTNER_MYROLE = "myRole";
    private static final String PARTNER_PARTNERROLE = "partnerRole";
    private MashupSEComponent mComponent;
    private HashSet mDeployedId;
    private ComponentContext mContext;
    private DeliveryChannel mChannel;
    private StatusProviderHelper mStatusProviderHelper;
    private MashupMapEntryTable mMapEntryTable;
    private Hashtable mWsdlMap = null;
    private MashupSERuntimeConfiguration mRuntimeConfig;

    /** Creates a new instance of MashupSEServiceUnitManager */
    public MashupSEServiceUnitManager(MashupSEComponent component) {
        mComponent = component;
    }

    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createSuccessMessage(taskName);

        return retMsg;
    }

    private String createExceptionMessage(String componentName, String taskName, String status,
            String locToken, String locParam, String locMessage, Throwable exObj) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam,
                exObj);

        return retMsg;
    }

    void initialize(MashupMapEntryTable mashupMapEntryTable, ComponentContext context,
            DeliveryChannel channel, StatusProviderHelper statusProviderHelper, Hashtable wsdlMap, MashupSERuntimeConfigurationMBean runtimeConfiguration) {
        mMapEntryTable = mashupMapEntryTable;
        mDeployedId = new HashSet();
        mContext = context;
        mChannel = channel;
        mStatusProviderHelper = statusProviderHelper;
        mWsdlMap = wsdlMap;
        mRuntimeConfig = (MashupSERuntimeConfiguration) runtimeConfiguration;
    }

    public MashupMapEntryTable getMashupMapEntryTable() {
        return this.mMapEntryTable;
    }

    /**
     * Deploy a Service Unit to the component. This is called by the JBI
     * implementation in order to deploy the given artifact to the implementing
     * component.
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     * @param serviceUnitRootPath -
     *            the full path to the Service Unit artifact root directory
     */
    public String deploy(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        //mLogger.fine(mMessages.getString("EDMSE-I0336.deploying_Su") + serviceUnitName + mMessages.getString("EDMSE-I0338.from") + serviceUnitRootPath);
        String retMsg = createSuccessMessage("deploy", mContext.getComponentName());
        mLogger.info(mMessages.getString("EDMSE-I0336.deploying_Su") + serviceUnitName + mMessages.getString("EDMSE-I0338.from") + serviceUnitRootPath + mMessages.getString("EDMSE-I0337.successfully"));
        return retMsg;
    }

    /**
     * Undeploy a Service Unit from the component. This is called by the JBI
     * implementation in order to undeploy the given Service Unit from the
     * implementing component. The deployment must be shut down before it can be
     * undeployed
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     * @param serviceUnitRootPath -
     *            the full path to the Service Unit artifact root directory
     */
    public String undeploy(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        //mLogger.info(mMessages.getString("EDMSE-I0339.unDeploying_SU") + serviceUnitName + mMessages.getString("EDMSE-I0338.from") + serviceUnitRootPath);
        String retMsg = createSuccessMessage("undeploy", mContext.getComponentName());

        mLogger.info(mMessages.getString("EDMSE-I0339.unDeploying_SU") + serviceUnitName + mMessages.getString("EDMSE-I0338.from") + serviceUnitRootPath + mMessages.getString("EDMSE-I0337.successfully"));
        return retMsg;
    }

    /**
     * Initialize the deployment. This is the first phase of a two-phase start,
     * where the component must prepare to receive service requests related to
     * the deployment (if any).
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     * @param serviceUnitRootPath -
     *            the full path to the Service Unit artifact root directory
     */
    public void init(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        String taskName = "init";        
        if (mDeployedId.contains(serviceUnitName)) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, mMessages.getString("EDMSE-E0104.failed"),
                    "mashupSum_Init_1", serviceUnitName, mMessages.getString("EDMSE-E0105.su_AlreadyDeployed"),
                    null);
            throw new DeploymentException(extMsg);
        }
        try {
            // load portmap.xml and mashupmap.xml
            File deploydir = new File(serviceUnitRootPath);
            String[] files = deploydir.list();
            // String portmapfile = null;
            String mashupmapfile = null;
            for (int i = 0; i < files.length; i++) {
                String filename = files[i];
                mLogger.fine(mMessages.getString("EDMSE-F0217.file_Found") + filename);
                if (filename.endsWith("edmmap.xml")) {
                    mashupmapfile = deploydir.getAbsolutePath() + File.separator + filename;
                    mLogger.fine("mashupmapfile" + mashupmapfile);
                }
            }

            // process wsdls and place them into wsdl map
            readAllDefinitions(deploydir, serviceUnitName, taskName, "mashupSE_Init_2");

            // load SU jbi.xml
            SUDescriptorSupport sud = new SUDescriptorSupport(serviceUnitRootPath);
            /*
             * Provides[] pds = sud.getProvides(); for (int i=0; i<pds.length;
             * i++) { mLogger.info("mashup Provide[" + i + " ]: " +
             * pds[i].getServiceName()+", "+pds[i].getEndpointName()); }
             */

            // Iterator portmaps = PortMapReader.parse(new File(portmapfile));
            Iterator portmaps = PortMapReader.parse(sud);
            MashuplMapReader.parse(new File(mashupmapfile), mMapEntryTable, serviceUnitName, deploydir,
                    mWsdlMap);

            // bind portmap entry and mashup entry using partnerLink
            while (portmaps.hasNext()) {
                PortMapEntry entry = (PortMapEntry) portmaps.next();
                QName serviceName = entry.getServiceName();
                QName partnerLink = entry.getPartnerLink();
                QName endpoint = entry.getEndPoint();

                if (entry.getRole().equalsIgnoreCase(PARTNER_MYROLE)) {
                    List list = mMapEntryTable.getEntryList();
                    for (int i = 0, I = list.size(); i < I; i++) {
                        MashupMapEntry mashupMapEntry = (MashupMapEntry) list.get(i);
                        QName mashupPartnerLink = mashupMapEntry.getPartnerLink();
                        if (partnerLink.equals(mashupPartnerLink)) {

                            ServiceEndpoint serviceEndpoint = mContext.activateEndpoint(
                                    serviceName, endpoint.getLocalPart());

                            mashupMapEntry.setService(serviceName);
                            mashupMapEntry.setServiceEndpoint(serviceEndpoint);

                            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                            String statusId = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, endpoint.getLocalPart());
                            reporting.addProvisioningEndpoint(statusId);
                            EndpointStatus endpointStatus = reporting.getEndpointStatus(statusId);
                            mashupMapEntry.setEndpointStatus(statusId, endpointStatus);
                            mLogger.info("Activated mashup (" + mashupMapEntry.getFile() + ", " + mashupMapEntry.getPortType() + ", " + mashupMapEntry.getOperation() + ") Inbound service: " + serviceName);
                        }
                    }
                    continue;
                }
                if (entry.getRole().equalsIgnoreCase(PARTNER_PARTNERROLE)) {
                    List list = mMapEntryTable.getEntryList();
                    for (int i = 0, I = list.size(); i < I; i++) {
                        MashupMapEntry mashupMapEntry = (MashupMapEntry) list.get(i);
                        QName mashupOutPartnerLink = mashupMapEntry.getOutPartnerLink(); // mashupOutPartnerLink
                        // maybe
                        // null
                        if (partnerLink.equals(mashupOutPartnerLink)) {
                            mashupMapEntry.setOutService(serviceName);

                            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                            String statusId = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, endpoint.getLocalPart());
                            reporting.addConsumingEndpoint(statusId);
                            EndpointStatus outEndpointStatus = reporting.getEndpointStatus(statusId);
                            mashupMapEntry.setOutEndpointStatus(statusId, outEndpointStatus);

                            mLogger.info("Set outbound service (" + serviceName + "," + mashupMapEntry.getFile() + ", " + mashupMapEntry.getOutPortType() + ", " + mashupMapEntry.getOutOperation() + ")");
                        }
                    }
                    continue;
                }
            }

            // Add serviceUnitName to lookup table
            mDeployedId.add(serviceUnitName);
        } catch (Exception e) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED",
                    "mashupSeSum_Init_2", serviceUnitName, mMessages.getString("EDMSE-E0106.su_initError"), e);
            throw new DeploymentException(extMsg, e);
        }
        mLogger.info(mMessages.getString("EDMSE-I0341.SU_Initialized") + serviceUnitName + ",serviceUnitRootPath: " + serviceUnitRootPath + mMessages.getString("EDMSE-I0337.successfully"));
    }

    private void readAllDefinitions(File asaDir, String id, String taskName, String locToken)
            throws DeploymentException {
        /*
         * CatalogManager catalogManager = new CatalogManager();
         * catalogManager.setCatalogFiles(asaDir.getAbsolutePath() +
         * File.separator + "xml-catalog.xml");
         * catalogManager.setRelativeCatalogs(true); EntityResolver resolver =
         * new CatalogResolver(catalogManager);
         */
        EntityResolver resolver = null;
        List wsdls = listWSDLFiles(asaDir);
        File[] wsdlFiles = (File[]) wsdls.toArray(new File[0]);

        // read all wsdl files to see if
        if (wsdlFiles != null) {
            for (int i = 0; i < wsdlFiles.length; i++) {
                try {
                    Definition def = readWsdl(wsdlFiles[i], resolver);
                    QName key = getWsdlMapKey(def);
                    mWsdlMap.put(key, def);
                    mLogger.info("Added WSDL " + def.getTargetNamespace() + ", file: " + wsdlFiles[i].getName());
                } catch (Exception e) {
                    String msg = mMessages.getString("EDMSE-E0106.error_ReadingWSDL") + wsdlFiles[i] + " : " + e.getMessage();
                    String exMsg = createExceptionMessage(mContext.getComponentName(), taskName,
                            mMessages.getString("EDMSE-E0104.failed"), locToken, id, msg, e);
                    throw new DeploymentException(exMsg, e);
                }
            }
        }
    }

    private QName getWsdlMapKey(Definition def) {
        QName key = null;
        Iterator iterator = def.getPortTypes().keySet().iterator();
        while (iterator.hasNext()) {
            QName element = (QName) iterator.next();
            PortType pt = def.getPortType(element);
            key = pt.getQName();
            mLogger.info(mMessages.getString("EDMSE-E0106.error_ReadingWSDL") + key);
            break;
        }
        return key;
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

    private Definition readWsdl(File f, EntityResolver resolver) throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

    /**
     * Shut down the deployment. This causes the deployment to return to the
     * state it was in after deploy() and before init().
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        String taskName = "init";

        mLogger.info(mMessages.getString("EDMSE-I0342.shuttingDown_SU") + serviceUnitName);
        if (!mDeployedId.contains(serviceUnitName)) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, mMessages.getString("EDMSE-E0104.failed"),
                    "mashupSeSum_ShutDown_1", serviceUnitName, mMessages.getString("EDMSE-E0107.error_DeployingSU"),
                    null);
            throw new DeploymentException(extMsg);
        }
        try {
            // Remove serviceUnitName from lookup table
            mDeployedId.remove(serviceUnitName);
        /*	StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
        List list = mMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
        for (int i = 0, I = list.size(); i < I; i++) {
        MashupMapEntry mashupMapEntry = (MashupMapEntry) list.get(i);
        ServiceEndpoint serviceEndpoint = mashupMapEntry.getServiceEndpoint();
        if (serviceEndpoint != null) {
        mContext.deactivateEndpoint(serviceEndpoint);
        }
        String endpointStatusId = mashupMapEntry.getEndpointStatusId();
        if (endpointStatusId != null) {
        reporting.removeProvisioningEndpoints(new String[] { endpointStatusId });
        }
        String outEndpointStatusId = mashupMapEntry.getOutEndpointStatusId();
        if (outEndpointStatusId != null) {
        reporting.removeConsumingEndpoints(new String[] { outEndpointStatusId });
        }
        mMapEntryTable.removeEntry(mashupMapEntry);
        }*/
        } catch (Exception e) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, mMessages.getString("EDMSE-E0104.failed"),
                    "mashupSeSum_ShutDown_2", serviceUnitName, mMessages.getString("EDMSE-E0108.SUshutdown_Error"), e);
            throw new DeploymentException(extMsg, e);
        }
        mLogger.info(mMessages.getString("EDMSE-I0342.shuttingDown_SU") + serviceUnitName + mMessages.getString("EDMSE-I0337.successfully"));
    }

    /**
     * Start the deployment. This is the second phase of a two-phase start,
     * where the component can now initiate service requests related to the
     * deployment.
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     */
    public void start(String serviceUnitName) throws DeploymentException {        
        List list = mMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
        for (int i = 0, I = list.size(); i < I; i++) {
            MashupMapEntry mashupMapEntry = (MashupMapEntry) list.get(i);
            mashupMapEntry.setStarted(true);
            registerMashupMonitorMbean(mashupMapEntry);
        }
        mLogger.info(mMessages.getString("EDMSE-I0344.SU_Started") + serviceUnitName + mMessages.getString("EDMSE-I0337.successfully"));
    }

    /**
     * Stop the deployment. This causes the component to cease generating
     * service requests related to the deployment. This returns the deployment
     * to a state equivalent to after init() was called
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     */
    public void stop(java.lang.String serviceUnitName) throws DeploymentException {
        List list = mMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
        for (int i = 0, I = list.size(); i < I; i++) {
            MashupMapEntry mashupMapEntry = (MashupMapEntry) list.get(i);
            mashupMapEntry.setStarted(false);
            unregisterMashupMonitorMbean(mashupMapEntry);
        }
        mLogger.info(mMessages.getString("EDMSE-I0346.SU_Stopped") + serviceUnitName + mMessages.getString("EDMSE-I0337.successfully"));
    }

    private void unregisterMashupMonitorMbean(MashupMapEntry mashupMapEntry) {
    }

    private void registerMashupMonitorMbean(MashupMapEntry mashupMapEntry) {
    }
}
