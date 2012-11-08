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
 * @(#)SQLSEServiceUnitManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Output;
import javax.wsdl.PortType;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.UnknownExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.EntityResolver;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jdbcbc.extensions.JDBCExtensionRegistry;
import com.sun.jbi.jdbcbc.extensions.JDBCOperation;
import com.sun.jbi.jdbcbc.extensions.JDBCOperationInput;
import com.sun.jbi.jdbcbc.extensions.JDBCOperationOutput;
import com.sun.jbi.jdbcbc.util.Configuration.PortMap;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import java.util.Arrays;

// Additional imports for replacing the ibm WSDL factory to Sun extensions
import com.sun.wsdl4j.ext.impl.WSDLFactoryEx;
import com.sun.wsdl4j.ext.impl.WSDLReaderEx;

// common-util and qos imports
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.management.descriptor.EndpointIdentifier;


/**
 * Used by SQLSE.
 * Implements the ServiceUnitManager interface of JBI.
 */
public class SQLSEServiceUnitManager implements ServiceUnitManager {
    private static final Messages mMessages = Messages.getMessages(SQLSEServiceUnitManager.class);
    private static final Logger mLogger = Messages.getLogger(SQLSEServiceUnitManager.class);
    private static final String PARTNER_MYROLE = "myRole";
    private static final String PARTNER_PARTNERROLE = "partnerRole";
    private HashMap<String,File> mDeployedIds;
    private HashMap<String,EndpointBean[]> mDeployedEndpoints;
    private JDBCComponentContext mContext;
    private DeployHelper mHelper;
    private SQLSELifeCycle mLifeCycle;
    private SQLMapEntryTable mSQLMapEntryTable;
    private final Hashtable<QName,Definition> mWSDLMap = new Hashtable<QName,Definition>();
    private SQLEngineFileEntry[] mSQLEngineFileEntries;

	private Map<EndpointInfo, List<ServiceQuality>> mEndpointQos;

    /** Creates a new instance of DeployerMBean 
     * @param context 
     * @param lifeCycle 
     * @param sqlmap 
     */
    /*protected SQLSEServiceUnitManager(final ComponentContext context,
        final SQLSELifeCycle lifeCycle, final SQLMapEntryTable sqlmap) {
        mContext = context;
        mDeployedIds = new HashMap<String,File>();
        mDeployedEndpoints = new HashMap<String,EndpointBean[]>();
        mHelper = new DeployHelper();
        mLifeCycle = lifeCycle;
        mSQLMapEntryTable = sqlmap;
    }*/

	protected SQLSEServiceUnitManager(final JDBCComponentContext context,
        final SQLSELifeCycle lifeCycle, final SQLMapEntryTable sqlmap) {
        mContext = context;
        mDeployedIds = new HashMap<String,File>();
        mDeployedEndpoints = new HashMap<String,EndpointBean[]>();
        mHelper = new DeployHelper();
        mLifeCycle = lifeCycle;
        mSQLMapEntryTable = sqlmap;
    }

    /**
     * Initiate a BC Deployment.
     *
     * @param suId -
     *            ID of the ASA being deployed
     *            Path of the ASA jar file
     * @param asaFilePath 
     * @return 
     * @throws javax.jbi.management.DeploymentException 
     */
    //@Override
    public String deploy(final String suId, final String asaFilePath)
        throws DeploymentException {
        String retMsg = null;
        final String taskName = "deploy";

        if (SQLSEServiceUnitManager.mLogger.isLoggable(Level.INFO)) {
            SQLSEServiceUnitManager.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00301.SQLSESUM_DEPLOY_SU"),
                new Object[] { suId, asaFilePath });
        }

        retMsg = createSuccessMessage(taskName, mContext.getContext().getComponentName());

        return retMsg;
    }

    //@Override
    public void init(final String suId, final String suPath) throws DeploymentException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00302.SQLSESUM_INIT_SU"),
                new Object[] { suId, suPath });
        }

		// have the throttling configuration loaded for the endpoint thats being Initialized.
	DeploymentLookup lookUpUtil = new DeploymentLookup(mContext.getContext());
        mEndpointQos = lookUpUtil.lookupServiceQualities(suPath);
        if (mEndpointQos != null && mEndpointQos.size() > 0) {
            for (Iterator itor = mEndpointQos.keySet().iterator(); itor.hasNext(); ) {
                EndpointInfo endpointInfo = (EndpointInfo) itor.next();
                ServiceQuality[] qos = 
                    (ServiceQuality[])((List)mEndpointQos.get(endpointInfo)).toArray(new ServiceQuality[0]);
                mContext.getBindingChannel().addServiceQualityToEndpoint(endpointInfo, qos);
            }
        }

        try {
            // Prepare for start if the deployment hasn't been processed yet.
            if (mDeployedIds.get(suId) == null) {
                final StatusProviderHelper statusProviderHelper = mLifeCycle.getStatusProviderHelper();
                processDeployment(suId, suPath, statusProviderHelper);
            }

            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00303.SQLSESUM_INIT_SU_SUCCESS"), suId);
            }
        } catch (final DeploymentException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00304.SQLSESUM_INIT_SU_FAIL"), ex);
            throw ex;
        }
    }

    /**
     * Process a deployment, validating it and preparing to start it..
     *
     * @param suId
     *            the name of the Service Unit being initialized.
     * @param path
     *            the full path to the Service Unit artifact root directory.
     * @throws DeploymentException
     *             if the Service Unit is not deployed, or is in an incorrect
     *             state.
     */
    private void processDeployment(final String suId, final String path,
        final StatusProviderHelper statusProviderHelper) throws DeploymentException {
        final String taskName = "processDeployment";

        try {
            final File asaDir = new File(path);
            final String[] files = asaDir.list();
            String sqlmapfile = null;
            String engineFile = null;

            for (final String filename : files) {
                if (filename.endsWith("sqlmap.xml")) {
                    sqlmapfile = asaDir.getAbsolutePath() + File.separator +
                        filename;
                } else if (filename.endsWith("sqlse_engine.xml")) {
                    engineFile = asaDir.getAbsolutePath() + File.separator +
                        filename;
                }
            }

            // process wsdls and place them into wsdl map
            mHelper.readAllDefinitions(asaDir, suId, taskName, "SQLSE_Init_2");

            SQLMapReader.parse(new File(sqlmapfile), mSQLMapEntryTable, suId,
                asaDir, mWSDLMap);
            mSQLEngineFileEntries = SQLEngineFileReader.parse(asaDir,
                    new File(engineFile));
            mHelper.deploy(asaDir, suId, mContext.getContext(), path, statusProviderHelper);


        } catch (final FileNotFoundException ex) {
            final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                    taskName, "FAILED", "SQLSE_PROCESS_2", suId,
                    "Processing deployment error: " + ex.getMessage(), ex);
            mLogger.log(Level.SEVERE,"Deployment failure owing to",ex);
            throw new DeploymentException(exMsg);
        } catch (final IOException ex) {
            final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                    taskName, "FAILED", "SQLSE_PROCESS_3", suId,
                    "Processing deployment error: " + ex.getMessage(), ex);
            mLogger.log(Level.SEVERE,"Deployment failure owing to",ex);
            throw new DeploymentException(exMsg);
        } catch (final MessagingException ex) {
            final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                    taskName, "FAILED", "SQLSE_PROCESS_4", suId,
                    "Processing deployment error: " + ex.getMessage(), ex);
            mLogger.log(Level.SEVERE,"Deployment failure owing to",ex);
            throw new DeploymentException(exMsg);
        } catch (final Exception e) {
            final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                    taskName, "FAILED", "SQLSE_PROCESS_5", suId,
                    "Processing deployment error: " + e.getMessage(), e);
            mLogger.log(Level.SEVERE,"Deployment failure owing to",e);
            throw new DeploymentException(exMsg);
        }
    }

    //@Override
    public void start(final String suId) throws DeploymentException {
        mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00305.SQLSESUM_SU_START") + suId);

        final List list = mSQLMapEntryTable.getEntryListByServiceUnitName(suId);

        for (int i = 0, I = list.size(); i < I; i++) {
            final SQLMapEntry etlMapEntry = (SQLMapEntry) list.get(i);
            etlMapEntry.setStarted(true);
        }

        mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00306.SQLSESUM_SU_START_SUCCESS"), suId);
    }

    //@Override
    public void stop(final String suId) throws DeploymentException {
        mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00307.SQLSESUM_SU_STOP"), suId);

        final List list = mSQLMapEntryTable.getEntryListByServiceUnitName(suId);

        for (int i = 0, I = list.size(); i < I; i++) {
            final SQLMapEntry sqlMapEntry = (SQLMapEntry) list.get(i);
            sqlMapEntry.setStarted(false);
        }

        mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00308.SQLSESUM_SU_STOP_SUCCESS"), suId);
    }

    //@Override
    public void shutDown(final String suId) throws DeploymentException {

		//qos shoutdown
	try {
            // remove the QoS attributes for the undeployed endpoints
            if (mEndpointQos != null && mEndpointQos.size() > 0) {
                for (Iterator it = mEndpointQos.keySet().iterator(); it.hasNext();) {
                    EndpointInfo endpointInfo = (EndpointInfo) it.next();
                    ServiceQuality[] qos = 
                        (ServiceQuality[])((List)mEndpointQos.get(endpointInfo)).toArray(new ServiceQuality[0]);
                    mContext.getBindingChannel().removeServiceQualityFromEndpoint(endpointInfo, qos);
                }
            }
        }catch(Exception e){
            final String extMsg = mMessages.getString("SQLSE_E00310.SQLSESUM_SU_NOT_DEPLOYED");
            throw new DeploymentException(extMsg);
        }      
        final String taskName = "init";
        SQLSEServiceUnitManager.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00309.SQLSESUM_SU_SHUTDOWN"), suId);

        if (!mDeployedIds.containsKey((suId))) {
            final String extMsg = mMessages.getString("SQLSE_E00310.SQLSESUM_SU_NOT_DEPLOYED");
            throw new DeploymentException(extMsg);
        }

        try {
            // Remove suId from lookup table
            mDeployedIds.remove(suId);

            final StatusReporting reporting = mLifeCycle.getStatusProviderHelper()
                                                  .getStatusReporter();
            final List list = mSQLMapEntryTable.getEntryListByServiceUnitName(suId);

            for (int i = 0, I = list.size(); i < I; i++) {
                final SQLMapEntry etlMapEntry = (SQLMapEntry) list.get(i);
                final ServiceEndpoint serviceEndpoint = etlMapEntry.getServiceEndpoint();

                if (serviceEndpoint != null) {
                    mContext.getContext().deactivateEndpoint(serviceEndpoint);
                }

                final String endpointStatusId = etlMapEntry.getEndpointStatusId();

                if (endpointStatusId != null) {
                    reporting.removeProvisioningEndpoints(new String[] {
                            endpointStatusId
                        });
                }

                final String outEndpointStatusId = etlMapEntry.getOutEndpointStatusId();

                if (outEndpointStatusId != null) {
                    reporting.removeConsumingEndpoints(new String[] {
                            outEndpointStatusId
                        });
                }

                mSQLMapEntryTable.removeEntry(etlMapEntry);
            }
        } catch (final Exception e) {
            final String extMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                    taskName, "FAILED", "SQLSeSum_ShutDown_2", suId,
                    "Service Unit shutDown error", e);
            throw new DeploymentException(extMsg, e);
        }

        SQLSEServiceUnitManager.mLogger.log(Level.INFO, "SQLSESUM_SU_SHUTDOWN_SUCCESS", suId);
    }

    /**
     * Cancel a Service Deployment. If the deployment is in use (has
     * dependencies), then this operation may fail.
     *
     * @param name -
     *            name of the service unit
     * @param root -
     *            root of the service unit
     * @return 
     * @throws javax.jbi.management.DeploymentException 
     */
    //@Override
    public String undeploy(final String name, final String root) throws DeploymentException {
        String retMsg = null;
        final String taskName = "undeploy";

        if (SQLSEServiceUnitManager.mLogger.isLoggable(Level.FINE)) {
            SQLSEServiceUnitManager.mLogger.log(Level.FINE, "SQLSESUM_SU_UNDEPLOY",
                new Object[] { name, root });
        }

        retMsg = createSuccessMessage(taskName, mContext.getContext().getComponentName());

        return retMsg;
    }

    /**
     * Determine the message exchange pattern. For handling 1.1 wsdls, map
     * transmission primitives to the closest message exchange pattern, taking
     * into account the endpoint direction direction inbound: request-response
     * and solicit-response -> in-out one-way and notification -> in-only
     * direction outbound: request-response and solicit-response -> out-in
     * one-way and notification -> out-only
     *
     * @param pm
     *            the endpoint configuration from the portmap
     *            the binding operation definition from the wsdl
     * @return the message exchange pattern, null if no mapping could be
     *         determined.
     */
    private String determineMEP(final PortMap pm, final BindingOperation bo) {
        String mep = null;
        final OperationType type = bo.getOperation().getStyle();

        if (pm.getDirection().equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
            if (type.equals(OperationType.REQUEST_RESPONSE) ||
                    type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = "inout";
            } else if (type.equals(OperationType.ONE_WAY) ||
                    type.equals(OperationType.NOTIFICATION)) {
                mep = "inonly";
            }
        } else {
            if (type.equals(OperationType.REQUEST_RESPONSE) ||
                    type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = "outin";
            } else if (type.equals(OperationType.ONE_WAY) ||
                    type.equals(OperationType.NOTIFICATION)) {
                mep = "outonly";
            }
        }

        return mep;
    }

    /**
     * Keep track of the running status of each endpoint. This allows for quick
     * look-up of the state and provides for extensibility where an individual
     * endpoint could be controlled rather than the whole SU.
     */
    private void setEndpointsStatus(final EndpointBean[] endpoints, final String status) {
        if (endpoints != null) {
            for (EndpointBean element : endpoints) {
            element.setValue(EndpointBean.STATUS, status);
         }
        }
    }

    private String createSuccessMessage(final String taskName, final String componentName) {
        final JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);

        final String retMsg = msgBuilder.createSuccessMessage(taskName);

        return retMsg;
    }

    private String createExceptionMessage(final String componentName,
        final String taskName, final String status, final String locToken, final String locParam,
        final String locMessage, final Throwable exObj) {
        final JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);

        final String retMsg = msgBuilder.createExceptionMessage(taskName, locToken,
                locMessage, locParam, exObj);

        return retMsg;
    }

    private class DeployHelper {
        /**
         * List all wsdl files in the currentDir and below
         */
        private List<File> listWSDLFiles(final File currentDir) {
            final List<File> cumulativeResults = new ArrayList<File>();
            final File[] filesInCurrentDir = currentDir.listFiles();

            for (File element : filesInCurrentDir) {
            if (element.isFile()) {
			if (element.getName().toLowerCase()
			                                    .endsWith(".wsdl")) {
			    cumulativeResults.add(element);
			}
            } else if (element.isDirectory()) {
			final List<File> wsdlsInSubDirectories = listWSDLFiles(element);
			cumulativeResults.addAll(wsdlsInSubDirectories);
            }
         }

            return cumulativeResults;
        }

        private Definition readWsdl(final File f, final EntityResolver resolver)
            throws javax.wsdl.WSDLException {
            /*final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
            final WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
            reader.setExtensionRegistry(new JDBCExtensionRegistry());

            final Definition def = reader.readWSDL(f.getAbsolutePath());
            */
            
            // above default Implementation changed with current sun extension
            final WSDLFactoryEx wsdlFactory = new WSDLFactoryEx();
            WSDLReaderEx reader = wsdlFactory.newWSDLReaderEx();
            reader.setEntityResolver(resolver);
            reader.setExtensionRegistry(new JDBCExtensionRegistry());
            final Definition def = reader.readWSDL(f.getAbsolutePath());
            return def;
        }

        private QName getWsdlMapKey(final Definition def) {
            QName key = null;
            final Iterator iterator = def.getPortTypes().keySet().iterator();

            while (iterator.hasNext()) {
                final QName element = (QName) iterator.next();
                final PortType pt = def.getPortType(element);
                key = pt.getQName();

                break;
            }

            return key;
        }

        private PortType[] getPortTypes(final Definition def, final String serviceName,
            final String endpointName) {
            final List extensibilityElems = def.getExtensibilityElements();
            final Iterator iter = extensibilityElems.iterator();
            PortType portType = null;
            final PortType[] portTypes = new PortType[def.getPortTypes().size()];
            UnknownExtensibilityElement unknownElem = null;
            Element elem = null;
            int numPortTypes = 0;

            while (iter.hasNext()) {
                final ExtensibilityElement exElem = (ExtensibilityElement) iter.next();
                final QName elemType = exElem.getElementType();

                if ((elemType != null) && (elemType.getLocalPart() != null) &&
                        (exElem.getElementType().getLocalPart()
                                   .equals("partnerLinkType"))) {
                    if (exElem instanceof UnknownExtensibilityElement) {
                        unknownElem = (UnknownExtensibilityElement) exElem;
                        elem = unknownElem.getElement();
                    }

                    if (elem != null) {
                        final NodeList list = elem.getElementsByTagNameNS("http://docs.oasis-open.org/wsbpel/2.0/plnktype",
                                "role");

                        for (int i = 0; i < list.getLength(); i++) {
                            final Node n = list.item(i);
                            final Node partnerLinkNode = n.getAttributes()
                                                    .getNamedItem("name");

                            if (((n != null) && (partnerLinkNode != null)) &&
                                    (partnerLinkNode.getNodeValue()
                                                        .equals(endpointName))) {
                                final Node portTypeNode = n.getAttributes()
                                                     .getNamedItem("portType");
                                final String portTypeValue = portTypeNode.getNodeValue();
                                String portTypePrefix = "";
                                String portTypeLocalPart = "";

                                if (portTypeValue != null) {
                                    int prefixIndex = -1;
                                    prefixIndex = portTypeNode.getNodeValue()
                                                              .indexOf(":");
                                    portTypePrefix = portTypeValue.substring(0,
                                            prefixIndex);
                                    portTypeLocalPart = portTypeValue.substring(prefixIndex +
                                            1, portTypeValue.length());
                                }

                                final QName qname = new QName(def.getNamespace(
                                            portTypePrefix), portTypeLocalPart);
                                portType = def.getPortType(qname);
                                portTypes[numPortTypes++] = portType;
                            }
                        }
                    }
                }
            }

            return portTypes;
        }

        private Operation getOperation(final Definition def, final String portTypeName,
            final String operationName) {
            Operation operation = null;
            final PortType pType = def.getPortType(QName.valueOf(portTypeName));

            if (pType != null) {
                operation = pType.getOperation(operationName, null, null);
            }

            return operation;
        }

        private JDBCOperation[] getJDBCOperations(final PortType portType) {
            final List<JDBCOperation> jdbcOperations = new ArrayList<JDBCOperation>();

            if (portType != null) {
                final List operations = portType.getOperations();
                final Iterator operIter = (operations == null) ? null
                                                         : operations.iterator();

                while ((operIter != null) && operIter.hasNext()) {
                    operIter.next(); //just move the cursor to the next element.

                    final JDBCOperation op = new JDBCOperation();
                    jdbcOperations.add(op);
                }
            }

            return jdbcOperations.toArray(new JDBCOperation[jdbcOperations.size()]);
        }

        private JDBCOperationInput getJDBCOperationInput(final Input input,
            final String sqlText) {
            JDBCOperationInput operationInput = null;
            operationInput = new JDBCOperationInput();
            //  JDBCOperationInput jsql = new JDBCOperationInput();
            operationInput.setSql(sqlText);
            operationInput.setMessage(input.getMessage());
            operationInput.setInputName(input.getName());
            operationInput.setJDBCSql(operationInput);
            operationInput.setOperationType(JDBCUtil.getSQLStatementType(sqlText));

            return operationInput;
        }

        private JDBCOperationOutput getJDBCOperationOutput(final Output output) {
            JDBCOperationOutput operationOutput = null;
            operationOutput = new JDBCOperationOutput();
            operationOutput.setMessage(output.getMessage());
            operationOutput.setName(output.getName());
            operationOutput.setReturnPartName(output.getName() + "Part");
            operationOutput.setElementType(output.getMessage()
                                                 .getPart(output.getName() +
                    "Part").getTypeName());

            return operationOutput;
        }

        private void deploy(final File asaDir, final String id,
            final ComponentContext compContext, final String suPath,
            final StatusProviderHelper statusProviderHelper)
            throws IOException, DeploymentException,
                javax.jbi.messaging.MessagingException {
            try {
                // process wsdls and place them into wsdl map
                final SUDescriptorSupport sud = new SUDescriptorSupport(suPath);
				
		EndpointIdentifier[] svcs = sud.getServices();

                final Iterator portmaps = mHelper.parseSUD(sud);
                final ArrayList<EndpointBean> endPointArr = new ArrayList<EndpointBean>();

                // bind portmap entry and sql entry using partnerLink
                while (portmaps.hasNext()) {
                    final PortMapEntry entry = (PortMapEntry) portmaps.next();
                    final QName serviceName = entry.getServiceName();
                    final QName partnerLink = entry.getPartnerLink();
                    final QName endpoint = entry.getEndPoint();

                    if (entry.getRole().equalsIgnoreCase(SQLSEServiceUnitManager.PARTNER_MYROLE)) {
                        final List list = mSQLMapEntryTable.getEntryList();

                        for (int i = 0, I = list.size(); i < I; i++) {
                            final SQLMapEntry sqlMapEntry = (SQLMapEntry) list.get(i);
                            final QName sqlPartnerLink = sqlMapEntry.getPartnerLink();

                            if (partnerLink.equals(sqlPartnerLink)) {
                                final ServiceEndpoint serviceEndpoint = mContext.getContext().activateEndpoint(serviceName,
                                        endpoint.getLocalPart());
                                sqlMapEntry.setService(serviceName);
                                sqlMapEntry.setServiceEndpoint(serviceEndpoint);

                                final StatusReporting reporting = statusProviderHelper.getStatusReporter();
                                final String statusId = statusProviderHelper.createProvisioningEndpointIdentifier(serviceName,
                                        endpoint.getLocalPart());
                                reporting.addProvisioningEndpoint(statusId);

                                final EndpointStatus endpointStatus = reporting.getEndpointStatus(statusId);
                                sqlMapEntry.setEndpointStatus(statusId,
                                    endpointStatus);

                                SQLSEServiceUnitManager.mLogger.log(Level.INFO,
                                    "Activated wsdl (" +
                                    sqlMapEntry.getWSDLFileName() + ", " +
                                    sqlMapEntry.getPortType() + ", " +
                                    sqlMapEntry.getOperation() +
                                    ") inout service: " + serviceName);

                                final EndpointBean epBean = createEndPointBean(sqlMapEntry,svcs);
                                endPointArr.add(epBean);
                            }
                        }

                        continue;
                    }

                    if (entry.getRole().equalsIgnoreCase(SQLSEServiceUnitManager.PARTNER_PARTNERROLE)) {
                        final List list = mSQLMapEntryTable.getEntryList();

                        for (int i = 0, I = list.size(); i < I; i++) {
                            final SQLMapEntry sqlMapEntry = (SQLMapEntry) list.get(i);
                            final QName sqlOutPartnerLink = sqlMapEntry.getOutPartnerLink(); // sqlOutPartnerLink maybe null

                            if (partnerLink.equals(sqlOutPartnerLink)) {
                                sqlMapEntry.setOutService(serviceName);

                                final StatusReporting reporting = statusProviderHelper.getStatusReporter();
                                final String statusId = statusProviderHelper.createConsumingEndpointIdentifier(serviceName,
                                        endpoint.getLocalPart());
                                reporting.addConsumingEndpoint(statusId);

                                final EndpointStatus outEndpointStatus = reporting.getEndpointStatus(statusId);
                                sqlMapEntry.setOutEndpointStatus(statusId,
                                    outEndpointStatus);

                                SQLSEServiceUnitManager.mLogger.log(Level.INFO,
                                    "Set outbound service (" + serviceName +
                                    "," + sqlMapEntry.getWSDLFileName() + ", " +
                                    sqlMapEntry.getOutPortType() + ", " +
                                    sqlMapEntry.getOutOperation() + ")");

                                final EndpointBean epBean = createEndPointBean(sqlMapEntry, svcs);
                                endPointArr.add(epBean);
                            }
                        }

                        continue;
                    }
                }

                // Add suId to lookup table
                mDeployedIds.put(id, asaDir);

                final EndpointBean[] epbArray = new EndpointBean[endPointArr.size()];

                for (int j = 0; j < endPointArr.size(); j++) {
                    epbArray[j] = endPointArr.get(j);
                }

                mDeployedEndpoints.put(id, epbArray);
                mLifeCycle.activateEndpoints(epbArray);
            } catch (final Exception e) {
                e.printStackTrace();
            }
        }

        private EndpointBean createEndPointBean(final SQLMapEntry sqlMapEntry,EndpointIdentifier[] svcs)
            throws DeploymentException {
            // create endpoint bean for each operation
            final EndpointBean bcEndpoint = new EndpointBean();
            final String serviceName = sqlMapEntry.getServiceEndpoint()
                                            .getServiceName().toString();
            final String endPointName = sqlMapEntry.getServiceEndpoint()
                                             .getEndpointName();
            final SQLEngineFileEntry engineEntry = SQLEngineFileReader.findSQLEngineFileEntry(mSQLEngineFileEntries,
                    sqlMapEntry.getSQLFileName());

            if (engineEntry == null) {
                final String msg = SQLSEServiceUnitManager.mMessages.getString(
                        "SQLSESUM_FAIL_LOCATE_ENGINE_ENTRY") +
                    sqlMapEntry.getSQLFileName();
                final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                        "createEndPointBean", "FAILED",
                        "SQLSE_DEPLOY_HELPER_7", sqlMapEntry.getSQLFileName(),
                        msg, null);
                throw new DeploymentException(exMsg);
            }

            final ArrayList<EndpointBean> endPoints = new ArrayList<EndpointBean>();

            bcEndpoint.setValueObj(EndpointBean.WSDL_FILE,
                sqlMapEntry.getWSDLFileName());
            bcEndpoint.setValueObj(EndpointBean.DESCRIPTOR,
                sqlMapEntry.getWsdl()); //result
            bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_DRIVER_CLASS,
                engineEntry.getDriverClass());

            bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_URL,
                engineEntry.getDbURL());

            bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_NAME,
                    engineEntry.getDatabaseName());
            
            bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_USER,
                engineEntry.getUser());

            bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_PASSWORD,
                engineEntry.getPassword());

			bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_JNDI_NAME,
                engineEntry.getJNDI());
			
			bcEndpoint.setValue(EndpointBean.JDBC_TRANSACTION_REQUIRED,
	                engineEntry.getTransactionRequired());


            final String endpointNameLocalPart = QName.valueOf(endPointName)
                                                .getLocalPart();
            bcEndpoint.setValue(EndpointBean.ENDPOINT_NAME,
                endpointNameLocalPart);
            bcEndpoint.setValue(EndpointBean.SERVICE_NAME, serviceName);
            bcEndpoint.setValueObj(EndpointBean.FULL_SERVICE_NAME,
                sqlMapEntry.getServiceEndpoint().getServiceName());
            bcEndpoint.setValue(EndpointBean.STATUS,
                EndpointBean.STATUS_SHUTDOWN);

            bcEndpoint.setValue(EndpointBean.ENDPOINT_TYPE,
                EndpointBean.ENDPOINT_TYPE_OUTBOUND);

            final StatusReporting reporting = mLifeCycle.getStatusProviderHelper()
                                                  .getStatusReporter();
            final QName serviceNameQ = (QName) bcEndpoint.getValueObj(EndpointBean.FULL_SERVICE_NAME);
            final String portName = bcEndpoint.getValue(EndpointBean.ENDPOINT_NAME);
            String uniqueName = null;

            uniqueName = mLifeCycle.getStatusProviderHelper()
                                   .createProvisioningEndpointIdentifier(serviceNameQ,
                    portName);
            reporting.addProvisioningEndpoint(uniqueName);

            final EndpointStatus stat = reporting.getEndpointStatus(uniqueName);
            bcEndpoint.setEndpointStatus(stat);

            endPoints.add(bcEndpoint);

            final Map<String,OperationMetaData> operationNameToMetaData = new HashMap<String,OperationMetaData>();
            final PortType[] portTypes = getPortTypes(sqlMapEntry.getWsdl(),
                    serviceName, endPointName);

            if ((portTypes == null) || (portTypes.length == 0)) {
                final String msg = SQLSEServiceUnitManager.mMessages.getString("SQLSESUM_MISS_PORT_TYPE") +
                    serviceName;
                final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                        "createEndPointBean", "FAILED",
                        "SQLSE_DEPLOY_HELPER_12", serviceName, msg, null);
                throw new DeploymentException(exMsg);
            }

            for (final PortType portType : portTypes) {
                final JDBCOperation[] jdbcOperations = getJDBCOperations(portType);

                if ((jdbcOperations == null) || (jdbcOperations.length == 0)) {
                    final String msg = SQLSEServiceUnitManager.mMessages.getString("SQLSESUM_MISS_OPER") +
                        serviceName;
                    final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                            "createEndPointBean", "FAILED",
                            "SQLSE_DEPLOY_HELPER_7", serviceName, msg, null);
                    throw new DeploymentException(exMsg);
                }

                for (int operCount = 0; operCount < jdbcOperations.length;
                        operCount++) {
                    final JDBCOperation jdbcOperation = jdbcOperations[operCount];

                    // TODO: keep all the info separately in
                    // endpointbean.							
                    JDBCOperationInput jdbcOperationInput = null;
                    final Operation op = (Operation) portType.getOperations()
                                                       .get(operCount);
                    String sqlFileName = op.getName() + ".sql";
                    final SQLEngineFileEntry engineEntry1 = SQLEngineFileReader.findSQLEngineFileEntry(mSQLEngineFileEntries,
                            sqlFileName);

                    if (op.getInput() != null) {
                        jdbcOperationInput = getJDBCOperationInput(op.getInput(),
                                engineEntry1.getSqlText());
                    }

                    JDBCOperationOutput jdbcOperationOutput = null;

                    if (op.getOutput() != null) {
                        jdbcOperationOutput = getJDBCOperationOutput(op.getOutput());
                    }

                    JDBCOperationInput jdbcSql = null;

                    //   if (jdbcOperationInput != null) {
                    //       jdbcSql = jdbcOperationInput.getJDBCSql();
                    //   }
                    jdbcSql = jdbcOperationInput.getJDBCSql();

                    final String jdbcSqlStr = jdbcSql.getSql().trim();

                    if (jdbcSqlStr == null) {
                        final String msg = mMessages.getString("SQLSESUM_MISS_SQL") +
                            serviceName;
                        final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                                "createEndPointBean", "FAILED",
                                "SQLSE_DEPLOY_HELPER_8", serviceName, msg, null);
                        throw new DeploymentException(exMsg);
                    }

                    SQLSEServiceUnitManager.mLogger.log(Level.INFO,
                        "SQLSE portType operation " +
                        jdbcOperationInput.getOperationType() + " sql " +
                        jdbcSqlStr);

                    // create operation meta data
                    final OperationMetaData opMetaData = new OperationMetaData();
                    opMetaData.setOperation(op);
                    opMetaData.setJDBCOperation(jdbcOperation);
                    opMetaData.setJDBCOperationInput(jdbcOperationInput);
                    opMetaData.setJDBCSql(jdbcSql);
                    opMetaData.setJDBCOperationOutput(jdbcOperationOutput);
                    opMetaData.setDefinition(sqlMapEntry.getWsdl());
                    operationNameToMetaData.put(op.getName(), opMetaData);
                }
            }

            bcEndpoint.setValueObj(EndpointBean.OPERATION_NAME_TO_META_DATA,
                operationNameToMetaData);

            return bcEndpoint;
        }

        private void undeploy(final String asaId, final File asaDir,
            final StatusProviderHelper statusProviderHelper)
            throws DeploymentException {
            final String taskName = "undeploy";
            mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00317.SQLSESUM_UNDEPLOY_SU"),
                new Object[] { asaId, asaDir });

            try {
                mDeployedIds.remove(asaId);

                final EndpointBean[] endpoints = mDeployedEndpoints.remove(asaId);

                if (endpoints != null) {
                    final StatusReporting reporting = statusProviderHelper.getStatusReporter();

                    for (EndpointBean element : endpoints) {
                  final QName serviceName = (QName) element.getValueObj(EndpointBean.FULL_SERVICE_NAME);
                  final String portName = element.getValue(EndpointBean.ENDPOINT_NAME);
                  String uniqueName = null;

                  if (element.getValue(
					        EndpointBean.ENDPOINT_TYPE)
					                      .equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
					uniqueName = statusProviderHelper.createConsumingEndpointIdentifier(serviceName,
					        portName);
					reporting.removeConsumingEndpoints(new String[] {
					        uniqueName
					    });
                  } else {
					uniqueName = statusProviderHelper.createProvisioningEndpointIdentifier(serviceName,
					        portName);
					reporting.removeProvisioningEndpoints(new String[] {
					        uniqueName
					    });
                  }
               }
                }
            } catch (final Exception e) {
                final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                        taskName, "FAILED", "SQLSE_UNDEPLOY_HELPER_1", asaId,
                        "Failed to undeploy " + e.getMessage(), e);
                throw new DeploymentException(exMsg, e);
            }
        }

        private void readAllDefinitions(final File asaDir, final String id,
            final String taskName, final String locToken) throws DeploymentException {
            final CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(asaDir.getAbsolutePath() +
                File.separator + "xml-catalog.xml");
            catalogManager.setRelativeCatalogs(true);

            final EntityResolver resolver = new CatalogResolver(catalogManager);

            final List<File> wsdls = listWSDLFiles(asaDir);
            final File[] wsdlFiles = wsdls.toArray(new File[0]);

            if (wsdlFiles != null) {
                for (File element : wsdlFiles) {
                    try {
                        final Definition def = readWsdl(element, resolver);
                        final QName key = getWsdlMapKey(def);
                        mWSDLMap.put(key, def);
                    } catch (final Exception e) {
                        final String msg = SQLSEServiceUnitManager.mMessages.getString(
                                "SQLSESUM_fail_READ_WSDL") + element +
                            " : " + e.getMessage();
                        final String exMsg = createExceptionMessage(mContext.getContext().getComponentName(),
                                taskName, "FAILED", locToken, id, msg, e);
                        throw new DeploymentException(exMsg, e);
                    }
                }
            }
        }

        private Iterator<PortMapEntry> parseSUD(final SUDescriptorSupport sud) {
            LinkedList<PortMapEntry> entries = null;

            final Provides[] pds = sud.getProvides();

            for (final Provides p : pds) {
                final QName service = p.getServiceName();
                final QName endpoint = new QName(p.getEndpointName());
                final String role = "myRole";
                final QName partnerlink = p.getServiceName();

                if (entries == null) {
                    entries = new LinkedList<PortMapEntry>();
                }

                final PortMapEntry entry = new PortMapEntry(service, endpoint, role,
                        partnerlink);
                entries.add(entry);
            }

            return (entries != null) ? entries.iterator() : null;
        }

        
    }

	public void getQOSConfigurations (EndpointBean jdbcbcEndpoint,
                                       EndpointIdentifier endpointIdentifier,
                                       Map <EndpointInfo, List<ServiceQuality>> qosMap) {
        if (qosMap != null && qosMap.size() > 0) {
            // Until there's a total transitioning to use the common-util, there's a need to
            // create EndpointInfo using EndpointIndentifier
            EndpointInfo endpointInfo = new EndpointInfo (false, 
                                                          endpointIdentifier.getEndpointName(), 
                                                          null,
                                                          endpointIdentifier.getServiceName(),
                                                          null);
            List<ServiceQuality> qoss = qosMap.get(endpointInfo);
            Iterator<ServiceQuality> qossIter = qoss.iterator();
            while (qossIter.hasNext()) {
                ServiceQuality qos = qossIter.next();
                // Gather throttling config
                if (qos instanceof ThrottlingConfig) {
                    ThrottlingConfig throttleConfig = (ThrottlingConfig)qos;
                    jdbcbcEndpoint.setMaxConcurrencyLimit(throttleConfig.getMaxConcurrencyLimit());
                }
                // Other services....
            }
        }
    }

}
