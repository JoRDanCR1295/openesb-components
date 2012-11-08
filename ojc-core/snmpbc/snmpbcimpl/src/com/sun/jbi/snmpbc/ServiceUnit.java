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
 * @(#)ServiceUnit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.snmpbc.extensions.SNMPOperation;
import com.sun.jbi.snmpbc.packaging.EndpointConfiguration;
import com.sun.jbi.snmpbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.snmpbc.packaging.WSDLConfigurations;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * represents a deploy Service Unit in memory
 *
 * @author echou
 */
public class ServiceUnit {

    private int PRODUCTION_ENDPOINT_CONFIG_TYPE = 
               EndpointConfigurationFactory.END_POINT_CONFIG_TYPE_PORTMAP;    
    
    private static final Messages mMessages =
        Messages.getMessages(ServiceUnit.class);
    private static final Logger mLogger =
        Logger.getLogger(ServiceUnit.class.getName());
    
    private String suId;
    private String serviceUnitRootPath;
    private SNMPServiceUnitManager suManager;
    private ComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;    
    private Map<String, Endpoint> mEndpoints;
    private WSDLConfigurations mWSDLConfig;
    
    // local copy of adaptationIds for this ServiceUnit,
    // used to remove from global adaptation list during undeployment
    private List<String> adaptationIds;
    
    // local copy of mofIds for this ServiceUnit,
    // used to remove from global mof list during undeployment
    private List<String> mofIds;
    
    // local copy of pmKeys for this ServiceUnit,
    // used to remove from global pm list during undeployment
    private List<String> pmKeys;

    
    public ServiceUnit(String suId,
            String serviceUnitRootPath,
            SNMPServiceUnitManager suManager,
            ComponentContext context,
            StatusProviderHelper statusProviderHelper) throws JBIException {
        this.suId = suId;
        this.serviceUnitRootPath = serviceUnitRootPath;
        this.suManager = suManager;
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mEndpoints = Collections.synchronizedMap(new HashMap<String, Endpoint> ());
        mWSDLConfig = new WSDLConfigurations(serviceUnitRootPath); 
        if (EndpointConfigurationFactory.jbiRoutingEnabled()) {
            PRODUCTION_ENDPOINT_CONFIG_TYPE = 
                    EndpointConfigurationFactory.END_POINT_CONFIG_TYPE_SU_DESCRIPTOR;
        }
        
        adaptationIds = new ArrayList<String> ();
        mofIds = new ArrayList<String> ();
        pmKeys = new ArrayList<String> ();
        
        init();
    }

    
    /**
     * Retrieves the Id of this ServiceUnit.
     *
     * @return       the name of the Service as a QName
     */
    public String getServiceUnitId() {
        return suId;
    }

    /**
     * Initializes the ServiceUnit.  Parses the serviceUnitRootPath
     * to create the Endpoint objects.  A ServiceUnit may have
     * mutiple WSDLs, each with multiple endpoints that we care about.
     * <p>
     * This method will initialize the WSDLs, transforming it into a
     * set of Endpoints
     *
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    private void init() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, 
                        "SNMPBC-R00301.INIT_CALLED",
                        suId + ", " + serviceUnitRootPath);        
        }
        
        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
        try {
            EndpointConfiguration endpointConfig =
                EndpointConfigurationFactory.getEndpointConfiguration(
                                              PRODUCTION_ENDPOINT_CONFIG_TYPE,
                                              serviceUnitRootPath);
            
            Iterator it = mWSDLConfig.parse(endpointConfig.endpoints()).iterator();
            while (it.hasNext()) {
                Endpoint endpoint = (Endpoint)it.next();
                
                /*
                // Store the status...
                StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                QName serviceName = endpoint.getServiceName();
                String portName = endpoint.getEndpointName();
                String uniqueName = null;
                if (endpoint.getEndpointType() == Endpoint.INBOUND) {
                    uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                    reporting.addConsumingEndpoint(uniqueName);
                } else {
                    uniqueName = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                    reporting.addProvisioningEndpoint(uniqueName);
                }
                endpoint.setEndpointStatus(reporting.getEndpointStatus(uniqueName));
                */
                
                mEndpoints.put(getEndpointKey(endpoint), endpoint);
            }
        } catch (Throwable ex) {
            // mLogger.log(Level.SEVERE, "failed to initialize service unit", ex);
            throw new JBIException(ex);
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, 
                        "SNMPBC-R00302.INIT_SUCCEEDED",
                        suId);
        }        
    }

    /**
     * Starts this ServiceUnit.  This involves activating
     * all Endpoints that are part of this ServiceUnit.  All Endpoints
     * are moved into the EndpointState.RUNNING state.
     * <p>
     * TODO: What should happen if not all the Endpoints
     * can be activated?  Should I deactivate them or just leave
     * them?  For now, I'm going to assume that this method is
     * transactional.  Either all the Endpoints activate or none.
     * If any one fails to activate, the other activated Endpoints
     * will be deactivated.
     *
     * @exception    JBIException if a any Endpoint fails
     * to activate
     */
    public synchronized void start() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, 
                        "SNMPBC-R00303.START_CALLED",
                        suId);        
        }
        
        for (Iterator<Endpoint> endpointsIter = mEndpoints.values().iterator(); endpointsIter.hasNext(); ) {
            Endpoint endpoint = endpointsIter.next();
            Map operations = endpoint.getSNMPOperations();
            for (Iterator<Map.Entry> operationsIter = operations.entrySet().iterator(); operationsIter.hasNext(); ) {
                Map.Entry entry = operationsIter.next();
                QName operation = (QName) entry.getKey();
                SNMPOperation snmpOp = (SNMPOperation) entry.getValue();
                
                // there are three types of operation: mof, adaptation, fm
                if (snmpOp.getType() == SNMPOperation.SnmpOpType.MOF) {
                    // this is an MOF
                    String mofId = snmpOp.getMofId();
                    if (mofId == null || mofId.equals("")) {
                        mLogger.log(Level.WARNING, "SNMPBC-E00308.CANNOT_START_MOF_INVALID_MOFID",
                                mofId);
                        continue;
                    }
                    MOF mof = new MOF(suManager, mofId, operation, endpoint);
                    MOF prevValue = suManager.getMofs().putIfAbsent(mof.getId(), mof);
                    if (prevValue == null) {
                        mofIds.add(mof.getId());
                    } else {
                        mLogger.log(Level.WARNING, "SNMPBC-E00309.CANNOT_START_MOF_DUPLICATE_MOFID",
                                mof.getId());
                        continue;
                    }
                } else if (snmpOp.getType() == SNMPOperation.SnmpOpType.ADAPTATION) {
                    // this is an Adaptation
                    String adaptationId = snmpOp.getAdaptationId();
                    if (adaptationId == null || adaptationId.equals("")) {
                        mLogger.log(Level.WARNING, "SNMPBC-E00310.CANNOT_START_ADAPTATION_INVALID_ADAPTATIIONID",
                                adaptationId);
                        continue;
                    }
                    SNMPAdaptation snmpAdaptation =
                            new SNMPAdaptation(adaptationId, operation, endpoint);
                    SNMPAdaptation prevValue = suManager.getAdaptations().putIfAbsent(
                            snmpAdaptation.getId(), snmpAdaptation);
                    if (prevValue == null) {
                        adaptationIds.add(snmpAdaptation.getId());
                    } else {
                        mLogger.log(Level.WARNING, "SNMPBC-E00311.CANNOT_START_ADAPTATION_DUPLICATE_ADAPTATIONID",
                                snmpAdaptation.getId());
                        continue;
                    }
                } else {
                    // this is an PM
                    String mofIdRef = snmpOp.getMofIdRef();
                    if (mofIdRef == null || mofIdRef.equals("")) {
                        mLogger.log(Level.WARNING, "SNMPBC-E00312.CANNOT_START_PM_INVALID_MOFIDREF",
                                mofIdRef);
                        continue;
                    }
                    PM pm = new PM(mofIdRef, operation, endpoint);
                    String pmKey = PM.getEndpointKey(pm.getServiceName(), pm.getEndpointName());
                    PM prevValue = suManager.getPMs().putIfAbsent(pmKey, pm);
                    if (prevValue == null) {
                        pmKeys.add(pmKey);
                    } else {
                        mLogger.log(Level.WARNING, "SNMPBC-E00313.CANNOT_ACTIVATE_PM_WITH_SERVICENAME_AND_ENDPOINTNAME",
                                    new Object[] {pm.getServiceName(), pm.getEndpointName()});
                        continue;
                    }
                }
            }
        }
        
        // start MOFs at the end, so they will listen when Adaptations are registered
        for (Iterator<String> iter = mofIds.iterator(); iter.hasNext(); ) {
            String mofId = iter.next();
            MOF mof = suManager.getMofs().get(mofId);
            if (mof == null) {
                mLogger.log(Level.SEVERE, "SNMPBC-E00308.CANNOT_START_MOF_INVALID_MOFID", mofId);
                continue;
            }
            try {
                mof.start();
            } catch (Exception e) {
                mLogger.log(Level.SEVERE, mMessages.getString("SNMPBC-E00314.ERROR_STARTING_MOF", mof.getId()), e);
                continue;
            }
        }
        
        // register PM endpoints after MOFs
        for (Iterator<String> iter = pmKeys.iterator(); iter.hasNext(); ) {
            String pmKey = iter.next();
            PM pm = suManager.getPMs().get(pmKey);
            if (pm == null) {
                mLogger.log(Level.SEVERE, "SNMPBC-E00315.CANNOT_ACTIVATE_PM_INVALID_PMKEY", pmKey);
                continue;
            }
            MOF mof = suManager.getMofs().get(pm.getMOFIdRef());
            if (mof == null) {
                mLogger.log(Level.WARNING, "SNMPBC-E00316.NON_EXISTENT_MOF_WITH_MOFIDREF", pm.getMOFIdRef());
            }
            try {
                pm.activatePM(mContext);
            } catch (JBIException je) {
                mLogger.log(Level.SEVERE, "SNMPBC-EOO317.ERROR_ACTIVATING_ENDPOINT_WITH_PMKEY", pmKey);
                continue;
            }
        }
    }

    /**
     * Stops this ServiceUnit.  This involves deactivating
     * all Endpoints that are part of this ServiceUnit.  All
     * Endpoints are moved into the EndpointState.STOPPED state.
     * <p>
     * TODO: What should happen if not all Endpoints deactivate?
     * Unlike the activate() method, I'm NOT going to assume
     * this is transactional.  It seems silly to deactivate a number of
     * Endpoint, and if one fails, re-activate them.  I'll just throw
     * an error, and have the user decide how to deal with it.
     *
     * @exception    JBIException if any Endpoint fails
     * to deactivate
     */
    public synchronized void stop() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, 
                        "SNMPBC-R00304.STOP_CALLED",
                        suId);        
        }
        
        // deactivate all PMs that is part of this ServiceUnit
        for (Iterator<String> iter = pmKeys.iterator(); iter.hasNext(); ) {
            String pmKey = iter.next();
            PM pm = suManager.getPMs().remove(pmKey);
            if (pm == null) {
                mLogger.log(Level.SEVERE, "SNMPBC-E00318.CANNOT_DEACTIVATE_PM_INVALID_PMKEY",
                            pmKey);
                continue;
            }
            try {
                pm.deactivatePM(mContext);
            } catch (JBIException je) {
                mLogger.log(Level.SEVERE, "SNMPBC-EOO319.ERROR_DEACTIVATING_ENDPOINT_WITH_PMKEY",
                        pmKey);
                continue;
            }
        }
        pmKeys.clear();
        
        // stop all MOFs that is part of this ServiceUnit
        for (Iterator<String> iter = mofIds.iterator(); iter.hasNext(); ) {
            String mofId = iter.next();
            MOF mof = suManager.getMofs().remove(mofId);
            if (mof == null) {
                mLogger.log(Level.SEVERE, "SNMPBC-E00320.CANNOT_STOP_PM_INVALID_PMKEY", mofId);
                continue;
            }
            try {
                mof.stop();
            } catch (Exception e) {
                mLogger.log(Level.SEVERE,
                            mMessages.getString("SNMPBC-EOO321.ERROR_STOPPING_ENDPOINT_WITH_PMKEY", mof.getId()),
                            e);
                continue;
            }
        }
        mofIds.clear();
        
        // remove all adaptations from global list
        for (Iterator<String> iter = adaptationIds.iterator(); iter.hasNext(); ) {
            String adaptationId = iter.next();
            SNMPAdaptation adaptation = suManager.getAdaptations().remove(adaptationId);
            if (adaptation == null) {
                mLogger.log(Level.SEVERE, "SNMPBC-E00322.CANNOT_STOP_ADAPTATION_INVALID_ADAPTATIIONID",
                            adaptationId);
                continue;
            }
        }
        adaptationIds.clear();
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, 
                        "SNMPBC-R00305.STOP_SUCCEEDED",
                        suId);
        }
    }

    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public synchronized void shutdown() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, 
                        "SNMPBC-R00306.SHUTDOWN_CALLED",
                        suId);        
        }
        
        // remove all endpoints
        mEndpoints.clear();
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, 
                        "SNMPBC-R00307.SHUTDOWN_SUCCEEDED",
                        suId);        
        }
    }

    /**
     * Retrieves an Endpoint from this ServiceUnit 
     *
     * @param        serviceName the name of the service
     * @param        endpointName the name of the Endpoint to retrieve
     * @param        endpointType the endpoint type, either 
     *               Endpoint.EndpointType.INBOUND or Endpoint.EndpointType.OUTBOUND
     * @return      The Endpoint instance or null if no endpoint is found
     *               that matches the given endpointName and serviceName.
     */
    public Endpoint getEndpoint(String serviceName, 
                                String endpointName,
                                int endpointType) {

        Endpoint endpoint = mEndpoints.get(getEndpointKey(serviceName,
                                                           endpointName,
                                                           endpointType));                
        return endpoint;
    }

    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     *
     * @return       the set of Endpoints
     */
    public Collection<Endpoint> getEndpoints() {
        return Collections.unmodifiableCollection(mEndpoints.values());
    }
    
    private String getEndpointKey(Endpoint endpoint) {
        return getEndpointKey (endpoint.getServiceName().toString(),
                               endpoint.getEndpointName(),
                               endpoint.getEndpointType());
    }
    
    private String getEndpointKey(String serviceName, 
                                  String endpointName,
                                  int endpointType) {
        return suId + 
               serviceName + 
               endpointName +
               Endpoint.endpointTypeToString(endpointType);               
    }
    
}
