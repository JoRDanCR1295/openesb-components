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
 * @(#)HL7BCManagement.java
 *                                                                                                                
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.                                                
 *                                                                                                                
 * END_HEADER - DO NOT EDIT                                                                                       
 */

package com.sun.jbi.hl7bc.management;

import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnection;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObjectFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.SequenceNumDBO;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.HashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.management.MBeanException;
import javax.jbi.JBIException;
import com.sun.jbi.alerter.NotificationEvent;

import javax.jbi.component.ComponentContext;
import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.HL7BindingComponent;
import com.sun.jbi.hl7bc.HL7BindingDeployer;
import com.sun.jbi.hl7bc.HL7ComponentContext;
import com.sun.jbi.hl7bc.ServiceUnit;
import com.sun.jbi.hl7bc.util.AlertsUtil;
import com.sun.jbi.hl7bc.I18n;
import javax.naming.InitialContext;

/**
 * The Management MBean implementation to suspend and resume endpoints.
 */
public class HL7BCManagement implements HL7BCManagementMBean {

    private Logger mLogger = Logger.getLogger(HL7BCManagement.class.getName());

    private HL7BindingDeployer deployer;
    private Properties config;
    private DBConnectionFactory dbConnectionFactory;
    private DBObjectFactory dbObjectFactory;

    private ComponentContext context;


    public HL7BCManagement(HL7BindingDeployer deployer, Properties config, ComponentContext context) {
        this.deployer = deployer;
        this.context = context;
        this.config = config;

        try {
            InitialContext ic = context.getNamingContext();
            dbConnectionFactory = new DBConnectionFactory(config, ic, context.getInstallRoot());
            dbObjectFactory = DBObjectFactory.getDBObjectFactory(dbConnectionFactory.getType());
        } catch (Exception e) {
            e.printStackTrace();
        }

    }


    /**
     * Suspend a endpoint
     * 
     * @param endpointName a unique endpoint identifier
     * @throws MBeanException on error
     */
    public boolean suspend(String endpointName) throws MBeanException {
        boolean result = false;
        if (endpointName == null || endpointName.equals("")) {
            throw new MBeanException(new Exception(I18n.msg("E0133: Invalid endpoint name: ''{0}''",
                    endpointName)));
        }
        Endpoint endpoint = findEndpoint(endpointName);
        if (endpoint == null) {
            throw new MBeanException(new Exception(I18n.msg("E0134: Failed to find the endpoint with name [{0}]",
                    endpointName)));
        }
        try {
            endpoint.getServiceUnit().suspend(endpoint);
            result = true;
        } catch (Exception e) {
            throw new MBeanException(e, I18n.msg("E0135: Failed to suspend the endpoint with unqiue ID of [{0}]", endpointName)
                    + e.getMessage());
        }
        return result;

    }

    /**
     * Resume a endpoint
     * 
     * @param endpointName a unique endpoint identifier 
     * @throws MBeanException on error
     */
    public boolean resume(String endpointName) throws MBeanException {
        boolean isResumed = false;
        if (endpointName == null || endpointName.equals("")) {
            throw new MBeanException(new Exception(I18n.msg("E0133: Invalid endpoint name: ''{0}''",
                    endpointName)));
        }
        Endpoint endpoint = findEndpoint(endpointName);
        if (endpoint == null) {
            throw new MBeanException(new Exception(I18n.msg("E0134: Failed to find the endpoint with name [{0}]",
                    endpointName)));
        }

        try {
            endpoint.getServiceUnit().resume(endpoint);
            isResumed = true;
        } catch (Exception e) {
            throw new MBeanException(e, I18n.msg("E0135: Failed to suspend the endpoint with unqiue ID of [{0}]", endpointName)
                    + e.getMessage());
        }
        return isResumed;
    }

    /**
     * Indicates if the named endpoint is active (i.e., not suspended).
     *
     * @param endpointName Endpoint identification. The identifier is not
     * necessarily a canonical name; it must be in the set of identifiers
     * returned by a preceding call to {@link #listActiveEndpoints()} or {@link
     * #listInactiveEndpoints()}.
     *
     * @return true if the specified endpoint is active.
     * @throws javax.management.MBeanException if the status of the specified
     * endpoint cannot be obtained for any reason.
     */
    public boolean isEndpointActive(String endpointName) throws MBeanException {
        boolean isActive = false;
        Set<Endpoint> endpoints = getActiveEndpoints();
        for (Endpoint endpoint : endpoints) {
            if (endpointName.equals(makeEndpointIdentifier(endpoint))) {
                isActive = true;
                break;
            }
        }
        return isActive;
    }

    /**
     * Returns identifiers for all active managed endpoints.
     *
     * @return An array with zero or more endpoint identifiers.
     */
    public String[] listActiveEndpoints() {
        Collection<String> endpoints = createActiveEndpointNames();
        return endpoints.toArray(new String[endpoints.size()]);
    }

    /**
     * Returns identifiers for all suspended managed endpoints.
     *
     * @return An array with zero or more endpoint identifiers.
     */
    public String[] listInactiveEndpoints() {
        Collection<String> endpoints = createInactiveEndpointNames();
        return endpoints.toArray(new String[endpoints.size()]);
    }

    public List<String> listEndpoints() {
        List<String> endpointsList = new ArrayList<String>();
        Set<Endpoint> endpoints = getActiveEndpoints();
        endpoints.addAll(getInactiveEndpoints());
        for (Endpoint endpoint : endpoints ) {
            endpointsList.add(makeEndpointIdentifier(endpoint));
        }
        return endpointsList;
    }

    public List<String> getEndpoints(String serviceUnit) {
        List<String> endpointsList = new ArrayList<String>();

        if (deployer != null) {
            for (ServiceUnit su : deployer.getServiceUnits()) {
                for (Endpoint endpoint : su.getEndpoints()) {
                    try {
                        if ( serviceUnit.equals(su.getServiceUnitId()) ) {
                            endpointsList.add(makeEndpointIdentifier(endpoint));
                        }
                    } catch (Exception e) {
                        String msg = "Failed to get list of endpoints for service unit " +
							          su.getServiceUnitId();
                        if (mLogger.isLoggable(Level.WARNING)) {
                            mLogger.log(Level.WARNING, msg);
                        }
                        AlertsUtil.getAlerter().minor(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                                AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_STARTED, NotificationEvent.EVENT_TYPE_ALERT,
                                "HL7BC-W0106");
                    }
                }
            }
        }
        return endpointsList;
    }


    /** Returns identifiers for all suspended managed endpoints.
      *
      * @return An array with zero or more endpoint identifiers.
      */
    public List<String> listServiceUnits() {
        List<String> suList = new ArrayList<String>();
        if (deployer != null) {
            for (ServiceUnit su : deployer.getServiceUnits()) {
                suList.add(su.getServiceUnitId());
            }
        }
        return suList;
    }



    /**
     * Generates unique names for a given endpoint.  The default implementation
     * is to create a name that is the concatenation of the fully qualified
     * service name, a comma, and the endpoint name.
     *
     * @param endpoint The endpoint for which to generate an identifier.
     *
     * @return A name for the endpoint.
     */
    private String makeEndpointIdentifier(Endpoint endpoint) {
        assert endpoint != null;
        return endpoint.getServiceName().toString() + ',' + endpoint.getEndpointName();
    }

    /**
     * Return the set of all non-suspended endpoints.
     * 
     * @return Set of endpoints. The set will not be modified, and thus may be immutable.
     */
    private Set<Endpoint> getActiveEndpoints() {
        Set<Endpoint> endpoints = new HashSet<Endpoint>();
        HL7BindingDeployer deployer = this.deployer;
        if (deployer != null) {
            for (ServiceUnit su : deployer.getServiceUnits()) {
                for (Endpoint endpoint : su.getEndpoints()) {
                    try {
                        if (!su.isSuspended(endpoint)) {
                            endpoints.add(endpoint);
                        }
                    } catch (JBIException e) {
                        String msg = I18n.msg("W0105: Failed to obtain state of service endpoint {0},{1} during attempted enumeration of active endpoints", endpoint.getServiceName().toString(),
							endpoint.getEndpointName() );
                        if (mLogger.isLoggable(Level.WARNING)) {
                            mLogger.log(Level.WARNING, msg);
                        }
                        AlertsUtil.getAlerter().minor(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                                AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_STARTED, NotificationEvent.EVENT_TYPE_ALERT,
                                "HL7BC-W0105");
                    }
                }
            }
        }
        return endpoints;
    }

    /**
     * Return the set of all suspended endpoints.
     *
     * @return Set of endpoints. The set will not be modified, and thus may be
     *         immutable.
     */
    private Set<Endpoint> getInactiveEndpoints() {
        Set<Endpoint> endpoints = new HashSet<Endpoint>();
        HL7BindingDeployer deployer = this.deployer;
        if (deployer != null) {
            for (ServiceUnit su : deployer.getServiceUnits()) {
                for (Endpoint endpoint : su.getEndpoints()) {
                    try {
                        if (su.isSuspended(endpoint)) {
                            endpoints.add(endpoint);
                        }
                    } catch (JBIException e) {
                        String msg = I18n.msg("W0106: Failed to obtain state of service endpoint {0},{1} during attempted enumeration of inactive endpoints",
							          endpoint.getServiceName().toString(), endpoint.getEndpointName());
                        if (mLogger.isLoggable(Level.WARNING)) {
                            mLogger.log(Level.WARNING, msg);
                        }
                        AlertsUtil.getAlerter().minor(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                                AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_STARTED, NotificationEvent.EVENT_TYPE_ALERT,
                                "HL7BC-W0106");
                    }
                }
            }
        }
        return endpoints;
    }

    private Collection<String> createActiveEndpointNames() {
        Set<Endpoint> endpoints = getActiveEndpoints();
        List<String> endpointNames = new LinkedList<String>();
        for (Endpoint endpoint : endpoints) {
            endpointNames.add(makeEndpointIdentifier(endpoint));
        }
        return endpointNames;
    }

    private Collection<String> createInactiveEndpointNames() {
        Set<Endpoint> endpoints = getInactiveEndpoints();
        List<String> endpointNames = new LinkedList<String>();
        for (Endpoint endpoint : endpoints) {
            endpointNames.add(makeEndpointIdentifier(endpoint));
        }
        return endpointNames;
    }

    private Endpoint findEndpoint(String endpointName) {
        assert endpointName != null;
        Endpoint target = null;
        Set<Endpoint> endpoints = getActiveEndpoints();
        endpoints.addAll(getInactiveEndpoints());
        for (Endpoint endpoint : endpoints) {
            if (endpointName.equals(makeEndpointIdentifier(endpoint))) {
                target = endpoint;
                break;
            }
        }
        return target;
    }

    private String getServiceUnitPath(String serviceUnit) {
        String suPath = "";
        if ( deployer!=null ) {
            for ( ServiceUnit su : deployer.getServiceUnits() ) {
                String suId = su.getServiceUnitId();
                if ( suId.equals(serviceUnit) ) {
                    return su.getServiceUnitPath();
                }
            }
        }
        return suPath;
    }

    private DBConnection getDBConnection() throws SQLException, Exception {
        return dbConnectionFactory.createConnection();
    }

    public long getSequenceNumber(String serviceUnit) throws MBeanException {
        int seqNum = -1;
        DBConnection dbConnection = null;
        try {
            dbConnection = getDBConnection();
            // queryKey is serviceUnitPath

            String queryKey = getServiceUnitPath(serviceUnit);
            SequenceNumDBO seqNoDBO = dbObjectFactory.createSequenceNumDBO(queryKey);
            ResultSet rs = dbConnection.getRow(seqNoDBO);
            if (rs.next()) {
                seqNoDBO.populateDBO(rs);
                seqNum = seqNoDBO.getESN();
            }
        } catch(Exception e ) {
            throw new MBeanException(e);
        } finally {
            try {
            if (dbConnection != null) {
                dbConnection.close();
            }
            } catch(Exception e) {
                e.printStackTrace();
            }
        }
        return seqNum;
    }


}
