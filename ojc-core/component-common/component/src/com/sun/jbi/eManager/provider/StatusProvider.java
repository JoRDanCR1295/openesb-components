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
 * @(#)StatusProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.eManager.provider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.management.MalformedObjectNameException;
import javax.management.MBeanException;
import javax.management.ObjectName;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.TabularData;

/**
 *
 * @author Sun Microsystems
 */
public class StatusProvider implements StatusReporting, StatusProviderMBean {

//    private final static Logger logger = Logger.getLogger(StatusProvider.class.getName());

    /** A short display name for this component */
    String shortDisplayName;

    //////////////////////////////////////////////////////
    // members that track status information
    //////////////////////////////////////////////////////
    /**  Map from the endpoint name to the relevant EndpointStatus object */
    protected Map endpointStatusMap = Collections.synchronizedMap(new HashMap());
    /**  The list of provisioning endpoints for that component */
    protected List provisioningEndpoints = Collections.synchronizedList(new ArrayList());
    /**  The list of consuming endpoints for that component */
    protected List consumingEndpoints = Collections.synchronizedList(new ArrayList());

    protected volatile String [] performanceMeasurementCategories = new String[0];
    
    protected PerformanceMeasurement performanceMeasurementCallback = null;
    
    
    /**
     * Instantiate object
     */
    public StatusProvider(String aShortDisplayName) {
        this.shortDisplayName = aShortDisplayName;
    }

    /**
     * Retrieves a short display name, should be kept as succinct as possible
     * - as a guideline max 16 characters
     */
    public String getShortDisplayName() {
        return this.shortDisplayName;
    }

    /**
     * Constructs the Object Name of this MBean given a component type and name
     * @param componentType
     * @param componentName
     * @return
     * @throws MalformedObjectNameException
     * @throws NullPointerException
     */
    public static ObjectName constructObjectName(String componentType, String componentName) throws MalformedObjectNameException, NullPointerException {
        ObjectName objectName = null;
        String objectNameString = null;
        String type = null;
        if(componentType != null) {
            if(componentType.equals("Binding") ||
                    componentType.equals("bindingComponents")) {
                type = "bindingComponents";
            } else
                if(componentType.equals("Engine") ||
                    componentType.equals("engineComponents")) {
                type = "serviceEngines";
                } else
                    if(componentType.equals("Namespace") ||
                    componentType.equals("sharedLibraries")) {
                type = "sharedLibraries";
                    } else
                        if(componentType.equals("serviceAssemblies") ||
                    componentType.equals("Deployment")) {
                type = "serviceAssemblies";
                        }
        }
        if((type != null) &&
                (componentName != null)) {
            objectNameString = OBJECT_NAME_PREFIX+
                    type+
                    OBJECT_NAME_SUFFIX+
                    componentName;
            objectName = new ObjectName(objectNameString);
        }
        return objectName;
    }

    /**
     * @return Returns the consumingEndpoints.
     */
    public String[] getConsumingEndpoints() {
        return (String[]) consumingEndpoints.toArray(new String[0]);
    }

    /**
     * @return Returns the provisioningEndpoints.
     */
    public String[] getProvisioningEndpoints() {
        return (String[]) provisioningEndpoints.toArray(new String[0]);
    }

    /**
     * The total number of requests that have been handled
     * @return Returns the totalDone.
     */
    public Long getTotalDone() {
        long received = getTotalReceivedDones().longValue();
        long sent = getTotalSentDones().longValue();
        return Long.valueOf(received + sent);
    }

    /**
     * The total number of messages with faults
     * @return Returns the totalErrors.
     */
    public Long getTotalErrors() {
        long received = getTotalReceivedErrors().longValue();
        long sent = getTotalSentErrors().longValue();
        return Long.valueOf(received + sent);

    }

    /**
     * @return Returns the totalReceivedDones.
     */
    public Long getTotalReceivedDones() {
        long total = 0;
        Iterator valueIter = endpointStatusMap.values().iterator();
        synchronized(endpointStatusMap) {
            while (valueIter.hasNext()) {
                EndpointStatus value = (EndpointStatus) valueIter.next();
                total += value.getReceivedDones();
            }
        }
        return Long.valueOf(total);
    }

    /**
     * @return Returns the totalReceivedErrors.
     */
    public Long getTotalReceivedErrors() {
        long total = 0;
        Iterator valueIter = endpointStatusMap.values().iterator();
        synchronized(endpointStatusMap) {
            while (valueIter.hasNext()) {
                EndpointStatus value = (EndpointStatus) valueIter.next();
                total += value.getReceivedErrors();
            }
        }
        return Long.valueOf(total);
    }

    /**
     * @return Returns the totalReceivedReplies.
     */
    public Long getTotalReceivedReplies() {
        long total = 0;
        Iterator valueIter = endpointStatusMap.values().iterator();
        synchronized(endpointStatusMap) {
            while (valueIter.hasNext()) {
                EndpointStatus value = (EndpointStatus) valueIter.next();
                total += value.getReceivedReplies();
            }
        }
        return Long.valueOf(total);
    }

    /**
     * @return Returns the totalReceivedRequests.
     */
    public Long getTotalReceivedRequests() {
        long total = 0;
        Iterator valueIter = endpointStatusMap.values().iterator();
        synchronized(endpointStatusMap) {
            while (valueIter.hasNext()) {
                EndpointStatus value = (EndpointStatus) valueIter.next();
                total += value.getReceivedRequests();
            }
        }
        return Long.valueOf(total);
    }

    /**
     * @return Returns the totalReplies.
     */
    public Long getTotalReplies() {
        long received = getTotalReceivedReplies().longValue();
        long sent = getTotalSentReplies().longValue();
        return Long.valueOf(received + sent);

    }

    /**
     * @return Returns the totalRequests.
     */
    public Long getTotalRequests() {
        long received = getTotalReceivedRequests().longValue();
        long sent = getTotalSentRequests().longValue();
        return Long.valueOf(received + sent);
    }

    /**
     * @return Returns the totalSentDones.
     */
    public Long getTotalSentDones() {
        long total = 0;
        Iterator valueIter = endpointStatusMap.values().iterator();
        synchronized(endpointStatusMap) {
            while (valueIter.hasNext()) {
                EndpointStatus value = (EndpointStatus) valueIter.next();
                total += value.getSentDones();
            }
        }
        return Long.valueOf(total);
    }

    /**
     * @return Returns the totalSentErrors.
     */
    public Long getTotalSentErrors() {
        long total = 0;
        Iterator valueIter = endpointStatusMap.values().iterator();
        synchronized(endpointStatusMap) {
            while (valueIter.hasNext()) {
                EndpointStatus value = (EndpointStatus) valueIter.next();
                total += value.getSentErrors();
            }
        }
        return Long.valueOf(total);
    }

    /**
     * @return Returns the totalSentReplies.
     */
    public Long getTotalSentReplies() {
        long total = 0;
        Iterator valueIter = endpointStatusMap.values().iterator();
        synchronized(endpointStatusMap) {
            while (valueIter.hasNext()) {
                EndpointStatus value = (EndpointStatus) valueIter.next();
                total += value.getSentReplies();
            }
        }
        return Long.valueOf(total);
    }

    /**
     * @return Returns the totalSentRequests.
     */
    public Long getTotalSentRequests() {
        long total = 0;
        Iterator valueIter = endpointStatusMap.values().iterator();
        synchronized(endpointStatusMap) {
            while (valueIter.hasNext()) {
                EndpointStatus value = (EndpointStatus) valueIter.next();
                total += value.getSentRequests();
            }
        }
        return Long.valueOf(total);
    }


        /* (non-Javadoc)
         * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getSentRequests(java.lang.String)
         */
    public Long getSentRequests(String endpoint) {
        EndpointStatus stat = getEndpointStatus(endpoint);
        return Long.valueOf(stat == null ? 0 : stat.getSentRequests());
    }

        /* (non-Javadoc)
         * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getSentReplies(java.lang.String)
         */
    public Long getSentReplies(String endpoint) {
        EndpointStatus stat = getEndpointStatus(endpoint);
        if (stat == null) {
            return Long.valueOf(0);
        } else {
            return Long.valueOf(stat.getSentReplies());
        }
    }

        /* (non-Javadoc)
         * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getSentErrors(java.lang.String)
         */
    public Long getSentErrors(String endpoint) {
        EndpointStatus stat = getEndpointStatus(endpoint);
        return Long.valueOf(stat == null ? 0 : stat.getSentErrors());
    }

        /* (non-Javadoc)
         * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getSentDones(java.lang.String)
         */
    public Long getSentDones(String endpoint) {
        EndpointStatus stat = getEndpointStatus(endpoint);
        return Long.valueOf(stat == null ? 0 : stat.getSentDones());
    }

        /* (non-Javadoc)
         * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getReceivedRequests(java.lang.String)
         */
    public Long getReceivedRequests(String endpoint) {
        EndpointStatus stat = getEndpointStatus(endpoint);
        return Long.valueOf(stat == null ? 0 : stat.getReceivedRequests());
    }

        /* (non-Javadoc)
         * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getReceivedReplies(java.lang.String)
         */
    public Long getReceivedReplies(String endpoint) {
        EndpointStatus stat = getEndpointStatus(endpoint);
        return Long.valueOf(stat == null ? 0 : stat.getReceivedReplies());
    }

        /* (non-Javadoc)
         * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getReceivedErrors(java.lang.String)
         */
    public Long getReceivedErrors(String endpoint) {
        EndpointStatus stat = getEndpointStatus(endpoint);
        return Long.valueOf(stat == null ? 0 : stat.getReceivedErrors());
    }

        /* (non-Javadoc)
         * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getReceivedDones(java.lang.String)
         */
    public Long getReceivedDones(String endpoint) {
        EndpointStatus stat = getEndpointStatus(endpoint);
        return Long.valueOf(stat == null ? 0 : stat.getReceivedDones());
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusReporting#getEndpointStatus(java.lang.String)
     */
    public EndpointStatus getEndpointStatus(String endpoint) {
        return (EndpointStatus) this.endpointStatusMap.get(endpoint);
    }

    public void addConsumingEndpoints(String[] consumingEndpts) {
        for (int epCount = 0; epCount < consumingEndpts.length; epCount ++) {
            endpointStatusMap.put(consumingEndpts[epCount], new EndpointStatusImpl());
        }
        this.consumingEndpoints.addAll(Arrays.asList(consumingEndpts));
    }

    public void addConsumingEndpoint(String consumingEndpoint) {
        if (!consumingEndpoints.contains(consumingEndpoint)) {
            endpointStatusMap.put(consumingEndpoint, new EndpointStatusImpl());
            this.consumingEndpoints.add(consumingEndpoint);
        }
    }

    public void removeConsumingEndpoints(String[] consumingEndpts) {
        this.consumingEndpoints.removeAll(Arrays.asList(consumingEndpts));
        for (int epCount = 0; epCount < consumingEndpts.length; epCount ++) {
//            EndpointStatus removedEP = (EndpointStatus) 
            endpointStatusMap.remove(consumingEndpts[epCount]);
        }
    }

    public void addProvisioningEndpoints(String[] provEndpoints) {
        for (int epCount = 0; epCount < provEndpoints.length; epCount ++) {
            endpointStatusMap.put(provEndpoints[epCount], new EndpointStatusImpl());
        }
        this.provisioningEndpoints.addAll(Arrays.asList(provEndpoints));
    }

    public void addProvisioningEndpoint(String provisioningEndpoint) {
        if (!provisioningEndpoints.contains(provisioningEndpoint)) {
            endpointStatusMap.put(provisioningEndpoint, new EndpointStatusImpl());
            this.provisioningEndpoints.add(provisioningEndpoint);
        }
    }

    public void removeProvisioningEndpoints(String[] provisioningEndpts) {
        this.provisioningEndpoints.removeAll(Arrays.asList(provisioningEndpts));
        for (int epCount = 0; epCount < provisioningEndpts.length; epCount ++) {
//            EndpointStatus removedEP = (EndpointStatus) 
            endpointStatusMap.remove(provisioningEndpts[epCount]);
        }
    }

    
    /** 
     * Retrieves the performance insturmentation measurement for the specified endpoint. 
     * @param endpoint The endpoint name qualified by service name.
     * @return An instance of TabularData which holds the performance instrumentation measurement for the specified endpoint.
     * @throws OpenDataException upon error.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public TabularData getPerformanceInstrumentationMeasurement(String endpoint) throws OpenDataException, MBeanException {
        if (this.performanceMeasurementCallback==null) {
            throw new MBeanException(new Exception("measurement not supported for " + shortDisplayName));            
        }
        if (endpointStatusMap.containsKey(endpoint)) {
            return performanceMeasurementCallback.getPerformanceInstrumentationMeasurement(endpoint);
        } else {
            throw new MBeanException(new Exception(endpoint + " not found"));
        }
    }
    
    
    /** 
     * Resets the performance measurements on the endpoint.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public void clearPeformaceInstrumentationMeasurement (String endpoint) throws MBeanException {
        if (this.performanceMeasurementCallback==null) {
            throw new MBeanException(new Exception("measurement not supported for " + shortDisplayName));
        }
        if (endpointStatusMap.containsKey(endpoint)) {
            performanceMeasurementCallback.clearPeformaceInstrumentationMeasurement(endpoint);
        } else {
            throw new MBeanException(new Exception(endpoint));
        }
    }

    
    /**
     * Gets the performance instrumenation measurement categories supported by a component.
     * @return An array of String - each entry in the array specifies the category of measurement contained in the performance instrumenation measurement TabularData returned by the component.
     */
    public String[] getPerformanceMeasurementCategories() {
        return performanceMeasurementCategories;
    }
    
    
    /** 
     * Sets the performance instrumenation measurement categories supported by a component.
     * @param categories An array of String - each entry in the array specifies the category of measurement to be contained in the performance instrumenation measurement TabularData returned by the component.
     */
    public void setPerformanceMeasurementCategories(String[] categories) {
        performanceMeasurementCategories = categories;
    }
    
    
    public void setPeformanceMeasurementCallback (PerformanceMeasurement callback) {
        performanceMeasurementCallback = callback;
    }
        
    /** Retrieves the WSDLs and XSDs associated with the specified endpoint. */
    public String getWSDLDefinition(String endpoint) throws MBeanException {
        if (endpointStatusMap.get(endpoint) == null) {
            throw new MBeanException(new Exception("Unable to locate an activated endpoint with unique name: " + endpoint));
        }
        EndpointStatus status = (EndpointStatus) endpointStatusMap.get(endpoint);
        return status.getWSDLDefinition();
    }
    
    public String getWSDLImportedResource(String endpoint, String targetNamespace) throws MBeanException {
        if (endpointStatusMap.get(endpoint) == null) {
            throw new MBeanException(new Exception("Unable to locate an activated endpoint with unique name: " + endpoint));
        }
        EndpointStatus status = (EndpointStatus)endpointStatusMap.get(endpoint);
        return status.getWSDLImportedResource(targetNamespace);
    }
    
}
