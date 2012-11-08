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
 * @(#)SunStatisticsService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.statistics.providers;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.connectors.ServerConnector;
import com.sun.jbi.cam.manager.framework.common.DeploymentDescriptorsProcessor;
import com.sun.jbi.cam.model.management.Statistics;
import com.sun.jbi.cam.services.BaseServiceProvider;
import com.sun.jbi.cam.services.statistics.StatisticsService;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.management.ObjectName;

/**
 *
 * @author ylee
 *
 *  @todo 
 *      1 - filter provisioning and consuming endpoints by component (e.g SE/BC/SU)
 *
 */
public class SunStatisticsService extends BaseServiceProvider implements Serializable, StatisticsService {

    private static final String DOMAIN_NAME_PREFIX = "com.sun.ebi:";                    //$NON-NLS-1$
    private static final String SERVICE_TYPE_PREFIX="ServiceType=Status";               //$NON-NLS-1$
    private static final String INSTALLATION_TYPE_PREFIX = "InstallationType=";         //$NON-NLS-1$
    private static final String IDENTIFICATION_NAME_PREFIX = "IdentificationName=";     //$NON-NLS-1$
    private enum EndpointType { CONSUMER, PROVIDER }; 
    
    
    private long sentRequests;
    private long sentReplies;
    private long sentErrors;
    private long sentDones;

    private long receivedRequests;
    private long receivedReplies;
    private long receivedErrors;
    private long receivedDones;
   
    /** Creates a new instance of StatisticsService */
    public SunStatisticsService(ServerConnector connector,String targetName) {
        super(connector,targetName);
    }
    
    public long getReceivedDones() {
        return receivedDones;
    }
    
    public long getReceivedErrors() {
        return receivedErrors;
    }
    
    public long getReceivedReplies() {
        return receivedReplies;
    }
    
    public long getReceivedRequests() {
        return receivedRequests;
    }
    
    public long getSentDones() {
        return sentDones;
    }
    
    public long getSentErrors() {
        return sentErrors;
    }
    
    public long getSentReplies() {
        return sentReplies;
    }
    
    public long getSentRequests() {
        return sentRequests;
    }    
    
    private void reset() {
        sentRequests = 0;
        sentReplies  = 0;
        sentErrors   = 0;
        sentDones    = 0;
        receivedRequests = 0;
        receivedReplies  = 0;
        receivedErrors   = 0;
        receivedDones    = 0;        
    }
    
    
    private String getObjectName(String componentName, String componentType) {
        String name =
                    DOMAIN_NAME_PREFIX +
                    SERVICE_TYPE_PREFIX  + GenericConstants.COMMA_SEPARATOR +
                    INSTALLATION_TYPE_PREFIX  + Util.mapType(componentType) + GenericConstants.COMMA_SEPARATOR +
                    IDENTIFICATION_NAME_PREFIX + componentName;
        return name;
    }
    
    
    /**  Retrieves the list of provisioning endpoints for that component
     */
    public String[] getProvisioningEndpoints(String componentName,String componentType) {
        String[] result = null;
        try {
            String name = getObjectName(componentName, componentType);
            ObjectName objectName = new ObjectName(name);
            result = (String[])invoke(objectName,"getProvisioningEndpoints", //$NON-NLS-1$
                                       null);
        } catch(Exception e){
            e.printStackTrace();
        }
        return result;
    }    

    
    /**  Retrieves the list of consuming endpoints for that component
     */
    public String[] getConsumingEndpoints(String componentName,String componentType) {
        String[] result = null;
        try {
            String name = getObjectName(componentName, componentType);
            ObjectName objectName = new ObjectName(name);
            result = (String[])invoke(objectName,"getConsumingEndpoints", //$NON-NLS-1$
                                       null);
        } catch(Exception e){
            e.printStackTrace();
        }
        return result;
    }    
    
    
    public Statistics getStatisticsEndpoint(String endpoint, String componentName, String componentType) {
          // retrieve statistics from mbean
        reset();
        String name = getObjectName(componentName, componentType);
        try {
            ObjectName objectName = new ObjectName(name);
            
            if ( this.serverConnection != null &&  serverConnection.isRegistered(objectName) ) {
                 // todo - this only get the totals - not individual endpoints
                 Object[] params = {endpoint};
                 receivedErrors = invokeLong(objectName, "getReceivedErrors",params);
                 receivedDones = invokeLong(objectName, "getReceivedDones",params);
                 receivedReplies = invokeLong(objectName, "getReceivedReplies",params);
                 receivedRequests = invokeLong(objectName, "getReceivedRequests",params);
                 sentDones = invokeLong(objectName, "getSentDones",params);
                 sentErrors = invokeLong(objectName, "getSentErrors",params);
                 sentReplies = invokeLong(objectName, "getSentReplies",params);
                 sentRequests = invokeLong(objectName, "getSentRequests",params);
            }
            
        } catch(Exception e) {
            e.printStackTrace();
        }
        Statistics stats = new Statistics(endpoint, sentRequests, sentReplies, sentErrors, sentDones,
                            receivedRequests,receivedReplies,receivedErrors, receivedDones);
        
        return stats;
        
    }
    
    
    public List<Statistics> getProvisioningStatistics(String componentName,String componentType,String cType,String cName,String saName) {
        List<Statistics> list = new ArrayList<Statistics>();

        String name = Util.mapComponentValue(cName,componentName);
        String type = Util.mapComponentValue(cType,componentType);        
        
        // first get provisioning endpoints
        String[] endpoints = getProvisioningEndpoints(name,type);
        if ( endpoints!=null && endpoints.length>0 ) {
            // get SU endpoints
            boolean isSU = GenericConstants.SU_TYPE.equals(componentType);
            Map<String,String> suList = null;
            if ( isSU ) {
                suList = getSuEndpoints(EndpointType.PROVIDER,saName,componentName,targetName);
            }
            for ( int i=0; i<endpoints.length; i++) {
                String endpoint = endpoints[i];
                Statistics stats = null;
                // filter out endpoints that does not belong to SU
                if ( isSU ) {
                    if ( isSuEndpoint(endpoint,suList) ) {
                        stats = getStatisticsEndpoint(endpoint,cName,cType);        
                    }
                } else {
                    stats = getStatisticsEndpoint(endpoint,componentName,componentType);
                }
                if ( stats!=null ) {
                    list.add(stats);
                }
            }
        }
        return list;
    }
    
    
    public List<Statistics> getConsumingStatistics(String componentName,String componentType,String cType,String cName,String saName) {
        List<Statistics> list = new ArrayList<Statistics>();

        String name = Util.mapComponentValue(cName,componentName);
        String type = Util.mapComponentValue(cType,componentType);        
        
        // first get provisioning endpoints
        String[] endpoints = getConsumingEndpoints(name,type);
        if ( endpoints!=null && endpoints.length>0 ) {
            // get SU endpoints
            boolean isSU = GenericConstants.SU_TYPE.equals(componentType);
            Map<String,String> suList = null;
            if ( isSU ) {
                suList = getSuEndpoints(EndpointType.CONSUMER,saName,componentName,targetName);
            }
            for ( int i=0; i<endpoints.length; i++) {
                String endpoint = endpoints[i];
                Statistics stats = null;
                // filter out endpoints that does not belong to SU
                if ( isSU ) {
                    if ( isSuEndpoint(endpoint,suList) ) {
                        stats = getStatisticsEndpoint(endpoint,cName,cType);        
                    }
                } else {
                    stats = getStatisticsEndpoint(endpoint,componentName,componentType);
                }
                if ( stats!=null ) {
                    list.add(stats);
                }
            }
        }
        return list;
    }
    
    
    public Statistics getTotalStatistics(List<Statistics> provisioningStats, List<Statistics> consumingStats) {
        Statistics totalStats = new Statistics();

        // tally provisioning stats
        if ( provisioningStats!=null ) {
            for (Iterator iter=provisioningStats.iterator(); iter.hasNext(); ) {
                Statistics stats = (Statistics)iter.next();
                totalStats.add( stats.getSentRequests(), stats.getSentReplies(), stats.getSentErrors(), stats.getSentDones(),
                                stats.getReceivedRequests(), stats.getReceivedReplies(), stats.getReceivedErrors(), stats.getReceivedDones());
            }
        }
        
        // tally consuming stats
        if ( consumingStats!=null ) {
            for (Iterator iter=consumingStats.iterator(); iter.hasNext(); ) {
                Statistics stats = (Statistics)iter.next();
                totalStats.add( stats.getSentRequests(), stats.getSentReplies(), stats.getSentErrors(), stats.getSentDones(),
                                stats.getReceivedRequests(), stats.getReceivedReplies(), stats.getReceivedErrors(), stats.getReceivedDones());
            }
        }

        return totalStats;
    }
    
    
    private Map<String,String> getSuEndpoints(EndpointType type,String saName,String suName,String targetName) {
        Map<String,String> map = null;
        DeploymentDescriptorsProcessor ddProcessor = new DeploymentDescriptorsProcessor(targetName);      
        try { 
            if ( type==EndpointType.CONSUMER ) {
                map = ddProcessor.getConsumesEndPointListForSU(saName,suName);            
            } else {
                map = ddProcessor.getProvidesEndPointMapForSU(saName,suName);
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        return map;
    }
    
    
    private boolean isSuEndpoint(String endpoint,Map<String,String> suList) {
        boolean valid = false;
        if ( suList!=null ) {
            String endpt = Util.trimRight(endpoint,GenericConstants.COMMA_SEPARATOR);
            if ( suList.get(endpt)!=null ) {
                valid = true;
            }
        }
        return valid;
    }
        
    
    public Statistics getStatistics(String componentName, String componentType) {
        // retrieve statistics from mbean
        reset();
        String name = getObjectName(componentName, componentType);
        
        try {
            ObjectName objectName = new ObjectName(name);
            if ( this.serverConnection != null &&  serverConnection.isRegistered(objectName) ) {
                 // todo - this only get the totals - not individual endpoints
                 Object[] params = {""};
                 receivedErrors = invokeLong(objectName, "getReceivedErrors",params);
                 receivedDones = invokeLong(objectName, "getReceivedDones",params);
                 receivedReplies = invokeLong(objectName, "getReceivedReplies",params);
                 receivedRequests = invokeLong(objectName, "getReceivedRequests",params);
                 sentDones = invokeLong(objectName, "getSentDones",params);
                 sentErrors = invokeLong(objectName, "getSentErrors",params);
                 sentReplies = invokeLong(objectName, "getSentReplies",params);
                 sentRequests = invokeLong(objectName, "getSentRequests",params);
            }
            
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        Statistics stats = new Statistics(sentRequests, sentReplies, sentErrors, sentDones,
                            receivedRequests,receivedReplies,receivedErrors, receivedDones);
        
        return stats;
    }    
    
}
