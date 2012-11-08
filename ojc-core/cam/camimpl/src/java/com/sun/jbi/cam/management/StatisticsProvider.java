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
 * @(#)StatisticsProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.management;

import com.sun.jbi.cam.common.EndpointURL;
import com.sun.jbi.cam.model.management.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

/**
 * @author ylee
 * @author Graj
 *
 */
public class StatisticsProvider implements Serializable {

    private String componentType;
    private String componentName;

    // Has a table of serviceNames to Statistics List
    private Map<Object,List<Statistics>> consumingStatisticsTable = new HashMap<Object,List<Statistics>>();
    private Map<Object,List<Statistics>> provisioningStatisticsTable = new HashMap<Object,List<Statistics>>();

    private StatusProvider statusProvider;

    /**
     *
     */
    public StatisticsProvider() {
    }

    /**
     *
     */
    public StatisticsProvider(StatusProvider provider) {
        if(provider != null) {
            this.statusProvider = provider;
            this.componentName = provider.getComponentName();
            this.componentType = provider.getComponentType();
            this.initialize();
        }
    }

    /**
     *
     *
     */
    public void initialize() {
        String[] endpointStrings = null;
        EndpointURL endpointURL = null;
        if ((statusProvider != null) && (statusProvider.getObjectName() != null)) {
            endpointStrings = statusProvider.getConsumingEndpoints();
            if(endpointStrings != null) {
                for(int index = 0; index < endpointStrings.length; index++) {
                    if(endpointStrings[index] != null) {
                        endpointURL = new EndpointURL(endpointStrings[index]);
                        this.populateConsumerStatisticsTable(endpointURL);
                    }

                }
            }
            endpointStrings = statusProvider.getProvisioningEndpoints();
            if(endpointStrings != null) {
                for(int index = 0; index < endpointStrings.length; index++) {
                    if(endpointStrings[index] != null) {
                        endpointURL = new EndpointURL(endpointStrings[index]);
                        this.populateProviderStatisticsTable(endpointURL);
                    }
                }
            }
        }
    }

    /**
     *
     * @param endpointURL
     */
    private void populateProviderStatisticsTable(EndpointURL endpointURL) {
        if(endpointURL == null) {
            return;
        }
        Statistics stats = null;
        QName qName = endpointURL.getServiceQName();
        String endpoint = endpointURL.getEndpointURL();
        List<Statistics> statisticsList = this.provisioningStatisticsTable.get(endpointURL.getServiceQName());
        if(statisticsList == null) {
            statisticsList = new ArrayList<Statistics>();
        }

        long sentRequests = statusProvider.getSentRequests(endpoint);
        long sentReplies = statusProvider.getSentReplies(endpoint);
        long sentErrors = statusProvider.getSentErrors(endpoint);
        long sentDones = statusProvider.getSentDones(endpoint);

        long receivedRequests = statusProvider.getReceivedRequests(endpoint);
        long receivedReplies = statusProvider.getReceivedReplies(endpoint);
        long receivedErrors = statusProvider.getReceivedErrors(endpoint);
        long receivedDones = statusProvider.getReceivedDones(endpoint);

        stats = new Statistics(endpointURL.getEndpointURL(),
                sentRequests,
                sentReplies,
                sentErrors,
                sentDones,
                receivedRequests,
                receivedReplies,
                receivedErrors,
                receivedDones);

        statisticsList.add(stats);
        this.provisioningStatisticsTable.put(qName, statisticsList);
    }

    /**
     *
     * @param endpointURL
     */
    private void populateConsumerStatisticsTable(EndpointURL endpointURL) {
        if(endpointURL == null) {
            return;
        }
        Statistics stats = null;
        QName qName = endpointURL.getServiceQName();
        String endpoint = endpointURL.getEndpointURL();
        List<Statistics> statisticsList = this.consumingStatisticsTable.get(qName);
        if(statisticsList == null) {
            statisticsList = new ArrayList<Statistics>();
        }
        long sentRequests = statusProvider.getSentRequests(endpoint);
        long sentReplies = statusProvider.getSentReplies(endpoint);
        long sentErrors = statusProvider.getSentErrors(endpoint);
        long sentDones = statusProvider.getSentDones(endpoint);

        long receivedRequests = statusProvider.getReceivedRequests(endpoint);
        long receivedReplies = statusProvider.getReceivedReplies(endpoint);
        long receivedErrors = statusProvider.getReceivedErrors(endpoint);
        long receivedDones = statusProvider.getReceivedDones(endpoint);

        stats = new Statistics(endpointURL.getEndpointURL(),
                sentRequests,
                sentReplies,
                sentErrors,
                sentDones,
                receivedRequests,
                receivedReplies,
                receivedErrors,
                receivedDones);
        statisticsList.add(stats);
        this.consumingStatisticsTable.put(qName, statisticsList);
    }

    
    public Statistics getStatistics() {
        Statistics stats = new Statistics();
        // todo - populate statistics
        return stats;
    }
    
    
    public Statistics generateStatistics(List list) {
        Statistics statistics = new Statistics();
        
        long sentRequests = 0L;
        long sentReplies = 0L;
        long sentErrors = 0L;
        long sentDones = 0L;

        long receivedRequests = 0L;
        long receivedReplies = 0L;
        long receivedErrors = 0L;
        long receivedDones = 0L;

        Statistics stats = null;
        Iterator iterator = list.iterator();
        while(iterator.hasNext() == true) {
            stats = (Statistics) iterator.next();
            if(stats != null) {
                sentRequests += stats.getSentRequests();
                sentReplies += stats.getSentReplies();
                sentErrors += stats.getSentErrors();
                sentDones += stats.getSentDones();

                receivedRequests += stats.getReceivedRequests();
                receivedReplies += stats.getReceivedReplies();
                receivedErrors += stats.getReceivedErrors();
                receivedDones += stats.getReceivedDones();
            }
        }
        statistics.setSentRequests(sentRequests);
        statistics.setSentReplies(sentReplies);
        statistics.setSentErrors(sentErrors);
        statistics.setSentDones(sentDones);

        statistics.setReceivedRequests(receivedRequests);
        statistics.setReceivedReplies(receivedReplies);
        statistics.setReceivedErrors(receivedErrors);
        statistics.setReceivedDones(receivedDones);

        return statistics;
    }



    /**
     * @return Returns the statusProvider.
     */
    public StatusProvider getStatusProvider() {
        return this.statusProvider;
    }

    /**
     * @param statusProvider The statusProvider to set.
     */
    public void setStatusProvider(StatusProvider statusProvider) {
        this.statusProvider = statusProvider;
    }

    /**
     * @return Returns the componentName.
     */
    public String getComponentName() {
        return this.componentName;
    }

    /**
     * @return Returns the componentType.
     */
    public String getComponentType() {
        return this.componentType;
    }

    /**
     * @return Returns the consumingStatisticsTable.
     */
    public Map getConsumingStatisticsTable() {
        return this.consumingStatisticsTable;
    }

    /**
     * @return Returns the provisioningStatisticsTable.
     */
    public Map getProvisioningStatisticsTable() {
        return this.provisioningStatisticsTable;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
    }

}
