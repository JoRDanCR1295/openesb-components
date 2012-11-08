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
 * @(#)StatisticsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic;

import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.management.StatisticsProvider;
import com.sun.jbi.cam.management.StatusProvider;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.model.management.Statistics;
import com.sun.jbi.cam.services.statistics.StatisticsService;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import com.sun.jbi.cam.services.ServiceManager;
import com.sun.jbi.cam.services.ServiceManagerFactory;
import java.util.Map;
import java.util.logging.Logger;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 *
 * @author ylee
 */
public class StatisticsBean extends BaseBean implements Serializable {
    
    private StatisticsService statsService = null;
    
    private Logger logger = Logger.getLogger(StatisticsBean.class.getName());
    
    private List<Statistics> pStats;        // Provisioning Statistics
    private List<Statistics> cStats;        // Consuming Statistics
    
    /** Creates a new instance of StatisticsProvider */
    public StatisticsBean() {
    }
    
    
    protected void getStatisticsService() {
        // setup request configuration data
        setup();
        statsService = serviceManager.getStatisticsService(tName);
    }
    
    public TableDataProvider getStatistics() {
        
        Statistics stats = getStatisticsInstance();
        
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        list = stats.generateDisplayStatistics();
        
        provider = new ObjectListDataProvider(list);
        
        return provider;
    }
    
    
    public TableDataProvider getProvisioningStatistics() {
        
        getStatisticsService();
        
        String name = Util.mapComponentValue(cName,componentName);
        String type = Util.mapComponentValue(cType,componentType);
        
        pStats = statsService.getProvisioningStatistics(componentName,componentType,cType,cName,pName);
        
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        
        if ( pStats!=null ) {
            for ( Iterator iter=pStats.iterator(); iter.hasNext(); ) {
                Statistics stats = (Statistics)iter.next();
                list.addAll(stats.generateDisplayStatistics());
            }
        }
        
        provider = new ObjectListDataProvider(list);
        
        return provider;
    }
    
    public TableDataProvider getConsumingStatistics() {
        getStatisticsService();
        
        cStats = statsService.getConsumingStatistics(componentName,componentType,cType,cName,pName);
        
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        
        if ( cStats!=null ) {
            for ( Iterator iter=cStats.iterator(); iter.hasNext(); ) {
                Statistics stats = (Statistics)iter.next();
                list.addAll(stats.generateDisplayStatistics());
            }
        }
        
        provider = new ObjectListDataProvider(list);
        
        return provider;
    }
    
    
    public TableDataProvider getTotalsStatistics() {
        getStatisticsService();
        
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        
        Statistics stats = statsService.getTotalStatistics(pStats,cStats);
        
        // tally up statistics for all end points
        list = stats.generateDisplayStatistics();
        
        provider = new ObjectListDataProvider(list);
        
        return provider;
    }
    
    
    public Statistics  getStatisticsInstance() {
        
        // setup request configuration data
        setup();
        
        statsService = serviceManager.getStatisticsService(tName);
        
        // todo - getting container statistics only
        String name = Util.mapComponentValue(cName,componentName);
        String type = Util.mapComponentValue(cType,componentType);
        Statistics stats = statsService.getStatistics(name,type);
        return stats;
        
    }
    
    
    public String getLabel() {
        String label = "";
        if ( GenericConstants.BC_TYPE.equals(componentType) ) {
            label = Messages.getString("statistics_bc_label");
        } else if ( GenericConstants.SE_TYPE.equals(componentType) ) {
            label = Messages.getString("statistics_se_label");
        } else if ( GenericConstants.SU_TYPE.equals(componentType) ) {
            label = Messages.getString("statistics_su_label");
        }
        return label;
    }
    
    public String getTitle() {
        //return getName()+" - " + Messages.getString("statistics_title");
        return getTitle("statistics_title");
    }
    
    public String getTableTitle() {
        return getTableTitle("statistics_bc_tabletitle","statistics_se_tabletitle","statistics_su_tabletitle");
    }
    
    public String showGraph() {
        // switch to graphical view
        System.out.println(">>>> show graphical view");
        return GenericConstants.SUCCESS;        //$NON-NLS-1$
    }

    
    ///////////////////////////////////////////////////
    // Charting support - return raw DisplayStatistics
    ///////////////////////////////////////////////////
    public List<DisplayStatistics> getProvisioningStatisticsList() {
        getStatisticsService();
        String name = Util.mapComponentValue(cName,componentName);
        String type = Util.mapComponentValue(cType,componentType);
        pStats = statsService.getProvisioningStatistics(componentName,componentType,cType,cName,pName);
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        if ( pStats!=null ) {
            for ( Iterator iter=pStats.iterator(); iter.hasNext(); ) {
                Statistics stats = (Statistics)iter.next();
                list.addAll(stats.generateDisplayStatistics());
            }
        }
        return list;
    }
    public List<DisplayStatistics> getConsumingStatisticsList() {
        getStatisticsService();
        cStats = statsService.getConsumingStatistics(componentName,componentType,cType,cName,pName);
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        if ( cStats!=null ) {
            for ( Iterator iter=cStats.iterator(); iter.hasNext(); ) {
                Statistics stats = (Statistics)iter.next();
                list.addAll(stats.generateDisplayStatistics());
            }
        }
        return list;
    }
    
    public Map<String, Double> getConsumingTotalsList() {
        Map<String, Double> consumingTotalsMap = new HashMap<String, Double>();
        double totalReceivedRequests = 0D,
                totalReceivedReplies = 0D,
                totalReceivedErrors = 0D,
                totalReceivedDones = 0D,
                totalSentRequests = 0D,
                totalSentReplies = 0D,
                totalSentErrors = 0D,
                totalSentDones = 0D;
        getStatisticsService();
        cStats = statsService.getConsumingStatistics(componentName,componentType,cType,cName,pName);
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        if ( cStats!=null ) {
            for ( Iterator iter=cStats.iterator(); iter.hasNext(); ) {
                Statistics stats = (Statistics)iter.next();
                totalReceivedRequests += stats.getReceivedRequests();
                totalReceivedReplies +=  stats.getReceivedReplies();
                totalReceivedErrors +=  stats.getReceivedErrors();
                totalReceivedDones +=  stats.getReceivedDones();
                totalSentRequests +=  stats.getSentRequests();
                totalSentReplies +=  stats.getSentReplies();
                totalSentErrors +=  stats.getSentErrors();
                totalSentDones +=  stats.getSentDones();
            }
        }
        consumingTotalsMap.put(Messages.getString("statistics_receivedRequests"), new Double(totalReceivedRequests));
        consumingTotalsMap.put(Messages.getString("statistics_receivedReplies"), new Double(totalReceivedReplies));
        consumingTotalsMap.put(Messages.getString("statistics_receivedErrors"), new Double(totalReceivedErrors));
        consumingTotalsMap.put(Messages.getString("statistics_receivedDones"), new Double(totalReceivedDones));
        consumingTotalsMap.put(Messages.getString("statistics_sentRequests"), new Double(totalSentRequests));
        consumingTotalsMap.put(Messages.getString("statistics_sentReplies"), new Double(totalSentReplies));
        consumingTotalsMap.put(Messages.getString("statistics_sentErrors"), new Double(totalSentErrors));
        consumingTotalsMap.put(Messages.getString("statistics_sentDones"), new Double(totalSentDones));
        
        return consumingTotalsMap;
    }
    
    public Map<String, Double> getProvisioningTotalsList() {
        Map<String, Double> provisioningTotalsMap = new HashMap<String, Double>();
        double totalReceivedRequests = 0D,
                totalReceivedReplies = 0D,
                totalReceivedErrors = 0D,
                totalReceivedDones = 0D,
                totalSentRequests = 0D,
                totalSentReplies = 0D,
                totalSentErrors = 0D,
                totalSentDones = 0D;
        getStatisticsService();
        cStats = statsService.getProvisioningStatistics(componentName,componentType,cType,cName,pName);
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        if ( cStats!=null ) {
            for ( Iterator iter=cStats.iterator(); iter.hasNext(); ) {
                Statistics stats = (Statistics)iter.next();
                totalReceivedRequests += stats.getReceivedRequests();
                totalReceivedReplies +=  stats.getReceivedReplies();
                totalReceivedErrors +=  stats.getReceivedErrors();
                totalReceivedDones +=  stats.getReceivedDones();
                totalSentRequests +=  stats.getSentRequests();
                totalSentReplies +=  stats.getSentReplies();
                totalSentErrors +=  stats.getSentErrors();
                totalSentDones +=  stats.getSentDones();
            }
        }
        provisioningTotalsMap.put(Messages.getString("statistics_receivedRequests"), new Double(totalReceivedRequests));
        provisioningTotalsMap.put(Messages.getString("statistics_receivedReplies"), new Double(totalReceivedReplies));
        provisioningTotalsMap.put(Messages.getString("statistics_receivedErrors"), new Double(totalReceivedErrors));
        provisioningTotalsMap.put(Messages.getString("statistics_receivedDones"), new Double(totalReceivedDones));
        provisioningTotalsMap.put(Messages.getString("statistics_sentRequests"), new Double(totalSentRequests));
        provisioningTotalsMap.put(Messages.getString("statistics_sentReplies"), new Double(totalSentReplies));
        provisioningTotalsMap.put(Messages.getString("statistics_sentErrors"), new Double(totalSentErrors));
        provisioningTotalsMap.put(Messages.getString("statistics_sentDones"), new Double(totalSentDones));
        
        return provisioningTotalsMap;
    }
    
    ///////////////////////////////////////////////////
    
    
}
