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
 * @(#)StatisticsService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.statistics;

import com.sun.jbi.cam.model.management.Statistics;
import java.util.List;

/**
 *
 * @author ylee
 */
public interface StatisticsService {
    
    // define interfaces here
    
    public long getReceivedDones();
    
    public long getReceivedErrors();
    
    public long getReceivedReplies();
    
    public long getReceivedRequests();
    
    public long getSentDones();
    
    public long getSentErrors();
    
    public long getSentReplies();
    
    public long getSentRequests();
    
    public String[] getConsumingEndpoints(String componentName,String componentType);
    
    public String[] getProvisioningEndpoints(String componentName,String componentType);    
    
    public Statistics getStatistics(String componentName, String componentType);
    
    public List<Statistics> getProvisioningStatistics(String componentName,String componentType,String cType,String cName,String pName);
    
    public List<Statistics> getConsumingStatistics(String componentName,String componentType,String cType,String cName,String pName);
    
    public Statistics getTotalStatistics(List<Statistics> provisiongStats, List<Statistics> consumingStats);
    
     public Statistics getStatisticsEndpoint(String endpoint, String componentName, String componentType);
  

}
