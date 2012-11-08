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
 * @(#)RealTimeStatisticServlet.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.connectors.LocalServerConnector;
import com.sun.jbi.cam.manager.framework.generic.ServiceUnitsBean;
import com.sun.jbi.cam.model.management.JBIServiceUnitStatus;
import com.sun.jbi.cam.model.management.Statistics;
import com.sun.jbi.cam.services.ServiceFactory;
import com.sun.jbi.cam.services.statistics.StatisticsService;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 *
 * @author rdamir
 */
public class RealTimeStatisticServlet extends HttpServlet {
    
    
    public static final String CONTENT_TYPE_HEADER = "Content-Type";
    public static final String CONTENT_TYPE_VALUE = "text/plain;charset=UTF-8";
    public static final String CACHE_CONTROL_HEADER = "Cache-Control";
    public static final String CACHE_CONTROL_VALUE = "no-cache";

    private Logger logger = 
            Logger.getLogger(RealTimeStatisticServlet.class.getName());
   
    
    /** Creates a new instance of RealTimeStatisticServlet */
    public RealTimeStatisticServlet() {
    }
    
    
    public void doGet(HttpServletRequest request, HttpServletResponse response)
        throws ServletException, IOException {
        doPost(request, response);
    }

    public void doPost(HttpServletRequest request, HttpServletResponse response)
        throws ServletException, IOException {
        
        
        String svgElementType =  
                request.getParameter(GenericConstants.SVG_ELEMENT_TYPE);
        if(svgElementType.equals(GenericConstants.SERVICE_UNIT_TYPE)) {
            getServiceUnitStatistics(request,response);
            return;
        }
        getEndPointStatistics(request,response);
    }
    
    private void getServiceUnitStatistics(HttpServletRequest request,
        HttpServletResponse response)    throws ServletException, IOException {

        boolean test= request.getParameter("test") == null ? false :true;
        RealTimeStatisticHelper statsBean = new RealTimeStatisticHelper();
        String targetName = 
                request.getParameter(GenericConstants.COMPONENT_TNAME);
        String componentName = 
                request.getParameter(GenericConstants.COMPONENT_NAME);
        String componentType = 
                request.getParameter(GenericConstants.COMPONENT_TYPE);
        String cType = request.getParameter(GenericConstants.COMPONENT_CTYPE);
        String cName = request.getParameter(GenericConstants.COMPONENT_CNAME);
        String saName = request.getParameter(GenericConstants.COMPONENT_PNAME);
                
        statsBean.setup(componentName,componentType,cName,cType,saName,
                       targetName);
        String state = getServiceUnitState(componentName,componentType,cName,
                       cType,saName, targetName);
        StringBuffer buffer = new StringBuffer(state + ";");
        StatisticsService statsService = ServiceFactory.getStatisticsService(new LocalServerConnector(),targetName);
        List<Statistics> consumeStat = 
              statsService.getConsumingStatistics(componentName, componentType,
                           cType,cName,saName);
        List<Statistics> provideStat = 
                statsService.getProvisioningStatistics(componentName, componentType,
                           cType,cName,saName);
        Statistics stats =  statsService.getTotalStatistics(provideStat,consumeStat);
        AppendStatistics(stats,buffer,test);
        returnStatistics(buffer,response);
    }
    
    private String getServiceUnitState(String componentName,String componentType,
            String cname,String ctype,String pname,String tname) {
        String status = Messages.getString("state.Unavailable");
        String state = Messages.getString("state.State");
     
        ServiceUnitsBean susBean =  new ServiceUnitsBean();
        List<JBIServiceUnitStatus> suStatusList = 
                susBean.getSAServiceUnitStatusList(pname,tname);
        if ( suStatusList!=null ) {
           Iterator<JBIServiceUnitStatus> suIter = suStatusList.iterator();
           while( suIter.hasNext() ) {
                JBIServiceUnitStatus suStatus = suIter.next();
                String targetName = suStatus.getTargetName();
                if(targetName.equals(cname)) {
                    status = suStatus.getStatus();
                    break;
                }
           }
        }
        return state+status;
    }
    private void getEndPointStatistics(HttpServletRequest request,
        HttpServletResponse response)  throws ServletException, IOException {

 
        boolean test= request.getParameter("test") == null ? false :true;
        String name= request.getParameter(GenericConstants.COMPONENT_NAME);
        String type= request.getParameter(GenericConstants.COMPONENT_TYPE);
        String targetName = request.getParameter(GenericConstants.COMPONENT_TNAME);
        String endPoint = request.getParameter(GenericConstants.ENDPOINT_KEY);
        String  fqEndPoint = request.getParameter(GenericConstants.FQ_ENDPOINT_NAME);
        
        StatisticsService statsService = ServiceFactory.getStatisticsService(new LocalServerConnector(),targetName);
        Statistics  stats = statsService.getStatisticsEndpoint(fqEndPoint,name,
                type);
        StringBuffer buffer = new StringBuffer(endPoint + ";");
        AppendStatistics(stats,buffer,test);
        returnStatistics(buffer,response);
    }
     
    private void returnStatistics(StringBuffer buffer, 
         HttpServletResponse response)  throws ServletException, IOException {
        
        response.setStatus(HttpServletResponse.SC_OK);
        response.setHeader(CONTENT_TYPE_HEADER, CONTENT_TYPE_VALUE);
        response.setHeader(CACHE_CONTROL_HEADER, CACHE_CONTROL_VALUE);
        
        // Write out the message on the response stream
        OutputStream outputStream = response.getOutputStream();
        try {
                outputStream.write(buffer.toString().getBytes());
        } catch (IOException e2) {
        }
        outputStream.flush();
    }
    
    private void AppendStatistics(Statistics stats,StringBuffer buffer,
            boolean test) {
        buffer.append(Messages.getString("statistics_receivedRequests") + "=" +
                getReceivedRequests(stats,test)+";");
        buffer.append(Messages.getString("statistics_receivedReplies") + "=" +
                getReceivedReplies(stats,test)+";");
        buffer.append(Messages.getString("statistics_receivedErrors") + "=" +
                getReceivedErrors(stats,test)+";");
        buffer.append(Messages.getString("statistics_receivedDones") + "=" +
                getReceivedDones(stats,test)+";");
        buffer.append(Messages.getString("statistics_sentRequests") + "=" +
                getSentRequests(stats,test)+";");
        buffer.append(Messages.getString("statistics_sentReplies") + "=" +
                getSentReplies(stats,test)+";");
        buffer.append(Messages.getString("statistics_sentErrors") + "=" +
                getSentErrors(stats,test)+";");
        buffer.append(Messages.getString("statistics_sentDones") + "=" +
                getSentDones(stats,test));
        buffer.trimToSize();
     
    }
    private long  getReceivedRequests(Statistics stats, boolean test) {
        if(!test) {
            return stats.getReceivedRequests();
        }
     
        return (long)(Math.random()*10000L);
    }

    private long  getReceivedReplies(Statistics stats, boolean test) {
        if(!test) {
            return stats.getReceivedReplies();
        }
     
        return (long)(Math.random()*10000L);
    }

    private long  getReceivedErrors(Statistics stats, boolean test) {
        if(!test) {
            return stats.getReceivedErrors();
        }
     
         return (long)(Math.random()*10000L);
   }

    private long  getReceivedDones(Statistics stats, boolean test) {
        if(!test) {
            return stats.getReceivedDones();
        }
     
         return (long)(Math.random()*10000L);
    }

    
    
   private long  getSentRequests(Statistics stats, boolean test) {
        if(!test) {
            return stats.getSentRequests();
        }
     
         return (long)(Math.random()*10000L);
    }

    private long  getSentReplies(Statistics stats, boolean test) {
        if(!test) {
            return stats.getSentReplies();
        }
     
        return (long)(Math.random()*10000L);
    }

    private long  getSentErrors(Statistics stats, boolean test) {
        if(!test) {
            return stats.getSentErrors();
        }
     
         return (long)(Math.random()*10000L);
   }

    private long  getSentDones(Statistics stats, boolean test) {
        if(!test) {
            return stats.getSentDones();
        }
     
         return (long)(Math.random()*10000L);
}


    
}
