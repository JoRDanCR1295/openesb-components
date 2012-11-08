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
 * @(#)JpaDAO.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.persist.dao;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.Query;

import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.MessageTracking;
import com.sun.jbi.common.tale.core.persist.entity.AleChannels;
import com.sun.jbi.common.tale.core.persist.entity.CsfCmeLog;
import com.sun.jbi.common.tale.core.persist.entity.CsfLoggerCodes;
import com.sun.jbi.common.tale.core.persist.entity.CsfLoggerLog;
import com.sun.jbi.common.tale.core.persist.entity.ExchangeInfo;
import com.sun.jbi.common.tale.core.persist.entity.MsgTracking;


/**
 * JpaDAO - DAO used for JPA operations
 * @author Sun MicroSystems
 */
public class JpaDAO {

    private EntityManagerFactory emf;
    private EntityManager em;
    private String PERSISTENCE_UNIT_NAME = "alese";
    
    public JpaDAO() {
       
    }
    
    public void initEntityManager() {
        //set the current thread's TCCL to this class' classloader to get around
        //the problem of CL not finding persistence.xml
        ClassLoader thisCl = getClass().getClassLoader();        
        ClassLoader oldThreadCL = Thread.currentThread().getContextClassLoader();        
        Thread.currentThread().setContextClassLoader(thisCl);
        emf = Persistence.createEntityManagerFactory(PERSISTENCE_UNIT_NAME);
        Thread.currentThread().setContextClassLoader(oldThreadCL);
        em = emf.createEntityManager();
    } 
    
    public void closeEntityManager() {
        em.close();
        emf.close();
    }   
    
    /**
     * Gets the logger code record for the given loggercode
     * @param loggerCode
     * @return CsfLoggerCodes record
     */
    public CsfLoggerCodes getLoggerCodeRecord(int loggerCode) {        
        Query findLoggerCode = em.createNamedQuery("CsfLoggerCodes.findByLoggerCode");
        findLoggerCode.setParameter("loggerCode", loggerCode);
        CsfLoggerCodes loggerCodeRecord = null;
        try {
            loggerCodeRecord = (CsfLoggerCodes)findLoggerCode.getSingleResult();
        } catch (Exception ex) {
            //Logger Code Not found
        }        
        return loggerCodeRecord;
    }
    
    /**
     * Gets the list of ALE Channels for the given loggerCode
     * @param loggerCode
     * @return list
     */
    public List getChannelList(int loggerCode) {
        List loggerChannelList = new ArrayList();
        Query findLoggerChannels = em.createNamedQuery("LoggerCodesChannel.findByLoggerCode");
        findLoggerChannels.setParameter("loggerCode", loggerCode);
        try {
            loggerChannelList = findLoggerChannels.getResultList();
        } catch (Exception ex) {
            //
        }
        return loggerChannelList;
    }
    
    /**
     * Gets the ALE Channel record for the given channelCode
     * @param channelCode
     * @return AleChannels record
     */
    public AleChannels getALEChannel(int channelCode) {
        AleChannels aleChannel = null;
        Query findALEChannel = em.createNamedQuery("AleChannels.findByAleChannelCode");
        findALEChannel.setParameter("aleChannelCode", channelCode);
        try {
            aleChannel = (AleChannels) findALEChannel.getSingleResult();
        } catch (Exception ex) {
            //
        }
        return aleChannel;
    }
    
    public void persistRecordsInDB(TaleRequest req) {
        CsfCmeLog csfCmeLog = createCsfCmeLog(req);
        
        ExchangeInfo exInfo = createExchangeInfo(csfCmeLog, req);
        csfCmeLog.setExchangeInfo(exInfo);
        
        Collection<MsgTracking> msgTrackings = createMsgTrackings(csfCmeLog, req);
        csfCmeLog.setMsgTrackings(msgTrackings);
        
        CsfLoggerLog csfLoggerLog = createCsfLoggerLog(csfCmeLog, req);
        csfCmeLog.setCsfLoggerLog(csfLoggerLog);   
        
        em.getTransaction().begin();        
        em.persist(csfCmeLog);       
        em.getTransaction().commit();
    }
    
    private CsfCmeLog createCsfCmeLog(TaleRequest req) {
        CsfCmeLog csfCmeLog = new CsfCmeLog(req.getSourceInfo().getApplicationType(),
                req.getSourceInfo().getServiceName(),
                req.getSourceInfo().getModuleName(),
                req.getSourceInfo().getUnitName());        
        csfCmeLog.setMesgDatestamp(req.getSourceInfo().getDateTimeStamp());
        csfCmeLog.setAppMesgId(req.getSourceInfo().getAppMessageID());
        csfCmeLog.setAppDatestamp(req.getSourceInfo().getDateTimeStamp());
        csfCmeLog.setProjectName(req.getSourceInfo().getProjectName());        
        csfCmeLog.setAppName(req.getSourceInfo().getApplicationName());        
        csfCmeLog.setInstanceName(req.getSourceInfo().getInstanceName());
        csfCmeLog.setInsertDateTime(req.getSourceInfo().getDateTimeStamp());
        csfCmeLog.setServerType(req.getEnvironmentInfo().getServerType());
        csfCmeLog.setServerName(req.getEnvironmentInfo().getServerName());
        csfCmeLog.setHostName(req.getEnvironmentInfo().getHostName());
        csfCmeLog.setLogicalhostName(req.getEnvironmentInfo().getLogicalHostName());
        csfCmeLog.setEnvironmentName(req.getEnvironmentInfo().getEnvName());
        csfCmeLog.setComponentType(req.getSourceInfo().getComponentType());
        csfCmeLog.setComponentName(req.getSourceInfo().getComponentName());
        return csfCmeLog;
    }
    
    private CsfLoggerLog createCsfLoggerLog(CsfCmeLog csfCmeLog, TaleRequest req){
        CsfLoggerLog loggerLog = new CsfLoggerLog(csfCmeLog, req.getCode());
        loggerLog.setLogDetails(req.getDetails());
        loggerLog.setDisplayMesg(req.getDisplayMessage());
        return loggerLog;
    }
    
    private ExchangeInfo createExchangeInfo(CsfCmeLog csfCmeLog, TaleRequest req){
        ExchangeInfo exInfo = new ExchangeInfo(csfCmeLog, req.getExchangeInfo().getExchangeID());
        exInfo.setEndpointName(req.getExchangeInfo().getEndPointName());
        exInfo.setServiceName(req.getExchangeInfo().getServiceName());
        exInfo.setOpeartionName(req.getExchangeInfo().getOperationName());
        return exInfo;
    }
    
    private Collection<MsgTracking> createMsgTrackings(CsfCmeLog csfCmeLog, TaleRequest req){
        Collection<MsgTracking> msgTrackings = csfCmeLog.getMsgTrackings();
        if (req.getMsgTrackingList() != null && req.getMsgTrackingList().size() > 0) {
            for (MessageTracking msgTrack : req.getMsgTrackingList()) {
                MsgTracking msgTrackJPA = new MsgTracking(csfCmeLog, msgTrack.getTrackID());
                msgTrackings.add(msgTrackJPA);
            }
        }
        return msgTrackings;
    }
}
