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
 * @(#)SchedulerEndPoint.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.scheduler.domain;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.crl.util.WrapperUtil;
import com.sun.jbi.engine.scheduler.SchedulerSEComponentManager;
import com.sun.jbi.engine.scheduler.SchedulerSEEndpointManager;
import java.io.File;
import java.io.FilenameFilter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

/**
 * Implements a scheduler SE endpoint.
 *
 * @author sunsoabi_edwong
 */
public class SchedulerEndPoint implements Endpoint<SchedulerServiceDef>,
        SchedulerConstants {

    private SchedulerSEEndpointManager mEpMgr;
    private boolean mIsStarted;
    private EndpointInfo mEndPtInfo;
    private ServiceUnit mSrvcUnit;
    private Scheduler mScheduler;
    private Logger mLogger;
    private Definition mWsdlDef;
    private String mConsumeOperation;
    private QName mInputMessageQName;
    private String mInputName;
    

    public SchedulerEndPoint(SchedulerSEEndpointManager epMgr,
            EndpointInfo endPtInfo, ServiceUnit srvcUnit) {
        super();

        mEpMgr = epMgr;
        mIsStarted = false;
        mEndPtInfo = endPtInfo;
        mSrvcUnit = srvcUnit;
    }

    public boolean isStarted() {
        return mIsStarted;
    }

    public void start() throws JBIException {
        initScheduler();
        if (!mIsStarted) {
            mIsStarted = true;
            try {
                if (mScheduler.isInStandbyMode()) {
                    mScheduler.start();
                }
            } catch (SchedulerException ex) {
                getLogger().log(Level.SEVERE, null, ex);
                mIsStarted = false;
            }
        }
    }

    public void stop() throws JBIException {
        if (mIsStarted && (mScheduler != null)) {
            mIsStarted = false;
            try {
                if (!mScheduler.isInStandbyMode()) {
                    mScheduler.standby();
                }
            } catch (SchedulerException ex) {
                getLogger().log(Level.SEVERE, null, ex);
            }
        }
    }

    public EndpointInfo getInfo() {
        return mEndPtInfo;
    }

    public SchedulerServiceDef getServiceDef(String opName) {
        return null;
    }

    public void setServiceDef(SchedulerServiceDef def, Operation... ops) {
    }

    public Operation getOperation(String name) {
        return null;
    }

    public Collection getOperations() {
        return null;
    }

    private Logger getLogger() {
        // try init component logger
        if (null == mLogger) {
            mLogger = Util.getLogger(mEpMgr.getComponentContext(),
                    getClass().getName());
        }

        return mLogger;
    }

    private void initScheduler() throws JBIException {
        if (null == mScheduler) {
            mScheduler = mEpMgr.getComponentManager().getScheduler(
                    mSrvcUnit.getName());
            try {
                SchedulerModel[] schedModels = readSchedulerArtifact();
                String triggerID = mEndPtInfo.getEndpointName();
                String uniqName = triggerID;
                if (uniqName.endsWith(SERVICE_PORT_SUFFIX)) {
                    uniqName = uniqName.substring(0,
                        uniqName.length() - SERVICE_PORT_SUFFIX.length());
                }
                if (schedModels != null) {
                    for (SchedulerModel schedModel : schedModels) {
                        if (uniqName.equals(schedModel.getGroup() + "-" //NOI18N
                                + schedModel.getName())) {
                            for (SchedulerModel.Trigger myTrigger
                                    : schedModel.getTriggers()) {
                                if (myTrigger.isEnabled()) {
                                    scheduleJob(myTrigger);
                                }
                            }
                        }
                    }
                }
                
                if (readWSDL(triggerID) == null) {
                    throw new JBIException(
                            "Cannot find operation/message QName/input name!");
                }
            } catch (SchedulerException se) {
                se.printStackTrace();
            } catch (ParseException pe) {
                pe.printStackTrace();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    protected SchedulerModel[] readSchedulerArtifact() {
        List<SchedulerModel> results = new ArrayList<SchedulerModel>();
        File installRootDir = new File(mSrvcUnit.getRootPath());
        if (installRootDir.isDirectory()) {
            String[] schedFiles = installRootDir.list(new FilenameFilter() {

                public boolean accept(File dir, String name) {
                    return name.toLowerCase().endsWith(ARTIFACT_FILE_EXT);
                }
            });
            
            if (schedFiles != null) {
                for (String schedFilename : schedFiles) {
                    File schedFile = new File(installRootDir, schedFilename);
                    if (schedFile.exists() && !schedFile.isDirectory()) {
                        SchedulerModel result =
                                SchedulerModel.readXml(schedFile);
                        if (result != null) {
                            getLogger().info("Scheduler artifact read: "
                                    + result.toString());
                            results.add(result);
                        }
                    }
                }
            }
        }
        
        return results.toArray(new SchedulerModel[0]);
    }

    public Job createSchedulerJob() {
        return new SchedulerJob();
    }

    private Definition readWSDL(String triggerID) throws Exception {
        Definition result = null;
        mWsdlDef = null;
        
        File installRootDir = new File(mSrvcUnit.getRootPath());
        if (!installRootDir.isDirectory()) {
            return result;
        }
        
        String[] wsdls = installRootDir.list(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.toLowerCase().endsWith(".wsdl");            //NOI18N
            }
        });
        if (null == wsdls) {
            return result;
        }
        
        for (String wsdlFilename : wsdls) {
            File wsdlFile = new File(installRootDir, wsdlFilename);
            if (wsdlFile.isDirectory()) {
                continue;
            }
            
            WSDLFactory wsdlFac = WSDLFactory.newInstance();
            WSDLReader wsdlRdr = wsdlFac.newWSDLReader();
            result = wsdlRdr.readWSDL(wsdlFile.getCanonicalPath());
            if (null == result.getServices()) {
                continue;
            }
            
            for (Iterator iter = result.getServices().entrySet().iterator();
                    iter.hasNext(); ) {
                Map.Entry me = (Map.Entry) iter.next();
                if (!((me.getKey() instanceof QName)
                        && (me.getValue() instanceof Service))) {
                    continue;
                }
                
                Service service = (Service) me.getValue();
                if (!service.getQName().equals(mEndPtInfo.getServiceName())) {
                    continue;
                }
                
                Port port = service.getPort(triggerID);
                if (null == port) {
                    continue;
                }
                
                Binding binding = port.getBinding();
                if (binding.getBindingOperations().isEmpty()) {
                    continue;
                }
                
                BindingOperation bindingOp = (BindingOperation) binding
                        .getBindingOperations().get(0);
                String consumeOperation = bindingOp.getName();
                if ((bindingOp.getBindingInput() == null)
                        || Util.isEmpty(consumeOperation)) {
                    continue;
                }
                
                String inputName = bindingOp.getBindingInput().getName();
                PortType portType = binding.getPortType();
                if (null == portType) {
                    continue;
                }
                Operation portTypeOp = portType.getOperation(consumeOperation,
                        inputName, null);
                if (null == portTypeOp) {
                    continue;
                }
                Message inputMessage = portTypeOp.getInput().getMessage();
                if ((null == inputMessage)
                        || (inputMessage.getQName() == null)) {
                    continue;
                }
                
                mWsdlDef = result;
                mConsumeOperation = consumeOperation;
                QName inputMessageQName = inputMessage.getQName();
                String inputMessagePrefix = mWsdlDef.getPrefix(
                        inputMessageQName.getNamespaceURI());
                mInputMessageQName = new QName(
                        inputMessageQName.getNamespaceURI(),
                        inputMessageQName.getLocalPart(),
                        inputMessagePrefix);
                mInputName = inputName;
                return mWsdlDef;
            }
        }
        return mWsdlDef;
    }

    private void scheduleJob(SchedulerModel.Trigger myTrigger)
            throws SchedulerException, ParseException {
        String jobName = myTrigger.getName() + JOB_SUFFIX;
        JobDetail jobDet = new JobDetail(jobName,
                myTrigger.getGroup(), SchedulerJob.class);
        String jobDescription = JOB_DESCRIPTION
                + (Util.isEmpty(myTrigger.getDescription()) ? ""        //NOI18N
                        : myTrigger.getDescription());
        jobDet.setDescription(jobDescription);
        JobDataMap jobDataMap = new JobDataMap();
        jobDataMap.put(SchedulerSEComponentManager.ENDPOINT_INFO_PROP,
                mEndPtInfo);
        jobDataMap.put(SchedulerSEComponentManager.SCHED_TRIGGER_PROP,
                myTrigger);
        jobDet.setJobDataMap(jobDataMap);

        Trigger trigger = toTrigger(myTrigger);
        if (trigger != null) {
            mScheduler.scheduleJob(jobDet, trigger);
        }
    }
    
    private Trigger toTrigger(SchedulerModel.Trigger myTrigger)
            throws ParseException {
        if (myTrigger.getSimpleTrigger() != null) {
            SchedulerModel.Trigger.SimpleTrigger mySimpleTrigger =
                    myTrigger.getSimpleTrigger();
            String qTriggerName = myTrigger.getName();
            String qTriggerGroup = myTrigger.getGroup();
            java.util.Date qStartTime = new java.util.Date();
            java.util.Date qEndTime = null;
            int qRepeatCount = 0;
            long qRepeatInterval = 0L;
            
            if ((mySimpleTrigger.getStarting() != null)
                    && (mySimpleTrigger.getStarting().getDate() != null)) {
                qStartTime = mySimpleTrigger.getStarting().getDate().valueOf();
            }
            if ((mySimpleTrigger.getEnding() != null)
                    && (mySimpleTrigger.getEnding().getDate() != null)) {
                qEndTime = mySimpleTrigger.getEnding().getDate().valueOf();
            }
            if (mySimpleTrigger.getRepeat() != null) {
                qRepeatCount = mySimpleTrigger.getRepeat().valueOf();
                if (mySimpleTrigger.getRepeat().getInterval() != null) {
                    qRepeatInterval = mySimpleTrigger.getRepeat()
                            .getInterval().valueOf();
                }
            }
            
            Trigger trigger = new SimpleTrigger(qTriggerName, qTriggerGroup,
                    qStartTime, qEndTime, qRepeatCount, qRepeatInterval);
            trigger.setDescription(myTrigger.getDescription());
            return trigger;
        }
        return null;
    }

    public class SchedulerJob implements Job {

        public void execute(JobExecutionContext jobExecContxt)
                throws JobExecutionException {
            QName schedulerServiceQName = getInfo().getServiceName();
            ServiceEndpoint schedulerEndpoint = mEpMgr.getComponentContext()
                    .getEndpoint(schedulerServiceQName,
                            getInfo().getEndpointName());
            if (schedulerEndpoint != null) {
                getLogger().info("Found consumer endpoint: "
                        + schedulerEndpoint.getEndpointName()
                        + " service: " + schedulerEndpoint.getServiceName());

                try {
                    MessageExchangeFactory meFac = mEpMgr.getComponentContext()
                            .getDeliveryChannel().createExchangeFactory(
                                    schedulerEndpoint);
                    InOnly inOnlyME = meFac.createInOnlyExchange();
                    
                    QName consumeOpQName = QName.valueOf(
                            mConsumeOperation);
                    inOnlyME.setOperation(consumeOpQName);
                    
                    NormalizedMessage inOnlyMsg = inOnlyME.createMessage();
                    inOnlyME.setInMessage(inOnlyMsg);
                    
                    Document domDoc = XmlUtil.newDocument();
                    domDoc.setXmlVersion("1.0");                        //NOI18N
                    
                    Element jbiMessage = WrapperUtil.createJBIMessageWrapper(
                        domDoc, mInputMessageQName, mInputName);
                    domDoc.appendChild(jbiMessage);
                    
                    Element schedMessage = domDoc.createElementNS(
                            SCHEDULER_SCHEMA_NAMESPACE,
                            SCHEDULER_PREFIX + ":"                      //NOI18N
                                    + MESSAGE_ELEM_NCNAME);
                    Element messageMetadata = domDoc.createElementNS(
                            SCHEDULER_SCHEMA_NAMESPACE,
                            SCHEDULER_PREFIX + ":"                      //NOI18N
                                    + METADATA_ELEM_NCNAME);
                    schedMessage.appendChild(messageMetadata);
                    
                    messageMetadata.setAttribute(DATE_ATTR_NCNAME,
                            getFormattedDate(jobExecContxt));
                    messageMetadata.setAttribute(NAME_ATTR_NCNAME,
                            jobExecContxt.getTrigger().getName());
                    messageMetadata.setAttribute(GROUP_ATTR_NCNAME,
                            jobExecContxt.getTrigger().getGroup());
                    
                    Element messagePayload = domDoc.createElementNS(
                            SCHEDULER_SCHEMA_NAMESPACE,
                            SCHEDULER_PREFIX + ":"                      //NOI18N
                                    + PAYLOAD_ELEMENT_NCNAME);
                    schedMessage.appendChild(messagePayload);
                    Text payloadText = domDoc.createTextNode(
                            getPayloadText(jobExecContxt));
                    messagePayload.appendChild(payloadText);
                    
                    Element jbiPart = WrapperUtil.createJBIWrappedPart(domDoc,
                            schedMessage);
                    jbiMessage.appendChild(jbiPart);
                    
                    Source inOnlySrc = new DOMSource(domDoc);
                    getLogger().info("Sending: " + XmlUtil.print(inOnlySrc));
                    inOnlySrc = new DOMSource(domDoc);
                    inOnlyMsg.setContent(inOnlySrc);
                    mEpMgr.getComponentContext().getDeliveryChannel()
                            .send(inOnlyME);
                } catch (Exception ex) {
                    getLogger().log(Level.SEVERE, "Can't send to Endpoint!",
                            ex);
                }
            } else {
                getLogger().warning("Cannot find schedulee endpoint!");
            }
        }

        private String getFormattedDate(JobExecutionContext jobExecContxt) {
            String result = null;
            SchedulerModel.Trigger myTrigger =
                    (SchedulerModel.Trigger) jobExecContxt
                            .getMergedJobDataMap().get(
                                    SchedulerSEComponentManager
                                            .SCHED_TRIGGER_PROP);
            if ((myTrigger != null) && (myTrigger.getMessage() != null)) {
                SchedulerModel.Trigger.Message message = myTrigger.getMessage();
                result = (Util.isEmpty(message.getDateFormat())
                    ? new SimpleDateFormat()
                    : new SimpleDateFormat(message.getDateFormat()))
                            .format(jobExecContxt.getScheduledFireTime());
            } else {
                result = (new java.util.Date()).toString();
            }
            
            return result;
        }

        private String getPayloadText(JobExecutionContext jobExecContxt) {
            String payloadText = null;
            SchedulerModel.Trigger myTrigger =
                    (SchedulerModel.Trigger) jobExecContxt
                            .getMergedJobDataMap().get(
                                    SchedulerSEComponentManager
                                            .SCHED_TRIGGER_PROP);
            if ((myTrigger != null) && (myTrigger.getMessage() != null)) {
                SchedulerModel.Trigger.Message message = myTrigger.getMessage();
                payloadText = message.getPayload();
            }
            if (Util.isEmpty(payloadText)) {
                payloadText = jobExecContxt.getTrigger().getGroup()
                        + ":" + jobExecContxt.getTrigger().getName()    //NOI18N
                        + " triggered!";                                //NOI18N
            }
            
            return payloadText;
        }
    }

}
