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

package org.glassfish.openesb.schedulerbc.domain;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.descriptor.parsers.DefaultCatalogResolver;
import com.sun.jbi.common.qos.config.AppVar.VarType;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.descriptor.QosServices;
import com.sun.jbi.common.qos.descriptor.QosServicesDescriptor;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;
import com.sun.wsdl4j.ext.WSDL4JExt;
import java.util.Date;
import javax.jbi.management.DeploymentException;
import org.glassfish.openesb.schedulerbc.SchedulerComponentManager;
import org.glassfish.openesb.schedulerbc.SchedulerEndpointManager;
import java.io.File;
import java.io.FilenameFilter;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.Semaphore;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import javax.jbi.JBIException;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.glassfish.openesb.schedulerbc.I18n;
import org.glassfish.openesb.schedulerbc.SchedulerExchangeHandler;
import org.glassfish.openesb.schedulerbc.SchedulerServiceUnitManager;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.ActivePeriodEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.BindingEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.BindingOperationEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.TriggerEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl.SchedulerExtensionRegistry;
import org.quartz.CronExpression;
import org.quartz.CronTrigger;
import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.ObjectAlreadyExistsException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.w3c.dom.Document;
import org.w3c.dom.Text;
import org.xml.sax.EntityResolver;

/**
 * Implements a Scheduler BC endpoint.
 *
 * @author sunsoabi_edwong
 */
public class SchedulerEndPoint implements Endpoint<SchedulerServiceDef>,
        SchedulerConstants {

    private SchedulerEndpointManager mEpMgr;
    private boolean mIsStarted;
    private SchedulerEndPointInfo mEndPtInfo;
    private ServiceUnit mSrvcUnit;
    private Scheduler mScheduler;
    private Logger mLogger;
    private String mConsumeOperation;
    private Message mInputMessage;
    private String mInputPartName;
    private String appConfigName;
    private Map<String, AppVarTemplate> messageTemplates =
            new Hashtable<String, AppVarTemplate>();
    private StringBuffer undefinedAppVars = new StringBuffer();
    private Map<String, String> nmprops = new Hashtable<String, String>();
    private RedeliveryConfig redeliveryConfig;
    private ThrottlingConfig throttlingConfig;
    private Semaphore throttle;
    private String serviceAssemblyName;
    private boolean readWSDLOnce = false;
    private List<Trigger> staticTriggers = new ArrayList<Trigger>();
    private List<JobDetail> staticJobs = new ArrayList<JobDetail>();
    private SchedulerComponentManager compMgr;
    private SchedulerServiceUnitManager suMgr;

    private static final String DOT = ".";                              //NOI18N
    
    public SchedulerEndPoint(SchedulerEndpointManager epMgr,
            EndpointInfo endPtInfo, ServiceUnit srvcUnit)
            throws DeploymentException {
        super();

        mEpMgr = epMgr;
        mEndPtInfo = new SchedulerEndPointInfo(endPtInfo);
        mSrvcUnit = srvcUnit;

        DeploymentLookup deploymentLU =
                new DeploymentLookup(mEpMgr.getComponentContext());
        ServiceAssembly serviceAssembly = deploymentLU.getServiceAssembly(
                srvcUnit.getName());
        serviceAssemblyName = serviceAssembly.getIdentification().getName();

        compMgr = mEpMgr.getComponentManager();
        suMgr = (SchedulerServiceUnitManager) compMgr.getServiceUnitManager();
        mScheduler = compMgr.getScheduler();
        
        QName schedulerServiceQName = getInfo().getServiceName();
        mEpMgr.initializeEndpointActive(schedulerServiceQName,
                getInfo().getEndpointName());
    }

    public boolean isStarted() {
        return mIsStarted;
    }
    
    private String formatFireDate(JobDataMap jobDataMap, Date date) {
        String result = null;
        TriggerDetails td = null;
        String tdStr = (String) jobDataMap.get(
                SchedulerComponentManager.SCHED_TRIGGER_DETAILS_PROP);
        if (!Util.isEmpty(tdStr)) {
            td = TriggerDetails.valueOf(tdStr);
        }

        if (null == date) {
            return SchedAttrValue.NEVER.getDisplay();
        }

        if ((td != null) && (td.getDateFormat() != null)) {
            result = td.getDateFormat().format(date);
        } else {
            result = date.toString();
        }

        return result;
    }

    private void scheduleStaticTriggers() throws SchedulerException {
        for (int i = 0; i < staticJobs.size(); i++) {
            try {
                JobDetail jobDet = staticJobs.get(i);
                Trigger trigger = staticTriggers.get(i);
                mScheduler.scheduleJob(jobDet, trigger);
                
                Date nextFireDate = trigger.getNextFireTime();
                if (nextFireDate != null) {
                    String nextFireTime = formatFireDate(jobDet.getJobDataMap(),
                            nextFireDate);
                    I18n.info(log(), "SCHEDBC-5008: {0} trigger {1} "   //NOI18N
                            + "will fire first at: {2}",                //NOI18N
                            triggerType(trigger),
                            trigger.getFullName(),
                            nextFireTime);
                }
            } catch (ObjectAlreadyExistsException oaee) {
                // ignore: due to SchedulerBC in other clusters
                // also scheduling same jobs
            }
        }
    }

    public void start() throws JBIException {
        try {
            if (!readWSDLOnce) {
                initSchedulerEndPoint();
                scheduleStaticTriggers();
                readWSDLOnce = true;
            }

            suMgr.resumeTriggers(getServiceAssemblyName());
            
            mIsStarted = true;
        } catch (SchedulerException se) {
            throw new JBIException(se);
        }

    }

    public void stop() throws JBIException {
        try {
            suMgr.pauseTriggers(getServiceAssemblyName());
            mIsStarted = false;
        } catch (SchedulerException se) {
            throw new JBIException(se);
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

    private Logger log() {
        // try init component logger
        if (null == mLogger) {
            mLogger = Util.getLogger(mEpMgr.getComponentContext(),
                    getClass().getName());
        }

        return mLogger;
    }

    private void initSchedulerEndPoint() throws JBIException {
        try {
            readAppConfigName();

            if (readWSDL(mEndPtInfo.getEndpointName()) == null) {
                String errMsg = I18n.severe(log(),
                        "SCHEDBC-7003: WSDL for Endpoint {0} does "     //NOI18N
                        + "not represent a valid Scheduler binding",    //NOI18N
                        mEndPtInfo.getEndpointName());
                throw new JBIException(errMsg);
            }
        } catch (Exception ex) {
            String errMsg = I18n.severe(log(),
                    "SCHEDBC-7004: Cannot initialize Scheduler "        //NOI18N
                    + "Endpoint {0}", ex,                               //NOI18N
                    mEndPtInfo.getEndpointName());
            throw new JBIException(errMsg);
        }

        redeliveryConfig = mEpMgr.getContext().getMessagingChannel()
                .getServiceQuality(mEndPtInfo, RedeliveryConfig.class);
        throttlingConfig = mEpMgr.getContext().getMessagingChannel()
                .getServiceQuality(mEndPtInfo, ThrottlingConfig.class);
        if (throttlingConfig != null) {
            if (throttlingConfig.getMaxConcurrencyLimit() > 0) {
                throttle = new Semaphore(
                        throttlingConfig.getMaxConcurrencyLimit(), true);
            }
        }
    }

    public Job createSchedulerJob() {
        return new SchedulerJob();
    }

    private Definition readWSDL(String epName) throws Exception {
        File installRootDir = new File(mSrvcUnit.getRootPath());
        if (!installRootDir.isDirectory()) {
            I18n.finer(log(), "SCHEDBC-2001: Installation root "        //NOI18N
                    + "{0} is not a directory",                         //NOI18N
                    installRootDir.getAbsolutePath());
            return null;
        }
        
        return readWSDL(installRootDir, epName);
    }
    
    private String readAppConfigName() throws DeploymentException {
        QosServices qosSvcs =
                QosServicesDescriptor.parse(mSrvcUnit.getRootPath());
        if (qosSvcs != null) {
            appConfigName = qosSvcs.getApplicationConfiguration(mEndPtInfo);
            if (!Util.isEmpty(appConfigName)) {
                I18n.config(log(), "SCHEDBC-4004: Using Application "   //NOI18N
                        + "Configuration {0} for endpoint {1}",         //NOI18N
                        appConfigName, mEndPtInfo.getEndpointName());
            } else {
                I18n.config(log(), "SCHEDBC-4024: Not using "           //NOI18N
                        + "supplemental Application Configuration for " //NOI18N
                        + "endpoint {0} since none specified",          //NOI18N
                        mEndPtInfo.getEndpointName());
            }
        }
        return appConfigName;
    }
    
    private Definition readWSDL(File dir, String epName) throws Exception {
        Definition retDef = null;
        
        String[] wsdls = dir.list(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return (name.toLowerCase().endsWith(".wsdl")            //NOI18N
                        || (new File(dir, name)).isDirectory());
            }
        });
        if (null == wsdls) {
            return retDef;
        }
        
        for (String wsdlFilename : wsdls) {
            File wsdlFile = new File(dir, wsdlFilename);
            if (wsdlFile.isDirectory()) {
                retDef = readWSDL(wsdlFile, epName);
                if (retDef != null) {
                    return retDef;
                }
                continue;
            }
            
            I18n.finer(log(), "SCHEDBC-2002: Reading potential "        //NOI18N
                    + "Scheduler WSDL: {0}", wsdlFile.getAbsolutePath());
            
            EntityResolver resolver = null;
            WSDLReader wsdlRdr = null;
            ExtensionRegistry extReg = null;
            Definition tmpDef = null;

            try {
                resolver = new DefaultCatalogResolver(
                        mSrvcUnit.getRootPath());
                wsdlRdr = WSDL4JExt.newWSDLReader(resolver);
                extReg = new SchedulerExtensionRegistry();
                wsdlRdr.setExtensionRegistry(extReg);
                tmpDef = wsdlRdr.readWSDL(wsdlFile.getCanonicalPath());
            } catch (Exception ex) {
                I18n.warning(log(), "SCHEDBC-6025: Cannot process "     //NOI18N
                        + "potential Scheduler WSDL {0}; disregard "    //NOI18N
                        + "and fail forward...",                        //NOI18N
                        ex, wsdlFile.getAbsolutePath());
                continue;
            }
            
            // Look for standard WSDL artifacts that pertain to Scheduler
            if (null == tmpDef.getServices()) {
                I18n.finest(log(), "SCHEDBC-1001: No service "          //NOI18N
                        + "element found in WSDL");                     //NOI18N
                continue;
            }
            
            for (Iterator iter = tmpDef.getServices().entrySet().iterator();
                    iter.hasNext(); ) {
                Map.Entry me = (Map.Entry) iter.next();
                if (!((me.getKey() instanceof QName)
                        && (me.getValue() instanceof Service))) {
                    I18n.finest(log(), "SCHEDBC-1002: Bad "             //NOI18N
                            + "service element");                       //NOI18N
                    continue;
                }
                
                Service service = (Service) me.getValue();
                if (!service.getQName().equals(mEndPtInfo.getServiceName())) {
                    I18n.finest(log(), "SCHEDBC-1003: Service "         //NOI18N
                            + "element does not match endpoint");       //NOI18N
                    continue;
                }
                
                Port port = service.getPort(epName);
                if (null == port) {
                    I18n.finest(log(), "SCHEDBC-1004: No port "         //NOI18N
                            + "element under service");                 //NOI18N
                    continue;
                }
                
                Binding binding = port.getBinding();
                if (binding.getBindingOperations().isEmpty()) {
                    I18n.finest(log(), "SCHEDBC-1005: No "              //NOI18N
                            + "binding operations found");              //NOI18N
                    continue;
                }
                
                BindingOperation bindingOp = (BindingOperation) binding
                        .getBindingOperations().get(0);
                String consumeOperation = bindingOp.getName();
                if ((bindingOp.getBindingInput() == null)
                        || Util.isEmpty(consumeOperation)) {
                    I18n.finest(log(), "SCHEDBC-1006: Unsuitable "      //NOI18N
                            + "binding operation found");               //NOI18N
                    continue;
                }
                
                BindingInput bindingInput = bindingOp.getBindingInput();
                String inputName = bindingInput.getName();
                PortType portType = binding.getPortType();
                if (null == portType) {
                    I18n.finest(log(), "SCHEDBC-1007: Binding "         //NOI18N
                            + "has no port type");                      //NOI18N
                    continue;
                }
                Operation portTypeOp = portType.getOperation(consumeOperation,
                        inputName, null);
                if (null == portTypeOp) {
                    I18n.finest(log(), "SCHEDBC-1008: "                 //NOI18N
                            + "No suitable portType operation found");  //NOI18N
                    continue;
                }
                Message inputMessage = portTypeOp.getInput().getMessage();
                if ((null == inputMessage)
                        || (inputMessage.getQName() == null)) {
                    I18n.finest(log(), "SCHEDBC-1009: "                 //NOI18N
                            + "No suitable input message found");       //NOI18N
                    continue;
                }
                
                // Next look for Scheduler extensions to WSDL
                BindingEx bindingEx = null;
                List bindingExts = binding.getExtensibilityElements();
                if (bindingExts != null) {
                    for (Iterator bItr = bindingExts.iterator();
                            bItr.hasNext(); ) {
                        Object kid = bItr.next();
                        if (kid instanceof BindingEx) {
                            bindingEx = (BindingEx) kid;
                            break;
                        }
                    }
                }
                if (null == bindingEx) {
                    I18n.finest(log(), "SCHEDBC-1010: "                 //NOI18N
                            + "No Scheduler namespace binding found");  //NOI18N
                    continue;
                }
                
                BindingOperationEx bindingOperationEx = null;
                List bindingOperationExts =
                        bindingOp.getExtensibilityElements();
                if (bindingOperationExts != null) {
                    for (Iterator oItr = bindingOperationExts.iterator();
                            oItr.hasNext(); ) {
                        Object kid = oItr.next();
                        if (kid instanceof BindingOperationEx) {
                            bindingOperationEx = (BindingOperationEx) kid;
                            break;
                        }
                    }
                }
                if ((null == bindingOperationEx)
                        || !SchedAttrValue.STATIC.equals(SchedAttrValue
                                .toEnum(bindingOperationEx.getMode()))) {
                    I18n.finest(log(), "SCHEDBC-1011: No suitable "     //NOI18N
                            + "Scheduler binding operation found");     //NOI18N
                    continue;
                }
                
                ActivePeriodEx activePeriodEx = null;
                List portExts = port.getExtensibilityElements();
                if (portExts != null) {
                    for (Iterator pItr = portExts.iterator();
                            pItr.hasNext(); ) {
                        Object kid = pItr.next();
                        if (kid instanceof ActivePeriodEx) {
                            activePeriodEx = (ActivePeriodEx) kid;
                            break;
                        }
                    }
                }
                if (null == activePeriodEx) {
                    I18n.finest(log(), "SCHEDBC-1012: No suitable "     //NOI18N
                            + "Scheduler active-period found");         //NOI18N
                    continue;
                }
                
                // Found valid Scheduler artifacts
                retDef = tmpDef;
                mConsumeOperation = consumeOperation;
                mInputMessage = inputMessage;
                if (inputMessage.getParts() != null) {
                    for (Iterator pItr = inputMessage.getParts().entrySet()
                            .iterator(); pItr.hasNext(); ) {
                        Map.Entry pMe = (Map.Entry) pItr.next();
                        if (pMe.getValue() instanceof Part) {
                            Part p = (Part) pMe.getValue();
                            mInputPartName = p.getName();
                            if (!Util.isEmpty(mInputPartName)) {
                                break;
                            }
                        }
                    }
                }
                if (Util.isEmpty(mInputPartName)) {
                    mInputPartName = SCHEDULER_PART_NAME;
                }
                
                I18n.finest(log(), "SCHEDBC-1013: Found suitable "      //NOI18N
                        + "Scheduler artifacts in WSDL: "               //NOI18N
                        + "consume operation ({0}), "                   //NOI18N
                        + "input part name ({1})", mConsumeOperation,   //NOI18N
                        mInputPartName);
                
                scheduleJobs(bindingEx, bindingOperationEx, bindingInput,
                        activePeriodEx);
                return retDef;
            }
        }
        return retDef;
    }
    
    private void scheduleJobs(BindingEx bindingEx,
            BindingOperationEx bindingOperationEx, BindingInput bindingInput,
            ActivePeriodEx activePeriodEx)
            throws SchedulerException, ParseException {
        String quartzGroup = getServiceAssemblyName();
        String subGroup = bindingEx.getGroup();
        String dateFormat = bindingEx.getDateFormat();
        String mode = bindingOperationEx.getMode();
        List inputKids = bindingInput.getExtensibilityElements();
        String starting = activePeriodEx.getStarting();
        String ending = activePeriodEx.getEnding();
        String timezone = activePeriodEx.getTimezone();
        if (I18n.configLoggable(log())) {
            I18n.config(log(), "SCHEDBC-4005: Scheduler "               //NOI18N
                    + "Group derived from Service Assembly name: {0}",  //NOI18N
                    quartzGroup);
            I18n.config(log(), "SCHEDBC-4006: Scheduler "               //NOI18N
                    + "Date Format via WSDL Configuration: {0}",        //NOI18N
                    dateFormat);
            I18n.config(log(), "SCHEDBC-4007: Scheduler "               //NOI18N
                    + "Mode via WSDL Configuration: {0}", mode);        //NOI18N
            I18n.config(log(), "SCHEDBC-4008: Scheduler "               //NOI18N
                    + "Start date via WSDL Configuration: {0}",         //NOI18N
                    starting);
            I18n.config(log(), "SCHEDBC-4009: Scheduler "               //NOI18N
                    + "End date via WSDL Configuration: {0}", ending);  //NOI18N
            String timezoneStr = timezone;
            if (Util.isEmpty(timezoneStr)) {
                timezoneStr = "default ("                               //NOI18N
                        + TimeZone.getDefault().getID() + ")";          //NOI18N
            }
            I18n.config(log(), "SCHEDBC-4010: Scheduler "               //NOI18N
                    + "Time Zone via WSDL Configuration: {0}",          //NOI18N
                    timezoneStr);
        }
        if (appConfigName != null) {
            String startingAppCfg = mEpMgr.getComponentManager()
                    .getSchedulerConfiguration().getStartDate(appConfigName);
            if (!Util.isEmpty(startingAppCfg)) {
                starting = startingAppCfg;
                I18n.config(log(), "SCHEDBC-4011: Scheduler "           //NOI18N
                        + "Start date overridden by Application "       //NOI18N
                        + "Configuration: {0}", starting);              //NOI18N
            }
            String endingAppCfg = mEpMgr.getComponentManager()
                    .getSchedulerConfiguration().getEndDate(appConfigName);
            if (!Util.isEmpty(endingAppCfg)) {
                ending = endingAppCfg;
                I18n.config(log(), "SCHEDBC-4012: Scheduler "           //NOI18N
                        + "End date overridden by Application "         //NOI18N
                        + "Configuration: {0}", ending);                //NOI18N
            }
            String timezoneAppCfg = mEpMgr.getComponentManager()
                    .getSchedulerConfiguration().getTimeZone(appConfigName);
            if (!Util.isEmpty(timezoneAppCfg)) {
                timezone = timezoneAppCfg;
                I18n.config(log(), "SCHEDBC-4013: Scheduler "           //NOI18N
                        + "Time Zone overridden by Application "        //NOI18N
                        + "Configuration: {0}", timezone);              //NOI18N
            }
        }
        
        for (Object kid : inputKids) {
            if (kid instanceof TriggerEx) {
                TriggerEx triggerEx = (TriggerEx) kid;
                if (!triggerEx.isEnabled()) {
                    I18n.config(log(), "SCHEDBC-4014: "                 //NOI18N
                            + "Scheduler trigger {0}.{1}.{2} not "      //NOI18N
                            + "enabled and thus bypassed",              //NOI18N
                            quartzGroup, subGroup, triggerEx.getName());
                    continue;
                }
                
                String jobName = subGroup + DOT + triggerEx.getName()
                        + STATIC_JOB_SUFFIX;
                JobDetail jobDet = new JobDetail(jobName,
                        quartzGroup, SchedulerJob.class);
                String jobDescription = JOB_DESCRIPTION
                        + (Util.isEmpty(triggerEx.getDescription())
                                ? triggerEx.getName()
                                : triggerEx.getDescription());
                jobDet.setDescription(jobDescription);
                
                I18n.config(log(), "SCHEDBC-4015: "                     //NOI18N
                        + "Quartz Scheduler job name {0} "              //NOI18N
                        + "will be scheduled", jobName);                //NOI18N
                if (!Util.isEmpty(jobDescription)) {
                    I18n.config(log(), "SCHEDBC-4016: Quartz "          //NOI18N
                            + "Scheduler job descripton: {0}",          //NOI18N
                            jobDescription);
                }
                
                JobDataMap jobDataMap = new JobDataMap();
                jobDataMap.put(SchedulerComponentManager.ENDPOINT_INFO_PROP,
                        mEndPtInfo.valueAsString());
                TriggerDetails td = new TriggerDetails(quartzGroup, subGroup,
                        dateFormat, mode, starting, ending, timezone,
                        triggerEx, log());
                jobDataMap.put(
                        SchedulerComponentManager.SCHED_TRIGGER_DETAILS_PROP,
                        td.valueAsString());
                jobDet.setJobDataMap(jobDataMap);
                
                Trigger trigger = toTrigger(td);
                if (trigger != null) {
                    staticJobs.add(jobDet);
                    staticTriggers.add(trigger);
                }
            }
        }
    }

    private String repeatCountStr(int repeatCount) {
        return (SimpleTrigger.REPEAT_INDEFINITELY == repeatCount)
            ? I18n.getString(SchedulerEndPoint.class,
                    "STR_INDEFINITE_NUMBER_OF")                         //NOI18N
            : Integer.toString(repeatCount);
    }
    
    private Trigger toTrigger(TriggerDetails td) throws ParseException {
        TriggerEx triggerEx = td.getTriggerEx();
        Trigger trigger = null;
        if (triggerEx != null) {
            String triggerGroup = td.getQuartzGroup();
            String triggerSubGroup = td.getGroup();
            String triggerName = triggerSubGroup + DOT + triggerEx.getName();
            Date startTime = new Date();
            Date endTime = null;
            if ((td.getStarting() != null)
                    && !SchedAttrValue.NOW.equals(
                            SchedAttrValue.toEnum(td.getStarting()))) {
                startTime = td.getDateFormat().parse(td.getStarting());
            }
            I18n.finest(log(), "SCHEDBC-1014: Scheduler "               //NOI18N
                    + "Start date: {0}",                                //NOI18N
                    XSD_STD_SDF.format(startTime.getTime()));
            
            if ((td.getEnding() != null)
                    && !SchedAttrValue.NEVER.equals(
                            SchedAttrValue.toEnum(td.getEnding()))) {
                endTime = td.getDateFormat().parse(td.getEnding());
                I18n.finest(log(), "SCHEDBC-1015: Scheduler "           //NOI18N
                        + "End date: {0}",                              //NOI18N
                        XSD_STD_SDF.format(endTime.getTime()));
            } else {
                I18n.finest(log(), "SCHEDBC-1016: Scheduler "           //NOI18N
                        + "End date: never");                           //NOI18N
            }
            TimeZone tz = td.getTimezone();
            SchedAttrValue trigType = SchedAttrValue.toEnum(
                    triggerEx.getType());
            switch (trigType) {
            case SIMPLE: {
                int repeatCount = SimpleTrigger.REPEAT_INDEFINITELY;
                long repeatInterval = 1000L;

                if ((triggerEx.getRepeat() != null)
                        && !SchedAttrValue.INDEFINITE.equals(
                                SchedAttrValue.toEnum(triggerEx.getRepeat()))) {
                    repeatCount = Integer.parseInt(triggerEx.getRepeat());
                }
                if (triggerEx.getInterval() != null) {
                    repeatInterval = Long.parseLong(triggerEx.getInterval());
                }

                trigger = new SimpleTrigger(triggerName, triggerGroup,
                        startTime, endTime, repeatCount, repeatInterval);
                
                trigger.setMisfireInstruction(SimpleTrigger
                    .MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT);
                
                if (I18n.configLoggable(log())) {
                    I18n.config(log(), "SCHEDBC-4017: Scheduling "      //NOI18N
                            + "{0} trigger {1} to repeat "              //NOI18N
                            + "{2} times, every {3} milliseconds",      //NOI18N
                            trigType.getDisplay(),
                            trigger.getFullName(),
                            repeatCountStr(repeatCount),
                            Long.toString(repeatInterval));
                }
                break;
            } case CRON: {
                String cronExpr = triggerEx.getCronExpr();
                if (CronExpression.isValidExpression(cronExpr)) {
                    CronTrigger cTrigger = new CronTrigger(triggerName,
                            triggerGroup, cronExpr);
                    cTrigger.setStartTime(startTime);
                    cTrigger.setEndTime(endTime);
                    cTrigger.setTimeZone(tz);
                    trigger = cTrigger;
                    
                    trigger.setMisfireInstruction(
                            CronTrigger.MISFIRE_INSTRUCTION_DO_NOTHING);
                    
                    I18n.config(log(), "SCHEDBC-4018: "                 //NOI18N
                            + "Scheduling {0} trigger {1} "             //NOI18N
                            + "to fire for condition: {2}",             //NOI18N
                            trigType.getDisplay(),
                            trigger.getFullName(),
                            cronExpr);
                } else {    // invalid Cron expression
                    new CronExpression(cronExpr);   // throws ParseException
                }
                break;
            } case HYBRID: {
                String cronExpr = triggerEx.getCronExpr(); 
                if (CronExpression.isValidExpression(cronExpr)) {
                    long duration = 8 * 60 * 60 * 1000L;
                    if (triggerEx.getDuration() != null) {
                        duration = Long.parseLong(triggerEx.getDuration());
                    }
                    int repeatCount = SimpleTrigger.REPEAT_INDEFINITELY;
                    long repeatInterval = 1000L;

                    if ((triggerEx.getRepeat() != null)
                            && !SchedAttrValue.INDEFINITE.equals(
                                SchedAttrValue.toEnum(triggerEx.getRepeat()))) {
                        repeatCount = Integer.parseInt(triggerEx.getRepeat());
                    }
                    if (triggerEx.getInterval() != null) {
                        repeatInterval = Long.parseLong(triggerEx.getInterval());
                    }
                    HybridTrigger hTrigger = new HybridTrigger(triggerName,
                            triggerGroup, startTime, endTime, cronExpr, tz,
                            duration, repeatCount, repeatInterval,
                            Util.getLogger(mEpMgr.getComponentContext(),
                                    HybridTrigger.class.getName()));
                    trigger = hTrigger;
                    
                    trigger.setMisfireInstruction(HybridTrigger
                        .MISFIRE_INSTRUCTION_RESCHEDULE_NEXT_WITH_EXISTING_COUNT);

                    if (I18n.configLoggable(log())) {
                        I18n.config(log(), "SCHEDBC-4021: " //NOI18N
                                + "Scheduling {0} trigger {1} "         //NOI18N
                                + "to fire for condition: {2} "         //NOI18N
                                + "to last {3} millisecs, repeating "   //NOI18N
                                + "{4} times at {5} millisecs interval",//NOI18N
                                trigType.getDisplay(),
                                trigger.getFullName(),
                                cronExpr, Long.toString(duration),
                                repeatCountStr(repeatCount),
                                Long.toString(repeatInterval));
                    }
                } else { // Invalid Cron Expression
                    new CronExpression(cronExpr);   // throws ParseException
                }
                break;
            } default:
                break;
            }
            
            if (trigger != null) {
                trigger.setDescription(triggerEx.getDescription());
            }
        }
        return trigger;
    }

    private String triggerType(Trigger t) {
        if (t instanceof SimpleTrigger) {
            return SchedAttrValue.SIMPLE.getDisplay();
        } else if (t instanceof CronTrigger) {
            return SchedAttrValue.CRON.getDisplay();
        } else if (t instanceof HybridTrigger) {
            return SchedAttrValue.HYBRID.getDisplay();
        }
        return "???";                                                   //NOI18N
    }
    
    private InOnly sendInOnly(Source source, String[][] props,
            JobExecutionContext jobExecContext) throws JBIException {
        InOnly inOnlyME = null;

        QName schedulerServiceQName = getInfo().getServiceName();
        ServiceEndpoint schedulerEndpoint = mEpMgr.getComponentContext()
            .getEndpoint(schedulerServiceQName, getInfo().getEndpointName());
        
        if (schedulerEndpoint != null) {
            I18n.fine(log(), "SCHEDBC-3001: Found consumer endpoint "   //NOI18N
                    + "{0} for service {1}",                            //NOI18N
                    getInfo().getEndpointName(), getInfo().getServiceName());
            
            ManagerContext mgrContext = mEpMgr.getComponentManager()
                    .getManagerContext();
            MessagingChannel msgChan = mgrContext.getMessagingChannel();
            
            inOnlyME = msgChan.getExchangeFactory().createInOnlyExchange();
            
            inOnlyME.setEndpoint(schedulerEndpoint);
            
            QName consumeOpName = QName.valueOf(mConsumeOperation);
            inOnlyME.setOperation(consumeOpName);
            
            NormalizedMessage inOnlyNM = inOnlyME.createMessage();
            inOnlyME.setInMessage(inOnlyNM);
            inOnlyNM.setContent(source);
            
            for (String[] keyVal : props) {
                inOnlyNM.setProperty(keyVal[0], keyVal[1]);
            }
            
            QName uuidQName = new QName(schedulerServiceQName.getNamespaceURI(),
                    mConsumeOperation);
            String msgId = uuidQName.toString() + "-"                   //NOI18N
                    + inOnlyME.getExchangeId();
            Redelivery.setUniqueId(inOnlyME, msgId);
            
            if (I18n.fineLoggable(log())) {
                I18n.fine(log(), "SCHEDBC-3002: Endpoint {0} sending "  //NOI18N
                        + "one-way message: {1}",                       //NOI18N
                        getInfo().getEndpointName(),
                        XmlUtil.print(source));
            }
            
            mgrContext.getCorrelationMap().put(inOnlyME.getExchangeId(),
                    jobExecContext);

            long sendStartTime = System.currentTimeMillis();

            if (throttle != null) {
                try {
                    throttle.acquire();
                    if (I18n.fineLoggable(log())) {
                        long acquireTime =
                                System.currentTimeMillis() - sendStartTime;
                        I18n.fine(log(), "SCHEDBC-3008: Acquired send " //NOI18N
                                + "permit in {0} milliseconds, "        //NOI18N
                                + "{1} more available",                 //NOI18N
                                Long.toString(acquireTime),
                                Integer.toString(throttle.availablePermits()));
                    }
                } catch (InterruptedException ie) {
                    I18n.warning(log(), "SCHEDBC-6017: Interrupted, "   //NOI18N
                            + "so abort waiting in throttling queue");  //NOI18N
                    return null;
                }
            }

            msgChan.send(inOnlyME);

            I18n.finer(log(), "SCHEDBC-2005: Sent Message Exchange "    //NOI18N
                    + "id={0}, role={1}, status={2}",                   //NOI18N
                    inOnlyME.getExchangeId(), inOnlyME.getRole().toString(),
                    inOnlyME.getStatus().toString());

            // Wait for result of sending to NMR
            synchronized (jobExecContext) {
                boolean interrupted = false;
                while (jobExecContext.getResult() == null) {
                    try {
                        jobExecContext.wait(50);
                    } catch (InterruptedException ie) {
                        interrupted = true;
                    }
                    try {
                        if (jobExecContext.getScheduler().isShutdown()) {
                            if (interrupted) {
                                I18n.warning(log(), "SCHEDBC-6011: "    //NOI18N
                                    + "Interuppted by Scheduler shut "  //NOI18N
                                    + "down, so abort waiting for "     //NOI18N
                                    + "Message Exchange outcome");      //NOI18N
                            } else {
                                I18n.warning(log(), "SCHEDBC-6018: "    //NOI18N
                                    + "Scheduler shut down so abort "   //NOI18N
                                    + "waiting for Message Exchange "   //NOI18N
                                    + "outcome");
                            }
                            break;
                        }
                    } catch (SchedulerException ex) {
                        I18n.warning(log(), "SCHEDBC-6019: Scheduler "  //NOI18N
                                + "problems so abort waiting for "      //NOI18N
                                + "Message Exchange outcome", ex);      //NOI18N
                        break;
                    }
                }
            }

            if (throttle != null) {
                throttle.release();
                if (I18n.fineLoggable(log())) {
                    I18n.fine(log(), "SCHEDBC-3009: Released send "     //NOI18N
                            + "permit, {0} more available",             //NOI18N
                            Integer.toString(throttle.availablePermits()));
                }
            }

            if (I18n.fineLoggable(log()) && (jobExecContext.getResult() != null)
                    && !((SchedulerExchangeHandler.Result)
                            jobExecContext.getResult()).hasFailed()) {
                long sendElapsedTime = System.currentTimeMillis()
                        - sendStartTime;
                I18n.fine(log(), "SCHEDBC-3007: Trigger Message "       //NOI18N
                        + "Exchange took {0} milliseconds",             //NOI18N
                        Long.toString(sendElapsedTime));
            }
        } else {
            I18n.warning(log(), "SCHEDBC-6007: Cannot find Scheduler "  //NOI18N
                    + "consumer endpoint {0} from Component context",   //NOI18N
                    getInfo().getEndpointName());
        }

        return inOnlyME;
    }

    public class SchedulerJob implements Job {
        
        public void execute(JobExecutionContext jobExecContext)
                throws JobExecutionException {
            // If endpoint is suspended, just log and return
            if (mEpMgr.isEndpointSuspended(getInfo().getServiceName(),
                    getInfo().getEndpointName())) {
                I18n.warning(log(), "SCHEDBC-6026: Endpoint {0} "       //NOI18N
                        + "is suspended so ignoring trigger {1}",       //NOI18N
                        mEpMgr.toCompositeEndpointName(
                                getInfo().getServiceName(),
                                getInfo().getEndpointName()),
                        jobExecContext.getTrigger().getFullName());
                return;
            }
            
            JobExecutionException jee = null;
            final String SCHEDBC_6006 = "SCHEDBC-6006: Consumer "       //NOI18N
                    + "endpoint {0} cannot send to (consume) provider " //NOI18N
                    + "endpoint";                                       //NOI18N
            String sendErr =
                    I18n.idLoc(SCHEDBC_6006, getInfo().getEndpointName())[1];
            try {
                WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
                wrapperBuilder.initialize(null, mInputMessage, null);

                Document domDoc = XmlUtil.newDocument();
                domDoc.setXmlVersion("1.0");                            //NOI18N

                String triggerDate = getFormattedDate(jobExecContext, false);
                StringBuilder nmPropNames = new StringBuilder();
                String[][] props = {
                    {SchedulerNMProperty.INBOUND_DATE.getKey(),
                            triggerDate},
                    {SchedulerNMProperty.INBOUND_GROUP.getKey(),
                            jobExecContext.getTrigger().getGroup()},
                    {SchedulerNMProperty.INBOUND_NAME.getKey(),
                            jobExecContext.getTrigger().getName()},
                };
                for (String[] keyVal : props) {
                    nmprops.put(keyVal[0], keyVal[1]);
                    if (nmPropNames.length() > 0) {
                        nmPropNames.append(REDELIVERY_NM_PROPS_DELIM);
                    }
                    nmPropNames.append(keyVal[0]);
                }
                jobExecContext.getMergedJobDataMap().put(REDELIVERY_NM_PROPS,
                        nmPropNames.toString());

                Text payloadText = domDoc.createTextNode(
                        getPayloadText(jobExecContext));

                wrapperBuilder.addPart(mInputPartName,
                        new NodeListImpl(payloadText));

                Source inOnlySrc = new DOMSource(wrapperBuilder.getResult());

                sendInOnly(inOnlySrc, props, jobExecContext);

                if (jobExecContext.getResult() != null) {
                    SchedulerExchangeHandler.Result result =
                            (SchedulerExchangeHandler.Result)
                                jobExecContext.getResult();
                    if (result.hasFailed()) {
                        if (redeliveryConfig != null) {
                            switch (redeliveryConfig.getFailure()) {
                            case delete:
                                I18n.warning(log(), "SCHEDBC-6012: "    //NOI18N
                                    + "Failed {0} attempt(s) to consume "//NOI18N
                                    + "provider endpoint, disposing "   //NOI18N
                                    + "this trigger message instance "  //NOI18N
                                    + "\"{1}\"", Integer.toString(      //NOI18N
                                        redeliveryConfig.getMaxRetries()),
                                    payloadText.getNodeValue());
                                break;
                            case error:
                                jee = new JobExecutionException(sendErr,
                                        result.getError());
                                I18n.warning(log(), "SCHEDBC-6013: "    //NOI18N
                                    + "Failed {0} attempt(s) to consume "//NOI18N
                                    + "provider endpoint, flagging "    //NOI18N
                                    + "this trigger message instance "  //NOI18N
                                    + "\"{1}\" as FAILED in Quartz",    //NOI18N
                                    Integer.toString(
                                        redeliveryConfig.getMaxRetries()),
                                    payloadText.getNodeValue());
                                break;
                            case redirect:
                                I18n.warning(log(), "SCHEDBC-6016: "    //NOI18N
                                        + "Failed {0} attempt(s) to "   //NOI18N
                                        + "consume intended provider "  //NOI18N
                                        + "with this trigger message "  //NOI18N
                                        + "\"{1}\"; redirected to "     //NOI18N
                                        + "{2},{3}",                    //NOI18N
                                        Integer.toString(
                                            redeliveryConfig.getMaxRetries()),
                                        payloadText.getNodeValue(),
                                        (redeliveryConfig.getRedirect() != null)
                                            ? redeliveryConfig.getRedirect()
                                                .getEndpoint().getServiceName()
                                                .toString()
                                            : "???",                    //NOI18N
                                        (redeliveryConfig.getRedirect() != null)
                                            ? redeliveryConfig.getRedirect()
                                                .getEndpoint()
                                                .getEndpointName()
                                            : "???");                   //NOI18N
                                break;
                            case suspend:
                                mEpMgr.suspendEndpoint(
                                        getInfo().getServiceName(),
                                        getInfo().getEndpointName());

                                I18n.warning(log(), "SCHEDBC-6014: "    //NOI18N
                                    + "Failed {0} attempt(s) to consume "//NOI18N
                                    + "provider endpoint with this "    //NOI18N
                                    + "trigger message \"{1}\"; "       //NOI18N
                                    + "Endpoint {2} has been "          //NOI18N
                                    + "auto-SUSPENDED",                 //NOI18N
                                    Integer.toString(
                                        redeliveryConfig.getMaxRetries()),
                                    payloadText.getNodeValue(),
                                    mEpMgr.toCompositeEndpointName(
                                            getInfo().getServiceName(),
                                            getInfo().getEndpointName()));
                                break;
                            }
                        } else {
                            jee = new JobExecutionException(sendErr,
                                    result.getError());
                            I18n.warning(log(), "SCHEDBC-6015: Failed " //NOI18N
                                    + "to consume provider endpoint "   //NOI18N
                                    + "with this trigger message "      //NOI18N
                                    + "\"{0}\"",                        //NOI18N
                                    payloadText.getNodeValue());
                        }
                    }
                }

                if (null == jee) {
                    I18n.fine(log(), "SCHEDBC-3004: Trigger {0} "       //NOI18N
                            + "successfully consumed provider endpoint",//NOI18N
                            jobExecContext.getTrigger().getFullName());
                }
            } catch (Exception ex) {
                I18n.warning(log(), SCHEDBC_6006, ex,
                        getInfo().getEndpointName());
                jee = new JobExecutionException(sendErr, ex);
            }

            if (I18n.fineLoggable(log())) {
                I18n.fine(log(), "SCHEDBC-3005: {0} "                   //NOI18N
                        + "trigger {1} will next fire at: {2}",         //NOI18N
                        triggerType(jobExecContext.getTrigger()),
                        jobExecContext.getTrigger().getFullName(),
                        getFormattedDate(jobExecContext, true));
            }

            if (jee != null) {
                throw jee;
            }
        }

        private String getFormattedDate(JobExecutionContext jobExecContxt,
                boolean next) {
            Date date = (next ? jobExecContxt.getNextFireTime()
                    : jobExecContxt.getScheduledFireTime());
            if (null == date) {
                return SchedAttrValue.NEVER.getDisplay();
            }
            return formatFireDate(jobExecContxt.getMergedJobDataMap(), date);
        }

        private String getPayloadText(JobExecutionContext jobExecContxt) {
            String payloadText = null;
            TriggerDetails td = null;
            String tdStr = (String) jobExecContxt.getMergedJobDataMap()
                    .get(SchedulerComponentManager.SCHED_TRIGGER_DETAILS_PROP);
            if (!Util.isEmpty(tdStr)) {
                td = TriggerDetails.valueOf(tdStr);
            }
            if ((td != null) && (td.getTriggerEx() != null)) {
                payloadText = td.getTriggerEx().getMessage();
            }
            if (Util.isEmpty(payloadText)) {
                payloadText = jobExecContxt.getTrigger().getGroup()
                        + ":" + jobExecContxt.getTrigger().getName()    //NOI18N
                        + " triggered!";                                //NOI18N
            } else {
                payloadText = evalAppVars(payloadText);
            }
            
            return payloadText;
        }
        
        private String evalAppVars(String msg) {
            Matcher matcher = APP_VAR_PATTERN.matcher(msg);
            AppVarTemplate template = messageTemplates.get(msg);
            if (null == template) {
                template = new AppVarTemplate();
                int nextStart = 0;
                while (matcher.find()) {
                    int start = matcher.start(1) - 2;
                    template.addPart(nextStart, start);
                    nextStart = matcher.end(1) + 1;
                    template.addName(matcher.group(1));
                }
                if (nextStart < msg.length()) {
                    template.addPart(nextStart, msg.length());
                }
                messageTemplates.put(msg, template);
            }
            return template.eval(msg);
        }
    }
        
    private class AppVarTemplate {
        private List<int[]> parts;
        private List<String> names;
        
        public AppVarTemplate() {
            super();
            parts = new ArrayList<int[]>();
            names = new ArrayList<String>();
        }
        
        public void addPart(int start, int end) {
            parts.add(new int[] {start, end});
        }
        
        public void addName(String name) {
            names.add(name);
        }
        
        public String eval(String message) {
            StringBuilder sb = new StringBuilder();
            StringBuilder sbMasked = new StringBuilder();
            int nidx = 0;
            for (int[] range : parts) {
                sb.append(message, range[0], range[1]);
                sbMasked.append(message, range[0], range[1]);
                if (nidx < names.size()) {
                    String appVarName = names.get(nidx);
                    
                    // First try defined Application Variables
                    String appVarVal = mEpMgr.getComponentManager()
                            .getSchedulerConfiguration().getAppVar(appVarName);
                    
                    if (null == appVarVal) {
                        // Try NM Inbound properties
                        appVarVal = nmprops.get(appVarName);
                        
                        if (null == appVarVal) {
                            // Try plain old system property
                            appVarVal = System.getProperty(appVarName);

                            if (null == appVarVal) {
                                appVarVal = "${" + appVarName + "}";    //NOI18N
                                if (undefinedAppVars.indexOf(appVarName) == -1) {
                                    undefinedAppVars.append(appVarName)
                                            .append('|');               //NOI18N
                                    I18n.warning(log(), "SCHEDBC-6008: "//NOI18N
                                        + "Application variable {0} "   //NOI18N
                                        + "is undefined", appVarName);  //NOI18N
                                }
                            }
                        }
                    }
                    
                    sb.append(appVarVal);
                    sbMasked.append((VarType.Password.equals(mEpMgr
                            .getComponentManager().getSchedulerConfiguration()
                                    .getAppVarType(appVarName))
                                            ? PASSWORD_MASK : appVarVal));
                    nidx++;
                }
            }
            String evalMessage = sb.toString();
            if (!message.equals(evalMessage)) {
                String evalMessageMasked = sbMasked.toString();
                I18n.fine(log(), "SCHEDBC-3003: Evaluated "             //NOI18N
                        + "message [{0}] to [{1}]",                     //NOI18N
                        message, evalMessageMasked);
            }
            return evalMessage;
        }
    }

    public String getServiceAssemblyName() {
        return serviceAssemblyName;
    }
}
