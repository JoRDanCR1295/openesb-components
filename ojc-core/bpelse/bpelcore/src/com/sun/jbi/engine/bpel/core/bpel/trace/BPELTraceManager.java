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
 * @(#)BPELTracer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.trace;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.jxpath.Pointer;
import org.w3c.dom.Node;

import com.sun.bpel.model.From;
import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.alerter.Alerter;
import com.sun.jbi.alerter.AlerterImpl;
import com.sun.jbi.alerter.EventFactory;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.util.NDC;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.management.BPELSEManagement;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.FromEvaluatorFactory;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.FromEvaluatorFactory.FromEvaluator;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.FileHandler;

/**
 *
 *
 *
 * @author Sun Microsystems
 */
public class BPELTraceManager {

    /**
     * The category under which BPEL processes will log
     */
    public static final String BPEL_TRACE_CATEGORY = BPELTraceManager.class.getName();//"sun-bpel-engine.Application";

    /* Current logger can return a logger only with a class. Until new logging api's are available, use
     * BPELTraceManager as the log category for BPEL process logging.
     */
    private static final Logger BPEL_LOGGER = Logger.getLogger(BPELTraceManager.class.getName());

    /* Logger for the BPELTraceManager class */
    private static final Logger LOGGER = Logger.getLogger(BPELTraceManager.class.getName());

    /* The name of the component */
    private static final String COMPONENT_NAME = "sun-bpel-engine";

    /* The type of the component. Service Engine in this case */
    private static final String COMPONENT_TYPE = "ServiceEngine";

    /* The server type */
    private static final String SERVER_TYPE = "glassfish";

    /* */
    private static final String LINE_SEPARATOR = System.getProperty("line.separator", "\n");

    /* Instance of BPELTraceManager */
    private static BPELTraceManager bpelTraceManager;

    private static DateFormat simpleDateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS");

    private static NotificationEvent EVENT = new EventFactory().getNotificationEvent();

    /*
     * Private constructor
     */
    private BPELTraceManager() {
    }

    static {
        bpelTraceManager = new BPELTraceManager();
    }

    /**
     *
     * @return
     */
    public static BPELTraceManager getInstance() {
        return bpelTraceManager;
    }

    /**
     * Could be called for Virtual activities like CompensateUnit for which the
     * RActivity is null, hence care is taken to avoid trace calls.
     *
     * @param trace
     * @param context
     */
    public void doTraceOnStart(RActivity activity, Context context,
            BPELProcessInstance process) {
        if (activity != null) {
            Trace trace = activity.getTrace();
            if (trace != null) {
                if (trace.hasOnStartLogs()) {
                    logMessages(trace.getOnStartLogs().iterator(), context,
                            activity.getName(), process);
                }

                if (trace.hasOnStartAlerts()) {
                    sendAlerts(trace.getOnStartAlerts().iterator(), context);
                }
            }
        }
    }

    /**
     * Could be called for Virtual activities like CompensateUnit for which the
     * RActivity is null, hence care is taken to avoid trace calls.
     *
     * @param trace
     * @param context
     */
    public void doTraceOnComplete(RActivity activity, Context context,
            BPELProcessInstance process) {
        if (activity != null) {
            Trace trace = activity.getTrace();
            if (trace != null) {
                if (trace.hasOnCompleteLogs()) {
                    logMessages(trace.getOnCompleteLogs().iterator(), context,
                            activity.getName(), process);
                }

                if (trace.hasOnCompleteAlerts()) {
                    sendAlerts(trace.getOnCompleteAlerts().iterator(), context);
                }
            }
        }
    }

    /*
     * 
     */
    private void logMessages(Iterator<Log> logIter, Context context, String actName, BPELProcessInstance process) {
        if (!process.getBPELProcessManager().isLoggingEnabled()) {
            return;
        }

        String assemblyName = process.getBPELProcessManager().getServiceAssemblyName();

        Logger serviceAssemblyLogger = getServiceAssemblyLogger(assemblyName);

        NDC ndc = null;
        try {
            List<String> contextNDC = new ArrayList(20);
            contextNDC.add("Service Assembly Name");
            contextNDC.add(assemblyName);
            contextNDC.add("BPEL Process Name");
            contextNDC.add(process.getBPELProcessManager().getBPELProcess().getName());
            contextNDC.add("Process Instance Id");
            contextNDC.add(process.getId());
            contextNDC.add("Activity Name");
            contextNDC.add(actName);
            contextNDC.addAll(process.getBPELProcessManager().getExtraNDC());

            Object [] arrContextNDC = contextNDC.toArray();
            ndc = NDC.enter(arrContextNDC);

            while (logIter.hasNext()) {
                Log log = logIter.next();
                try {
                    String level = log.getLevel();
                    From from = log.getFrom();

                    if (level.equals(Log.SEVERE)) {
                        serviceAssemblyLogger.log(Level.SEVERE, evaluateFrom(arrContextNDC, from, context), arrContextNDC);
                    } else if (level.equals(Log.WARNING)) {
                        if (serviceAssemblyLogger.isLoggable(Level.WARNING)) {
                            serviceAssemblyLogger.log(Level.WARNING, evaluateFrom(arrContextNDC, from, context), arrContextNDC);
                        }
                    } else if (level.equals(Log.CONFIG)) {
                        if (serviceAssemblyLogger.isLoggable(Level.CONFIG)) {
                            serviceAssemblyLogger.log(Level.CONFIG, evaluateFrom(arrContextNDC, from, context), arrContextNDC);
                        }
                    } else if (level.equals(Log.INFO)) {
                        if (serviceAssemblyLogger.isLoggable(Level.INFO)) {
                            serviceAssemblyLogger.log(Level.INFO, evaluateFrom(arrContextNDC, from, context), arrContextNDC);
                        }
                    } else if (level.equals(Log.FINE)) {
                        if (serviceAssemblyLogger.isLoggable(Level.FINE)) {
                            serviceAssemblyLogger.log(Level.FINE, evaluateFrom(arrContextNDC, from, context), arrContextNDC);
                        }
                    } else if (level.equals(Log.FINER)) {
                        if (serviceAssemblyLogger.isLoggable(Level.FINER)) {
                            serviceAssemblyLogger.log(Level.FINER, evaluateFrom(arrContextNDC, from, context), arrContextNDC);
                        }
                    } else if (level.equals(Log.FINEST)) {
                        if (serviceAssemblyLogger.isLoggable(Level.FINEST)) {
                            serviceAssemblyLogger.log(Level.FINEST, evaluateFrom(arrContextNDC, from, context), arrContextNDC);
                        }
                    } else {
                        LOGGER.warning(I18n.loc("BPCOR-6086: Invalid log level '{0}' specified at location {1}.",
                                level, log.getLocator().getLineNumber()));
                    }
                } catch (Exception e) {
                    // log exception and continue.
                    LOGGER.warning(I18n.loc("BPCOR-6087: Encountered an exception while logging at location {0}. "
                            + "Exception: {1}.", log.getLocator().getLineNumber(), e));
                }
            }
        } finally {
            // exit ndc
            if (ndc != null) {
                ndc.exit();
            }
        }
    }

    private Logger getServiceAssemblyLogger(String serviceAssemblyName) {
            String loggerName
                    = new StringBuilder(BPELTraceManager.class.getName())
                            .append('.')
                            .append(serviceAssemblyName)
                            .toString();
            return Logger.getLogger(loggerName);
            /*
        try {    
            logger.addHandler(new FileHandler("Users/david/openesb-standalone-packaging-3.0.2/logs/" + serviceAssemblyName + ".log", true));
        } catch (IOException ex) {
            Logger.getLogger(BPELTraceManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SecurityException ex) {
            Logger.getLogger(BPELTraceManager.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return logger;
            */
    }

    private static String buildString(Object[] ctx) {
        StringBuilder buff = new StringBuilder();

        int len = ctx.length;
        if (len == 1) {
            buff.append("[{0}]");
        } else if ((len % 2) == 0) {	// even count, key-value pairs
            buff.append('[');
            boolean comma = false;
            // ndc prints out key values in backwards order, so shall we
            for (int i = (len - 1); i >= 0; i -= 2) {
                if (comma) {
                    buff.append(',');
                }
                buff.append('{').append((i - 1)).append("}={").append(i).append('}');
                comma = true;
            }
            buff.append(']');
        }

        return buff.toString();
    }

    /*
     * 
     */
    private void sendAlerts(Iterator<Alert> alertIter, Context context) {
        while (alertIter.hasNext()) {
            Alert alert = alertIter.next();
            try {
                String level = alert.getLevel();
                From from = alert.getFrom();
                Alerter alerter = new AlerterImpl();
                String deploymentName = context.getProcessInstance().getBPELProcessManager().getServiceUnitName();

                // Developer Note: Alerter also has a severity level 'FATAL'. This is not applicable to 
                // alerts from within BPEL.
                if (level.equals(Alert.CRITICAL)) {
                    alerter.custom(NotificationEvent.EVENT_TYPE_ALERT, NotificationEvent.SEVERITY_TYPE_CRITICAL,
                            evaluateFrom(null, from, context), COMPONENT_NAME, deploymentName, SERVER_TYPE,
                            COMPONENT_TYPE, NotificationEvent.OPERATIONAL_STATE_RUNNING, null);
                } else if (level.equals(Alert.MAJOR)) {
                    alerter.custom(NotificationEvent.EVENT_TYPE_ALERT, NotificationEvent.SEVERITY_TYPE_MAJOR,
                            evaluateFrom(null, from, context), COMPONENT_NAME, deploymentName, SERVER_TYPE,
                            COMPONENT_TYPE, NotificationEvent.OPERATIONAL_STATE_RUNNING, null);
                } else if (level.equals(Alert.MINOR)) {
                    alerter.custom(NotificationEvent.EVENT_TYPE_ALERT, NotificationEvent.SEVERITY_TYPE_MINOR,
                            evaluateFrom(null, from, context), COMPONENT_NAME, deploymentName, SERVER_TYPE,
                            COMPONENT_TYPE, NotificationEvent.OPERATIONAL_STATE_RUNNING, null);
                } else if (level.equals(Alert.WARNING)) {
                    alerter.custom(NotificationEvent.EVENT_TYPE_ALERT, NotificationEvent.SEVERITY_TYPE_WARNING,
                            evaluateFrom(null, from, context), COMPONENT_NAME, deploymentName, SERVER_TYPE,
                            COMPONENT_TYPE, NotificationEvent.OPERATIONAL_STATE_RUNNING, null);
                } else if (level.equals(Alert.INFO)) {
                    alerter.custom(NotificationEvent.EVENT_TYPE_ALERT, NotificationEvent.SEVERITY_TYPE_INFO,
                            evaluateFrom(null, from, context), COMPONENT_NAME, deploymentName, SERVER_TYPE,
                            COMPONENT_TYPE, NotificationEvent.OPERATIONAL_STATE_RUNNING, null);
                } else {
                    LOGGER.warning(I18n.loc("BPCOR-6088: Invalid alert level '{0}' specified at location {1}.",
                            level, alert.getLocator().getLineNumber()));
                }
            } catch (Exception e) {
                // log exception and continue.
                LOGGER.warning(I18n.loc("BPCOR-6089: Encountered an exception while sending an alert at "
                        + "location {0}. Exception: {1}.", alert.getLocator().getLineNumber(), e));
            }
        }
    }

    /*
     * Evaluates <code>from</code> and returns a <code>String</code>.
     */
    private String evaluateFrom(Object [] contextNDC, From from, Context context) throws Exception {

        StringBuilder traceMessage = null;
        
        if (contextNDC != null) {
            traceMessage = new StringBuilder(buildString(contextNDC));
        } else {
            traceMessage = new StringBuilder();
        }
        
        FromEvaluator fromEval = FromEvaluatorFactory.getFromEvaluator(from);
        Object fromVal = fromEval.evaluateFrom(from, context, null);

        if (fromVal instanceof Iterator) {
        //    StringBuffer stringBuffer = new StringBuffer();
            Iterator<?> sourcePtrIterator = (Iterator<?>) fromVal;
            while (sourcePtrIterator.hasNext()) {
                Pointer currentSourcePtr = (Pointer) sourcePtrIterator.next();
                /*
                 * The source can be an DOMAttributePointer, Object (string,
                 * etc), or a Node represent an element with simple context.
                 * sourcePtr.getValue().toString()) will work for all three
                 * cases.
                 */
                traceMessage.append(currentSourcePtr.getValue().toString());
                if (sourcePtrIterator.hasNext()) {
                    traceMessage.append(LINE_SEPARATOR);
                }
            }
        //    traceMessage = stringBuffer.toString();
        } else if (fromVal instanceof WSMessage) {
            // JBIMessageImpl implements WSMessage and has overridden the toString() method to 
            // give relevant output. Hence using that method.
            traceMessage.append(fromVal.toString());
        } else if (fromVal instanceof Node) {
            traceMessage.append(DOMHelper.createXmlString((Node) fromVal));
        } else {
            // Most probably it is a String, Number or Boolean. Do a simple toString on it.
            traceMessage.append(fromVal.toString());
        }

        return traceMessage.toString();
    }

    public void alertEngineStatusChange(String engineId, int operationState) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = I18n.loc("BPCOR-5005: Engine {0} status changed to {1} at {2}",
                    engineId, EVENT.getOpStateString(operationState),
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));
            alerter.info(message, COMPONENT_NAME, null, null, NotificationEvent.COMPONENT_TYPE_BPEL,
                    operationState, NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            try {
                LOGGER.warning(I18n.loc("BPCOR-6139: Failed to send an alert for engine status change : "
                        + "Engine {0}, status {1}", engineId, operationState));
            } catch (Exception ex) {
            }
        }
    }

    public void alertUnableToCreateInstance(String engineId, String bpName,
            Throwable th) {
        try {
            Alerter alerter = new AlerterImpl();
            String cause = null;
            if (th != null) {
                StringWriter strWrt = new StringWriter();
                PrintWriter pr = new PrintWriter(strWrt);
                th.printStackTrace(pr);
                strWrt.flush();
                cause = strWrt.getBuffer().toString();
            }
            String message = I18n.loc("BPCOR-7010: Engine {0} unable to create process instance for"
                    + " : {1} at {3}. Cause : {2}", engineId, bpName, cause,
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));

            alerter.critical(message, COMPONENT_NAME,
                    null, null, NotificationEvent.COMPONENT_TYPE_BPEL,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            try {
                LOGGER.warning(I18n.loc("BPCOR-6140: Failed to send an alert for unable to create instance: "
                        + "Engine {0}, bp {1}", engineId, bpName));
            } catch (Exception ex) {
            }
        }
    }

    public void alertBPInstanceChangeByAPI(String engineId, String bpName,
            String instanceId, String varName,
            BPELSEManagement.ActionType actionType) {
        try {
            Alerter alerter = new AlerterImpl();
            String msg = null;
            if (varName != null) {
                msg = I18n.loc("BPCOR-5006: Business process instance variable changed by API at {4}. Engine : {0}, "
                        + "process: {1}, instance: {2}, variable: {3}", engineId, bpName, instanceId, varName,
                        simpleDateFormat.format(new Date(System.currentTimeMillis())));
            } else {
                msg = I18n.loc("BPCOR-5007: Business process instance {3}  by API at {4} . Engine : {0}, "
                        + "process: {1}, instance: {2}", engineId, bpName, instanceId, actionType.toString(),
                        simpleDateFormat.format(new Date(System.currentTimeMillis())));
            }
            alerter.info(msg, COMPONENT_NAME, null, null,
                    NotificationEvent.COMPONENT_TYPE_BPEL,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            try {
                LOGGER.warning(I18n.loc("BPCOR-6141: Failed to send an alert for instance state change by API : "
                        + "Engine {0}, process: {1}, instance: {2}, type {3}", engineId, bpName, instanceId,
                        actionType.toString()));
            } catch (Exception ex) {
            }
        }

    }

    public void alertBPInstanceTerminatedOnUnHandledFault(String engineId,
            String bpName, String instanceId, String cause) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = I18n.loc("BPCOR-6142: Business process instance terminated on unhandled fault at {4}. "
                    + "Engine : {0}, process: {1}, instance: {2}. Cause : {3}", engineId, bpName, instanceId, cause,
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));
            alerter.major(message, COMPONENT_NAME,
                    null, null, NotificationEvent.COMPONENT_TYPE_BPEL,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            try {
                LOGGER.warning(I18n.loc("BPCOR-6143: Failed to send an alert for instance terminated on unhandled "
                        + "fault : Engine : {0}, process: {1}, instance: {2},  Cause : {3}", engineId, bpName,
                        instanceId, cause));
            } catch (Exception ex) {
            }
        }
    }

    public void alertUnableToConnectToDB(String engineId, String cause) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = I18n.loc("BPCOR-6144: Unable to connect to database at {2}. Engine : {0}, Cause : {1}",
                    engineId, cause, simpleDateFormat.format(new Date(System.currentTimeMillis())));
            LOGGER.warning(message);
            alerter.major(message, COMPONENT_NAME,
                    null, null, NotificationEvent.COMPONENT_TYPE_BPEL,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            try {
                LOGGER.warning(I18n.loc("BPCOR-6145: Failed to send an alert for unable to connect to DB. "
                        + "Engine : {0}, Cause : {1}", engineId, cause));
            } catch (Exception ex) {
            }
        }
    }

    public void alertDBConnectionRestored(String engineId) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = I18n.loc("BPCOR-6146: DB connection restored at {1}. Engine : {0}", engineId,
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));
            LOGGER.info(message);
            alerter.major(message, COMPONENT_NAME,
                    null, null, NotificationEvent.COMPONENT_TYPE_BPEL,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            try {
                LOGGER.warning(I18n.loc("BPCOR-6147: Failed to send an alert for DB Connection restored : Engine {0}",
                        engineId));
            } catch (Exception ex) {
            }
        }
    }

    public void alertSUStatusChange(String engineId, String suName,
            int operationState) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = I18n.loc("BPCOR-5008: SU {1} on Engine {0} changed status to {2} at {3}", engineId,
                    suName, EVENT.getOpStateString(operationState),
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));
            alerter.info(message, COMPONENT_NAME,
                    suName, null, NotificationEvent.COMPONENT_TYPE_BPEL,
                    operationState, NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            try {
                LOGGER.warning(I18n.loc("BPCOR-6148: Failed to send an alert for SU status chang : Engine {0}, "
                        + "SU {1}, status {1}", engineId, suName, operationState));
            } catch (Exception ex) {
            }
        }
    }

}
