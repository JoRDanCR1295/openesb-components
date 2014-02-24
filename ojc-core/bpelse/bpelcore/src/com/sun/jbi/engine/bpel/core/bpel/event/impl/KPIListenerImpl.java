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
 * @(#)$Id: KPIListenerImpl.java,v 1.6 2008/04/30 17:22:28 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;


import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import com.sun.bpel.model.Invoke;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.event.ActivityEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventListener;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELInstanceEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.util.EventProcessHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


public class KPIListenerImpl implements BPELEventListener {
    private XmlResourceProviderPool resourcePool = null;
    private boolean mDoProcess = false;
    private EventProcessHelper mEventHelper = null;

    private static final Logger LOGGER = Logger.getLogger(KPIListenerImpl.class.getName());

	public void processEvent(BPELEvent event) {
        if (mDoProcess) {
            switch (event.getEventType()) {
            case BP_START:
                startBPI(event);
                break;
            case BP_COMPLETE:
                completeBPI(event);
                break;
            case ACTIVITY_START:
                startBPActivity(event);
                break;
            case ACTIVITY_COMPLETED:
                completeBPActivity(event);
                break;
            case BP_TERMINATE:
                 terminatedBPI(event);
                 break;
            case BP_SUSPEND:
                 suspendBPI(event);
                 break;
            case BP_RESUME:
                resumedBPI(event);
                 break;
            case BP_FAULT:
                faultBPI(event);
                break;
            case ACTIVITY_FAULTED:
                faultBPActivity(event);
                break;
            case VARIABLE_CHANGED:
                variableChanged(event);
                break;
            default:
                if (LOGGER.isLoggable(Level.FINE)) {
                    LOGGER.log(Level.FINE, I18n.loc("KPI for {0} is not implemented", event.getEventType()));
                }
            }
        }
    }

	
	private String fillInInstanceEventValues(String template, BPELInstanceEvent event) {
		String mstr =  replaceAll(template,"${1}",event.getEventId());
		mstr =  replaceAll(mstr,"${2}",""+event.getEngineId());
		mstr =  replaceAll(mstr,"${3}",""+event.getBPELName());
		mstr =  replaceAll(mstr,"${4}",""+event.getInstanceId());
		mstr =  replaceAll(mstr,"${5}",""+event.getTimeStamp().getTime());
		return mstr;

	}

	private String fillInActivityEventValues(String template, ActivityEvent event) {
        String mstr =  replaceAll(template,"${1}",event.getEventId());
        mstr =  replaceAll(mstr,"${2}",""+event.getEngineId());
        mstr =  replaceAll(mstr,"${3}",""+event.getBPELName());
        mstr =  replaceAll(mstr,"${4}",""+event.getInstanceId());
        mstr =  replaceAll(mstr,"${5}",""+event.getTimeStamp().getTime());
        mstr =  replaceAll(mstr,"${6}",""+event.getActivityId());
        mstr =  replaceAll(mstr,"${7}",""+event.getActivityName());
        mstr =  replaceAll(mstr,"${8}",""+event.getActivityXpath());

        return mstr;
	}

    private static final String startBPIEventMsgStr = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorInstances_iep\" name=\"input\" type=\"msgns:BPInstanceStartEventStream_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:BPInstanceStartEventStream_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime></msgns:BPInstanceStartEventStream_MsgObj></jbi:part></jbi:message>";
	
	private void startBPI(BPELEvent event) {
			String mstr =  fillInInstanceEventValues(startBPIEventMsgStr, (BPELInstanceEvent)event);
			mEventHelper.sendKPIMEx(
			        new QName("MonitorInstances_iep", "InputService"), "BPInstanceStartEventStream", mstr);
	}


private static final String startBPIEventFault =
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorInstances_iep\" name=\"input\" type=\"msgns:BPInstanceFault_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:BPInstanceFault_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime></msgns:BPInstanceFault_MsgObj></jbi:part></jbi:message>";
      	private void faultBPI(BPELEvent event) {
			String mstr =  fillInInstanceEventValues(startBPIEventFault, (BPELInstanceEvent)event);
                        if (LOGGER.isLoggable(Level.FINE)) {
                          LOGGER.log(Level.FINE, I18n.loc("KPI InstanceFault XML={0}",mstr));
                        }
			mEventHelper.sendKPIMEx(
			        new QName("MonitorInstances_iep", "InputService"), "BPInstanceFault", mstr);
	}


   private static final String startBPIEventSuspend =
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorInstances_iep\" name=\"input\" type=\"msgns:BPInstanceSuspended_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:BPInstanceSuspended_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime></msgns:BPInstanceSuspended_MsgObj></jbi:part></jbi:message>";
      	private void suspendBPI(BPELEvent event) {
			String mstr =  fillInInstanceEventValues(startBPIEventSuspend, (BPELInstanceEvent)event);
                        if (LOGGER.isLoggable(Level.FINE)) {
                          LOGGER.log(Level.FINE, I18n.loc("KPI InstanceSuspended XML={0}",mstr));
                        }
			mEventHelper.sendKPIMEx(
			        new QName("MonitorInstances_iep", "InputService"), "BPInstanceSuspended", mstr);
	}

   private static final String startBPIEventResumed =
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorInstances_iep\" name=\"input\" type=\"msgns:BPInstanceResumed_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:BPInstanceResumed_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime></msgns:BPInstanceResumed_MsgObj></jbi:part></jbi:message>";
      	private void resumedBPI(BPELEvent event) {
			String mstr =  fillInInstanceEventValues(startBPIEventResumed, (BPELInstanceEvent)event);
                        if (LOGGER.isLoggable(Level.FINE)) {
                          LOGGER.log(Level.FINE, I18n.loc("KPI InstanceResumed XML={0}",mstr));
                        }
			mEventHelper.sendKPIMEx(
			        new QName("MonitorInstances_iep", "InputService"), "BPInstanceResumed", mstr);
	}

    private static final String startBPIEventTerminated =
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorInstances_iep\" name=\"input\" type=\"msgns:BPInstanceTerminated_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:BPInstanceTerminated_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime></msgns:BPInstanceTerminated_MsgObj></jbi:part></jbi:message>";
      	private void terminatedBPI(BPELEvent event) {
			String mstr =  fillInInstanceEventValues(startBPIEventTerminated, (BPELInstanceEvent)event);
                        if (LOGGER.isLoggable(Level.FINE)) {
                          LOGGER.log(Level.FINE, I18n.loc("KPI InstanceTerminated XML={0}",mstr));
                        }
			mEventHelper.sendKPIMEx(
			        new QName("MonitorInstances_iep", "InputService"), "BPInstanceTerminated", mstr);
	}

    private static final String completeBPIEventMsgStr = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorInstances_iep\" name=\"input\" type=\"msgns:BPInstanceComplete_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:BPInstanceComplete_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime></msgns:BPInstanceComplete_MsgObj></jbi:part></jbi:message>";
	
	private void completeBPI(BPELEvent event) {
			String mstr =  fillInInstanceEventValues(completeBPIEventMsgStr, (BPELInstanceEvent)event);
			mEventHelper.sendKPIMEx(new QName("MonitorInstances_iep", "InputService"),"BPInstanceComplete", mstr);
	}

    private static final String startBPActivityEventMsgStr = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorActivities_iep\" name=\"input\" type=\"msgns:ActivityStart_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:ActivityStart_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime><msgns:activityID>${6}</msgns:activityID><msgns:activityName>${7}</msgns:activityName><msgns:activityXPath>${8}</msgns:activityXPath></msgns:ActivityStart_MsgObj></jbi:part></jbi:message>";

	private void startBPActivity(BPELEvent event) {
            if (isInvoke (event)) {            
                String msgStr = fillInActivityEventValues(startBPActivityEventMsgStr, (ActivityEvent)event);
                mEventHelper.sendKPIMEx(new QName("MonitorActivities_iep", "InputService"), "ActivityStart", msgStr);
            }
	}


    private static final String startBPActivityFaultStr = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorActivities_iep\" name=\"input\" type=\"msgns:ActivityFault_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:ActivityFault_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime><msgns:activityID>${6}</msgns:activityID><msgns:activityName>${7}</msgns:activityName><msgns:activityXPath>${8}</msgns:activityXPath></msgns:ActivityFault_MsgObj></jbi:part></jbi:message>";

	private void faultBPActivity(BPELEvent event) {
            if (isInvoke (event)) {
                String msgStr = fillInActivityEventValues(startBPActivityFaultStr, (ActivityEvent)event);
                if (LOGGER.isLoggable(Level.FINE)) {
                          LOGGER.log(Level.FINE, I18n.loc("KPI ActivityFault XML={0}",msgStr));
                        }
                mEventHelper.sendKPIMEx(new QName("MonitorActivities_iep", "InputService"), "ActivityFault", msgStr);
            }
	}


     private void variableChanged(BPELEvent event)   {
          VariableEvent ve=(VariableEvent)event;
          String msgStr=ve.toXML();
           msgStr="<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorActivities_iep\" name=\"input\" type=\"msgns:VariableChange_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part>" +
                   "<msgns:VariableChange_MsgObj>" +
                   ve.toXML()+
                   "</msgns:VariableChange_MsgObj>" +
                   "</jbi:part></jbi:message>";
          if (LOGGER.isLoggable(Level.FINE)) {
                          LOGGER.log(Level.FINE, I18n.loc("KPI Variable Change XML={0}",msgStr));
           }
           mEventHelper.sendKPIMEx(new QName("MonitorActivities_iep", "InputService"), "VariableChange", msgStr);
     }

    private static boolean isInvoke (BPELEvent event) {        
        ActivityEvent actEvent = (ActivityEvent) event;
        String xpath = actEvent.getActivityXpath();
        String act = null;
        int lastComponentStart = xpath.lastIndexOf("/");
        if (lastComponentStart != -1) {
            lastComponentStart = lastComponentStart + 1;
            act = xpath.substring(lastComponentStart);
        } else {
            act = xpath;
        }        
        if (act.indexOf(Invoke.TAG) != -1) {
            return true;
        }
        return false;
    }

    private static final String completeBPActivityEventMsgStr = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><jbi:message xmlns:msgns=\"MonitorActivities_iep\" name=\"input\" type=\"msgns:ActivityFinished_Msg\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><msgns:ActivityFinished_MsgObj><msgns:eventID>${1}</msgns:eventID><msgns:engineID>${2}</msgns:engineID><msgns:bpID>${3}</msgns:bpID><msgns:processID>${4}</msgns:processID><msgns:eventTime>${5}</msgns:eventTime><msgns:activityID>${6}</msgns:activityID><msgns:activityName>${7}</msgns:activityName><msgns:activityXPath>${8}</msgns:activityXPath></msgns:ActivityFinished_MsgObj></jbi:part></jbi:message>";

	private void completeBPActivity(BPELEvent event) {
        if (isInvoke (event)) { 
			String msgStr = fillInActivityEventValues(completeBPActivityEventMsgStr, (ActivityEvent)event);
			mEventHelper.sendKPIMEx(new QName("MonitorActivities_iep", "InputService"), "ActivityFinished", msgStr);
        }
	}

	public void init(EventProcessHelper eventHelper) {
		resourcePool = (XmlResourceProviderPool)BPELSERegistry.getInstance()
        .lookup(XmlResourceProviderPool.class.getName());    
            mEventHelper = eventHelper;
		if (eventHelper.getEngine().isKPIEnabled()) {
			mDoProcess = true;
		}
	}

	public void resetProperties(Properties properties) {
		LOGGER.log(Level.INFO, "Reset Properties");
		String kpiEnabled = properties.getProperty(Engine.KPI_ENABLED);
        if (kpiEnabled != null && kpiEnabled.equalsIgnoreCase("true")) {
        	mDoProcess = true;         	
        } else if (kpiEnabled != null && !kpiEnabled.equalsIgnoreCase("true")) {
        	mDoProcess = false;
        }
	}


	private  String replaceAll(String s, String match, String replacement) {
        StringBuffer sb = new StringBuffer();
        String temp = s;
        while (true) {
            int i = temp.indexOf(match);
            if (i < 0) {
                sb.append(temp);
                return sb.toString();
            }
            sb.append(temp.substring(0, i));
            sb.append(replacement);
            temp = temp.substring(i + match.length());
        }
    }

    public void shutdown() {
    }
    

}
