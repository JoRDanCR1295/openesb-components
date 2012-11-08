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

package com.sun.jbi.engine.iep.core.runtime.util.alert;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.alerter.Alerter;
import com.sun.jbi.alerter.AlerterImpl;
import com.sun.jbi.alerter.EventFactory;
import com.sun.jbi.alerter.NotificationEvent;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;

/**
 *
 * @author rdwivedi
 */
public class IEPSEAlertSender {
    
    private static final Messages mMessages = Messages.getMessages(IEPSEAlertSender.class);
    private static DateFormat simpleDateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS");
    private static NotificationEvent EVENT = new EventFactory().getNotificationEvent();
    /* The name of the component */
    private static final String COMPONENT_NAME = "sun-iep-engine";
    /* The type of the component. Service Engine in this case */
    private static final String COMPONENT_TYPE = "ServiceEngine";
    /* Instance of IEPSEAlertSender */
    private static IEPSEAlertSender iepAlerter;
    
    private IEPSEAlertSender() {}
    static {
        iepAlerter = new IEPSEAlertSender();
    }

    /**
     * 
     * @return
     */
    public static IEPSEAlertSender getInstance() {
        return iepAlerter;
    }
    public void alertEngineStatusChange(String engineId, int operationState) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = mMessages.getString("IEPSEAlertSender.Engine_Status_Changed", 
                    engineId, EVENT.getOpStateString(operationState), 
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));
            alerter.info(message, COMPONENT_NAME, null, null, COMPONENT_TYPE/*NotificationEvent.COMPONENT_TYPE_IEP*/,
                    operationState, NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            mMessages.log(Level.WARNING,"IEPSEAlertSender.Failed_To_Send_AlertForEngineStatus_Change", engineId, operationState);
        }
    }
    
    public void alertUnableToConnectToDB(String engineId, String cause) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = mMessages.getString("IEPSEAlertSender.Unable_To_Connect_Database",
                    engineId, cause, simpleDateFormat.format(new Date(System.currentTimeMillis())));
            mMessages.log(Level.WARNING,"IEPSEAlertSender.Unable_To_Connect_Database",
                    engineId, cause, simpleDateFormat.format(new Date(System.currentTimeMillis())));
            alerter.major(message, COMPONENT_NAME,
                    null, null, COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
                mMessages.log(Level.WARNING,"IEPSEAlertSender.Failed_To_Send_AlertFor_Unable_to_connect_to_DB", engineId, cause);
            
        }
    }
    
    public void alertDBConnectionRestored(String engineId) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = mMessages.getString("IEPSEAlertSender.DB_Connection_restored", engineId,
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));
            mMessages.log(Level.WARNING,"IEPSEAlertSender.Unable_To_Connect_Database",engineId,
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));
            alerter.major(message, COMPONENT_NAME,
                    null, null, COMPONENT_TYPE,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            
               mMessages.log(Level.WARNING,"IEPSEAlertSender.Failed_To_Send_AlertFor_DB_Connection_restored", 
                        engineId);
        }
    }    

    public void alertSUStatusChange(String engineId, String suName,
            int operationState) {
        try {
            Alerter alerter = new AlerterImpl();
            String message = mMessages.getString("IEPSEAlertSender.SU_Status_Changed", engineId,
                    suName, EVENT.getOpStateString(operationState), 
                    simpleDateFormat.format(new Date(System.currentTimeMillis())));
            alerter.info(message, COMPONENT_NAME,
                    suName, null, COMPONENT_TYPE,
                    operationState, NotificationEvent.EVENT_TYPE_ALERT, null);
        } catch (Exception e) {
            // log exception and continue.
            
                mMessages.log(Level.WARNING,"IEPSEAlertSender.Failed_To_Send_AlertFor_SU_Status_Changed", engineId, suName, operationState);
           
        }
    }
}
